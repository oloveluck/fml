#lang racket/base

;;; check.rkt — a map checked against its structures: the spec-author's loop for mappings.
;;;
;;; `check-map` reports, as fhir/diagnostics positioned in the FML (rules, sources and targets
;;; remember where they were parsed), what the two StructureDefinitions already decide:
;;;   - a source path with no such element on the source type ("`nmae` is not an element of Patient");
;;;   - a target path the target type lacks;
;;;   - a value whose type cannot go into the element (a string literal into HumanName, a complex
;;;     variable into a primitive element, a primitive into a complex one);
;;;   - a group call with the wrong number of arguments, or naming a group neither this map nor its
;;;     imports has;
;;;   - a required element of the target type (min >= 1) the entry group never assigns (a warning:
;;;     a dependent group may fill it).
;;; Types flow as in the engine: a parameter's declared type, the element types the registry
;;; knows, a variable bound by `as`. Anything unknown stops that branch — fail-open, never a
;;; finding on what the structures do not decide.
;;; `check-map/examples` runs the map over example sources and validates each result (fhir-validation,
;;; loaded on demand); a validation issue is traced to the rule that wrote the element.

(require racket/contract racket/list racket/string racket/match
         "parser/ast.rkt"
         (only-in "transform.rkt" uses-types map-imports run-map)
         (only-in fhir/diagnostic diagnostic attached-loc loc)
         (only-in fhir/model/introspect full-field-specs)
         (only-in fhir/model/schema field-spec? fs-name fs-min fs-ftype ftype-type? ftype-type-name ftype-prim? ftype-prim-code
                  choice-group? choice-group-members)
         (only-in fhir/model/registry type-registered? primitive-name?)
         (only-in fhir/serialize/from-json from-json)
         (only-in fhir/result/result ok? ok-value))

(provide (contract-out
          ;; check-map : map [#:source file] -> (listof diagnostic)   the static findings
          [check-map (->* (fml-map?) (#:source (or/c string? path? #f)) (listof diagnostic?))]
          ;; check-map/examples : map examples [#:profiles sds #:source file] -> (listof diagnostic)
          ;; each example (FHIR JSON) transformed, the result validated against its base type and the
          ;; profiles; issues positioned at the rule that wrote the element
          [check-map/examples (->* (fml-map? (listof hash?)) (#:profiles (listof hash?) #:source (or/c string? path? #f))
                                   (listof diagnostic?))]))

;; ---- types ----------------------------------------------------------------------
;; A Ty is a symbol (a registered complex type) | (prim code) | #f (unknown: stop checking)
(struct prim (code) #:transparent)
(define (ty->string ty) (cond [(prim? ty) (prim-code ty)] [(symbol? ty) (symbol->string ty)] [else "?"]))
(define (ftype->ty ft)
  (cond [(ftype-prim? ft) (prim (ftype-prim-code ft))]
        [(and (ftype-type? ft) (type-registered? (ftype-type-name ft))) (ftype-type-name ft)]
        [else #f]))

;; element-of : type-symbol element-name -> (values found? ty)   a field, or a choice base
(define (element-of ty name)
  (define specs (with-handlers ([exn:fail? (lambda (_) #f)]) (full-field-specs ty)))
  (define sym (string->symbol name))
  (cond
    [(not specs) (values #t #f)]                             ; a type we cannot describe: open
    [(findf (lambda (e) (and (field-spec? e) (eq? (fs-name e) sym))) specs)
     => (lambda (f) (values #t (ftype->ty (fs-ftype f))))]
    [(for/or ([e (in-list specs)] #:when (choice-group? e))    ; `value` on a value[x]
       (ormap (lambda (m) (string-prefix? (symbol->string m) name)) (choice-group-members e)))
     (values #t #f)]
    [(for/or ([e (in-list specs)] #:when (field-spec? e))      ; `valueString` written out
       (and (regexp-match? #px"^[a-z]+[A-Z]" name) (eq? (fs-name e) sym)))
     (values #t #f)]
    [else (values #f #f)]))

;; path-type : type-symbol dotted-path -> (values missing-element|#f ty)
(define (path-type ty path)
  (let loop ([ty ty] [segs (string-split path ".")])
    (cond [(null? segs) (values #f ty)]
          [(not (symbol? ty)) (values #f #f)]
          [else (define-values (found? next) (element-of ty (car segs)))
                (if found? (loop next (cdr segs)) (values (car segs) ty))])))

;; ---- the walk -------------------------------------------------------------------
;; A Scope maps a variable name to (cons 'source|'target Ty)
(define (check-map m #:source [source #f])
  (define types (uses-types m))
  (define maps (map-imports m))
  (define all-groups (append-map fml-map-groups maps))
  (define (type-sym name) (and name (or (hash-ref types name #f) (and (type-registered? (string->symbol name)) (string->symbol name)))))
  (define findings '())
  (define (report! node severity fmt . args)
    (set! findings (cons (diagnostic 'fml 'check severity (apply format fmt args)
                                     (or (attached-loc node) (loc (and source (if (path? source) (path->string source) source)) #f #f #f #f)))
                         findings)))
  ;; a group's scope from its parameters
  (define (group-scope g)
    (for/hash ([p (in-list (fml-group-params g))])
      (values (fml-param-name p) (cons (fml-param-mode p) (type-sym (fml-param-type p))))))
  (define (check-group g)
    (for ([r (in-list (fml-group-rules g))]) (check-rule r (group-scope g))))
  (define (check-rule r sc)
    ;; sources bind their variables
    (define sc1
      (for/fold ([sc sc]) ([s (in-list (fml-rule-sources r))])
        (define ctx (hash-ref sc (fml-source-context s) #f))
        (define ctx-ty (and ctx (cdr ctx)))
        (cond
          [(not ctx) (report! s 'error "`~a` is not a variable in scope" (fml-source-context s)) sc]
          [(not (fml-source-element s)) (if (fml-source-variable s) (hash-set sc (fml-source-variable s) (cons 'source ctx-ty)) sc)]
          [else
           (define-values (missing ty) (if (symbol? ctx-ty) (path-type ctx-ty (fml-source-element s)) (values #f #f)))
           (when missing (report! s 'error "`~a` is not an element of ~a" missing (ty->string ty)))
           (if (fml-source-variable s) (hash-set sc (fml-source-variable s) (cons 'source (and (not missing) ty))) sc)])))
    ;; targets: the element exists, the value fits, the variable binds
    (define sc2
      (for/fold ([sc sc1]) ([t (in-list (fml-rule-targets r))])
        (cond
          [(not (fml-target-context t))                       ; a bare invocation target
           (if (fml-target-variable t) (hash-set sc (fml-target-variable t) (cons 'source #f)) sc)]
          [else
           (define ctx (hash-ref sc (fml-target-context t) #f))
           (cond
             [(not ctx) (report! t 'error "`~a` is not a variable in scope" (fml-target-context t)) sc]
             [(eq? (car ctx) 'source) (report! t 'error "`~a` is a source; a target context must be a target" (fml-target-context t)) sc]
             [else
              (define ctx-ty (cdr ctx))
              (define-values (missing ty)
                (cond [(not (fml-target-element t)) (values #f ctx-ty)]
                      [(symbol? ctx-ty) (path-type ctx-ty (fml-target-element t))]
                      [else (values #f #f)]))
              (when missing (report! t 'error "`~a` is not an element of ~a" missing (ty->string ty)))
              (define elem-ty (and (not missing) ty))
              (check-assignment t (fml-target-transform t) elem-ty sc)
              (if (fml-target-variable t) (hash-set sc (fml-target-variable t) (cons 'target (value-type-of t elem-ty sc))) sc)])])))
    ;; dependents: the group and its arity
    (for ([d (in-list (fml-rule-dependents r))])
      (define g (findf (lambda (g) (equal? (fml-group-name g) (fml-call-name d))) all-groups))
      (cond [(and (not g) (null? (fml-map-imports m)))
             (report! r 'error "no group ~a in this map (groups: ~a)" (fml-call-name d) (string-join (map fml-group-name all-groups) ", "))]
            [(and g (not (= (length (fml-call-args d)) (length (fml-group-params g)))))
             (report! r 'error "group ~a takes ~a parameters, called with ~a" (fml-call-name d) (length (fml-group-params g)) (length (fml-call-args d)))]
            [else (void)])
      (for ([a (in-list (fml-call-args d))] #:when (and (string? a) (not (hash-ref sc2 a #f))))
        (report! r 'error "`~a` is not a variable in scope" a)))
    (for ([n (in-list (fml-rule-nested r))]) (check-rule n sc2)))
  ;; the type a target variable gets: create('T') names it; otherwise the element's
  (define (value-type-of t elem-ty sc)
    (match (fml-target-transform t)
      [(fml-invoke "create" (list (fml-literal ty))) (or (type-sym ty) elem-ty)]
      [_ elem-ty]))
  ;; what the structures decide about an assignment
  (define (check-assignment t tr elem-ty sc)
    (define (complex? ty) (and (symbol? ty) #t))
    (define (var-ty name) (let ([b (hash-ref sc name #f)]) (and b (cdr b))))
    (match tr
      [(fml-literal v) #:when (complex? elem-ty)
       (report! t 'error "a string cannot be assigned to ~a, a ~a" (fml-target-element t) elem-ty)]
      [(fml-var name)
       (define vty (var-ty name))
       (cond [(not (hash-ref sc name #f)) (report! t 'error "`~a` is not a variable in scope" name)]
             [(and (complex? vty) (prim? elem-ty))
              (report! t 'error "~a is a ~a; it cannot be assigned to ~a, a ~a" name vty (fml-target-element t) (prim-code elem-ty))]
             [(and (prim? vty) (complex? elem-ty))
              (report! t 'error "~a is a ~a; it cannot be assigned to ~a, a ~a" name (prim-code vty) (fml-target-element t) elem-ty)]
             [else (void)])]
      [(fml-invoke name args)
       (for ([a (in-list args)] #:when (and (string? a) (not (hash-ref sc a #f))))
         (report! t 'error "`~a` is not a variable in scope" a))]
      [_ (void)]))
  (for ([g (in-list (fml-map-groups m))]) (check-group g))
  ;; required target elements the entry group never assigns
  (define entry (and (pair? (fml-map-groups m)) (car (fml-map-groups m))))
  (when entry
    (define tparam (findf (lambda (p) (eq? (fml-param-mode p) 'target)) (fml-group-params entry)))
    (define tty (and tparam (type-sym (fml-param-type tparam))))
    (define specs (and (symbol? tty) (with-handlers ([exn:fail? (lambda (_) #f)]) (full-field-specs tty))))
    (when specs
      (define written
        (let collect ([rules (fml-group-rules entry)] [acc '()])
          (for/fold ([acc acc]) ([r (in-list rules)])
            (define here (for/list ([t (in-list (fml-rule-targets r))]
                                    #:when (and (equal? (fml-target-context t) (fml-param-name tparam)) (fml-target-element t)))
                           (car (string-split (fml-target-element t) "."))))
            (collect (fml-rule-nested r) (append here acc)))))
      ;; a dependent group given the whole target may write anything
      (define calls-out? (let any ([rules (fml-group-rules entry)])
                           (for/or ([r (in-list rules)])
                             (or (for/or ([d (in-list (fml-rule-dependents r))]) (member (fml-param-name tparam) (fml-call-args d)))
                                 (any (fml-rule-nested r))))))
      (unless calls-out?
        (for ([f (in-list specs)] #:when (and (field-spec? f) (>= (fs-min f) 1)
                                              (not (member (symbol->string (fs-name f)) written))))
          (report! entry 'warning "~a.~a is required (min ~a) but no rule of ~a assigns it"
                   tty (fs-name f) (fs-min f) (fml-group-name entry))))))
  (reverse findings))

;; ---- examples, validated ----------------------------------------------------------
(define (check-map/examples m examples #:profiles [profiles '()] #:source [source #f])
  (define validate-issues (dynamic-require 'fhir/conformance/validate 'validate-issues
                                           (lambda () (error 'check-map/examples "validating needs the fhir-validation package: raco pkg install fhir-validation"))))
  (define issue-severity (dynamic-require 'fhir/conformance/validate 'issue-severity))
  (define issue-path (dynamic-require 'fhir/conformance/validate 'issue-path))
  (define issue-msg (dynamic-require 'fhir/conformance/validate 'issue-msg))
  (dynamic-require 'fhir/r5/schemas #f)
  (uses-types m)
  (define entry (car (fml-map-groups m)))
  (define tparam (findf (lambda (p) (eq? (fml-param-mode p) 'target)) (fml-group-params entry)))
  (define paths (target-paths entry (and tparam (fml-param-name tparam))))
  (append*
   (for/list ([ex (in-list examples)] [i (in-naturals 1)])
     (define r (from-json ex #:lenient? #t))
     (cond
       [(not (ok? r)) (list (diagnostic 'fml 'check 'error (format "example ~a is not a FHIR resource" i) (loc source #f #f #f #f)))]
       [else
        (define out (with-handlers ([exn:fail? (lambda (e) e)]) (run-map m (ok-value r))))
        (cond
          [(exn? out) (list (diagnostic 'fml 'check 'error (format "example ~a: ~a" i (exn-message out)) (loc source #f #f #f #f)))]
          [else
           (for/list ([iss (in-list (validate-issues out #:profiles profiles))]
                      #:when (member (issue-severity iss) '("error" "fatal" "warning")))
             (define rule (trace-target paths (issue-path iss)))
             (diagnostic 'fml 'validate (if (equal? (issue-severity iss) "warning") 'warning 'error)
                         (format "example ~a: ~a: ~a" i (issue-path iss) (issue-msg iss))
                         (or (and rule (attached-loc rule)) (loc source #f #f #f #f))))])]))))

;; target-paths : entry-group target-var -> (listof (cons element-path target))   every target's path
;; from the entry target, variables followed (`tgt.component as c` then `c.code` is component.code)
(define (target-paths g tvar)
  (define acc '())
  (define (walk rules env)
    (for ([r (in-list rules)])
      (define env2
        (for/fold ([env env]) ([t (in-list (fml-rule-targets r))])
          (define base (hash-ref env (fml-target-context t) #f))
          (define path (and base (if (fml-target-element t) (if (equal? base "") (fml-target-element t) (string-append base "." (fml-target-element t))) base)))
          (when path (set! acc (cons (cons path t) acc)))
          (if (and path (fml-target-variable t)) (hash-set env (fml-target-variable t) path) env)))
      (walk (fml-rule-nested r) env2)))
  (walk (fml-group-rules g) (if tvar (hash tvar "") (hash)))
  acc)

;; trace-target : paths validator-path -> target|#f   the target whose path shares the longest
;; leading run of steps with the issue's path (type root dropped, indexes and choice suffixes ignored)
(define (trace-target paths issue-path)
  (define segs (let ([s (map (lambda (x) (regexp-replace #px"\\[.*$" x "")) (string-split issue-path "."))])
                 (if (and (pair? s) (regexp-match? #px"^[A-Z]" (car s))) (cdr s) s)))
  (define (score path)
    (let loop ([a (string-split path ".")] [b segs] [n 0])
      (if (and (pair? a) (pair? b) (or (equal? (car a) (car b)) (string-prefix? (car b) (car a)))) (loop (cdr a) (cdr b) (add1 n)) n)))
  (define best (for/fold ([best #f]) ([p (in-list paths)])
                 (define s (score (car p)))
                 (if (and (> s 0) (or (not best) (> s (car best)))) (cons s (cdr p)) best)))
  (and best (cdr best)))

(module+ test
  (require rackunit (only-in fhir/r5/schemas) (only-in "parser/parse.rkt" parse-fml)
           (only-in fhir/diagnostic diagnostic-message diagnostic-severity loc-line diagnostic-loc))
  (define (findings src) (check-map (ok-value (parse-fml src #:source "t.map"))))
  (define good (string-append
    "map \"http://x/m\" = \"m\"\n"
    "uses \"http://hl7.org/fhir/StructureDefinition/QuestionnaireResponse\" alias QR as source\n"
    "uses \"http://hl7.org/fhir/StructureDefinition/Patient\" alias Patient as target\n"
    "group entry(source src : QR, target tgt : Patient) {\n"
    "  src.item as item where linkId = 'sex' -> tgt.gender = (item.answer.valueString);\n"
    "  src.subject as s -> tgt.managingOrganization = s;\n"
    "  src.authored as a -> tgt.birthDate = cast(a, 'date');\n"
    "  src.item as item -> tgt.name as n then naming(item, n);\n"
    "}\n"
    "group naming(source item, target n : HumanName) { item.text as t -> n.text = t; }\n"))
  (check-equal? (findings good) '())
  ;; the planted mistakes, each at its line
  (define bad (string-append
    "map \"http://x/m\" = \"m\"\n"
    "uses \"http://hl7.org/fhir/StructureDefinition/QuestionnaireResponse\" alias QR as source\n"
    "uses \"http://hl7.org/fhir/StructureDefinition/Observation\" alias Observation as target\n"
    "group entry(source src : QR, target tgt : Observation) {\n"
    "  src.itme as item -> tgt.status = 'final';\n"                   ; 5: no element itme
    "  src.item as item -> tgt.valeu = 'x';\n"                        ; 6: no element valeu
    "  src.subject as s -> tgt.code = 'text';\n"                      ; 7: a string into a CodeableConcept
    "  src.subject as s -> tgt.id = s;\n"                             ; 8: a Reference into an id
    "  src.item as item -> tgt.component as c then nosuch(item, c);\n" ; 9: no group
    "  src.item as item -> tgt.component as c then comp(item);\n"      ; 10: arity
    "}\n"
    "group comp(source item, target c) { item.text as t -> c.valueString = t; }\n"))
  (define ds (findings bad))
  (check-equal? (for/list ([d ds]) (loc-line (diagnostic-loc d))) '(5 6 7 8 9 10 4))
  (check-true (regexp-match? #rx"`itme` is not an element of QuestionnaireResponse" (diagnostic-message (car ds))))
  (check-true (regexp-match? #rx"`valeu` is not an element of Observation" (diagnostic-message (cadr ds))))
  (check-true (regexp-match? #rx"string cannot be assigned to code, a CodeableConcept" (diagnostic-message (caddr ds))))
  (check-true (regexp-match? #rx"s is a Reference; it cannot be assigned to id" (diagnostic-message (list-ref ds 3))))
  (check-true (regexp-match? #rx"no group nosuch" (diagnostic-message (list-ref ds 4))))
  (check-true (regexp-match? #rx"takes 2 parameters, called with 1" (diagnostic-message (list-ref ds 5))))
  ;; the required Observation.code is never assigned: a warning at the group
  (check-equal? (diagnostic-severity (last ds)) 'warning)
  (check-true (regexp-match? #rx"Observation.code is required" (diagnostic-message (last ds))))
  ;; examples, validated: the Patient built from a response is a valid Patient
  (define m (ok-value (parse-fml good)))
  (define qr (hasheq 'resourceType "QuestionnaireResponse" 'status "completed" 'authored "2024-01-02"
                     'item (list (hasheq 'linkId "sex" 'answer (list (hasheq 'valueString "female"))))))
  (check-equal? (check-map/examples m (list qr)) '()))
