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
;;;     a dependent group may fill it);
;;;   - what the text wrote that the specified language does not have (warnings, from the parse:
;;;     parser/parse.rkt `specification-notes`).
;;; Types flow as in the engine: a parameter's declared type, the element types the registry
;;; knows, a variable bound by `as`. Anything unknown stops that branch — fail-open, never a
;;; finding on what the structures do not decide.
;;; `check-map/examples` runs the map over example sources and validates each result (fhir-validation,
;;; loaded on demand); a validation issue is traced to the rule that wrote the element.

(require (only-in fhir/suggest did-you-mean closest-names)
         racket/contract racket/list racket/string racket/match
         (only-in fhir/serialize/lenient lenient-drops drop->string)
         "parser/ast.rkt"
         (only-in "transform.rkt" uses-types map-imports unresolved-imports transform-names run-map current-empty-targets)
         (only-in "parser/parse.rkt" specification-notes)
         (only-in fhir/diagnostic diagnostic diagnostic? attached-loc loc)
         "schema.rkt"
         (only-in fhir/model/introspect full-field-specs)
         (only-in fhir/model/schema field-spec? fs-name fs-min)
         (only-in fhir/model/registry type-registered?)
         (only-in fhir/serialize/from-json from-json)
         (only-in fhir/registry register-resources!)
         (only-in json read-json jsexpr->string)
         (only-in fhirpath/print fhirpath->string)
         (only-in fhir/result/result ok? ok-value))

(provide (contract-out
          ;; check-map : map [#:source file] -> (listof diagnostic)   the static findings
          [check-map (->* (fml-map?) (#:source (or/c string? path? #f)) (listof diagnostic?))]
          ;; check-map/examples : map examples [#:profiles sds #:source file] -> (listof diagnostic)
          ;; each example (FHIR JSON) transformed, the result validated against its base type and the
          ;; profiles; issues positioned at the rule that wrote the element
          ;; validate-result : map result [#:profiles sds #:source file #:label str] -> (listof diagnostic)
          [validate-result (->* (fml-map? any/c) (#:profiles (listof hash?) #:source (or/c string? path? #f) #:label string?
                                                 #:drops list?)
                                (listof diagnostic?))]
          ;; profiles-from : spec -> (listof StructureDefinition)   a JSON file, an FSH project (a
          ;; directory or a .fsh file: its profiles, its definitions registered), a canonical
          [profiles-from (-> (or/c string? path? hash?) (listof hash?))]
          [check-map/examples (->* (fml-map? (listof hash?)) (#:profiles (listof hash?) #:source (or/c string? path? #f)
                                                              #:names (or/c #f (listof string?)))
                                   (listof diagnostic?))]))

;; Types are fml/schema's: a Ty is a registered type symbol, (prim code), or #f (unknown: nothing to
;; decide, fail-open); `path-type` walks a dotted path through the schema.

;; ---- the walk -------------------------------------------------------------------
;; A Binding is (binding mode ty): what a variable names — mode 'source | 'target, ty a Ty
;; A Scope is (hash name -> Binding)
;; A Cx is (cx types groups map at): the `uses` resolved to type symbols, every callable group (this
;; map's and its imports'), the map, and `at` : node -> loc (where a finding points)
;; Every check is a function of the data it checks, returning the findings (listof diagnostic) — a
;; binding step returns (values scope findings).
(struct binding (mode ty) #:transparent)
(struct cx (types groups map at))

(define (check-map m #:source [source #f])
  (define fallback (loc (and source (if (path? source) (path->string source) source)) #f #f #f #f))
  (define c (cx (uses-types m) (append-map fml-map-groups (map-imports m)) m
                (lambda (node) (or (attached-loc node) fallback))))
  (append (append-map (lambda (g) (check-group c g)) (fml-map-groups m))
          (check-imports c)
          (check-required c)
          (specification-notes m)))                    ; what the text wrote beyond the specified language

;; check-imports : cx -> (listof diagnostic)   an import nothing resolves, said once: its groups are
;; unknown here, so a call to a group this map lacks cannot be told from a misspelt one (such a map
;; checked clean, whatever group it called)
(define (check-imports c)
  (for/list ([u (in-list (unresolved-imports (cx-map c)))])
    (finding c (cx-map c) 'warning
             "imports ~s: no StructureMap with that url was found (check it with the map beside it, or its package): calls to groups this map does not define are not checked" u)))

(define (finding c node severity fmt . args)
  (diagnostic 'fml 'check severity (apply format fmt args) ((cx-at c) node)))
(define (findings-if test c node fmt . args) (if test (list (apply finding c node 'error fmt args)) '()))

;; type-of : cx type-name|#f -> Ty   a declared type: an alias from `uses`, else a registered type
(define (type-of c name)
  (and name (or (hash-ref (cx-types c) name #f) (and (type-registered? (string->symbol name)) (string->symbol name)))))
;; path-type/ty : Ty path -> (values missing ty)   only a known complex type decides anything
(define (path-type/ty ty path) (if (symbol? ty) (path-type ty path) (values #f #f)))

;; group-scope : cx group -> scope   the map's constants (`let`: values of a type the check does
;; not decide), then the group's parameters, which hide them
(define (group-scope c g)
  (for/fold ([sc (for/hash ([k (in-list (fml-map-constants (cx-map c)))]) (values (car k) (binding 'source #f)))])
            ([p (in-list (fml-group-params g))])
    (hash-set sc (fml-param-name p) (binding (fml-param-mode p) (type-of c (fml-param-type p))))))
(define (check-group c g) (check-rules c (fml-group-rules g) (group-scope c g)))
(define (check-rules c rules sc) (append-map (lambda (r) (check-rule c r sc)) rules))

;; a rule: its sources bind, then its targets, then the dependents and nested rules see both
(define (check-rule c r sc)
  (define-values (sc1 ds1) (bind-all c bind-source (fml-rule-sources r) sc))
  (define-values (sc2 ds2) (bind-all c bind-target (fml-rule-targets r) sc1))
  (append ds1 ds2
          (append-map (lambda (d) (check-dependent c r d sc2)) (fml-rule-dependents r))
          (check-rules c (fml-rule-nested r) sc2)))
(define (bind-all c bind xs sc)
  (for/fold ([sc sc] [ds '()]) ([x (in-list xs)])
    (define-values (sc* ds*) (bind c x sc))
    (values sc* (append ds ds*))))

;; element-suggestion : name Ty -> string   " (did you mean `name`?)" from the type's elements, or ""
(define (element-suggestion missing ty)
  (define specs (and (symbol? ty) (with-handlers ([exn:fail? (lambda (_) #f)]) (full-field-specs ty))))
  (if (and (string? missing) specs)
      (did-you-mean missing (for/list ([f (in-list specs)] #:when (field-spec? f)) (symbol->string (fs-name f))))
      ""))

;; a source clause: its context must be in scope, its element on the context's type; `as` binds
(define (bind-source c s sc)
  (define b (hash-ref sc (fml-source-context s) #f))
  (define (bound ty) (if (fml-source-variable s) (hash-set sc (fml-source-variable s) (binding 'source ty)) sc))
  (cond
    [(not b) (values sc (list (finding c s 'error "`~a` is not a variable in scope" (fml-source-context s))))]
    [(not (fml-source-element s)) (values (bound (binding-ty b)) '())]
    [else
     (define-values (missing ty) (path-type/ty (binding-ty b) (fml-source-element s)))
     (values (bound (and (not missing) ty))
             (findings-if missing c s "`~a` is not an element of ~a~a" missing (ty->string ty) (element-suggestion missing ty)))]))

;; a target clause: what `target-clause` finds, and a transform the engine does not run (the
;; specification's escape, dateOp and pointer: such a map checked clean and failed when run)
(define (bind-target c t sc)
  (define-values (sc2 found) (target-clause c t sc))
  (values sc2 (append found (unsupported-transform c t))))

;; unsupported-transform : cx target -> (listof diagnostic)
(define (unsupported-transform c t)
  (match (fml-target-transform t)
    [(fml-invoke (? string? name) _)
     (findings-if (not (member name (transform-names))) c t
                  "the transform ~a is not one this engine runs (it runs: ~a)" name (string-join (transform-names) " "))]
    [_ '()]))

;; a target clause: a target context in scope, the element on its type, a value that fits; `as` binds
(define (target-clause c t sc)
  (define b (and (fml-target-context t) (hash-ref sc (fml-target-context t) #f)))
  (define (bound ty) (if (fml-target-variable t) (hash-set sc (fml-target-variable t) (binding 'target ty)) sc))
  (cond
    [(not (fml-target-context t))                        ; a bare invocation: its value is a source
     (values (if (fml-target-variable t) (hash-set sc (fml-target-variable t) (binding 'source #f)) sc) '())]
    [(not b) (values sc (list (finding c t 'error "`~a` is not a variable in scope" (fml-target-context t))))]
    [(eq? (binding-mode b) 'source)
     (values sc (list (finding c t 'error "`~a` is a source; a target context must be a target" (fml-target-context t))))]
    [else
     (define-values (missing ty)
       (if (fml-target-element t) (path-type/ty (binding-ty b) (fml-target-element t)) (values #f (binding-ty b))))
     (define elem-ty (and (not missing) ty))
     (values (bound (created-type c t elem-ty))
             (append (findings-if missing c t "`~a` is not an element of ~a~a" missing (ty->string ty) (element-suggestion missing ty))
                     (check-assignment c t elem-ty sc)))]))

;; the type a target variable gets: create('T') names it; otherwise the element's
(define (created-type c t elem-ty)
  (match (fml-target-transform t)
    [(fml-invoke "create" (list (fml-literal ty))) (or (type-of c ty) elem-ty)]
    [_ elem-ty]))

;; what the structures decide about an assignment: a string into a complex element, a complex
;; variable into a primitive one (or the reverse), a variable not in scope
(define (check-assignment c t elem-ty sc)
  (define (complex? ty) (and (symbol? ty) #t))
  (define (unbound a) (findings-if (and (string? a) (not (hash-ref sc a #f))) c t "`~a` is not a variable in scope" a))
  (match (fml-target-transform t)
    [(fml-literal _) (findings-if (complex? elem-ty) c t "a string cannot be assigned to ~a, a ~a" (fml-target-element t) elem-ty)]
    [(fml-var name)
     (define b (hash-ref sc name #f))
     (define vty (and b (binding-ty b)))
     (cond [(not b) (unbound name)]
           [(and (complex? vty) (prim? elem-ty))
            (list (finding c t 'error "~a is a ~a; it cannot be assigned to ~a, a ~a" name vty (fml-target-element t) (prim-code elem-ty)))]
           [(and (prim? vty) (complex? elem-ty))
            (list (finding c t 'error "~a is a ~a; it cannot be assigned to ~a, a ~a" name (prim-code vty) (fml-target-element t) elem-ty))]
           [else '()])]
    [(fml-invoke _ args) (append-map unbound args)]
    [_ '()]))

;; a dependent call: the group exists (here or in an import), its arity, its arguments in scope
(define (check-dependent c r d sc)
  (define g (findf (lambda (g) (equal? (fml-group-name g) (fml-call-name d))) (cx-groups c)))
  (append
   ;; decided when every import resolved (the groups in hand are all there are)
   (cond [(and (not g) (null? (unresolved-imports (cx-map c))))
          (list (finding c r 'error "no group ~a in this map~a (groups: ~a)" (fml-call-name d)
                         (if (null? (fml-map-imports (cx-map c))) "" " or the maps it imports")
                         (string-join (map fml-group-name (cx-groups c)) ", ")))]
         [(and g (not (= (length (fml-call-args d)) (length (fml-group-params g)))))
          (list (finding c r 'error "group ~a takes ~a parameters, called with ~a"
                         (fml-call-name d) (length (fml-group-params g)) (length (fml-call-args d))))]
         [else '()])
   (append-map (lambda (a) (findings-if (and (string? a) (not (hash-ref sc a #f))) c r "`~a` is not a variable in scope" a))
               (fml-call-args d))))

;; the entry group's target type: a required element (min >= 1) no rule of the group assigns is a
;; warning — unless a dependent call receives the whole target, which may write anything
(define (check-required c)
  (define entry (and (pair? (fml-map-groups (cx-map c))) (car (fml-map-groups (cx-map c)))))
  (define tparam (and entry (findf (lambda (p) (eq? (fml-param-mode p) 'target)) (fml-group-params entry))))
  (define tty (and tparam (type-of c (fml-param-type tparam))))
  (define specs (and (symbol? tty) (with-handlers ([exn:fail? (lambda (_) #f)]) (full-field-specs tty))))
  (define tname (and tparam (fml-param-name tparam)))
  (define (rules-of rules f) (append-map (lambda (r) (append (f r) (rules-of (fml-rule-nested r) f))) rules))
  (define (written r)
    (for/list ([t (in-list (fml-rule-targets r))] #:when (and (equal? (fml-target-context t) tname) (fml-target-element t)))
      (car (string-split (fml-target-element t) "."))))
  (define (handed-over r)
    (for/list ([d (in-list (fml-rule-dependents r))] #:when (member tname (fml-call-args d))) d))
  (cond
    [(not specs) '()]
    [(pair? (rules-of (fml-group-rules entry) handed-over)) '()]
    [else
     (define assigned (rules-of (fml-group-rules entry) written))
     (for/list ([f (in-list specs)]
                #:when (and (field-spec? f) (>= (fs-min f) 1) (not (member (symbol->string (fs-name f)) assigned))))
       (finding c entry 'warning "~a.~a is required (min ~a) but no rule of ~a assigns it"
                tty (fs-name f) (fs-min f) (fml-group-name entry)))]))

;; ---- examples, validated ----------------------------------------------------------
(define (validator name)
  (dynamic-require 'fhir/conformance/validate name
                   (lambda () (error 'check-map/examples "validating needs the fhir-validation package: raco pkg install fhir-validation"))))

;; #:names: what each example is called in a finding (its file, `qr.json`), else `example i`
(define (check-map/examples m examples #:profiles [profiles '()] #:source [source #f] #:names [names #f])
  (dynamic-require 'fhir/r5/schemas #f)
  (uses-types m)
  (append*
   (for/list ([ex (in-list examples)] [n (in-naturals 1)])
     (define i (if names (list-ref names (sub1 n)) n))
     (define r (from-json ex #:lenient? #t))
     (cond
       [(not (ok? r)) (list (diagnostic 'fml 'check 'error (format "~a is not a FHIR resource" (example-label i)) (loc source #f #f #f #f)))]
       [else
        (parameterize ([current-empty-targets (make-hasheq)])
          (define out (with-handlers ([exn:fail? (lambda (e) e)]) (run-map m (ok-value r))))
          (cond
            [(exn? out) (list (diagnostic 'fml 'check 'error (format "~a: ~a" (example-label i) (exn-message out)) (loc source #f #f #f #f)))]
            [else (validate-result m out #:profiles profiles #:source source #:label (format "~a: " (example-label i))
                                   ;; what the lenient read left out of this example
                                   #:drops (if (hash? ex) (lenient-drops ex (ok-value r)) '()))]))]))))
;; example-label : (number | name) -> string
(define (example-label i) (if (number? i) (format "example ~a" i) i))

;; profiles-from : spec -> (listof SD)   where a profile comes from, as a user names it
(define (profiles-from spec)
  (define (sd? r) (and (hash? r) (equal? (hash-ref r 'resourceType #f) "StructureDefinition")))
  (cond
    [(hash? spec) (list spec)]
    [(and (file-exists? spec) (regexp-match? #rx"[.]json$" (if (path? spec) (path->string spec) spec)))
     (list (call-with-input-file spec read-json))]
    [(or (directory-exists? spec) (and (file-exists? spec) (regexp-match? #rx"[.]fsh$" (if (path? spec) (path->string spec) spec))))
     (define compile-project (dynamic-require 'fsh/project 'compile-project
                                              (lambda () (raise-user-error 'fml "--profile ~a: reading an FSH project needs the fsh package" spec))))
     (define artifacts (dynamic-require 'fsh/project 'project-artifacts))
     (define sds (filter sd? (artifacts (compile-project spec))))
     ;; its definitions resolve by canonical (a profile of a profile, an extension)
     (register-resources! sds #:source (string->symbol (format "fml --profile ~a" spec)))
     (define profiles (filter (lambda (sd) (equal? (hash-ref sd 'derivation #f) "constraint")) sds))
     (when (null? profiles) (raise-user-error 'fml "--profile ~a: the FSH defines no profile" spec))
     profiles]
    [else
     (define resolve (dynamic-require 'fhir/sd/operations 'resolve-structure-definition))
     (define sd (resolve spec))
     (unless sd
       (raise-user-error 'fml "profile ~a is not a JSON file, an FSH project, or a canonical the data, the registered modules or the packages define" spec))
     (list sd)]))

;; validate-result : map result-json [#:profiles sds #:source s #:label str] -> (listof diagnostic)
;; the map's result held to its base type, the profiles it claims and those given that are of its
;; type (a profile of another type does not apply); each problem at the rule that wrote the element,
;; or, when no rule writes it, at the map with the rules' words: "no rule of <group> writes `x`"
(define (validate-result m out #:profiles [profiles '()] #:source [source #f] #:label [label ""] #:drops [drops '()])
  (define validate-issues (validator 'validate-issues))
  (define issue-severity (validator 'issue-severity))
  (define issue-path (validator 'issue-path))
  (define issue-msg (validator 'issue-msg))
  (define entry (car (fml-map-groups m)))
  (define tparam (findf (lambda (p) (eq? (fml-param-mode p) 'target)) (fml-group-params entry)))
  (define paths (target-paths entry (and tparam (fml-param-name tparam))))
  (define type (and (hash? out) (hash-ref out 'resourceType #f)))
  (define applicable (filter (lambda (sd) (member (hash-ref sd 'type type) (list type))) profiles))
  (for/list ([iss (in-list (validate-issues out #:profiles applicable))]
             #:when (member (issue-severity iss) '("error" "fatal" "warning")))
    (define rule (trace-target paths (issue-path iss)))
    (define at (and rule (attached-loc rule)))
    ;; nothing there: no rule writes the element, or the one that does wrote nothing (its source
    ;; matched nothing in this input — a missing item, a `where` that held for none)
    (define unwritten
      (let* ([segs (string-split (issue-path iss) ".")]
             [elem (and (> (length segs) 1) (regexp-replace #px"\\[x\\]$" (cadr segs) ""))])
        (cond [(not elem) ""]
              [(not rule) (format " (no rule of ~a writes `~a`)" (fml-group-name entry) elem)]
              [(and (hash? out) (not (for/or ([k (in-hash-keys out)]) (regexp-match? (pregexp (string-append "^" (regexp-quote elem) "(?:[A-Z]|$)")) (symbol->string k)))))
               (define empty (and (current-empty-targets) (hash-ref (current-empty-targets) rule 'none)))
               (cond
                 ;; a rule that "matched nothing" because the read dropped its value (`05/01/1950`
                 ;; is no date) is said so: the item was there, the map never saw it
                 [(and (eq? empty 'none) (pair? drops))
                  (format " (the rule here wrote nothing: its source matched nothing the map could read — ~a could not be read)"
                          (string-join (map drop->string drops) ", "))]
                 [(eq? empty 'none)
                  (format " (the rule here wrote nothing: its source matched nothing in the input)")]
                 ;; the source matched, but a `where` held for none of it (the gender answer was there,
                 ;; as a valueString, and `where value.system = …` dropped it)
                 [(and (pair? empty) (eq? (car empty) 'filtered))
                  (format " (the rule here wrote nothing: its source matched ~a item~a, but `where (~a)` held for none~a)"
                          (caddr empty) (if (= 1 (caddr empty)) "" "s")
                          (let ([w (cadr empty)]) (if (fml-fhirpath? w) (fhirpath->string (fml-fhirpath-ast w)) (fhirpath->string w)))
                          (where-evidence (cadr empty) (cadddr empty)))]
                 [else
                   ;; the source matched; the value expression gave nothing for what it read (a
                   ;; `(a.value.code)` over an answer written as valueString)
                   (format " (the rule here wrote nothing: its source matched, but ~a was empty~a)"
                           (value-text rule)
                           (if (hash? empty) (format " for ~a" (compact-source empty)) ""))])]
              [else ""])))
    ;; a `where` that held for none is said at the rule that has it (the fix is there), not at the
    ;; nested rule that would have written the element
    (define where-rule
      (let ([e (and rule (current-empty-targets) (hash-ref (current-empty-targets) rule #f))])
        (and (pair? e) (eq? (car e) 'filtered) (>= (length e) 5) (list-ref e 4))))
    (diagnostic 'fml 'validate (if (equal? (issue-severity iss) "warning") 'warning 'error)
                (format "~a~a: ~a~a" label (issue-path iss) (issue-msg iss) unwritten)
                (or (and where-rule (attached-loc where-rule)) at (loc source #f #f #f #f)))))

;; value-text : target -> string   the target's value as the map writes it: `(a.value.code)`, `a`
;; where-evidence : where items -> string   what the items held for the field a `where` tests against a
;; literal (`linkId = 'birthDate'`: "the linkIds there: name, gender, birth_date — did you mean
;; 'birth_date'?"); else the first item (the first item alone, the name item, told nothing)
(define (where-evidence w items)
  (define text (if (fml-fhirpath? w) (fhirpath->string (fml-fhirpath-ast w)) (fhirpath->string w)))
  (define tests (regexp-match* #px"([A-Za-z_][A-Za-z0-9_.]*)\\s*=\\s*'([^']*)'" text #:match-select cdr))
  (define (field-values field)
    (remove-duplicates
     (for*/list ([it (in-list items)] #:when (hash? it)
                 [v (in-value (for/fold ([v it]) ([k (in-list (string-split field "."))])
                                (and (hash? v) (hash-ref v (string->symbol k) #f))))]
                 #:when (or (string? v) (number? v) (boolean? v)))
       (format "~a" v))))
  (define said
    (for/list ([t (in-list tests)] #:when (pair? (field-values (car t))))
      (define vals (field-values (car t)))
      (define near (closest-names (cadr t) vals #:limit 1))
      (format "the ~a there: ~a~a" (car t) (string-join vals ", ")
              (if (pair? near) (format " — did you mean '~a'?" (car near)) ""))))
  (cond [(pair? said) (format " (~a)" (string-join said "; "))]
        [(and (pair? items) (hash? (car items))) (format " (the first: ~a)" (compact-source (car items)))]
        [else ""]))

(define (value-text t)
  (match (fml-target-transform t)
    [(fml-fhirpath ast) (format "`(~a)`" (fhirpath->string ast))]
    [(fml-var name) (format "`~a`" name)]
    [_ "its value"]))
;; compact-source : jsexpr -> string   the source value read, short: {"valueString": "female"}
(define (compact-source js)
  (define text (jsexpr->string (if (hash? js) (for/hasheq ([(k v) (in-hash js)] #:unless (memq k '(id extension))) (values k v)) js)))
  (if (> (string-length text) 100) (string-append (substring text 0 97) "...") text))

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
    "  src.item as item where (linkId = 'sex') -> tgt.gender = (item.answer.valueString);\n"
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
    "  src.subject as s -> tgt.category = 'text';\n"                  ; 7: a string into a CodeableConcept
    "  src.subject as s -> tgt.id = s;\n"                             ; 8: a Reference into an id
    "  src.item as item -> tgt.component as c then nosuch(item, c);\n" ; 9: no group
    "  src.item as item -> tgt.component as c then comp(item);\n"      ; 10: arity
    "}\n"
    "group comp(source item, target c) { item.text as t -> c.valueString = t; }\n"))
  (define ds (findings bad))
  (check-equal? (for/list ([d ds]) (loc-line (diagnostic-loc d))) '(5 6 7 8 9 10 4))
  (check-true (regexp-match? #rx"`itme` is not an element of QuestionnaireResponse" (diagnostic-message (car ds))))
  (check-true (regexp-match? #rx"`valeu` is not an element of Observation" (diagnostic-message (cadr ds))))
  (check-true (regexp-match? #rx"string cannot be assigned to category, a CodeableConcept" (diagnostic-message (caddr ds))))
  (check-true (regexp-match? #rx"s is a Reference; it cannot be assigned to id" (diagnostic-message (list-ref ds 3))))
  (check-true (regexp-match? #rx"no group nosuch" (diagnostic-message (list-ref ds 4))))
  (check-true (regexp-match? #rx"takes 2 parameters, called with 1" (diagnostic-message (list-ref ds 5))))
  ;; a constant is in scope in every group
  (check-equal? (findings "/// url = 'http://x/m'\n/// name = 'm'\nuses \"http://hl7.org/fhir/StructureDefinition/Patient\" alias P as target\nlet male = 'male';\ngroup g(source src, target tgt : P) { src -> tgt.gender = male; }\n") '())
  ;; the required Observation.code is never assigned: a warning at the group
  (check-equal? (diagnostic-severity (last ds)) 'warning)
  (check-true (regexp-match? #rx"Observation.code is required" (diagnostic-message (last ds))))
  ;; examples, validated: the Patient built from a response is a valid Patient
  (define m (ok-value (parse-fml good)))
  (define qr (hasheq 'resourceType "QuestionnaireResponse" 'status "completed" 'authored "2024-01-02"
                     'item (list (hasheq 'linkId "sex" 'answer (list (hasheq 'valueString "female"))))))
  (check-equal? (check-map/examples m (list qr)) '()))
