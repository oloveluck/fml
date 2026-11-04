#lang racket/base

;;; structuremap.rkt — FML AST <-> the StructureMap resource (jsexpr), both directions.
;;;
;;; The StructureMap is the canonical, interchange form of a mapping (the FHIR analog of ELM for
;;; CQL): groups/inputs/rules with source/target/dependent parts, transform codes per the spec's
;;; StructureMap model. `fml->structuremap` emits it; `structuremap->fml` reads one back into the
;;; AST the engine runs (embedded FHIRPath strings are parsed, memoized), so a StructureMap
;;; resource from a package or a data source runs without its FML source. Law (tests/):
;;;   structuremap->fml ∘ fml->structuremap = id on the AST, up to the FML-only surface (`///`
;;;   metadata, conceptmap prefixes) and the names `rule-N` given to unnamed rules.

(require racket/contract racket/list racket/string
         "parser/ast.rkt"
         (only-in fhirpath/print fhirpath->string)
         (only-in fhirpath/parser/parse parse-fhirpath)
         (only-in fhirpath/parser/ast fp-lit fp-lit? fp-lit-value)
         (only-in fhir/serialize/jsexpr-util drop-false))

(provide (contract-out [fml->structuremap (-> fml-map? hash?)]
                       [structuremap->fml (-> hash? fml-map?)]))

(define (fml->structuremap m)
  (drop-false
   (hasheq 'resourceType "StructureMap"
           'url (fml-map-url m)
           'name (fml-map-name m)
           'status "active"
           'structure (let ([us (fml-map-uses m)])
                        (and (pair? us)
                             (for/list ([u (in-list us)])
                               (drop-false (hasheq 'url (fml-use-url u)
                                                   'mode (symbol->string (fml-use-mode u))
                                                   'alias (fml-use-alias u))))))
           'import (let ([is (fml-map-imports m)]) (and (pair? is) is))
           'contained (let ([cms (fml-map-conceptmaps m)])
                        (and (pair? cms) (map conceptmap->jsexpr cms)))
           'group (for/list ([g (in-list (fml-map-groups m))]) (group->jsexpr g)))))

;; an embedded concept map as a contained ConceptMap (url #name), one group per system pair
(define (conceptmap->jsexpr cm)
  (define by-pair
    (for/fold ([h (hash)]) ([m (in-list (fml-conceptmap-mappings cm))])
      (hash-update h (cons (vector-ref m 0) (vector-ref m 3)) (lambda (l) (append l (list m))) '())))
  (hasheq 'resourceType "ConceptMap" 'id (fml-conceptmap-name cm) 'url (string-append "#" (fml-conceptmap-name cm))
          'status "active"
          'group (for/list ([(pair ms) (in-hash by-pair)])
                   (hasheq 'source (car pair) 'target (cdr pair)
                           'element (for/list ([m (in-list ms)])
                                      (hasheq 'code (vector-ref m 1)
                                              'target (list (hasheq 'code (vector-ref m 4)
                                                                    'relationship (relationship-of (vector-ref m 2))))))))))
(define (relationship-of op)
  (case op [("==" "=") "equivalent"] [("-" "~") "related-to"] [("<=" "<") "source-is-narrower-than-target"]
           [(">=" ">") "source-is-broader-than-target"] [("!=") "not-related-to"] [else "related-to"]))

(define (group->jsexpr g)
  (drop-false
   (hasheq 'name (fml-group-name g)
           'extends (fml-group-extends g)
           'typeMode (cond [(member "type+" (fml-group-flags g)) "type-and-types"]
                           [(member "types" (fml-group-flags g)) "types"]
                           [else #f])
           'input (for/list ([p (in-list (fml-group-params g))])
                    (drop-false (hasheq 'name (fml-param-name p)
                                        'type (fml-param-type p)
                                        'mode (symbol->string (fml-param-mode p)))))
           'rule (for/list ([r (in-list (fml-group-rules g))] [i (in-naturals)])
                   (rule->jsexpr r i)))))

(define (rule->jsexpr r i)
  (drop-false
   (hasheq 'name (or (fml-rule-name r) (format "rule-~a" i))
           'source (for/list ([s (in-list (fml-rule-sources r))]) (source->jsexpr s))
           'target (let ([ts (fml-rule-targets r)])
                     (and (pair? ts) (for/list ([t (in-list ts)]) (target->jsexpr t))))
           'rule (let ([ns (fml-rule-nested r)])
                   (and (pair? ns) (for/list ([n (in-list ns)] [j (in-naturals)]) (rule->jsexpr n j))))
           'dependent (let ([ds (fml-rule-dependents r)])
                        (and (pair? ds)
                             (for/list ([d (in-list ds)])
                               (hasheq 'name (fml-call-name d)
                                       'parameter (for/list ([a (in-list (fml-call-args d))])
                                                    (if (string? a)
                                                        (hasheq 'valueId a)
                                                        (hasheq 'valueString (fhirpath->string (fml-fhirpath-ast a))))))))))))

;; fp-text : (fml-fhirpath ast)|#f -> the printed expression string (for condition/check/log)
(define (fp-text f) (and f (fhirpath->string (fml-fhirpath-ast f))))

(define (source->jsexpr s)
  (drop-false
   (hasheq 'context (fml-source-context s)
           'element (fml-source-element s)
           'variable (fml-source-variable s)
           'condition (fp-text (fml-source-where s))
           'check (fp-text (fml-source-check s))
           'logMessage (fp-text (fml-source-log-expr s))
           'listMode (and (fml-source-list-mode s) (symbol->string (fml-source-list-mode s))))))

(define (target->jsexpr t)
  (define-values (transform params)
    (let ([tr (fml-target-transform t)])
      (cond
        [(not tr) (values #f #f)]
        [(fml-literal? tr) (values "copy" (list (hasheq 'valueString (fml-literal-value tr))))]
        ;; a numeric or boolean literal is a copy of a typed value (as the reference writes it)
        [(and (fml-fhirpath? tr) (fp-lit? (fml-fhirpath-ast tr))
              (let ([v (fp-lit-value (fml-fhirpath-ast tr))]) (or (exact-integer? v) (boolean? v))))
         (values "copy" (list (arg->parameter tr)))]
        [(fml-fhirpath? tr) (values "evaluate" (list (hasheq 'valueString (fhirpath->string (fml-fhirpath-ast tr)))))]
        [(fml-var? tr) (values "copy" (list (hasheq 'valueId (fml-var-name tr))))]
        [(fml-invoke? tr) (values (fml-invoke-name tr) (map arg->parameter (fml-invoke-args tr)))]
        [else (values #f #f)])))

  (drop-false
   (hasheq 'context (fml-target-context t)
           'element (fml-target-element t)
           'variable (fml-target-variable t)
           'transform transform
           'parameter params
           'listMode (let ([ms (append (if (fml-target-list-mode t) (list (symbol->string (fml-target-list-mode t))) '())
                                       (if (fml-target-share t) (list "share") '()))])
                       (and (pair? ms) ms))
           'listRuleId (fml-target-share t))))

;; an invocation argument as a StructureMap parameter: a variable is valueId, a string literal
;; valueString, a numeric/boolean literal its typed value, any other expression its text
(define (arg->parameter a)
  (cond [(fml-literal? a) (hasheq 'valueString (fml-literal-value a))]
        [(fml-fhirpath? a)
         (define ast (fml-fhirpath-ast a))
         (cond [(and (fp-lit? ast) (exact-integer? (fp-lit-value ast))) (hasheq 'valueInteger (fp-lit-value ast))]
               [(and (fp-lit? ast) (boolean? (fp-lit-value ast))) (hasheq 'valueBoolean (fp-lit-value ast))]
               [else (hasheq 'valueString (fhirpath->string ast))])]
        [else (hasheq 'valueId a)]))

;; ---- StructureMap -> AST ------------------------------------------------------
(define (jref h k [d #f]) (if (hash? h) (hash-ref h k d) d))
(define (jlist h k) (let ([v (jref h k '())]) (if (list? v) v '())))
(define (fp-of s) (and (string? s) (fml-fhirpath (parse-fhirpath s))))

(define (structuremap->fml sm)
  (fml-map (jref sm 'url) (jref sm 'name) '()
           (for/list ([s (in-list (jlist sm 'structure))])
             (fml-use (jref s 'url) (jref s 'alias) (string->symbol (jref s 'mode "source"))))
           (filter string? (jlist sm 'import))
           (map group<-jsexpr (jlist sm 'group))
           (for/list ([c (in-list (jlist sm 'contained))] #:when (equal? (jref c 'resourceType) "ConceptMap"))
             (conceptmap<-jsexpr c))))

;; a contained ConceptMap back to the embedded form (R5 relationship or R4 equivalence)
(define (conceptmap<-jsexpr c)
  (define name (or (jref c 'id) (let ([u (jref c 'url "")]) (if (string-prefix? u "#") (substring u 1) u))))
  (fml-conceptmap name (hash)
                  (for*/list ([g (in-list (jlist c 'group))] [e (in-list (jlist g 'element))] [t (in-list (jlist e 'target))])
                    (vector (jref g 'source) (jref e 'code) (op-of (or (jref t 'relationship) (jref t 'equivalence)))
                            (jref g 'target) (jref t 'code)))))
(define (op-of rel)
  (case rel
    [("equivalent" "equal") "=="]
    [("source-is-narrower-than-target" "wider" "subsumes") "<="]
    [("source-is-broader-than-target" "narrower" "specializes") ">="]
    [("not-related-to" "disjoint" "unmatched") "!="]
    [else "-"]))

(define (group<-jsexpr g)
  (fml-group (jref g 'name)
             (for/list ([i (in-list (jlist g 'input))])
               (fml-param (string->symbol (jref i 'mode "source")) (jref i 'name) (jref i 'type)))
             (jref g 'extends)
             (case (jref g 'typeMode) [("types") '("types")] [("type-and-types") '("type+")] [else '()])
             (map rule<-jsexpr (jlist g 'rule))))

(define (rule<-jsexpr r)
  (fml-rule (map source<-jsexpr (jlist r 'source))
            (map target<-jsexpr (jlist r 'target))
            (for/list ([d (in-list (jlist r 'dependent))])
              (fml-call (jref d 'name)
                        (for/list ([p (in-list (jlist d 'parameter))])
                          (cond [(jref p 'valueId) => values]
                                [else (fml-fhirpath (parse-fhirpath (parameter-text p)))]))))
            (map rule<-jsexpr (jlist r 'rule))
            (let ([n (jref r 'name)]) (and (string? n) (not (regexp-match? #px"^rule-[0-9]+$" n)) n))))

(define (source<-jsexpr s)
  (fml-source (jref s 'context) (jref s 'element) (jref s 'variable)
              (fp-of (jref s 'condition)) (fp-of (jref s 'check)) (fp-of (jref s 'logMessage))
              (let ([m (jref s 'listMode)]) (and (string? m) (string->symbol m)))))

;; a parameter's value as FHIRPath text (a typed literal prints as itself)
(define (parameter-text p)
  (cond [(jref p 'valueString) => values]
        [(jref p 'valueId) => values]
        [(hash-has-key? p 'valueInteger) (number->string (jref p 'valueInteger))]
        [(hash-has-key? p 'valueDecimal) (let ([v (jref p 'valueDecimal)]) (if (number? v) (number->string v) (format "~a" v)))]
        [(hash-has-key? p 'valueBoolean) (if (jref p 'valueBoolean) "true" "false")]
        [else ""]))

(define (target<-jsexpr t)
  (define tr (jref t 'transform))
  (define ps (jlist t 'parameter))
  ;; a parameter as an invocation argument: valueId a variable; valueString a literal, except the
  ;; expression of `evaluate` (its last parameter); a typed literal a FHIRPath literal
  (define (arg p last?)
    (cond [(jref p 'valueId) => values]
          [(jref p 'valueString) => (lambda (s) (if (and last? (equal? tr "evaluate")) (fml-fhirpath (parse-fhirpath s)) (fml-literal s)))]
          [else (fml-fhirpath (parse-fhirpath (parameter-text p)))]))
  (define transform
    (cond
      [(not tr) #f]
      [(and (equal? tr "copy") (= 1 (length ps)) (jref (car ps) 'valueId)) => fml-var]
      [(and (equal? tr "copy") (= 1 (length ps)) (jref (car ps) 'valueString)) => fml-literal]
      [(and (equal? tr "copy") (= 1 (length ps))) (fml-fhirpath (parse-fhirpath (parameter-text (car ps))))]
      [(and (equal? tr "evaluate") (= 1 (length ps)) (jref (car ps) 'valueString)) => (lambda (s) (fml-fhirpath (parse-fhirpath s)))]
      [else (fml-invoke tr (for/list ([p (in-list ps)] [i (in-naturals 1)]) (arg p (= i (length ps)))))]))
  (define share (and (member "share" (jlist t 'listMode)) (jref t 'listRuleId)))
  (define mode (for/or ([m (in-list (jlist t 'listMode))]) (and (member m '("first" "last" "single")) (string->symbol m))))
  (fml-target (jref t 'context) (jref t 'element) transform (jref t 'variable) share mode))

(module+ test
  (require rackunit (only-in "parser/parse.rkt" parse-fml) (only-in fhir/result/result ok-value))
  (define sm (fml->structuremap
              (ok-value (parse-fml "map \"http://x\" = \"t\"\nuses \"http://h/P\" alias P as target\ngroup g(source s, target t : P) { s -> t.gender = 'female' \"r\"; }"))))
  (check-equal? (hash-ref sm 'resourceType) "StructureMap")
  (define rule (car (hash-ref (car (hash-ref sm 'group)) 'rule)))
  (check-equal? (hash-ref (car (hash-ref rule 'target)) 'transform) "copy")
  ;; the round trip: every construct back to the AST it came from
  (define src (string-append
               "map \"http://x/m\" = \"m\"\n"
               "conceptmap \"cm\" { prefix s = \"http://s\" prefix t = \"http://t\" s:a == t:b s:c - t:d }\n"
               "uses \"http://hl7.org/fhir/StructureDefinition/QuestionnaireResponse\" alias QR as source\n"
               "uses \"http://hl7.org/fhir/StructureDefinition/Patient\" alias Patient as target\n"
               "imports \"http://x/other\"\n"
               "group entry(source src : QR, target tgt : Patient) extends base <<types>> {\n"
               "  src.item as item where linkId = 'x' check (item.exists()) log('hi') -> tgt.name as n share nm, n.text = (item.answer.valueString) then helper(item, n) \"r1\";\n"
               "  src.id first as i -> tgt.id = truncate(i, 20), tgt.gender = 'female', tgt.active = true, tgt.deceasedBoolean = i, tgt.multipleBirthInteger = 2;\n"
               "  src.name as nm -> tgt.name as t first then { nm.text as x -> t.text = x; };\n"
               "  src -> tgt.birthDate = cast(src, 'date'), tgt.text = create('Narrative') as nar then { src -> nar.status = 'generated'; };\n"
               "  src.item as item -> evaluate(item, answer.first()) as ans;\n"
               "}\n"
               "group helper(source s, target t) { s.text as x -> t.family = x; }\n"))
  (define ast (ok-value (parse-fml src)))
  (define (key m) (struct-copy fml-map m [metadata '()]
                               [conceptmaps (for/list ([c (fml-map-conceptmaps m)]) (struct-copy fml-conceptmap c [prefixes (hash)]))]))
  (check-equal? (structuremap->fml (fml->structuremap ast)) (key ast)))
