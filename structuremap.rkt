#lang racket/base

;;; structuremap.rkt — FML AST <-> the StructureMap resource (jsexpr), both directions.
;;;
;;; The StructureMap is the canonical, interchange form of a mapping (the FHIR analog of ELM for
;;; CQL): groups/inputs/rules with source/target/dependent parts, transform codes per the spec's
;;; StructureMap model. `fml->structuremap` emits it; `structuremap->fml` reads one back into the
;;; AST the engine runs (embedded FHIRPath strings are parsed, memoized), so a StructureMap
;;; resource from a package or a data source runs without its FML source. Law (tests/):
;;;   structuremap->fml ∘ fml->structuremap = id on the AST, up to the FML-only surface (how the
;;;   metadata was written, conceptmap prefixes) and the names `rule-N` given to unnamed rules.
;;; The `/// name = value` metadata lines are the resource's other elements (title, status,
;;; description, jurisdiction …), both ways.

(require racket/contract racket/list racket/string
         "parser/ast.rkt"
         (only-in fhirpath/print fhirpath->string)
         (only-in fhirpath/parser/parse parse-fhirpath)
         (only-in fhirpath/parser/ast fp-lit fp-lit? fp-lit-value)
         (only-in "schema.rkt" element-of element-key element-list? element-ty)
         (only-in fhir/serialize/jsexpr-util drop-false jref jlist))

(provide (contract-out [fml->structuremap (-> fml-map? hash?)]
                       [structuremap->fml (-> hash? fml-map?)]))

(define (fml->structuremap m)
  ;; the metadata lines are the resource's own elements; what the language has syntax for is the
  ;; syntax's. No status given is draft (the specification).
  (define stated (metadata->elements (fml-map-metadata m)))
  (for/fold ([sm stated]) ([(k v) (in-hash (syntax-elements m (hash-ref stated 'status "draft")))])
    (hash-set sm k v)))

;; syntax-elements : fml-map status -> jsexpr   the elements the map's declarations and groups state
(define (syntax-elements m status)
  (drop-false
   (hasheq 'resourceType "StructureMap"
           'url (fml-map-url m)
           'name (fml-map-name m)
           'status status
           'const (let ([cs (fml-map-constants m)])
                    (and (pair? cs) (for/list ([c (in-list cs)]) (hasheq 'name (car c) 'value (fp-text (cdr c))))))
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
      (hash-update h (cons (cm-mapping-source m) (cm-mapping-target m)) (lambda (l) (append l (list m))) '())))
  (hasheq 'resourceType "ConceptMap" 'id (fml-conceptmap-name cm) 'url (string-append "#" (fml-conceptmap-name cm))
          'status "active"
          'group (for/list ([(pair ms) (in-hash by-pair)])
                   (hasheq 'source (car pair) 'target (cdr pair)
                           'element (for/list ([m (in-list ms)])
                                      (hasheq 'code (cm-mapping-code m)
                                              'target (list (hasheq 'code (cm-mapping-target-code m)
                                                                    'relationship (relationship-of (cm-mapping-op m))))))))))
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
           'type (fml-source-type s)
           'min (or (fml-source-min s) #f)
           'max (fml-source-max s)
           'defaultValue (fp-text (fml-source-default s))
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
        [(fml-literal? tr) (values "copy" (list (arg->parameter tr)))]
        ;; a numeric or boolean literal is a copy of a typed value (as the reference writes it)
        [(typed-literal? tr) (values "copy" (list (arg->parameter tr)))]
        [(fml-fhirpath? tr) (values "evaluate" (list (arg->parameter tr)))]
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
           'listRuleId (let ([id (fml-target-share t)]) (and (string? id) id)))))

;; How a transform's argument crosses into a StructureMap parameter (and back, target<-jsexpr):
;;   a variable, a string             <-> valueId
;;   (fml-literal s)                  <-> valueString
;;   (fml-fhirpath (fp-lit n)) n int  <-> valueInteger n ;  a boolean  <-> valueBoolean
;;   (fml-fhirpath ast)               <-> valueString (its text; read back as an expression where
;;                                        the transform takes one — evaluate's last parameter)
(define (typed-literal? a)
  (and (fml-fhirpath? a) (fp-lit? (fml-fhirpath-ast a))
       (let ([v (fp-lit-value (fml-fhirpath-ast a))]) (or (exact-integer? v) (boolean? v)))))
(define (arg->parameter a)
  (cond [(fml-literal? a) (hasheq 'valueString (fml-literal-value a))]
        [(typed-literal? a)
         (define v (fp-lit-value (fml-fhirpath-ast a)))
         (if (boolean? v) (hasheq 'valueBoolean v) (hasheq 'valueInteger v))]
        [(fml-fhirpath? a) (hasheq 'valueString (fhirpath->string (fml-fhirpath-ast a)))]
        [else (hasheq 'valueId a)]))

;; ---- metadata <-> the resource's elements ------------------------------------------
;; `/// name{.property}* = value`: the name an element of StructureMap, each property a step into
;; it; a line with no value begins a complex element, a name written again repeats it. The
;; registered schema says which elements repeat (a single `jurisdiction` is still a list); without
;; one, an element repeats when it is written twice.

;; metadata->elements : (listof Metadata) -> jsexpr
(define (metadata->elements metadata)
  (for/fold ([sm (hasheq)]) ([m (in-list metadata)] #:unless (string=? (car m) ""))
    (set-at sm 'StructureMap (string-split (car m) ".") (cdr m))))

;; set-at : object type steps value -> object   the value at the path, inside the latest item of
;; each element on the way
(define (set-at object type steps value)
  (define e (element-of (and (symbol? type) type) (car steps)))
  (define key (if e (element-key e) (string->symbol (car steps))))
  (define repeats? (and e (element-list? e)))
  (define current (hash-ref object key #f))
  (define items (cond [(not current) '()] [(list? current) current] [else (list current)]))
  (cond
    [(null? (cdr steps))
     (define v (if (eq? value 'empty) (hasheq) value))
     (hash-set object key (if (or repeats? (pair? items)) (append items (list v)) v))]
    [else
     (define begun (if (null? items) (list (hasheq)) items))
     (define latest (let ([l (last begun)]) (if (hash? l) l (hasheq))))
     (define updated (append (drop-right begun 1) (list (set-at latest (and e (element-ty e)) (cdr steps) value))))
     (hash-set object key (if (or repeats? (list? current)) updated (car updated)))]))

;; the elements the language's own syntax states (and what a text cannot carry)
(define syntax-keys '(resourceType id meta text contained extension modifierExtension url name structure import const group))
;; the order a reader expects the rest in
(define metadata-order '(version title status experimental date publisher contact description useContext jurisdiction purpose copyright))

;; elements->metadata : StructureMap -> (listof Metadata)   every other element as metadata lines
(define (elements->metadata sm)
  (define keys (filter (lambda (k) (and (not (memq k syntax-keys)) (not (regexp-match? #rx"^_" (symbol->string k))))) (hash-keys sm)))
  (define ordered (append (filter (lambda (k) (memq k keys)) metadata-order)
                          (sort (filter (lambda (k) (not (memq k metadata-order))) keys) symbol<?)))
  (append* (for/list ([k (in-list ordered)]) (lines-of (symbol->string k) (hash-ref sm k)))))
(define (lines-of path v)
  (cond [(list? v) (append* (for/list ([item (in-list v)]) (lines-of path item)))]
        [(hash? v) (cons (cons path 'empty)
                         (append* (for/list ([k (in-list (sort (hash-keys v) symbol<?))]
                                             #:unless (regexp-match? #rx"^_|^extension$" (symbol->string k)))
                                    (lines-of (string-append path "." (symbol->string k)) (hash-ref v k)))))]
        [(or (string? v) (boolean? v) (number? v)) (list (cons path v))]
        [else '()]))

;; ---- StructureMap -> AST ------------------------------------------------------
(define (fp-of s) (and (string? s) (fml-fhirpath (parse-fhirpath s))))

(define (structuremap->fml sm)
  (fml-map (jref sm 'url) (jref sm 'name) (elements->metadata sm)
           (for/list ([s (in-list (jlist sm 'structure))])
             (fml-use (jref s 'url) (jref s 'alias) (string->symbol (jref s 'mode "source"))))
           (filter string? (jlist sm 'import))
           (for/list ([c (in-list (jlist sm 'const))] #:when (and (string? (jref c 'name)) (string? (jref c 'value))))
             (cons (jref c 'name) (fp-of (jref c 'value))))
           (map group<-jsexpr (jlist sm 'group))
           (for/list ([c (in-list (jlist sm 'contained))] #:when (equal? (jref c 'resourceType) "ConceptMap"))
             (conceptmap<-jsexpr c))))

;; a contained ConceptMap back to the embedded form (R5 relationship or R4 equivalence)
(define (conceptmap<-jsexpr c)
  (define name (or (jref c 'id) (let ([u (jref c 'url "")]) (if (string-prefix? u "#") (substring u 1) u))))
  (fml-conceptmap name (hash)
                  (for*/list ([g (in-list (jlist c 'group))] [e (in-list (jlist g 'element))] [t (in-list (jlist e 'target))])
                    (cm-mapping (jref g 'source) (jref e 'code) (op-of (or (jref t 'relationship) (jref t 'equivalence)))
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
  (fml-source (jref s 'context) (jref s 'element)
              (jref s 'type)
              (let ([n (jref s 'min)]) (and (exact-nonnegative-integer? n) n))
              (jref s 'max)
              (default<-jsexpr s)
              (let ([m (jref s 'listMode)]) (and (string? m) (string->symbol m)))
              (jref s 'variable)
              (fp-of (jref s 'condition)) (fp-of (jref s 'check)) (fp-of (jref s 'logMessage))))

;; default<-jsexpr : source -> (fml-fhirpath ast) | #f   R5's `defaultValue` is the FHIRPath text;
;; R4's `defaultValue[x]` a typed value, read as the literal it is
(define (default<-jsexpr s)
  (cond [(jref s 'defaultValue) => fp-of]
        [else (for/first ([(k v) (in-hash s)] #:when (regexp-match? #rx"^defaultValue[A-Z]" (symbol->string k)))
                (cond [(string? v) (fml-fhirpath (fp-lit v))]
                      [(or (boolean? v) (exact-integer? v)) (fml-fhirpath (fp-lit v))]
                      [(number? v) (fp-of (number->string v))]
                      [else #f]))]))

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
  (define share (and (member "share" (jlist t 'listMode)) (or (jref t 'listRuleId) #t)))
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
               "  src.item as item where (linkId = 'x') check (item.exists()) log('hi') -> tgt.name as n share nm, n.text = (item.answer.valueString) then helper(item, n) \"r1\";\n"
               "  src.id first as i -> tgt.id = truncate(i, 20), tgt.gender = 'female', tgt.active = true, tgt.deceasedBoolean = i, tgt.multipleBirthInteger = 2;\n"
               "  src.name as nm -> tgt.name as t first then { nm.text as x -> t.text = x; };\n"
               "  src -> tgt.birthDate = cast(src, 'date'), tgt.text = create('Narrative') as nar then { src -> nar.status = 'generated'; };\n"
               "  src.item as item -> evaluate(item, answer.first()) as ans;\n"
               "}\n"
               "group helper(source s, target t) { s.text as x -> t.family = x; }\n"))
  (define ast (ok-value (parse-fml src)))
  (define (key m) (struct-copy fml-map m [metadata '()]
                               [conceptmaps (for/list ([c (fml-map-conceptmaps m)]) (struct-copy fml-conceptmap c [prefixes (hash)]))]))
  (check-equal? (key (structuremap->fml (fml->structuremap ast))) (key ast))
  ;; what the specification's source clause states crosses too: a type, a cardinality, a default;
  ;; and a constant, a bare `share`, and the metadata as the resource's own elements
  (define full (ok-value (parse-fml (string-append
                                     "/// url = 'http://x/full'\n/// name = 'Full'\n/// title = 'A full map'\n/// status = 'active'\n"
                                     "/// experimental = true\n/// jurisdiction =\n/// jurisdiction.coding =\n"
                                     "/// jurisdiction.coding.system = 'urn:iso:std:iso:3166'\n/// jurisdiction.coding.code = 'AQ'\n"
                                     "/// description = \"\"\"\nYou should use my *awesome* map.\nIt does really cool things.\n\"\"\"\n"
                                     "let limit = 100;\n"
                                     "group g(source src, target tgt) {\n"
                                     "  src.value : integer 0..* default (10) first as vs0 where (value >= 10) check (value <= limit) log (value) -> tgt.value = vs0 share;\n"
                                     "}\n"))))
  (define full-sm (fml->structuremap full))
  (check-equal? (hash-ref full-sm 'url) "http://x/full")
  (check-equal? (hash-ref full-sm 'title) "A full map")
  (check-equal? (hash-ref full-sm 'status) "active")
  (check-equal? (hash-ref full-sm 'experimental) #t)
  (check-equal? (hash-ref full-sm 'description) "You should use my *awesome* map.\nIt does really cool things.")
  (check-equal? (hash-ref full-sm 'const) (list (hasheq 'name "limit" 'value "100")))
  (define full-source (car (hash-ref (car (hash-ref (car (hash-ref full-sm 'group)) 'rule)) 'source)))
  (check-equal? (for/list ([k '(type min max defaultValue listMode variable)]) (hash-ref full-source k))
                '("integer" 0 "*" "10" "first" "vs0"))
  (define full-target (car (hash-ref (car (hash-ref (car (hash-ref full-sm 'group)) 'rule)) 'target)))
  (check-equal? (hash-ref full-target 'listMode) '("share"))
  (check-false (hash-has-key? full-target 'listRuleId))
  ;; a map with no status is a draft (the specification)
  (check-equal? (hash-ref sm 'status) "draft")
  ;; and back: the same constants, sources and targets; the metadata as the elements it set
  (define back (structuremap->fml full-sm))
  (check-equal? (fml-map-constants back) (fml-map-constants full))
  (check-equal? (fml-map-groups back) (fml-map-groups full))
  (check-equal? (fml->structuremap back) full-sm))
