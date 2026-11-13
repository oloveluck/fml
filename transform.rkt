#lang racket/base

;;; transform.rkt — execute an FML map: source resource -> target resource (jsexpr).
;;;
;;; The engine runs the map's first group with (source node, fresh target). A Scope binds FML
;;; variables to VALUES: a source value is a typed node/primitive (FHIRPath-evaluable); a target
;;; value is a mutable TSLOT (a box over the jsexpr under construction, so nested rules mutate
;;; their parent in place — FML targets are built incrementally).
;;;
;;; Types come from the registered towers and from the map's `uses`: a structure the towers lack (a
;;; logical model) is resolved — the current data source, then the packages — and registered from
;;; its StructureDefinition (fhir/sd/logical), so its instances are typed nodes like any other.
;;;
;;; Rule execution (the StructureMap specification, as the reference implementation reads it):
;;;   every combination of the rule's source matches (each source's elements, where-filtered, in
;;;   its list mode; `check` must hold, `log` is reported): bind the `as` variables; run the target
;;;   ops in order, each threading the variables it binds; then the dependent group calls and the
;;;   nested rules in the extended scope.
;;;   A target without a transform CREATES its element; when the rule has one source and one
;;;   target and nothing after, the source value is carried over: a primitive is copied (coerced to
;;;   the target element's type), a complex value runs the `types` group whose parameter types match
;;;   (or is copied whole when there is none). `share v` reuses one element across rules.
;;;   Transforms: `= 'lit'`, `= (fhirpath)` over the rule's variables, `= var`, create, copy,
;;;   reference, cast, evaluate, truncate, translate (an embedded conceptmap or a ConceptMap the
;;;   terminology service can see), uuid, and the datatype constructors c cc qty id cp append.
;;; A value assigned to a primitive element is coerced to it: "12345" into an integer element is 12345.

(require racket/contract racket/match racket/list racket/string racket/random
         "parser/ast.rkt"
         (only-in fhir/model/node fhir? fhir-ref fhir-type fhir-field-names)
         (only-in fhir/primitives/primitives primitive->jsexpr)
         (only-in fhir/model/base primitive-type? primitive-type-value)
         "schema.rkt"
         (only-in fhir/model/schema type-schema-parent resource-parent?)
         (only-in fhir/model/registry type-registered? type-schema-of)
         (only-in fhirpath/types fhir-ancestors)
         (only-in fhir/sd/logical register-structure-definition! resolve-structure-definition/data)
         (only-in fhir/sd/sd-index resolve-canonical-raw)
         (only-in fhir/registry registered-resource)
         (only-in fhir/diagnostic diagnostic loc raise-diagnostic attached-loc)
         (only-in fhir/canonical canonical-url)
         (only-in fhir/data-source current-data-source data-source-all)
         (only-in "structuremap.rkt" structuremap->fml)
         (only-in fhir/terminology/operations current-terminology-resolver)
         (only-in fhir/terminology/service translate/json)
         (only-in fhirpath/parser/ast fp-invoke fp-invoke? fp-call fp-binop fp-unop fp-index fp-typeop fp-extern)
         (only-in fhirpath/eval eval-fhirpath)
         (only-in fhirpath/print fhirpath->string)
         (only-in fhirpath/value item->system fp-long? fp-long-v)
         (only-in fhirpath/decimal fp-decimal? decimal->string)
         (only-in fhirpath/temporal-ops moment? moment-kind moment->string)
         (only-in fhir/serialize/to-json node->jsexpr))

(provide (contract-out
          ;; run-map : parsed map × typed source resource -> the target resource jsexpr
          [run-map (-> fml-map? fhir? hash?)]
          ;; where a rule's `log(…)` goes: a procedure of the message (nowhere by default)
          [current-fml-log (parameter/c (-> string? any))]
          ;; when a mutable hasheq: target -> EmptyReason, for each target that wrote nothing
          ;; (validate-result says why an element was not written)
          [current-empty-targets (parameter/c (or/c #f (and/c hash? (not/c immutable?))))]
          [struct empty-value ([source any/c])]
          [struct empty-where ([where fml-fhirpath?] [count exact-nonnegative-integer?] [items list?] [rule fml-rule?])]
          ;; group-param : group mode -> fml-param | #f   the group's first parameter of that mode
          [group-param (-> fml-group? (or/c 'source 'target) (or/c fml-param? #f))]
          ;; url-leaf : canonical -> string   its last path step, the |version dropped (the type's name)
          [url-leaf (-> string? string?)]
          ;; uses-types : map -> (hash alias-or-type-name -> type symbol)   the `uses` resolved,
          ;; logical models registered along the way
          [uses-types (-> fml-map? hash?)]
          ;; register-structure-map! : map -> void   resolvable by its url in this process
          [register-structure-map! (-> fml-map? void?)]
          ;; resolve-structure-map : canonical -> map | #f   registered maps, then StructureMaps in
          ;; the current data source, the resources modules registered (fhir/registry), then the
          ;; packages (|version ignored)
          [resolve-structure-map (-> string? (or/c fml-map? #f))]
          ;; registered-structure-maps : -> (listof url)
          [registered-structure-maps (-> (listof string?))]
          ;; map-imports : map -> (listof map)   the map and its imports, transitively (missing ones
          ;; skipped), the map itself first
          [map-imports (-> fml-map? (listof fml-map?))]
          ;; unresolved-imports : map -> (listof url)   the imports nothing resolves, transitively
          [unresolved-imports (-> fml-map? (listof string?))]
          ;; transform-names : -> (listof string)   the transforms the engine runs
          [transform-names (-> (listof string?))]))

;; ---- maps by canonical ---------------------------------------------------------
(define registered (make-hash))
(define (register-structure-map! m)
  (when (string? (fml-map-url m)) (hash-set! registered (fml-map-url m) m)))
(define (registered-structure-maps) (sort (hash-keys registered) string<?))
(define converted (make-weak-hasheq))           ; a StructureMap jsexpr -> its AST, once
(define (resolve-structure-map url)
  (define key (canonical-url url))
  (define (as-ast sm) (hash-ref! converted sm (lambda () (structuremap->fml sm))))
  (or (hash-ref registered key #f)
      (let ([sm (findf (lambda (r) (equal? (hash-ref r 'url #f) key))
                       ((data-source-all (current-data-source)) "StructureMap"))])
        (and sm (as-ast sm)))
      (let ([sm (registered-resource "StructureMap" key)]) (and sm (as-ast sm)))
      (let ([sm (resolve-canonical-raw "StructureMap" key)]) (and sm (as-ast sm)))))

(define (map-imports m)
  (let loop ([todo (list m)] [seen '()] [acc '()])
    (cond
      [(null? todo) (reverse acc)]
      [(member (fml-map-url (car todo)) seen) (loop (cdr todo) seen acc)]
      [else
       (define im (car todo))
       (define more (filter values (map resolve-structure-map (fml-map-imports im))))
       (loop (append (cdr todo) more) (cons (fml-map-url im) seen) (cons im acc))])))

;; unresolved-imports : map -> (listof url)   the `imports` of the map (and of the maps it imports)
;; that nothing resolves: their groups are unknown here
(define (unresolved-imports m)
  (remove-duplicates
   (for*/list ([im (in-list (map-imports m))] [u (in-list (fml-map-imports im))]
               #:unless (resolve-structure-map u))
     u)))

(define current-fml-log (make-parameter void))

;; An EmptyReason says why a target wrote nothing; it is one of:
;;   (empty-value source)                  the rule matched and its value expression gave nothing;
;;                                         source: what the rule read, as JSON (#f: not a node)
;;   (empty-where where count items rule)  the rule's source matched `count` items and its `where`
;;                                         held for none; items: those read, as JSON; rule: the rule
;;                                         whose `where` it is (the problem is said at its line)
;; `current-empty-targets`, when set, is a mutable hasheq target -> EmptyReason filled by a run.
(struct empty-value (source) #:transparent)
(struct empty-where (where count items rule) #:transparent)
(define current-empty-targets (make-parameter #f))
;; note-empty! : target EmptyReason -> void   recorded when someone is listening
(define (note-empty! t reason)
  (define h (current-empty-targets))
  (when h (hash-set! h t reason)))

;; ---- target slots -----------------------------------------------------------
;; A TSLOT is (tslot get set! type prim) — get/set! close over the parent so mutation propagates up
;; to the root box; `type` is the element's TYPE SYMBOL (a complex type, from the registry), `prim`
;; its primitive code ("integer") when it is a primitive element; they drive the child shape (max>1
;; appends, max-1 sets, a choice element picks its key from the assigned value's type) and the
;; coercion of assigned values.
(struct tslot (get set! type prim))
(define (make-root-slot init type)
  (define b (box init))
  (tslot (lambda () (unbox b)) (lambda (v) (set-box! b v)) type #f))

;; choice-field : node elem -> the field holding `elem`: itself, or a choice variant (`value` ->
;; `valueDecimal`, whichever the node has)
(define (choice-field node elem)
  (define sym (string->symbol elem))
  (if (fhir-ref node sym #f)
      sym
      (or (for/first ([k (in-list (fhir-field-names node))]
                      #:when (let ([n (symbol->string k)])
                               (and (> (string-length n) (string-length elem))
                                    (string-prefix? n elem)
                                    (char-upper-case? (string-ref n (string-length elem))))))
            k)
          sym)))
;; type-name-of : value -> type name | #f   a node's type, a primitive's code, and for a plain
;; value (a default, a constant, an expression's result: a FHIRPath System value) the FHIR primitive it is
(define (type-name-of v)
  (cond [(primitive-type? v) (regexp-replace #rx"^struct:fhir-" (symbol->string (vector-ref (struct->vector v) 0)) "")]
        [(fhir? v) (symbol->string (fhir-type v))]
        [(boolean? v) "boolean"]
        [(exact-integer? v) "integer"]
        [(fp-decimal? v) "decimal"]
        [(string? v) "string"]
        [(moment? v) (case (moment-kind v) [(date) "date"] [(time) "time"] [else "dateTime"])]
        [else #f]))
;; narrowed : declared-type|#f forced-type-name|#f -> type|#f   the type of what goes into a
;; complex element: the type assigned (`create('Patient')`) when the element declares none the
;; registry knows, or one the assigned type is a kind of — Bundle.entry.resource holds any
;; resource, and the Patient created into it has a Patient's elements (its `name` repeats) — else
;; the declared type
(define (narrowed declared forced)
  (define assigned (and forced (not (and declared (equal? forced (symbol->string declared)))) (type-name->ty forced)))
  (cond [(not (symbol? assigned)) declared]
        [(not declared) assigned]
        [(memq declared (fhir-ancestors assigned)) assigned]
        [else declared]))

;; child-slot! : parent elem [forced-type] -> a slot for one occurrence of parent.elem
(define (child-slot! parent elem [forced-type #f])
  (define e (or (element-of (tslot-type parent) elem forced-type)
                (element (string->symbol elem) #f (type-name->ty forced-type))))   ; not in the schema: open
  (define key (element-key e))
  (define prim (and (prim? (element-ty e)) (prim-code (element-ty e))))
  (define child-type (and (not prim) (narrowed (element-ty e) forced-type)))
  (cond
    [(element-list? e)
     (define h0 (as-hash ((tslot-get parent))))
     (define lst (as-list (hash-ref h0 key '())))
     (define index (length lst))                           ; this occurrence's place in the list
     ((tslot-set! parent) (hash-set h0 key (append lst (list (hash)))))
     (tslot
      (lambda () (list-ref (as-list (hash-ref (as-hash ((tslot-get parent))) key '())) index))
      (lambda (v)
        (define h (as-hash ((tslot-get parent))))
        ((tslot-set! parent) (hash-set h key (list-set (as-list (hash-ref h key '())) index v))))
      child-type prim)]
    [else
     (tslot
      (lambda () (hash-ref (as-hash ((tslot-get parent))) key (hash)))
      (lambda (v) ((tslot-set! parent) (hash-set (as-hash ((tslot-get parent))) key v)))
      child-type prim)]))

(define (as-hash v) (if (hash? v) v (hash)))
(define (as-list v) (if (list? v) v (list v)))

;; finalize : drop empty hashes/lists so `tgt.name as name` with no assignments doesn't emit {}
(define (finalize v)
  (cond
    [(hash? v)
     (for/fold ([h (hash)]) ([(k x) (in-hash v)])
       (define fx (finalize x))
       (if (or (equal? fx (hash)) (equal? fx '())) h (hash-set h k fx)))]
    [(list? v) (filter (lambda (x) (not (or (equal? x (hash)) (equal? x '())))) (map finalize v))]
    [else v]))

;; ---- coercion to the target element's type -----------------------------------
;; coerce : jsexpr prim-code|#f -> jsexpr   what the element's primitive type holds
(define (coerce v prim)
  (cond
    [(or (not prim) (hash? v) (list? v) (eq? v 'null)) v]
    [(member prim '("integer" "positiveInt" "unsignedInt" "integer64"))
     (cond [(string? v) (let ([n (string->number (string-trim v))]) (if (and n (exact-integer? n)) n v))]
           [(and (number? v) (integer? v)) (inexact->exact v)]
           [else v])]
    [(equal? prim "decimal") (if (string? v) (or (string->number (string-trim v)) v) v)]
    [(equal? prim "boolean") (cond [(equal? v "true") #t] [(equal? v "false") #f] [else v])]
    [else (cond [(number? v) (number->string v)] [(boolean? v) (if v "true" "false")] [else v])]))

;; ---- scope ------------------------------------------------------------------
;; vars : name(string) -> source value | TSLOT ; shares : share-name -> TSLOT (per run)
(struct scope (vars shares) #:transparent)
(define (bind sc name v) (scope (hash-set (scope-vars sc) name v) (scope-shares sc)))
(define (lookup sc name) (hash-ref (scope-vars sc) name #f))
(define source-key " source")           ; the rule's current source element, for implicit copies

;; ---- the map's types ----------------------------------------------------------
;; A Ctx is (ctx map types maps constants): the map, its `uses` resolved to type symbols (alias and
;; type name, the imports' too), the maps whose groups are callable (the map first, then its
;; imports), and the values of their constants (`let name = expression;`) by name
(struct ctx (map types maps constants))

(define (uses-types m)
  (for/fold ([h (hash)]) ([u (in-list (fml-map-uses m))])
    (define url (canonical-url (fml-use-url u)))
    (define leaf (url-leaf url))
    (define sym
      (cond [(type-registered? (string->symbol leaf)) (string->symbol leaf)]
            [else (let ([sd (resolve-structure-definition/data url)])
                    (and sd (register-structure-definition! sd)))]))
    (define h1 (if sym (hash-set h leaf sym) h))
    (if (and sym (fml-use-alias u)) (hash-set h1 (fml-use-alias u) sym) h1)))

;; url-leaf : canonical -> string   the last step of its path (`…/StructureDefinition/Patient|4.0.1`
;; -> "Patient"): the name a `uses` gives its type when it has no alias
(define (url-leaf url)
  (define u (canonical-url url))
  (define end (let loop ([i (string-length u)]) (if (and (> i 0) (char=? (string-ref u (sub1 i)) #\/)) (loop (sub1 i)) i)))
  (define start (let loop ([i end]) (if (and (> i 0) (not (char=? (string-ref u (sub1 i)) #\/))) (loop (sub1 i)) i)))
  (substring u start end))

;; group-param : group mode -> fml-param | #f   the group's first parameter of that mode
(define (group-param g mode)
  (findf (lambda (p) (eq? (fml-param-mode p) mode)) (fml-group-params g)))

;; type-sym : ctx type-name|#f -> symbol|#f   a parameter's declared type
(define (type-sym c name)
  (and name (or (hash-ref (ctx-types c) name #f)
                (and (type-registered? (string->symbol name)) (string->symbol name)))))

;; ---- entry ------------------------------------------------------------------
(define (run-map m src)
  (define maps (map-imports m))
  (define c (ctx m (for/fold ([h (hash)]) ([im (in-list (reverse maps))]) ; the map's own names win
                     (for/fold ([h h]) ([(k v) (in-hash (uses-types im))]) (hash-set h k v)))
                 maps
                 (constant-values maps)))
  (define entry (car (fml-map-groups m)))
  (check-source-type! c entry src)
  (define target-sym (target-type-of c entry))
  (define root (make-root-slot (hash 'resourceType (symbol->string target-sym)) target-sym))
  (define sc (scope (for/fold ([vars (ctx-constants c)]) ([(k v) (in-hash (bind-params entry src root))]) (hash-set vars k v))
                    (make-hash)))
  (run-group c entry sc)
  (finalize ((tslot-get root))))

;; constant-values : (listof fml-map) -> (hash name -> value)   each constant's value: its FHIRPath
;; expression evaluated with the other constants it names in scope (they "can refer to other
;; constants"), each once; a circular definition is an error. They are bound first in every group's
;; scope, so a parameter or a rule's variable of the same name hides one (the specification). A
;; constant whose expression gives nothing has no value: naming it is an unbound variable.
(define (constant-values maps)
  (define expressions (for*/hash ([m (in-list (reverse maps))] [c (in-list (fml-map-constants m))]) (values (car c) (cdr c))))
  (define values-so-far (make-hash))
  (define (value-of name pending)
    (when (member name pending)
      (run-error (car maps) "the constant ~a is defined in terms of itself (~a)" name (string-join (reverse (cons name pending)) " -> ")))
    (hash-ref! values-so-far name
               (lambda ()
                 (define ast (fml-fhirpath-ast (hash-ref expressions name)))
                 (define others (for*/hash ([n (in-list (remove-duplicates (free-names ast)))] #:when (hash-has-key? expressions n)
                                            [v (in-value (value-of n (cons name pending)))] #:when v)
                                  (values n v)))
                 (match (fp-items ast #f (scope others (make-hash)))
                   ['() #f]
                   [(list one) one]
                   [several several]))))
  (for*/hash ([name (in-hash-keys expressions)] [v (in-value (value-of name '()))] #:when v) (values name v)))

;; free-names : fhirpath-ast -> (listof string)   the names an expression starts its paths with
(define (free-names ast)
  (match ast
    [(fp-invoke #f name) (list (symbol->string name))]
    [_ (append-map free-names (fp-parts ast))]))

;; check-source-type! : ctx group source -> void   the source is of the entry group's source type
;; (or a type derived from it); an error positioned at the group when it is not. Nothing is checked
;; when the parameter declares no type the map knows, or the source is not a typed node.
(define (check-source-type! c g src)
  (define sparam (group-param g 'source))
  (define declared (and sparam (type-sym c (fml-param-type sparam))))
  (define actual (and (fhir? src) (fhir-type src)))
  (when (and declared actual (not (memq declared (fhir-ancestors actual))))
    (run-error g "the source is of type ~a, but group ~a takes ~a : ~a"
               actual (fml-group-name g) (fml-param-name sparam) declared)))

;; the entry group's target type: its target parameter's type, else the first `uses … as target`
(define (target-type-of c g)
  (define tparam (group-param g 'target))
  (define use (findf (lambda (u) (eq? (fml-use-mode u) 'target)) (fml-map-uses (ctx-map c))))
  (or (and tparam (type-sym c (fml-param-type tparam)))
      (and use (type-sym c (or (fml-use-alias use) (url-leaf (fml-use-url use)))))
      (and tparam (fml-param-type tparam) (string->symbol (fml-param-type tparam)))
      (run-error g "cannot determine the target type: group ~a's target parameter has no type and the map has no `uses … as target`"
                 (fml-group-name g))))

(define (bind-params g src root)
  (for/fold ([h (hash)]) ([p (in-list (fml-group-params g))])
    (hash-set h (fml-param-name p) (if (eq? (fml-param-mode p) 'source) src root))))

;; run-group : execute a group's rules in scope, its `extends` parent first
(define (run-group c g sc)
  (define parent (and (fml-group-extends g) (find-group c (fml-group-extends g))))
  (when parent (run-group c parent sc))         ; a parent outside the map (an import) is not run
  (for ([r (in-list (fml-group-rules g))]) (run-rule c r sc)))

;; a group by name: this map's, then the imported maps'
(define (find-group c name)
  (for/or ([m (in-list (ctx-maps c))])
    (findf (lambda (g) (equal? (fml-group-name g) name)) (fml-map-groups m))))
(define (all-groups c) (append-map fml-map-groups (ctx-maps c)))

;; ---- rules ------------------------------------------------------------------
;; every combination of the sources' matches, then the targets, dependents and nested rules
(define (run-rule c r sc)
  (let loop ([ss (fml-rule-sources r)] [sc sc])
    (cond
      [(null? ss) (run-rule-body c r sc)]
      [else
       (define s (car ss))
       (for ([elem (in-list (source-matches c r s sc))])
         (define sc1 (bind (if (fml-source-variable s) (bind sc (fml-source-variable s) elem) sc) source-key elem))
         (when (fml-source-check s)
           (unless (fp-true? (fml-source-check s) elem sc1)
             (rule-error r "check condition failed: ~a" (fhirpath->string (fml-fhirpath-ast (fml-source-check s))))))
         (when (fml-source-log-expr s)
           ((current-fml-log) (string-join (for/list ([i (in-list (fp-items (fml-fhirpath-ast (fml-source-log-expr s)) elem sc1))])
                                             (format "~a" (item->jsexpr i)))
                                           " ")))
         (loop (cdr ss) sc1))])))

;; rule-label : rule -> string   how a message names the rule: its name, else its first source
(define (rule-label r) (or (fml-rule-name r) (source-text (car (fml-rule-sources r)))))

;; run-error : node format args … -> raises exn:fail:diagnostic   a failure while the map runs is a
;; diagnostic positioned at the node it is about (a rule, a group; parse.rkt attached where each was
;; written — the map itself has no place), so it renders as a parse error does and $transform
;; returns it as an OperationOutcome with the line. Every failure of a run goes through here: a
;; plain `error` was a stack trace at the command line.
(define (run-error node fmt . args)
  (raise-diagnostic (diagnostic 'fml 'transform 'error (apply format fmt args)
                                (or (attached-loc node) (loc #f #f #f #f #f)))))
;; rule-error : rule format args … -> raises exn:fail:diagnostic   a failure of one rule, named
(define (rule-error r fmt . args)
  (run-error r "Rule ~s: ~a" (rule-label r) (apply format fmt args)))

(define (run-rule-body c r sc)
  ;; a lone untransformed target carries the source value over (the specification's default)
  (define implicit? (and (= 1 (length (fml-rule-sources r))) (= 1 (length (fml-rule-targets r)))
                         (null? (fml-rule-dependents r)) (null? (fml-rule-nested r))))
  (define sc2 (for/fold ([sc sc]) ([t (in-list (fml-rule-targets r))]) (run-target c r t sc implicit?)))
  (for ([call (in-list (fml-rule-dependents r))]) (run-call c call sc2 r))
  (for ([nr (in-list (fml-rule-nested r))]) (run-rule c nr sc2)))

;; rule-targets* : rule -> (listof target)   the rule's targets and its nested rules', all the way down
(define (rule-targets* r)
  (append (fml-rule-targets r) (append-map rule-targets* (fml-rule-nested r))))

;; the elements a source clause matches: the context itself when no element; else each child,
;; where-filtered, in the clause's list mode
(define (source-matches c r s sc)
  (define context (lookup sc (fml-source-context s)))
  (unless context (rule-error r "unbound source variable ~a" (fml-source-context s)))
  (define present
    (cond [(not (fml-source-element s)) (list context)]
          [(fhir? context) (let ([v (fhir-ref context (choice-field context (fml-source-element s)) '())])
                             (cond [(null? v) '()] [(list? v) v] [else (list v)]))]
          [else '()]))
  ;; `min..max`: the element's cardinality in the source, or the transformation stops
  (when (or (fml-source-min s) (fml-source-max s))
    (define n (length present))
    (define max (let ([m (fml-source-max s)]) (and m (not (equal? m "*")) (string->number m))))
    (unless (and (>= n (or (fml-source-min s) 0)) (or (not max) (<= n max)))
      (rule-error r "~a has ~a value~a, and the rule takes ~a..~a"
                  (source-text s) n (if (= n 1) "" "s") (or (fml-source-min s) 0) (or (fml-source-max s) "*"))))
  ;; `: type`: only the elements of that type
  (define typed (if (fml-source-type s) (filter (lambda (e) (of-type? c e (fml-source-type s))) present) present))
  ;; `default (value)`: the value when the source has none, once
  (define base
    (if (and (null? typed) (fml-source-default s))
        (let ([items (fp-items (fml-fhirpath-ast (fml-source-default s)) #f sc)]) (if (pair? items) (list (car items)) '()))
        typed))
  ;; `where` sees the rule's alias bound to the candidate (`src.a as a where (a.length() <= 20)`)
  (define (with-candidate e) (if (fml-source-variable s) (bind sc (fml-source-variable s) e) sc))
  (define filtered
    (if (fml-source-where s)
        (filter (lambda (e) (fp-true? (fml-source-where s) e (with-candidate e))) base)
        base))
  ;; the source matched, and its `where` held for none: said at the targets this rule (and its nested
  ;; rules) would have written (the message blamed the input: "its source matched nothing")
  (when (and (fml-source-where s) (pair? base) (null? filtered) (current-empty-targets))
    ;; every item the source matched, so the message can say what the tested field held
    (define reason (empty-where (fml-source-where s) (length base)
                                (for/list ([b (in-list base)] #:when (fhir? b)) (value->jsexpr b))
                                r))
    (for ([t (in-list (rule-targets* r))]) (note-empty! t reason)))
  (case (fml-source-list-mode s)
    [(first) (if (pair? filtered) (list (car filtered)) '())]
    [(last) (if (pair? filtered) (list (last filtered)) '())]
    [(not_first) (if (pair? filtered) (cdr filtered) '())]
    [(not_last) (if (pair? filtered) (drop-right filtered 1) '())]
    [(only_one) (if (> (length filtered) 1)
                    (rule-error r "~a.~a matches ~a elements, only_one allows one"
                           (fml-source-context s) (fml-source-element s) (length filtered))
                    filtered)]
    [else filtered]))

;; source-text : source -> string   `src.value`, as a message names it
(define (source-text s)
  (string-append (fml-source-context s) (if (fml-source-element s) (string-append "." (fml-source-element s)) "")))

;; of-type? : ctx value type-name -> boolean   the value is of the type named: a type the map
;; `uses` (by its alias or name) or a FHIR type, itself or one derived from it; a primitive by its
;; code. A value whose type cannot be told is kept (nothing is decided about it).
(define (of-type? c v name)
  (define declared (or (type-sym c name) (string->symbol name)))
  (cond [(fhir? v) (and (memq declared (fhir-ancestors (fhir-type v))) #t)]
        [(type-name-of v) => (lambda (actual) (equal? actual (symbol->string declared)))]
        [else #t]))

;; ---- targets ----------------------------------------------------------------
(define (run-target c r t sc implicit?)
  (cond
    [(fml-target-context t) (run-element-target c r t sc implicit?)]
    [else
     ;; a bare invocation target (`evaluate(result, resolve()) as observation`): its value, a source
     ;; value when the expression yields a node, is bound to the alias
     (define tr (fml-target-transform t))
     (define v
       (match tr
         [(fml-invoke "evaluate" (list var (fml-fhirpath ast)))
          (let ([items (fp-items ast (lookup sc var) sc)]) (if (pair? items) (car items) 'null))]
         [_ (transform-value c r tr sc #f)]))
     (if (fml-target-variable t) (bind sc (fml-target-variable t) v) sc)]))

;; share-key : target -> key   what the rules that share one target element agree on: the rule id
;; `share <id>` names, else (the list mode `share` alone) the element itself
(define (share-key t)
  (match (fml-target-share t)
    [(? string? id) id]
    [_ (cons (fml-target-context t) (fml-target-element t))]))

(define (run-element-target c r t sc implicit?)
  (define context (lookup sc (fml-target-context t)))
  (unless (tslot? context) (rule-error r "target context ~a is not a target" (fml-target-context t)))
  (define tr (fml-target-transform t))
  (define src-elem (lookup sc source-key))
  ;; an expression's value, before its element is made (it names no target): what it is decides a
  ;; choice element's key as a variable's value does — `tgt.deceased = (d - 5 days)` is deceasedDateTime
  (define evaluated (match tr [(fml-fhirpath ast) (fp-items ast #f sc)] [_ #f]))
  ;; the value's type decides a choice element's key (value + T)
  (define forced-type (cond [(pair? evaluated) (type-name-of (car evaluated))]
                            [tr (value-type tr sc)]
                            [(and implicit? src-elem) (type-name-of src-elem)]
                            [else #f]))
  (define slot
    (cond
      [(not (fml-target-element t)) context]
      [(and (fml-target-share t) (hash-ref (scope-shares sc) (share-key t) #f))]
      [else
       (define elems (string-split (fml-target-element t) "."))
       (define s (for/fold ([s context]) ([e (in-list elems)] [i (in-naturals)])
                   (child-slot! s e (and (= i (sub1 (length elems))) forced-type))))
       (when (fml-target-share t) (hash-set! (scope-shares sc) (share-key t) s))
       s]))
  ;; a complex value with a `types` group for (its type, the element's type) runs that group
  (define typed-source (match tr [(fml-var name) (lookup sc name)] [#f (and implicit? (fml-target-element t) src-elem)] [_ #f]))
  (define group (and (fhir? typed-source) (tslot-type slot) (types-group c (fhir-type typed-source) (tslot-type slot))))
  (cond
    [group (call-group c group (list typed-source slot) sc)]
    [tr (define v (match evaluated
                    [#f (transform-value c r tr sc slot)]
                    ['() 'null]
                    [(cons first _) (item->jsexpr first)]))
        (check-primitive-target! r t slot v tr)
        (if (eq? v 'null)                               ; the rule matched; its value gave nothing
            (note-empty! t (empty-value (and (fhir? src-elem) (value->jsexpr src-elem))))
            ((tslot-set! slot) (coerce v (tslot-prim slot))))]
    [(and implicit? (fml-target-element t) src-elem)
     ((tslot-set! slot) (coerce (value->jsexpr src-elem) (tslot-prim slot)))]
    [else (void)])
  (if (fml-target-variable t) (bind sc (fml-target-variable t) slot) sc))

;; check-primitive-target! : rule target slot value -> void
;; A primitive element takes a primitive: a Coding assigned to `gender` (a code) was written as the
;; object `{"code": …, "system": …}`, and validation then blamed `code.code` with no line. The rule
;; is told, with the element of the object that would fit (`a.value.code`).
(define (check-primitive-target! r t slot v tr)
  (define prim (tslot-prim slot))
  (when (and prim (hash? v) (positive? (hash-count v)))     ; {} is a value the read dropped: said elsewhere
    (define keys (sort (map symbol->string (hash-keys v)) string<?))
    (define fits (for/first ([k (in-list '("code" "value" "reference" "display" "text"))] #:when (member k keys)) k))
    (rule-error r "`~a.~a` is a ~a, but the rule assigns an object {~a}~a"
                (fml-target-context t) (fml-target-element t) prim (string-join keys ", ")
                (cond [(not fits) ""]
                      [(fml-fhirpath? tr) (format ": assign its `~a`, `(~a.~a)`" fits (fhirpath->string (fml-fhirpath-ast tr)) fits)]
                      [(fml-var? tr) (format ": assign its `~a`, `(~a.~a)`" fits (fml-var-name tr) fits)]
                      [else (format ": assign its `~a`" fits)]))))

;; types-group : ctx source-type target-type -> the `types`/`type+` group whose parameters match
(define (types-group c st tt)
  (findf (lambda (g)
           (and (pair? (fml-group-flags g))
                (= 2 (length (fml-group-params g)))
                (let ([ps (fml-group-params g)])
                  (and (eq? (type-sym c (fml-param-type (car ps))) st)
                       (eq? (type-sym c (fml-param-type (cadr ps))) tt)))))
         (all-groups c)))

;; ---- transforms: the catalogue as data --------------------------------------------
;; A Transform is (transform run result-type):
;;   run         : ctx rule args scope slot -> jsexpr   ('null: nothing to assign)
;;   result-type : the FHIR type name of what it builds (what picks a choice target's key), or a
;;                 procedure of the args that says, or #f when it depends on the value
;; `transforms` maps the name written in a map (`create`, `qty`, …) to its Transform; an unknown
;; name is reported with the names there are.
(struct transform (run result-type))
(define transforms (make-hash))
(define (transform-names) (sort (hash-keys transforms) string<?))
(define-syntax-rule (define-transform name result-type (c r args sc slot) body ...)
  (hash-set! transforms name (transform (lambda (c r args sc slot) body ...) result-type)))
(define (transform-ref r name)
  (or (hash-ref transforms name #f)
      (rule-error r "the transform ~a is not supported (supported: ~a)" name (string-join (transform-names) " "))))

;; transform-value : the jsexpr value a transform produces
(define (transform-value c r tr sc slot)
  (match tr
    [(fml-literal v) v]
    [(fml-fhirpath ast) (fp-scalar ast sc)]
    [(fml-var name) (value->jsexpr (lookup sc name))]
    [(fml-invoke name args) ((transform-run (transform-ref r name)) c r args sc slot)]
    [_ (rule-error r "unsupported transform ~a" tr)]))

;; value-type : a transform's result type name for a choice target ("DateTime", "Quantity") or #f
(define (value-type tr sc)
  (match tr
    [(fml-invoke name args)
     (define t (hash-ref transforms name #f))
     (define rt (and t (transform-result-type t)))
     (if (procedure? rt) (rt args) rt)]
    [(fml-var name) (type-name-of (lookup sc name))]
    [_ #f]))
(define (literal-arg args i)                       ; the i-th argument when it is a literal, else #f
  (and (> (length args) i) (fml-literal? (list-ref args i)) (fml-literal-value (list-ref args i))))

;; resource-type? : type symbol -> boolean   a resource (its instances carry `resourceType`)
(define (resource-type? ty)
  (define sch (type-schema-of ty))
  (and sch (resource-parent? (type-schema-parent sch)) #t))

(define-transform "create" (lambda (args) (literal-arg args 0)) (c r args sc slot)
  (match args
    [(list (fml-literal ty)) (if (resource-type? (string->symbol ty)) (hash 'resourceType ty) (hash))]
    ['() (hash)]
    [_ (rule-error r "create takes a type name")]))
(define-transform "copy" #f (c r args sc slot)
  (match args [(list v) (value->jsexpr (lookup sc v))] [_ (rule-error r "copy takes one variable")]))
;; resource-identity : rule transform-name variable-name scope -> (values type id)
;; the type and id of the resource a variable holds, for a reference to it. A target resource being
;; built that has no id yet is given a random UUID (as the reference implementation does: the
;; reference then points at the resource the map writes); a source resource must have one — a
;; reference without an id (`Patient/`) refers to nothing.
(define (resource-identity r transform v sc)
  (define x (lookup sc v))
  (cond
    [(and (fhir? x) (resource-type? (fhir-type x)))
     (define id (pv (fhir-ref x 'id #f)))
     (unless (string? id) (rule-error r "~a(~a): the ~a has no id to refer to" transform v (fhir-type x)))
     (values (symbol->string (fhir-type x)) id)]
    [(and (tslot? x) (hash? ((tslot-get x))) (hash-ref ((tslot-get x)) 'resourceType #f))
     => (lambda (type)
          (define js ((tslot-get x)))
          (define id (or (let ([id (hash-ref js 'id #f)]) (and (string? id) id))
                         (let ([id (uuid)]) ((tslot-set! x) (hash-set js 'id id)) id)))
          (values type id))]
    [else (rule-error r "~a() needs a resource, ~a is not one" transform v)]))

;; reference(resource): the relative reference to it, `Type/id`
(define-transform "reference" "string" (c r args sc slot)
  (match args
    [(list (? string? v)) (let-values ([(type id) (resource-identity r "reference" v sc)]) (format "~a/~a" type id))]
    [_ (rule-error r "reference takes one variable")]))
;; pointer(resource): "the appropriate string to put in a reference that refers to the resource" —
;; as the reference implementation writes it, the resource's id as a URN (`urn:uuid:<id>`: what a
;; Bundle entry's fullUrl is, for a resource whose id is a UUID)
(define-transform "pointer" "string" (c r args sc slot)
  (match args
    [(list (? string? v)) (let-values ([(type id) (resource-identity r "pointer" v sc)]) (string-append "urn:uuid:" id))]
    [_ (rule-error r "pointer takes one variable")]))
;; cast(source, type?): "Target type can be left as implicit if there is one and only one target
;; type known" — the target element's own primitive type
(define-transform "cast" (lambda (args) (literal-arg args 1)) (c r args sc slot)
  (define (convert v ty)
    (define raw (value->jsexpr (lookup sc v)))
    (define out (coerce raw ty))
    (when (and (string? out) (member ty '("integer" "integer64" "positiveInt" "unsignedInt" "decimal" "boolean")))
      (rule-error r "Failed to cast ~v to type ~s" raw ty))
    out)
  (match args
    [(list v (fml-literal ty)) (convert v ty)]
    [(list v)
     (convert v (or (and slot (tslot-prim slot))
                    (rule-error r "cast(~a): the target's type is not known here — name it, cast(~a, 'integer')" v v)))]
    [_ (rule-error r "cast takes a variable and optionally a type name")]))
(define-transform "evaluate" #f (c r args sc slot)
  (match args
    [(list v (fml-fhirpath ast)) (fp-scalar-over ast (lookup sc v) sc)]
    [_ (rule-error r "evaluate takes a variable and an expression")]))
(define-transform "truncate" "string" (c r args sc slot)
  (define-values (s n)
    (match args
      [(list v (fml-literal n)) (values (value->jsexpr (lookup sc v)) (string->number n))]
      [(list v (fml-fhirpath ast)) (values (value->jsexpr (lookup sc v)) (fp-scalar ast sc))]
      [_ (rule-error r "truncate takes a variable and a length")]))
  (if (and (string? s) (exact-integer? n)) (substring s 0 (min (string-length s) n)) s))
(define-transform "translate" #f (c r args sc slot)
  (match args
    [(list v (fml-literal map-url)) (translate c r (value->jsexpr (lookup sc v)) map-url "code")]
    [(list v (fml-literal map-url) (fml-literal mode)) (translate c r (value->jsexpr (lookup sc v)) map-url mode)]
    [_ (rule-error r "translate takes a variable, a concept map and a mode")]))
(define-transform "uuid" "string" (c r args sc slot) (uuid))

;; ---- escape: one kind of escaping to another ----------------------------------------
;; An Escaping is (escaping read write): string -> string each way — `read` the text an escaped
;; string stands for, `write` a text escaped. The specification's four kinds, by name:
(struct escaping (read write))
;; code->string : integer string -> string   the character with that code; `otherwise` when no
;; character has it (a lone surrogate, a number beyond Unicode)
(define (code->string n otherwise)
  (if (or (<= 0 n #xD7FF) (<= #xE000 n #x10FFFF)) (string (integer->char n)) otherwise))
;; unescape-backslashes : string -> string   \n \t \r \b \f, \uXXXX (a surrogate pair is one
;; character) and \<any other> as itself (Java, JSON)
(define (unescape-backslashes s)
  (define (hex g from) (string->number (substring g from (+ from 4)) 16))
  (regexp-replace* #px"\\\\(u[dD][89abAB][0-9a-fA-F]{2}\\\\u[dD][c-fC-F][0-9a-fA-F]{2}|u[0-9a-fA-F]{4}|.)" s
                   (lambda (all g)
                     (cond [(= (string-length g) 11)       ; uD83D\uDE00
                            (code->string (+ #x10000 (* #x400 (- (hex g 1) #xD800)) (- (hex g 7) #xDC00)) all)]
                           [(> (string-length g) 1) (code->string (hex g 1) all)]
                           [else (case g [("n") "\n"] [("t") "\t"] [("r") "\r"] [("b") "\b"] [("f") "\f"] [else g])]))))
;; escape-backslashes : string boolean -> string   quotes, backslashes and control characters escaped;
;; ascii-only? (Java) writes every character beyond ASCII as \uXXXX, in UTF-16 units
(define (escape-backslashes s ascii-only?)
  (define (unit n) (string-append "\\u" (string-upcase (let ([h (number->string n 16)]) (string-append (make-string (- 4 (string-length h)) #\0) h)))))
  (apply string-append
         (for/list ([ch (in-string s)])
           (define n (char->integer ch))
           (cond [(char=? ch #\") "\\\""] [(char=? ch #\\) "\\\\"]
                 [(char=? ch #\newline) "\\n"] [(char=? ch #\tab) "\\t"] [(char=? ch #\return) "\\r"]
                 [(char=? ch #\backspace) "\\b"] [(char=? ch #\page) "\\f"]
                 [(< n 32) (unit n)]
                 [(and ascii-only? (> n #xFFFF))
                  (let ([m (- n #x10000)]) (string-append (unit (+ #xD800 (quotient m #x400))) (unit (+ #xDC00 (remainder m #x400)))))]
                 [(and ascii-only? (> n 126)) (unit n)]
                 [else (string ch)]))))
(define xml-entities '(("&amp;" . "&") ("&lt;" . "<") ("&gt;" . ">") ("&quot;" . "\"") ("&apos;" . "'")))
(define (unescape-xml s)
  (regexp-replace* #px"&(?:#([0-9]+)|#x([0-9a-fA-F]+)|[a-z]+);" s
                   (lambda (all dec hex)
                     (cond [dec (code->string (string->number dec) all)]
                           [hex (code->string (string->number hex 16) all)]
                           [(assoc all xml-entities) => cdr]
                           [else all]))))
(define (escape-xml s)
  (regexp-replace* #rx"[&<>\"']" s (lambda (ch) (car (findf (lambda (e) (equal? (cdr e) ch)) xml-entities)))))
(define escapings
  (hash "plain" (escaping values values)
        "java"  (escaping unescape-backslashes (lambda (s) (escape-backslashes s #t)))
        "json"  (escaping unescape-backslashes (lambda (s) (escape-backslashes s #f)))
        "xml"   (escaping unescape-xml escape-xml)))

;; arg-value : an Arg (parser/ast) scope -> its jsexpr value: a literal, a variable's, an expression's
(define (arg-value a sc)
  (match a
    [(fml-literal v) v]
    [(? string? name) (value->jsexpr (lookup sc name))]          ; a variable, by name
    [(fml-fhirpath ast) (fp-scalar ast sc)]))

;; translate : the value through an embedded conceptmap ('#name') or a ConceptMap the terminology
;; service resolves; the mode picks what comes back: code, system, display, Coding, CodeableConcept
(define (translate c r v map-url mode)
  (define-values (code system)
    (cond [(hash? v) (values (hash-ref v 'code #f) (hash-ref v 'system #f))]
          [else (values (if (string? v) v (format "~a" v)) #f)]))
  (define-values (t-system t-code t-display)
    (cond
      [(regexp-match? #rx"^#" map-url)
       (define cm (findf (lambda (cm) (equal? (fml-conceptmap-name cm) (substring map-url 1)))
                         (fml-map-conceptmaps (ctx-map c))))
       (unless cm (rule-error r "no conceptmap ~s in this map" map-url))
       (define m (findf (lambda (m) (and (equal? (cm-mapping-code m) code)
                                         (or (not system) (equal? (cm-mapping-source m) system))))
                        (fml-conceptmap-mappings cm)))
       (if m (values (cm-mapping-target m) (cm-mapping-target-code m) #f) (values #f #f #f))]
      [else
       (define ps (translate/json (current-terminology-resolver) map-url code system))
       (define match-part
         (for/first ([p (in-list (hash-ref ps 'parameter '()))] #:when (equal? (hash-ref p 'name #f) "match")) p))
       (define coding
         (and match-part
              (for/first ([q (in-list (hash-ref match-part 'part '()))] #:when (equal? (hash-ref q 'name #f) "concept"))
                (hash-ref q 'valueCoding #f))))
       (if coding (values (hash-ref coding 'system #f) (hash-ref coding 'code #f) (hash-ref coding 'display #f))
           (values #f #f #f))]))
  (cond
    [(not t-code) 'null]
    [else
     (case mode
       [("code") t-code]
       [("system") (or t-system 'null)]
       [("display") (or t-display 'null)]
       [("Coding") (drop-nulls (hasheq 'system t-system 'code t-code 'display t-display))]
       [("CodeableConcept") (hasheq 'coding (list (drop-nulls (hasheq 'system t-system 'code t-code 'display t-display))))]
       [else t-code])]))

(define (uuid)
  (define bs (crypto-random-bytes 16))
  (define hex (apply string-append (for/list ([b (in-bytes bs)]) (string-append (if (< b 16) "0" "") (number->string b 16)))))
  (string-append (substring hex 0 8) "-" (substring hex 8 12) "-4" (substring hex 13 16) "-"
                 (string (string-ref "89ab" (modulo (bytes-ref bs 8) 4))) (substring hex 17 20) "-" (substring hex 20 32)))

;; the spec's datatype constructors (FHIR Mapping Language, "Transforms"), their arguments evaluated:
;;   c(system, code [, display])            -> Coding
;;   cc(text) | cc(system, code [, display]) -> CodeableConcept
;;   qty(text) | qty(value, unit [, system, code]) -> Quantity  (text: "[comparator]value unit")
;;   id(system, value [, type])             -> Identifier
;;   cp([system,] value)                    -> ContactPoint
;;   append(v …)                            -> the strings joined
(define (drop-nulls h) (for/hasheq ([(k v) (in-hash h)] #:unless (or (not v) (eq? v 'null))) (values k v)))
(define (coding system code [display #f]) (drop-nulls (hasheq 'system system 'code code 'display display)))
(define (number-of v) (if (string? v) (or (string->number v) v) v))
(define-syntax-rule (define-constructor name result-type (r vals) body ...)
  (define-transform name result-type (c r args sc slot)
    (let ([vals (for/list ([a (in-list args)]) (arg-value a sc))]) body ...)))
(define-constructor "c" "Coding" (r vals)
  (match vals [(list system code) (coding system code)] [(list system code display) (coding system code display)]
         [_ (rule-error r "c takes a system, a code and optionally a display")]))
(define-constructor "cc" "CodeableConcept" (r vals)
  (match vals
    [(list text) (hasheq 'text text)]
    [(list system code more ...) (hasheq 'coding (list (apply coding system code more)))]
    [_ (rule-error r "cc takes a text, or a system, a code and optionally a display")]))
(define-constructor "qty" "Quantity" (r vals)
  (match vals
    [(list text)
     (match (regexp-match #px"^\\s*(<=|>=|<|>)?\\s*([-+]?[0-9.]+)\\s*(.*)$" (format "~a" text))
       [(list _ cmp v unit)
        (drop-nulls (hasheq 'comparator cmp 'value (string->number v) 'unit (and (positive? (string-length unit)) unit)))]
       [_ (rule-error r "qty: cannot read ~s" text)])]
    [(list v unit) (hasheq 'value (number-of v) 'unit unit)]
    [(list v unit system) (hasheq 'value (number-of v) 'unit unit 'system system)]
    [(list v unit system code) (hasheq 'value (number-of v) 'unit unit 'system system 'code code)]
    [_ (rule-error r "qty takes a text, or a value, a unit and optionally a system and a code")]))
;; "type is a code from the identifier type value set": a coding in that value set's code system
(define identifier-type-system "http://terminology.hl7.org/CodeSystem/v2-0203")
(define-constructor "id" "Identifier" (r vals)
  (match vals
    [(list system value) (hasheq 'system system 'value value)]
    [(list system value type)
     (hasheq 'system system 'value value 'type (hasheq 'coding (list (coding identifier-type-system type))))]
    [_ (rule-error r "id takes a system, a value and optionally a type")]))
;; contact-system : value -> "email" | "url" | "phone" | "fax" | "sms" | #f   "If no system is
;; provided, the system should be inferred from the content of the value": its scheme, an address
;; with an @, a web address, a number as a phone writes it; nothing when the content does not say
(define (contact-system v)
  (and (string? v)
       (cond [(regexp-match? #px"^(?i:mailto:)|^[^@\\s]+@[^@\\s]+$" v) "email"]
             [(regexp-match? #px"^(?i:https?://|ftp://|www[.])" v) "url"]
             [(regexp-match? #px"^(?i:fax:)" v) "fax"]
             [(regexp-match? #px"^(?i:sms:)" v) "sms"]
             [(regexp-match? #px"^(?i:tel:)|^[+(]?[0-9][-0-9 ()./]{4,}$" v) "phone"]
             [else #f])))
(define-constructor "cp" "ContactPoint" (r vals)
  (match vals
    [(list value) (drop-nulls (hasheq 'system (contact-system value) 'value value))]
    [(list system value) (hasheq 'system system 'value value)]
    [_ (rule-error r "cp takes a value, optionally after a system")]))
;; escape(source, fmt1, fmt2): the source, read as escaped in fmt1, written escaped in fmt2
(define-constructor "escape" "string" (r vals)
  (define (kind name)
    (or (and (string? name) (hash-ref escapings name #f))
        (rule-error r "escape: ~s is not a kind of escaping (~a)" name (string-join (sort (hash-keys escapings) string<?) ", "))))
  (match vals
    [(list s from to) (if (string? s) ((escaping-write (kind to)) ((escaping-read (kind from)) s)) s)]
    [_ (rule-error r "escape takes a variable and two kinds of escaping: escape(v, 'plain', 'json')")]))
(define-constructor "append" "string" (r vals)
  (apply string-append (for/list ([a (in-list vals)]) (if (string? a) a (format "~a" a)))))

;; ---- FHIRPath bridging -------------------------------------------------------
;; evaluate an FML FHIRPath AST (parsed once, at parse time) over a focus element, with the scope's
;; SOURCE vars as %externals. Two FML-dialect bridges, both pure AST rewrites:
;;   - a rule variable is referenced by BARE NAME (`item.answer…`) -> rewritten to an %external;
;;   - element-model `.value` steps read a primitive's value -> retried stripped when empty
;;     (the engine unwraps primitives implicitly, so `.value` is a no-op step there).
(define (fp-items ast focus sc)
  (define vars (for/hash ([(k v) (in-hash (scope-vars sc))] #:unless (or (tslot? v) (equal? k source-key)))
                 (values k (if (list? v) v (list v)))))        ; (a constant may hold several values)
  (define (try a)
    (with-handlers ([exn:fail? (lambda (_) '())])
      (eval-fhirpath (rewrite-vars a vars) (if focus (list focus) '()) #:externals vars #:strict? #f)))
  (define r (try ast))
  (if (null? r)
      (let ([stripped (strip-value-steps ast)])
        (if (equal? stripped ast) r (try stripped)))
      r))

;; A FHIRPath AST (fhirpath/parser/ast) is, for the three walks here, one of:
;;   (fp-invoke target|#f name) | (fp-call target|#f name args) | (fp-binop op left right)
;;   | (fp-unop op operand) | (fp-index target index) | (fp-typeop op operand type) | a leaf
;; fp-parts : ast -> (listof ast)   its sub-expressions
(define (fp-parts ast)
  (match ast
    [(fp-invoke t _) (if t (list t) '())]
    [(fp-call t _ args) (if t (cons t args) args)]
    [(fp-binop _ l r) (list l r)]
    [(fp-unop _ o) (list o)]
    [(fp-index t i) (list t i)]
    [(fp-typeop _ o _) (list o)]
    [_ '()]))
;; fp-map : (ast -> ast) ast -> ast   the node with f applied to each sub-expression
(define (fp-map f ast)
  (match ast
    [(fp-invoke t name) (fp-invoke (and t (f t)) name)]
    [(fp-call t name args) (fp-call (and t (f t)) name (map f args))]
    [(fp-binop op l r) (fp-binop op (f l) (f r))]
    [(fp-unop op o) (fp-unop op (f o))]
    [(fp-index t i) (fp-index (f t) (f i))]
    [(fp-typeop op o ty) (fp-typeop op (f o) ty)]
    [_ ast]))

;; rewrite-vars : chain-head (fp-invoke #f name) with name bound in scope -> (fp-extern name)
(define (rewrite-vars ast vars)
  (match ast
    [(fp-invoke #f name)
     (if (hash-ref vars (symbol->string name) #f) (fp-extern (symbol->string name)) ast)]
    [_ (fp-map (lambda (a) (rewrite-vars a vars)) ast)]))

;; strip-value-steps : drop `.value` navigation steps ((fp-invoke t 'value) -> t)
(define (strip-value-steps ast)
  (match ast
    [(fp-invoke t 'value) #:when t (strip-value-steps t)]
    [_ (fp-map strip-value-steps ast)]))

;; fp-true? : fml-fhirpath focus scope -> boolean   a where/check condition over the focus, with the
;; rule's variables in scope, read as FHIRPath reads a collection where a Boolean is expected
;; (singleton evaluation): a single Boolean is its value; a single item of another kind is true
;; (`where ($this.resource.ofType(Patient))` holds for the entry whose resource is a Patient);
;; nothing, or several items, is not true
(define (fp-true? fpath focus sc)
  (match (map item->system (fp-items (fml-fhirpath-ast fpath) focus sc))
    [(list (? boolean? b)) b]
    [(list _) #t]
    [_ #f]))

(define (fp-scalar ast sc)
  (define items (fp-items ast #f sc))
  (if (null? items) 'null (item->jsexpr (car items))))
(define (fp-scalar-over ast focus sc)
  (define items (fp-items ast focus sc))
  (if (null? items) 'null (item->jsexpr (car items))))

(define (pv x) (if (primitive-type? x) (primitive-type-value x) x))
(define (value->jsexpr v)
  (cond [(tslot? v) ((tslot-get v))]
        [(fhir? v) (node->jsexpr v)]
        [else (item->jsexpr v)]))
(define (item->jsexpr item)
  (define v (if (primitive-type? item) (primitive->jsexpr item) (item->system item)))
  (cond [(eq? v 'null) 'null]
        [(boolean? v) v]
        [(exact-integer? v) v]
        [(fp-long? v) (fp-long-v v)]
        [(fp-decimal? v) (string->number (decimal->string v))]
        [(string? v) v]
        [(moment? v) (regexp-replace #px"^@T?" (moment->string v) "")]
        [(fhir? v) (node->jsexpr v)]
        [(fhir? item) (node->jsexpr item)]
        [else (format "~a" v)]))

;; run-call : ctx call scope rule -> void
;; `then group(a, b)` — the callee's params bound to the argument values positionally. A group
;; nothing has, or an argument nothing binds, is the rule's error at its line (it was a plain
;; `error`: a stack trace at the command line, no line in an OperationOutcome)
(define (run-call c call sc r)
  (define g (or (find-group c (fml-call-name call))
                (rule-error r "unknown group ~a (the groups of this map~a: ~a)~a" (fml-call-name call)
                            (if (pair? (cdr (ctx-maps c))) " and its imports" "")
                            (string-join (map fml-group-name (all-groups c)) ", ")
                            (let ([missing (unresolved-imports (car (ctx-maps c)))])
                              (if (null? missing)
                                  ""
                                  (format "; not found: the imported ~a" (string-join missing ", ")))))))
  (unless (= (length (fml-call-args call)) (length (fml-group-params g)))
    (rule-error r "group ~a takes ~a parameters, called with ~a"
                (fml-group-name g) (length (fml-group-params g)) (length (fml-call-args call))))
  (define args (for/list ([a (in-list (fml-call-args call))])
                 (match a
                   [(? string? name) (or (lookup sc name) (rule-error r "group ~a: unbound argument ~a" (fml-call-name call) name))]
                   [(fml-literal v) v]
                   [(fml-fhirpath ast) (fp-scalar ast sc)])))
  (call-group c g args sc))

;; call-group : ctx group (listof value) scope -> void   the group's rules with its parameters bound
;; to the values, in order (as many as it has parameters), the constants under them
(define (call-group c g args sc)
  (define sc* (for/fold ([s (scope (ctx-constants c) (scope-shares sc))])
                        ([p (in-list (fml-group-params g))] [v (in-list args)])
                (bind s (fml-param-name p) v)))
  (run-group c g sc*))

(module+ test
  (require rackunit racket/file
           (only-in fhir/r5/schemas)
           (only-in "parser/parse.rkt" parse-fml)
           (only-in fhir/serialize/from-json from-json)
           (only-in fhir/result/result ok-value)
           (only-in fhir/data-source current-data-source resources->data-source)
           (only-in fhir/diagnostic exn:fail:diagnostic? exn:fail:diagnostic-diagnostic diagnostic-loc loc-line))
  (define qr (ok-value (from-json (hasheq 'resourceType "QuestionnaireResponse" 'id "12345" 'status "completed"
                                          'item (list (hasheq 'linkId "patient.sex"
                                                              'answer (list (hasheq 'valueString "female")))))
                                  #:lenient? #t)))
  (define m (ok-value (parse-fml #<<EOF
map "http://x" = "t"
uses "http://hl7.org/fhir/StructureDefinition/QuestionnaireResponse" alias QR as source
uses "http://hl7.org/fhir/StructureDefinition/Patient" alias Patient as target
group entry(source src : QR, target tgt : Patient) {
  src.item as item where (linkId = 'patient.sex') -> tgt.gender = (item.answer.valueString);
}
EOF
)))
  (check-equal? (run-map m qr) (hash 'resourceType "Patient" 'gender "female"))
  ;; logical models from StructureDefinitions in the data source: an implicit copy coerced to the
  ;; target's integer, a check, an embedded concept map, a types group
  (define (sd type . elems)
    (hasheq 'resourceType "StructureDefinition" 'url (string-append "http://x/" type) 'kind "logical" 'type type
            'differential (hasheq 'element (cons (hasheq 'path type 'min 1 'max "1") elems))))
  (define (el path code [max "1"]) (hasheq 'path path 'min 0 'max max 'type (list (hasheq 'code code))))
  (define sds (list (sd "TLeft" (el "TLeft.a" "string") (el "TLeft.d" "code") (el "TLeft.aa" "BackboneElement" "*") (el "TLeft.aa.ab" "string"))
                    (sd "TRight" (el "TRight.a" "integer") (el "TRight.d" "code") (el "TRight.aa" "BackboneElement" "*") (el "TRight.aa.ab" "string"))))
  (define m2 (ok-value (parse-fml #<<EOF
map "http://x/m2" = "m2"
conceptmap "cm" {
  prefix s = "http://s"
  prefix t = "http://t"
  s:vonhier == t:"nach-da"
}
uses "http://x/TLeft" alias TLeft as source
uses "http://x/TRight" alias TRight as target
group tutorial(source src : TLeft, target tgt : TRight) {
  src.a -> tgt.a;
  src.d as d check (d.length() < 20) -> tgt.d = translate(d, '#cm', 'code');
  src.aa as a -> tgt.aa = a;
}
group inner(source src : TLeftAa, target tgt : TRightAa) <<types>> {
  src.ab as b -> tgt.ab = append(b, '!');
}
EOF
)))
  (parameterize ([current-data-source (resources->data-source sds)])
    (uses-types m2)                                      ; TLeft/TRight exist before the source is read
    (define left (ok-value (from-json (hasheq 'resourceType "TLeft" 'a "12345" 'd "vonhier" 'aa (list (hasheq 'ab "x"))))))
    (check-equal? (run-map m2 left) (hash 'resourceType "TRight" 'a 12345 'd "nach-da" 'aa (list (hash 'ab "x!"))))
    ;; a check that fails is a diagnostic at the rule's line
    (define long (ok-value (from-json (hasheq 'resourceType "TLeft" 'd "this-code-is-far-too-long-for-the-check"))))
    (define e (with-handlers ([exn:fail:diagnostic? values]) (run-map m2 long)))
    (check-true (exn:fail:diagnostic? e))
    (check-equal? (loc-line (diagnostic-loc (exn:fail:diagnostic-diagnostic e))) 11)
    (check-regexp-match #rx"check condition failed" (exn-message e)))
  ;; a dependent group from an IMPORTED map: the import is a StructureMap resource in the data source
  (require (only-in "structuremap.rkt" fml->structuremap))
  (define lib (ok-value (parse-fml (string-append
    "map \"http://x/lib\" = \"lib\"\n"
    "group shout(source s, target t) { s.ab as b -> t.ab = append(b, '!!'); }\n"))))
  (define m3 (ok-value (parse-fml (string-append
    "map \"http://x/m3\" = \"m3\"\n"
    "imports \"http://x/lib\"\n"
    "uses \"http://x/TLeft\" alias TLeft as source\n"
    "uses \"http://x/TRight\" alias TRight as target\n"
    "group tutorial(source src : TLeft, target tgt : TRight) {\n"
    "  src.aa as a -> tgt.aa as t then shout(a, t);\n"
    "}\n"))))
  (parameterize ([current-data-source (resources->data-source (cons (fml->structuremap lib) sds))])
    (uses-types m3)
    (define left (ok-value (from-json (hasheq 'resourceType "TLeft" 'aa (list (hasheq 'ab "y"))))))
    (check-equal? (run-map m3 left) (hash 'resourceType "TRight" 'aa (list (hash 'ab "y!!"))))
    (check-equal? (fml-map-url (resolve-structure-map "http://x/lib|1.0")) "http://x/lib")))
