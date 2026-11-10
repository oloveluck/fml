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
         (only-in fhir/data-source current-data-source data-source-all)
         (only-in "structuremap.rkt" structuremap->fml)
         (only-in fhir/terminology/operations current-terminology-resolver)
         (only-in fhir/terminology/service translate/json)
         (only-in fhirpath/parser/ast fp-invoke fp-invoke? fp-call fp-binop fp-unop fp-index fp-typeop fp-extern)
         (only-in fhirpath/eval eval-fhirpath)
         (only-in fhirpath/print fhirpath->string)
         (only-in fhirpath/value item->system fp-long? fp-long-v)
         (only-in fhirpath/decimal fp-decimal? decimal->string)
         (only-in fhirpath/temporal-ops moment? moment->string)
         (only-in fhir/serialize/to-json node->jsexpr))

(provide (contract-out
          ;; run-map : parsed map × typed source resource -> the target resource jsexpr
          [run-map (-> fml-map? fhir? hash?)]
          ;; where a rule's `log(…)` goes: a procedure of the message (nowhere by default)
          [current-fml-log (parameter/c (-> string? any))]
          ;; when a mutable hasheq: each target whose value expression came out empty, with the
          ;; source value the rule read (validate-result says why an element was not written)
          [current-empty-targets (parameter/c (or/c #f (and/c hash? (not/c immutable?))))]
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
          [map-imports (-> fml-map? (listof fml-map?))]))

;; ---- maps by canonical ---------------------------------------------------------
(define registered (make-hash))
(define (register-structure-map! m)
  (when (string? (fml-map-url m)) (hash-set! registered (fml-map-url m) m)))
(define (registered-structure-maps) (sort (hash-keys registered) string<?))
(define converted (make-weak-hasheq))           ; a StructureMap jsexpr -> its AST, once
(define (resolve-structure-map url)
  (define key (car (regexp-split #rx"[|]" url)))
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

(define current-fml-log (make-parameter void))
(define current-empty-targets (make-parameter #f))

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
(define (type-name-of v)
  (cond [(primitive-type? v) (regexp-replace #rx"^struct:fhir-" (symbol->string (vector-ref (struct->vector v) 0)) "")]
        [(fhir? v) (symbol->string (fhir-type v))]
        [else #f]))
;; child-slot! : parent elem [forced-type] -> a slot for one occurrence of parent.elem
(define (child-slot! parent elem [forced-type #f])
  (define e (or (element-of (tslot-type parent) elem forced-type)
                (element (string->symbol elem) #f (type-name->ty forced-type))))   ; not in the schema: open
  (define key (element-key e))
  (define child-type (and (symbol? (element-ty e)) (element-ty e)))
  (define prim (and (prim? (element-ty e)) (prim-code (element-ty e))))
  (cond
    [(element-list? e)
     (define idx-box (box #f))
     (define h0 (as-hash ((tslot-get parent))))
     (define lst (as-list (hash-ref h0 key '())))
     (set-box! idx-box (length lst))
     ((tslot-set! parent) (hash-set h0 key (append lst (list (hash)))))
     (tslot
      (lambda () (list-ref (as-list (hash-ref (as-hash ((tslot-get parent))) key '())) (unbox idx-box)))
      (lambda (v)
        (define h (as-hash ((tslot-get parent))))
        ((tslot-set! parent) (hash-set h key (list-set (as-list (hash-ref h key '())) (unbox idx-box) v))))
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
;; A Ctx is (ctx map types maps): the map, its `uses` resolved to type symbols (alias and type name,
;; the imports' too), and the maps whose groups are callable (the map first, then its imports)
(struct ctx (map types maps))

(define (uses-types m)
  (for/fold ([h (hash)]) ([u (in-list (fml-map-uses m))])
    (define url (car (regexp-split #rx"[|]" (fml-use-url u))))
    (define leaf (last (string-split url "/")))
    (define sym
      (cond [(type-registered? (string->symbol leaf)) (string->symbol leaf)]
            [else (let ([sd (resolve-structure-definition/data url)])
                    (and sd (register-structure-definition! sd)))]))
    (define h1 (if sym (hash-set h leaf sym) h))
    (if (and sym (fml-use-alias u)) (hash-set h1 (fml-use-alias u) sym) h1)))

;; type-sym : ctx type-name|#f -> symbol|#f   a parameter's declared type
(define (type-sym c name)
  (and name (or (hash-ref (ctx-types c) name #f)
                (and (type-registered? (string->symbol name)) (string->symbol name)))))

;; ---- entry ------------------------------------------------------------------
(define (run-map m src)
  (define maps (map-imports m))
  (define c (ctx m (for/fold ([h (hash)]) ([im (in-list (reverse maps))]) ; the map's own names win
                     (for/fold ([h h]) ([(k v) (in-hash (uses-types im))]) (hash-set h k v)))
                 maps))
  (define entry (car (fml-map-groups m)))
  (check-source-type! c entry src)
  (define target-sym (target-type-of c entry))
  (define root (make-root-slot (hash 'resourceType (symbol->string target-sym)) target-sym))
  (define sc (scope (bind-params entry src root) (make-hash)))
  (run-group c entry sc)
  (finalize ((tslot-get root))))

;; check-source-type! : ctx group source -> void   the source is of the entry group's source type
;; (or a type derived from it); an error positioned at the group when it is not. Nothing is checked
;; when the parameter declares no type the map knows, or the source is not a typed node.
(define (check-source-type! c g src)
  (define sparam (findf (lambda (p) (eq? (fml-param-mode p) 'source)) (fml-group-params g)))
  (define declared (and sparam (type-sym c (fml-param-type sparam))))
  (define actual (and (fhir? src) (fhir-type src)))
  (when (and declared actual (not (memq declared (fhir-ancestors actual))))
    (raise-diagnostic
     (diagnostic 'fml 'transform 'error
                 (format "the source is of type ~a, but group ~a takes ~a : ~a"
                         actual (fml-group-name g) (fml-param-name sparam) declared)
                 (or (attached-loc g) (loc #f #f #f #f #f))))))

;; the entry group's target type: its target parameter's type, else the first `uses … as target`
(define (target-type-of c g)
  (define tparam (findf (lambda (p) (eq? (fml-param-mode p) 'target)) (fml-group-params g)))
  (define use (findf (lambda (u) (eq? (fml-use-mode u) 'target)) (fml-map-uses (ctx-map c))))
  (or (and tparam (type-sym c (fml-param-type tparam)))
      (and use (type-sym c (or (fml-use-alias use) (last (string-split (fml-use-url use) "/")))))
      (and tparam (fml-param-type tparam) (string->symbol (fml-param-type tparam)))
      (error 'fml "cannot determine the target type: no target parameter type and no `uses … as target`")))

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

(define (rule-label r)
  (or (fml-rule-name r)
      (let ([s (car (fml-rule-sources r))])
        (string-append (fml-source-context s) (if (fml-source-element s) (string-append "." (fml-source-element s)) "")))))

;; rule-error : rule format args … -> raises exn:fail:diagnostic   a failure while running a rule is
;; a diagnostic positioned at the rule (parse.rkt attached where it was written), so it renders as a
;; parse error does and $transform returns it as an OperationOutcome with the line
(define (rule-error r fmt . args)
  (raise-diagnostic (diagnostic 'fml 'transform 'error (format "Rule ~s: ~a" (rule-label r) (apply format fmt args))
                                (or (attached-loc r) (loc #f #f #f #f #f)))))

(define (run-rule-body c r sc)
  ;; a lone untransformed target carries the source value over (the specification's default)
  (define implicit? (and (= 1 (length (fml-rule-sources r))) (= 1 (length (fml-rule-targets r)))
                         (null? (fml-rule-dependents r)) (null? (fml-rule-nested r))))
  (define sc2 (for/fold ([sc sc]) ([t (in-list (fml-rule-targets r))]) (run-target c r t sc implicit?)))
  (for ([call (in-list (fml-rule-dependents r))]) (run-call c call sc2))
  (for ([nr (in-list (fml-rule-nested r))]) (run-rule c nr sc2)))

;; the elements a source clause matches: the context itself when no element; else each child,
;; where-filtered, in the clause's list mode
(define (source-matches c r s sc)
  (define context (lookup sc (fml-source-context s)))
  (unless context (rule-error r "unbound source variable ~a" (fml-source-context s)))
  (define base
    (cond [(not (fml-source-element s)) (list context)]
          [(fhir? context) (let ([v (fhir-ref context (choice-field context (fml-source-element s)) '())])
                             (cond [(null? v) '()] [(list? v) v] [else (list v)]))]
          [else '()]))
  ;; `where` sees the rule's alias bound to the candidate (`src.a as a where a.length() <= 20`)
  (define (with-candidate e) (if (fml-source-variable s) (bind sc (fml-source-variable s) e) sc))
  (define filtered
    (if (fml-source-where s)
        (filter (lambda (e) (fp-true? (fml-source-where s) e (with-candidate e))) base)
        base))
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

(define (run-element-target c r t sc implicit?)
  (define context (lookup sc (fml-target-context t)))
  (unless (tslot? context) (rule-error r "target context ~a is not a target" (fml-target-context t)))
  (define tr (fml-target-transform t))
  (define src-elem (lookup sc source-key))
  ;; the value's type decides a choice element's key (value + T)
  (define forced-type (cond [tr (value-type tr sc)]
                            [(and implicit? src-elem) (type-name-of src-elem)]
                            [else #f]))
  (define slot
    (cond
      [(not (fml-target-element t)) context]
      [(and (fml-target-share t) (hash-ref (scope-shares sc) (fml-target-share t) #f))]
      [else
       (define elems (string-split (fml-target-element t) "."))
       (define s (for/fold ([s context]) ([e (in-list elems)] [i (in-naturals)])
                   (child-slot! s e (and (= i (sub1 (length elems))) forced-type))))
       (when (fml-target-share t) (hash-set! (scope-shares sc) (fml-target-share t) s))
       s]))
  ;; a complex value with a `types` group for (its type, the element's type) runs that group
  (define typed-source (match tr [(fml-var name) (lookup sc name)] [#f (and implicit? (fml-target-element t) src-elem)] [_ #f]))
  (define group (and (fhir? typed-source) (tslot-type slot) (types-group c (fhir-type typed-source) (tslot-type slot))))
  (cond
    [group (call-group c group (list typed-source slot) sc)]
    [tr (define v (transform-value c r tr sc slot))
        (check-primitive-target! r t slot v tr)
        (if (eq? v 'null)
            (let ([h (current-empty-targets)])         ; the rule matched; its value gave nothing
              (when h (hash-set! h t (and (fhir? src-elem) (value->jsexpr src-elem)))))
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
(define-syntax-rule (define-transform name result-type (c r args sc slot) body ...)
  (hash-set! transforms name (transform (lambda (c r args sc slot) body ...) result-type)))
(define (transform-ref r name)
  (or (hash-ref transforms name #f)
      (rule-error r "the transform ~a is not supported (supported: ~a)" name (string-join (sort (hash-keys transforms) string<?) " "))))

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

(define-transform "create" (lambda (args) (literal-arg args 0)) (c r args sc slot)
  (match args
    [(list (fml-literal ty))
     (define sch (type-schema-of (string->symbol ty)))
     (if (and sch (resource-parent? (type-schema-parent sch))) (hash 'resourceType ty) (hash))]
    ['() (hash)]
    [_ (rule-error r "create takes a type name")]))
(define-transform "copy" #f (c r args sc slot)
  (match args [(list v) (value->jsexpr (lookup sc v))] [_ (rule-error r "copy takes one variable")]))
(define-transform "reference" "string" (c r args sc slot)
  (match args
    [(list v)
     (define src (lookup sc v))
     (cond [(fhir? src) (format "~a/~a" (fhir-type src) (pv (fhir-ref src 'id #f)))]
           [(tslot? src) (let ([js ((tslot-get src))]) (format "~a/~a" (hash-ref js 'resourceType "") (hash-ref js 'id "")))]
           [else (rule-error r "reference() needs a resource, ~a is not one" v)])]
    [_ (rule-error r "reference takes one variable")]))
(define-transform "cast" (lambda (args) (literal-arg args 1)) (c r args sc slot)
  (match args
    [(list v (fml-literal ty))
     (define raw (value->jsexpr (lookup sc v)))
     (define out (coerce raw ty))
     (when (and (string? out) (member ty '("integer" "integer64" "positiveInt" "unsignedInt" "decimal" "boolean")))
       (rule-error r "Failed to cast ~v to type ~s" raw ty))
     out]
    [_ (rule-error r "cast takes a variable and a type name")]))
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

;; arg-value : a transform argument -> its jsexpr value (literal, variable, or FHIRPath)
(define (arg-value a sc)
  (match a
    [(fml-literal v) v]
    [(? string? name) (value->jsexpr (lookup sc name))]          ; a variable, by name
    [(fml-fhirpath ast) (fp-scalar ast sc)]
    [_ (error 'fml "unsupported transform argument ~a" a)]))

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
(define-constructor "id" "Identifier" (r vals)
  (match vals
    [(list system value) (hasheq 'system system 'value value)]
    [(list system value type) (hasheq 'system system 'value value 'type (hasheq 'text type))]
    [_ (rule-error r "id takes a system, a value and optionally a type")]))
(define-constructor "cp" "ContactPoint" (r vals)
  (match vals
    [(list value) (hasheq 'value value)]
    [(list system value) (hasheq 'system system 'value value)]
    [_ (rule-error r "cp takes a value, optionally after a system")]))
(define-constructor "append" "string" (r vals)
  (apply string-append (for/list ([a (in-list vals)]) (if (string? a) a (format "~a" a)))))

;; ---- FHIRPath bridging -------------------------------------------------------
;; evaluate an FML FHIRPath AST (parsed once, at parse time) over a focus element, with the scope's
;; SOURCE vars as %externals. Two FML-dialect bridges, both pure AST rewrites:
;;   - a rule variable is referenced by BARE NAME (`item.answer…`) -> rewritten to an %external;
;;   - element-model `.value` steps read a primitive's value -> retried stripped when empty
;;     (the engine unwraps primitives implicitly, so `.value` is a no-op step there).
(define (fp-items ast focus sc)
  (define vars (for/hash ([(k v) (in-hash (scope-vars sc))] #:unless (or (tslot? v) (equal? k source-key))) (values k (list v))))
  (define (try a)
    (with-handlers ([exn:fail? (lambda (_) '())])
      (eval-fhirpath (rewrite-vars a vars) (if focus (list focus) '()) #:externals vars #:strict? #f)))
  (define r (try ast))
  (if (null? r)
      (let ([stripped (strip-value-steps ast)])
        (if (equal? stripped ast) r (try stripped)))
      r))

;; rewrite-vars : chain-head (fp-invoke #f name) with name bound in scope -> (fp-extern name)
(define (rewrite-vars ast vars)
  (match ast
    [(fp-invoke #f name)
     (if (hash-ref vars (symbol->string name) #f) (fp-extern (symbol->string name)) ast)]
    [(fp-invoke t name) (fp-invoke (rewrite-vars t vars) name)]
    [(fp-call t name args) (fp-call (and t (rewrite-vars t vars)) name (map (lambda (a) (rewrite-vars a vars)) args))]
    [(fp-binop op l r) (fp-binop op (rewrite-vars l vars) (rewrite-vars r vars))]
    [(fp-unop op o) (fp-unop op (rewrite-vars o vars))]
    [(fp-index t i) (fp-index (rewrite-vars t vars) (rewrite-vars i vars))]
    [(fp-typeop op o ty) (fp-typeop op (rewrite-vars o vars) ty)]
    [_ ast]))

;; strip-value-steps : drop `.value` navigation steps ((fp-invoke t 'value) -> t)
(define (strip-value-steps ast)
  (match ast
    [(fp-invoke t 'value) #:when t (strip-value-steps t)]
    [(fp-invoke t name) (fp-invoke (and t (strip-value-steps t)) name)]
    [(fp-call t name args) (fp-call (and t (strip-value-steps t)) name (map strip-value-steps args))]
    [(fp-binop op l r) (fp-binop op (strip-value-steps l) (strip-value-steps r))]
    [(fp-unop op o) (fp-unop op (strip-value-steps o))]
    [(fp-index t i) (fp-index (strip-value-steps t) (strip-value-steps i))]
    [(fp-typeop op o ty) (fp-typeop op (strip-value-steps o) ty)]
    [_ ast]))

;; a where/check condition over the focus, with the rule's variables in scope
(define (fp-true? fpath focus sc)
  (equal? (map item->system (fp-items (fml-fhirpath-ast fpath) focus sc)) '(#t)))

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

;; run-call : `then group(a, b)` — the callee's params bound to the argument values positionally
(define (run-call c call sc)
  (define g (or (find-group c (fml-call-name call))
                (error 'fml "unknown group ~a (the groups of this map~a: ~a)" (fml-call-name call)
                       (if (pair? (cdr (ctx-maps c))) " and its imports" "")
                       (string-join (map fml-group-name (all-groups c)) ", "))))
  (define args (for/list ([a (in-list (fml-call-args call))])
                 (cond [(string? a) (or (lookup sc a) (error 'fml "group ~a: unbound argument ~a" (fml-call-name call) a))]
                       [else (fp-scalar (fml-fhirpath-ast a) sc)])))
  (call-group c g args sc))

(define (call-group c g args sc)
  (unless (= (length args) (length (fml-group-params g)))
    (error 'fml "group ~a takes ~a parameters, called with ~a" (fml-group-name g) (length (fml-group-params g)) (length args)))
  (define sc* (for/fold ([s (scope (hash) (scope-shares sc))])
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
  src.item as item where linkId = 'patient.sex' -> tgt.gender = (item.answer.valueString);
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
