#lang racket/base

;;; transform.rkt — execute an FML map: source resource -> target resource (jsexpr).
;;;
;;; The engine walks the map's first group with (source-node, fresh-target). A Scope binds FML
;;; variables to VALUES: a source value is a typed node/primitive (FHIRPath-evaluable); a target
;;; value is a mutable TSLOT (a box holding the jsexpr under construction, so nested rules mutate
;;; their parent in place — FML targets are built incrementally).
;;;
;;; Rule execution (per the StructureMap spec, the subset the vendored suite exercises):
;;;   for each match of the rule's source (context var's element, filtered by `where` FHIRPath):
;;;     bind `as` var -> the element; execute each target op in order (targets may chain via the
;;;     vars bound by earlier ops); `share v` reuses ONE target element across rules that share v;
;;;     run `then` dependent group calls / nested rules in the extended scope.
;;;   target ops: `t.elem` (create/ensure), `= 'lit'`, `= (fhirpath)` (over the rule's variables),
;;;   `= var`, `= create('T')`, `= reference(v)`, `= cast(v,'T')`, `= copy(v)`, `= evaluate(v, fp)`.
;;;
;;; FML's FHIRPath dialect reads primitives via `.value` (element model); the repo's engine unwraps
;;; primitives implicitly, so a trailing `.value` step that fails/empties is retried stripped.

(require racket/contract racket/match racket/list racket/string
         "parser/ast.rkt"
         (only-in fhir/model/node fhir? fhir-ref fhir-type)
         (only-in fhir/model/base primitive-type? primitive-type-value)
         (only-in fhir/model/introspect full-field-specs)
         (only-in fhir/model/schema field-spec? fs-name fs-kind fs-ftype ftype-type? ftype-type-name)
         (only-in fhirpath/parser/ast fp-invoke fp-invoke? fp-call fp-binop fp-unop fp-index fp-typeop fp-extern)
         (only-in fhirpath/eval eval-fhirpath)
         (only-in fhirpath/value item->system fp-long? fp-long-v)
         (only-in fhirpath/decimal fp-decimal? decimal->string)
         (only-in fhirpath/temporal-ops moment? moment->string)
         (only-in fhir/serialize/to-json node->jsexpr))

(provide (contract-out
          ;; run-map : parsed map × typed source resource -> the target resource jsexpr
          [run-map (-> fml-map? fhir? hash?)]))

;; ---- target slots -----------------------------------------------------------
;; A TSLOT is (tslot get set! type) — get/set! close over the parent so mutation propagates up to
;; the root box; `type` is the element's TYPE SYMBOL, resolved from the runtime registry, which
;; drives the child shape: max>1 elements append into arrays, max-1 set scalars, and choice
;; (value[x]) elements pick their concrete key from the assigned value's declared type.
(struct tslot (get set! type))
(define (make-root-slot init type)
  (define b (box init))
  (tslot (lambda () (unbox b)) (lambda (v) (set-box! b v)) type))

;; the registry's field entry for `elem` on `type` -> (values key list? child-type) ; a choice base
;; (`value` on Extension) resolves via forced-type (from create('T')) to key value<T>
(define (field-shape type elem forced-type)
  (define specs (and type (with-handlers ([exn:fail? (lambda (_) #f)]) (full-field-specs type))))
  (define sym (string->symbol elem))
  (define plain (and specs (findf (lambda (e) (and (field-spec? e) (eq? (fs-name e) sym))) specs)))
  (cond
    [plain (values sym (eq? (fs-kind plain) 'list) (ftype->sym (fs-ftype plain)))]
    [(and specs forced-type
          (findf (lambda (e) (and (field-spec? e) (eq? (fs-name e) (choice-key sym forced-type)))) specs))
     => (lambda (m) (values (fs-name m) (eq? (fs-kind m) 'list) (string->symbol forced-type)))]
    [else (values sym #f (and forced-type (string->symbol forced-type)))]))  ; unknown type: scalar
(define (choice-key base ty) (string->symbol (string-append (symbol->string base) ty)))
(define (ftype->sym ft)
  (cond [(ftype-type? ft) (ftype-type-name ft)]
        [else #f]))                                ; primitives/resources carry no child schema

;; child-slot! : parent elem [forced-type] -> a slot for one occurrence of parent.elem
(define (child-slot! parent elem [forced-type #f])
  (define-values (key list? child-type) (field-shape (tslot-type parent) elem forced-type))
  (cond
    [list?
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
      child-type)]
    [else
     (tslot
      (lambda () (hash-ref (as-hash ((tslot-get parent))) key (hash)))
      (lambda (v) ((tslot-set! parent) (hash-set (as-hash ((tslot-get parent))) key v)))
      child-type)]))

(define (as-hash v) (if (hash? v) v (hash)))
(define (as-list v) (if (list? v) v (list v)))

;; finalize : jsexpr -> jsexpr with singleton-vs-array by FHIR conventions left as built; drop empty
;; hashes/lists so `tgt.name as name` with no assignments doesn't emit {}
(define (finalize v)
  (cond
    [(hash? v)
     (for/fold ([h (hash)]) ([(k x) (in-hash v)])
       (define fx (finalize x))
       (if (or (equal? fx (hash)) (equal? fx '())) h (hash-set h k fx)))]
    [(list? v) (filter (lambda (x) (not (or (equal? x (hash)) (equal? x '())))) (map finalize v))]
    [else v]))

;; ---- scope ------------------------------------------------------------------
;; vars : name(string) -> source value | TSLOT ; shares : share-name -> TSLOT (per run)
(struct scope (vars shares) #:transparent)
(define (bind sc name v) (scope (hash-set (scope-vars sc) name v) (scope-shares sc)))
(define (lookup sc name) (hash-ref (scope-vars sc) name #f))

;; ---- entry ------------------------------------------------------------------
(define (run-map m src)
  (define entry (car (fml-map-groups m)))
  (define target-type (target-type-of m entry))
  (define root (make-root-slot (hash 'resourceType target-type) (string->symbol target-type)))
  (define shares (make-hash))
  (define sc (scope (bind-params entry src root) shares))
  (run-group m entry sc)
  (finalize ((tslot-get root))))

;; the entry group's target parameter type, resolved through `uses … as target` aliases
(define (target-type-of m g)
  (define tparam (findf (lambda (p) (eq? (fml-param-mode p) 'target)) (fml-group-params g)))
  (define tname (and tparam (fml-param-type tparam)))
  (define use (findf (lambda (u) (and (eq? (fml-use-mode u) 'target)
                                      (or (not tname) (equal? (fml-use-alias u) tname))))
                     (fml-map-uses m)))
  (cond [use (last (string-split (fml-use-url use) "/"))]
        [tname tname]
        [else (error 'run-map "cannot determine the target type")]))

(define (bind-params g src root)
  (define ps (fml-group-params g))
  (for/fold ([h (hash)]) ([p (in-list ps)])
    (hash-set h (fml-param-name p) (if (eq? (fml-param-mode p) 'source) src root))))

;; run-group : execute every rule of a group in scope
(define (run-group m g sc)
  (for ([r (in-list (fml-group-rules g))]) (run-rule m r sc)))

;; ---- rules ------------------------------------------------------------------
(define (run-rule m r sc)
  (define s (car (fml-rule-sources r)))                     ; the suite uses single-source rules
  (define ctx (lookup sc (fml-source-context s)))
  (unless ctx (error 'run-map "unbound source variable ~a" (fml-source-context s)))
  (define matches (source-matches s ctx))
  (for ([elem (in-list matches)])
    (define sc1 (if (fml-source-variable s) (bind sc (fml-source-variable s) elem) sc))
    (define sc2 (run-targets m (fml-rule-targets r) sc1))
    (for ([c (in-list (fml-rule-dependents r))]) (run-call m c sc2))
    (for ([nr (in-list (fml-rule-nested r))]) (run-rule m nr sc2))))

;; the elements a source clause matches: context when no element; else each child, where-filtered
(define (source-matches s ctx)
  (define base
    (cond [(not (fml-source-element s)) (list ctx)]
          [(fhir? ctx) (let ([v (fhir-ref ctx (string->symbol (fml-source-element s)) '())])
                         (cond [(null? v) '()] [(list? v) v] [else (list v)]))]
          [else '()]))
  (define filtered
    (cond [(fml-source-where s)
           (filter (lambda (e) (fp-true? (fml-source-where s) e)) base)]
          [else base]))
  (case (fml-source-list-mode s)
    [(first) (if (pair? filtered) (list (car filtered)) '())]
    [(last) (if (pair? filtered) (list (last filtered)) '())]
    [else filtered]))

;; ---- targets ----------------------------------------------------------------
;; run one rule's comma-chained target ops, threading variable bindings; returns the extended scope
(define (run-targets m ts sc)
  (for/fold ([sc sc]) ([t (in-list ts)])
    (run-target m t sc)))

(define (run-target m t sc)
  (define ctx (lookup sc (fml-target-context t)))
  (unless (tslot? ctx) (error 'run-map "target context ~a is not a target" (fml-target-context t)))
  (define tr (fml-target-transform t))
  ;; create('T') on a choice element decides the concrete key (value + T), so thread the type in
  (define forced-type (match tr [(fml-invoke "create" (list (fml-literal ty))) ty] [_ #f]))
  (define slot
    (cond
      [(not (fml-target-element t)) ctx]
      [(and (fml-target-share t) (hash-ref (scope-shares sc) (fml-target-share t) #f))]
      [else
       (define elems (string-split (fml-target-element t) "."))
       (define s (for/fold ([s ctx]) ([e (in-list elems)] [i (in-naturals)])
                   (child-slot! s e (and (= i (sub1 (length elems))) forced-type))))
       (when (fml-target-share t) (hash-set! (scope-shares sc) (fml-target-share t) s))
       s]))
  (when tr
    (define v (transform-value m tr sc))
    (unless (eq? v 'null) ((tslot-set! slot) v)))   ; an empty expression assigns nothing
  (if (fml-target-variable t) (bind sc (fml-target-variable t) slot) sc))

;; transform-value : the jsexpr value a transform produces
(define (transform-value m tr sc)
  (match tr
    [(fml-literal v) v]
    [(fml-fhirpath ast) (fp-scalar ast sc)]
    [(fml-var name) (value->jsexpr (lookup sc name))]
    [(fml-invoke "create" (list (fml-literal ty))) (hash)]           ; typed empty element
    [(fml-invoke "copy" (list v)) (value->jsexpr (lookup sc v))]
    [(fml-invoke "reference" (list v))
     (define src (lookup sc v))
     (unless (fhir? src) (error 'run-map "reference() needs a resource"))
     (format "~a/~a" (fhir-type src) (pv (fhir-ref src 'id #f)))]
    [(fml-invoke "cast" (list v (fml-literal _ty))) (value->jsexpr (lookup sc v))]
    [(fml-invoke "evaluate" (list v (fml-fhirpath ast)))
     (fp-scalar-over ast (lookup sc v) sc)]
    [(fml-invoke "truncate" (list v (fml-literal n)))
     (let ([s (value->jsexpr (lookup sc v))]) (if (string? s) (substring s 0 (min (string-length s) (string->number n))) s))]
    [_ (error 'run-map "unsupported transform ~a" tr)]))

;; ---- FHIRPath bridging -------------------------------------------------------
;; evaluate an FML FHIRPath AST (parsed once, at parse time) over a focus element, with the scope's
;; SOURCE vars as %externals. Two FML-dialect bridges, both pure AST rewrites:
;;   - a rule variable is referenced by BARE NAME (`item.answer…`) -> rewritten to an %external;
;;   - element-model `.value` steps read a primitive's value -> retried stripped when empty
;;     (the engine unwraps primitives implicitly, so `.value` is a no-op step there).
(define (fp-items ast focus sc)
  (define vars (for/hash ([(k v) (in-hash (scope-vars sc))] #:unless (tslot? v)) (values k (list v))))
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

(define (fp-true? fpath focus)
  (define r (with-handlers ([exn:fail? (lambda (_) '())])
              (eval-fhirpath (strip-value-steps (fml-fhirpath-ast fpath)) (list focus) #:strict? #f)))
  (equal? (map item->system r) '(#t)))

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
  (define v (item->system item))
  (cond [(boolean? v) v]
        [(exact-integer? v) v]
        [(fp-long? v) (fp-long-v v)]
        [(fp-decimal? v) (string->number (decimal->string v))]
        [(string? v) v]
        [(moment? v) (regexp-replace #px"^@T?" (moment->string v) "")]
        [(fhir? v) (node->jsexpr v)]
        [(fhir? item) (node->jsexpr item)]
        [else (format "~a" v)]))

;; run-call : `then group(a, b)` — bind the callee's params to the argument values positionally
(define (run-call m c sc)
  (define g (findf (lambda (g) (equal? (fml-group-name g) (fml-call-name c))) (fml-map-groups m)))
  (unless g (error 'run-map "unknown group ~a" (fml-call-name c)))
  (define args (for/list ([a (in-list (fml-call-args c))])
                 (or (lookup sc a) (error 'run-map "unbound argument ~a" a))))
  (define sc* (for/fold ([s (scope (hash) (scope-shares sc))])
                        ([p (in-list (fml-group-params g))] [v (in-list args)])
                (bind s (fml-param-name p) v)))
  (run-group m g sc*))

(module+ test
  (require rackunit racket/file
           (only-in fhir/r5/schemas)
           (only-in "parser/parse.rkt" parse-fml)
           (only-in fhir/serialize/from-json from-json)
           (only-in fhir/result/result ok-value))
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
  (check-equal? (run-map m qr) (hash 'resourceType "Patient" 'gender "female")))
