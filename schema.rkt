#lang racket/base

;;; schema.rkt — what the registered schema says about an element, for the engine (transform.rkt:
;;; the shape of a target slot) and the checker (check.rkt: does the element exist, what can go in).
;;; One reading of the registry, so the two agree.
;;;
;;; A Ty is one of:
;;;   - a symbol          a registered complex type (Observation, HumanName, a logical model)
;;;   - (prim code)       a primitive element, by its FHIR code ("integer"); a `raw` leaf is "string"
;;;   - #f                unknown: nothing to decide about it (fail-open)
;;; An Element is (element key list? ty):
;;;   key   : the JSON key the element lives under (`valueQuantity` for `value` forced to Quantity)
;;;   list? : whether it repeats (a target appends; a source yields several)
;;;   ty    : its Ty

(require racket/contract racket/string
         (only-in fhir/model/introspect full-field-specs)
         (only-in fhir/model/schema field-spec? fs-name fs-kind fs-ftype ftype-type? ftype-type-name ftype-prim?
                  ftype-prim-code ftype-raw? type-schema-fields schema-field-choice-base)
         (only-in fhir/model/registry type-registered? type-schema-of))

(provide (contract-out
          [struct prim ([code string?])]
          [struct element ([key symbol?] [list? boolean?] [ty any/c])]
          ;; element-of : type element-name [forced-type-name] -> Element | #f
          ;; the field named; a choice base (`value`) resolved through the forced type to its variant
          ;; (`valueQuantity`) or left open; a type without a schema is open too. #f: no such element.
          [element-of (->* ((or/c symbol? #f) string?) ((or/c string? #f)) (or/c element? #f))]
          ;; path-type : type dotted-path -> (values missing-element|#f ty)   the type at the end of a
          ;; path, or the first step the structure lacks
          [path-type (-> symbol? string? (values (or/c string? #f) any/c))]
          [ty->string (-> any/c string?)]
          ;; type-name->ty : a type name as written ("Quantity", "integer") -> Ty
          [type-name->ty (-> (or/c string? #f) any/c)]))

(struct prim (code) #:transparent)
(struct element (key list? ty) #:transparent)

(define (ty->string ty) (cond [(prim? ty) (prim-code ty)] [(symbol? ty) (symbol->string ty)] [else "?"]))

(define (ftype->ty ft)
  (cond [(ftype-prim? ft) (prim (ftype-prim-code ft))]
        [(ftype-raw? ft) (prim "string")]
        [(and (ftype-type? ft) (type-registered? (ftype-type-name ft))) (ftype-type-name ft)]
        [else #f]))

;; FHIR's primitive types, by code (the datatypes page): a closed set
(define primitive-codes
  '("base64Binary" "boolean" "canonical" "code" "date" "dateTime" "decimal" "id" "instant" "integer" "integer64"
    "markdown" "oid" "positiveInt" "string" "time" "unsignedInt" "uri" "url" "uuid" "xhtml"))
(define (type-name->ty name)
  (cond [(not name) #f]
        [(member name primitive-codes) (prim name)]
        [(type-registered? (string->symbol name)) (string->symbol name)]
        [else #f]))

(define (specs-of ty) (and ty (with-handlers ([exn:fail? (lambda (_) #f)]) (full-field-specs ty))))
(define (field specs sym) (findf (lambda (e) (and (field-spec? e) (eq? (fs-name e) sym))) specs))
(define (field->element f) (element (fs-name f) (eq? (fs-kind f) 'list) (ftype->ty (fs-ftype f))))
(define (choice-key base ty) (string->symbol (string-append (symbol->string base) (capitalize ty))))
(define (capitalize s) (if (zero? (string-length s)) s (string-append (string-upcase (substring s 0 1)) (substring s 1))))

;; choice-base? : type name -> boolean   the schema's choice variants record their base (`value`)
(define (choice-base? ty sym)
  (define sch (type-schema-of ty))
  (and sch (for/or ([f (in-list (type-schema-fields sch))]) (eq? (schema-field-choice-base f) sym)) #t))

(define (element-of ty name [forced #f])
  (define specs (specs-of ty))
  (define sym (string->symbol name))
  (cond
    [(not specs) (element sym #f (type-name->ty forced))]                 ; no schema: open
    [(field specs sym) => field->element]
    [(and forced (field specs (choice-key sym forced))) => field->element] ; `value` + Quantity
    [(choice-base? ty sym) (element sym #f (type-name->ty forced))]        ; a choice, variant unknown
    [else #f]))

(define (path-type ty path)
  (let loop ([ty ty] [segs (string-split path ".")])
    (cond [(null? segs) (values #f ty)]
          [(not (symbol? ty)) (values #f #f)]
          [else (define e (element-of ty (car segs)))
                (if e (loop (element-ty e) (cdr segs)) (values (car segs) ty))])))

(module+ test
  (require rackunit (only-in fhir/r5/schemas))
  (check-equal? (element-of 'Observation "status") (element 'status #f (prim "code")))
  (check-equal? (element-of 'Observation "component") (element 'component #t 'ObservationComponent))
  (check-equal? (element-of 'Observation "id") (element 'id #f (prim "string")))          ; a raw leaf
  (check-equal? (element-of 'Observation "value" "Quantity") (element 'valueQuantity #f 'Quantity))
  (check-equal? (element-of 'Observation "value") (element 'value #f #f))                ; a choice, open
  (check-equal? (element-of 'Observation "valueString") (element 'valueString #f (prim "string")))
  (check-false (element-of 'Observation "nope"))
  (check-equal? (element-of #f "anything" "integer") (element 'anything #f (prim "integer")))  ; no schema
  (let-values ([(missing ty) (path-type 'Observation "component.valueQuantity.value")])
    (check-false missing) (check-equal? ty (prim "decimal")))
  (let-values ([(missing ty) (path-type 'Observation "component.nope.value")])
    (check-equal? missing "nope") (check-equal? ty 'ObservationComponent))
  (check-equal? (type-name->ty "Quantity") 'Quantity)
  (check-equal? (type-name->ty "integer") (prim "integer"))
  (check-equal? (ty->string (prim "code")) "code"))
