#lang racket/base

;;; registry.rkt — a StructureMap as a value: a typed procedure from its source structure to its
;;; target structure, found by canonical the way `fhir/operation`'s `operation` finds an
;;; OperationDefinition.
;;;
;;;   (structure-map "http://hl7.org/fhir/StructureMap/tutorial-step1")   ; by canonical
;;;   (structure-map ast)                 ; a parsed map; (structure-map sm-jsexpr) a StructureMap resource
;;;   (define step1 (structure-map url)) (step1 left)                      ; apply: FHIR JSON in, JSON out
;;;   (smap-source step1) (smap-target step1)                              ; its types
;;;   ((compose-maps a b) x)              ; b after a, when a's target structure is b's source
;;; Resolution: maps registered in the process (`register-structure-map!`, which a required
;;; `#lang fml` module does), StructureMaps in `current-data-source`, then the packages. A
;;; SMap is (smap ast source target): the AST and the type symbols of its entry group.

(require racket/contract racket/string
         "parser/ast.rkt"
         (only-in "structuremap.rkt" structuremap->fml)
         (only-in "transform.rkt" run-map uses-types resolve-structure-map registered-structure-maps)
         (only-in fhir/data-source current-data-source data-source? resources->data-source)
         (only-in fhir/serialize/from-json from-json)
         (only-in fhir/result/result ok? ok-value)
         (only-in fhir/model/node fhir?))

(provide (contract-out
          [structure-map (->* ((or/c string? fml-map? hash?)) (#:structures (or/c #f data-source? (listof hash?))) smap?)]
          [smap? (-> any/c boolean?)]
          [smap-ast (-> smap? fml-map?)]
          [smap-source (-> smap? (or/c symbol? #f))]
          [smap-target (-> smap? (or/c symbol? #f))]
          [compose-maps (-> smap? smap? smap?)]))

(struct smap (ast source target run)
  #:property prop:procedure (lambda (self src #:structures [structures #f]) ((smap-run self) src structures)))

(define (with-structures structures thunk)
  (parameterize ([current-data-source
                  (cond [(not structures) (current-data-source)]
                        [(data-source? structures) structures]
                        [else (resources->data-source structures)])])
    (thunk)))

(define (as-node src)
  (cond [(fhir? src) src]
        [else (dynamic-require 'fhir/r5/schemas #f)
              (define r (from-json src #:lenient? #t))
              (if (ok? r) (ok-value r) (raise-argument-error 'structure-map "a FHIR resource" src))]))

(define (structure-map ref #:structures [structures #f])
  (define ast
    (cond [(fml-map? ref) ref]
          [(hash? ref) (structuremap->fml ref)]
          [else (or (with-structures structures (lambda () (resolve-structure-map ref)))
                    (error 'structure-map "no StructureMap ~a (registered: ~a)" ref
                           (let ([rs (registered-structure-maps)]) (if (null? rs) "none" (string-join rs ", ")))))]))
  (define-values (source target) (with-structures structures (lambda () (entry-types ast))))
  (smap ast source target
        (lambda (src structures*)
          (with-structures (or structures* structures)
            (lambda () (uses-types ast) (run-map ast (as-node src)))))))

;; the entry group's parameter types: declared on the parameters, else from `uses … as source|target`
(define (entry-types ast)
  (define types (uses-types ast))
  (define g (and (pair? (fml-map-groups ast)) (car (fml-map-groups ast))))
  (define (param-type mode)
    (define p (and g (findf (lambda (p) (eq? (fml-param-mode p) mode)) (fml-group-params g))))
    (define u (findf (lambda (u) (eq? (fml-use-mode u) mode)) (fml-map-uses ast)))
    (or (and p (fml-param-type p) (hash-ref types (fml-param-type p) #f))
        (and u (hash-ref types (or (fml-use-alias u) (last-segment (fml-use-url u))) #f))
        (and p (fml-param-type p) (string->symbol (fml-param-type p)))))
  (values (param-type 'source) (param-type 'target)))
(define (last-segment url) (car (reverse (string-split (car (regexp-split #rx"[|]" url)) "/"))))

(define (compose-maps a b)
  (unless (and (smap-target a) (smap-source b) (eq? (smap-target a) (smap-source b)))
    (error 'compose-maps "the first map's target structure (~a) is not the second map's source structure (~a)"
           (smap-target a) (smap-source b)))
  (smap (smap-ast a) (smap-source a) (smap-target b)
        (lambda (src structures) (b (a src #:structures structures) #:structures structures))))

(module+ test
  (require rackunit (only-in fhir/r5/schemas) (only-in "parser/parse.rkt" parse-fml)
           (only-in "structuremap.rkt" fml->structuremap) (only-in "transform.rkt" register-structure-map!))
  (define qr2pat (ok-value (parse-fml (string-append
    "map \"http://x/qr2pat\" = \"qr2pat\"\n"
    "uses \"http://hl7.org/fhir/StructureDefinition/QuestionnaireResponse\" alias QR as source\n"
    "uses \"http://hl7.org/fhir/StructureDefinition/Patient\" alias Patient as target\n"
    "group entry(source src : QR, target tgt : Patient) {\n"
    "  src.item as item where (linkId = 'patient.sex') -> tgt.gender = (item.answer.valueString);\n"
    "}\n"))))
  (define pat2obs (ok-value (parse-fml (string-append
    "map \"http://x/pat2obs\" = \"pat2obs\"\n"
    "uses \"http://hl7.org/fhir/StructureDefinition/Patient\" alias Patient as source\n"
    "uses \"http://hl7.org/fhir/StructureDefinition/Observation\" alias Observation as target\n"
    "group entry(source src : Patient, target tgt : Observation) {\n"
    "  src.gender as g -> tgt.status = 'final', tgt.valueString = g;\n"
    "}\n"))))
  (define qr (hasheq 'resourceType "QuestionnaireResponse" 'status "completed"
                     'item (list (hasheq 'linkId "patient.sex" 'answer (list (hasheq 'valueString "female"))))))
  (define a (structure-map qr2pat))
  (check-equal? (smap-source a) 'QuestionnaireResponse)
  (check-equal? (smap-target a) 'Patient)
  (check-equal? (hash-ref (a qr) 'gender) "female")
  ;; by canonical: registered, and as a StructureMap resource in the data source
  (register-structure-map! qr2pat)
  (check-equal? (hash-ref ((structure-map "http://x/qr2pat") qr) 'gender) "female")
  (check-equal? (hash-ref ((structure-map "http://x/pat2obs"
                                          #:structures (list (fml->structuremap pat2obs)))
                           (a qr))
                          'valueString)
                "female")
  (check-exn #rx"no StructureMap http://x/nope" (lambda () (structure-map "http://x/nope")))
  ;; composition is checked by type
  (define b (structure-map pat2obs))
  (check-equal? (hash-ref ((compose-maps a b) qr) 'valueString) "female")
  (check-exn #rx"not the second map's source" (lambda () (compose-maps b a))))
