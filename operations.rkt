#lang racket/base

;;; operations.rkt — StructureMap/$transform as an operation value (fhir/operation).
;;;
;;;   ($transform content "srcMap" fml-text)            run an FML map over `content` (piped)
;;;   (pipeline qr (step $transform "srcMap" fml) (validated #:profile …))
;;; This implementation executes FML: the map is `srcMap` (FHIR Mapping Language text), and a
;;; `source`/`sourceMap` alone (a StructureMap in JSON) is refused with that explanation. The
;;; content is read leniently into the R5 tower (FML's transform engine works on typed nodes).

(require (only-in fhir/r5/operations transform-definition)
         fhir/operation
         (only-in fhir/result/result ok? ok-value err-error)
         (only-in fhir/serialize/from-json from-json)
         (only-in fhir/diagnostic diagnostic-message)
         (only-in cql/error cql-error-string)
         (only-in fml/parser/parse parse-fml)
         (only-in fml/transform run-map))

(provide $transform)

;; FML text -> parsed map (memoized: a pipeline applies the same map to many resources)
(define parsed (make-weak-hash))
(define (fml-map text)
  (hash-ref! parsed text
             (lambda ()
               (define r (parse-fml text))
               (if (ok? r)
                   (ok-value r)
                   (operation-error "invalid" "$transform: the FML map does not parse: ~a" (cql-error-string (err-error r)))))))

(define $transform
  (make-operation
   transform-definition #:subject "content"
   (lambda (in _)
     (dynamic-require 'fhir/r5/schemas #f)
     (define text (or (input in "srcMap")
                      (operation-error "not-supported"
                                       "$transform here runs FML: give the map as srcMap (FHIR Mapping Language text)~a"
                                       (if (or (input in "source") (input in "sourceMap")) "; a StructureMap in JSON is not executed" ""))))
     (define content (input in "content"))
     (define node (from-json content #:lenient? #t))
     (unless (ok? node) (operation-error "invalid" "$transform: the content is not a FHIR resource"))
     (run-map (fml-map text) (ok-value node)))))

(register-operation! $transform)

(module+ test
  (require rackunit racket/file racket/string)
  ;; the repo's example map (QuestionnaireResponse -> Patient.gender), applied by reference
  (define fml (string-join (cdr (string-split (file->string (collection-file-path "qr2pat.fmlmap" "fhir" "examples")) "\n")) "\n"))
  (define qr (hasheq 'resourceType "QuestionnaireResponse" 'status "completed"
                     'item (list (hasheq 'linkId "patient"
                                         'item (list (hasheq 'linkId "patient.sex"
                                                             'answer (list (hasheq 'valueString "female"))))))))
  (define p (call-operation "http://hl7.org/fhir/OperationDefinition/StructureMap-transform" qr "srcMap" fml))
  (check-equal? (hash-ref p 'resourceType) "Patient")
  (check-equal? (hash-ref p 'gender) "female")
  (check-exn #rx"give the map as srcMap" (lambda () ($transform qr "source" "http://example.org/map")))
  (check-exn #rx"does not parse" (lambda () ($transform qr "srcMap" "map oops"))))
