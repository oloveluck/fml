#lang racket/base

;;; operations.rkt — StructureMap/$transform as an operation value (fhir/operation).
;;;
;;;   ($transform content #:srcMap fml-text)            run an FML map over `content` (the subject)
;;;   (~> qr ($transform #:srcMap fml) (ensure-valid #:profile …))
;;; The map is `srcMap` (FHIR Mapping Language text), `sourceMap` (a StructureMap resource), or
;;; `source` (a canonical: a map registered in the process, a StructureMap in the current data source,
;;; or one in the packages). The content is read leniently into the R5 tower (the engine works on
;;; typed nodes).

(require racket/string
         (only-in fml/parser/ast fml-map-url)
         (only-in fhir/r5/operations transform-definition)
         fhir/operation
         (only-in fhir/result/result ok? ok-value err-error)
         (only-in fhir/serialize/from-json from-json)
         (only-in fhir/diagnostic exn:fail:diagnostic? exn:fail:diagnostic-diagnostic diagnostics->operation-outcome)
         (only-in "error.rkt" fml-error-string)
         (only-in fml/parser/parse parse-fml)
         (only-in fml/structuremap structuremap->fml)
         (only-in fml/transform run-map resolve-structure-map registered-structure-maps))

(provide $transform)

;; FML text -> parsed map (memoized: a pipeline applies the same map to many resources)
(define parsed (make-weak-hash))
(define (fml-map text)
  (hash-ref! parsed text
             (lambda ()
               (define r (parse-fml text))
               (if (ok? r)
                   (ok-value r)
                   (operation-error "invalid" "$transform: the FML map does not parse: ~a" (fml-error-string (err-error r)))))))

(define $transform
  (make-operation
   transform-definition #:subject "content"
   (lambda (in _)
     (dynamic-require 'fhir/r5/schemas #f)
     (define m
       (cond [(input in "srcMap") => fml-map]
             [(input in "sourceMap") => (lambda (sm) (if (and (hash? sm) (equal? (hash-ref sm 'resourceType #f) "StructureMap"))
                                                      (structuremap->fml sm)
                                                      (operation-error "invalid" "$transform: sourceMap is not a StructureMap")))]
             [(input in "source") => (lambda (url) (or (resolve-structure-map url)
                                                      (operation-error "not-found" "$transform: no StructureMap ~a (registered: ~a)"
                                                                       url (string-join (registered-structure-maps) ", "))))]
             [else (operation-error "invalid" "$transform: give the map as srcMap (FML text), sourceMap (a StructureMap) or source (its canonical)")]))
     (define content (input in "content"))
     (define node (from-json content #:lenient? #t))
     (unless (ok? node) (operation-error "invalid" "$transform: the content is not a FHIR resource"))
     (with-handlers ([exn:fail:diagnostic?
                      (lambda (e) (raise (exn:fail:operation (exn-message e) (current-continuation-marks)
                                                             (diagnostics->operation-outcome (list (exn:fail:diagnostic-diagnostic e))))))])
       (run-map m (ok-value node))))))

(register-operation! $transform)

(module+ test
  (require rackunit racket/file racket/string)
  ;; the repo's example map (QuestionnaireResponse -> Patient.gender), applied by reference
  (define fml (string-join (cdr (string-split (file->string (collection-file-path "qr2pat.fmlmap" "fhir" "examples")) "\n")) "\n"))
  (define qr (hasheq 'resourceType "QuestionnaireResponse" 'status "completed"
                     'item (list (hasheq 'linkId "patient"
                                         'item (list (hasheq 'linkId "patient.sex"
                                                             'answer (list (hasheq 'valueString "female"))))))))
  (define p (apply-operation "http://hl7.org/fhir/OperationDefinition/StructureMap-transform" qr #:srcMap fml))
  (check-equal? (hash-ref p 'resourceType) "Patient")
  (check-equal? (hash-ref p 'gender) "female")
  (check-exn #rx"no StructureMap" (lambda () ($transform qr #:source "http://example.org/map")))
  (check-exn #rx"does not parse" (lambda () ($transform qr #:srcMap "map oops")))
  ;; a source of another type than the entry group takes is refused, not read
  (check-exn #rx"the source is of type Observation, but group"
             (lambda () ($transform (hasheq 'resourceType "Observation" 'status "final" 'code (hasheq 'text "x")) #:srcMap fml)))
  ;; the same map as a StructureMap resource, and by canonical once registered
  (require (only-in fml/parser/parse parse-fml) (only-in fml/structuremap fml->structuremap)
           (only-in fml/transform register-structure-map!) (only-in fhir/result/result ok-value))
  (define ast (ok-value (parse-fml fml)))
  (check-equal? (hash-ref ($transform qr #:sourceMap (fml->structuremap ast)) 'gender) "female")
  (register-structure-map! ast)
  (check-equal? (hash-ref (apply-operation "StructureMap/$transform" qr #:source (fml-map-url ast)) 'gender) "female"))
