#lang racket/base

;;; drops-test.rkt — a `#lang fml` map's `transform` says what it could not read in its source (a new
;;; user's form held "01/05/1950" as a valueDate; the Patient came out without a birthDate and only the
;;; profile complained, about the wrong thing). `#:strict?` makes it an error.

(require rackunit racket/file racket/port)

(define dir (make-temporary-directory "fml-drops-~a"))
(define file (build-path dir "birth.map"))
(void (call-with-output-file file
  (lambda (o) (write-string (string-append
                             "#lang fml\n"
                             "map \"http://example.org/StructureMap/birth\" = \"Birth\"\n"
                             "uses \"http://hl7.org/fhir/StructureDefinition/QuestionnaireResponse\" as source\n"
                             "uses \"http://hl7.org/fhir/StructureDefinition/Patient\" as target\n"
                             "group main(source src : QuestionnaireResponse, target tgt : Patient) {\n"
                             "  src.item as item where (linkId = 'birthDate') then { item.answer as a -> tgt.birthDate = (a.value); };\n"
                             "}\n") o))))
(define transform (dynamic-require file 'transform))
(define (qr date) (hasheq 'resourceType "QuestionnaireResponse" 'status "completed"
                          'item (list (hasheq 'linkId "birthDate" 'answer (list (hasheq 'valueDate date))))))

;; a good date: read, no warning
(define quiet (open-output-string))
(check-equal? (hash-ref (parameterize ([current-error-port quiet]) (transform (qr "1950-05-01"))) 'birthDate) "1950-05-01")
(check-equal? (get-output-string quiet) "")
;; a bad one: the result lacks it, and the warning names it by its path
(define warned (open-output-string))
(check-false (hash-has-key? (parameterize ([current-error-port warned]) (transform (qr "01/05/1950"))) 'birthDate))
(check-regexp-match #rx"item\\[0\\][.]answer\\[0\\][.]valueDate [(]\"01/05/1950\"[)] not read" (get-output-string warned))
;; strict: an error
(check-exn #rx"transform: the source's item\\[0\\][.]answer\\[0\\][.]valueDate" (lambda () (transform (qr "01/05/1950") #:strict? #t)))
;; a date its month lacks says so, as the validator does (it said "not a valid value, or not an element")
(define cal (open-output-string))
(void (parameterize ([current-error-port cal]) (transform (qr "1990-02-30"))))
(check-regexp-match #rx"valueDate [(]\"1990-02-30\"[)] is no date: February 1990 has 28 days" (get-output-string cal))
(delete-directory/files dir)
