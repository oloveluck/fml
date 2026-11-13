#lang racket/base

;;; primitive-target-test.rkt — a primitive element takes a primitive. A new user's form answered
;;; gender with a valueCoding and the map kept `tgt.gender = (a.value)`: the Patient came out with
;;; `"gender": {"code": …, "system": …}`, and validation blamed `code.code` with no line. The rule
;;; now refuses it, at the rule, naming the element of the object that fits.

(require rackunit racket/file)

(define dir (make-temporary-directory "fml-prim-~a"))
(define file (build-path dir "gender.map"))
(void (call-with-output-file file
  (lambda (o) (write-string (string-append
                             "#lang fml\n"
                             "map \"http://example.org/StructureMap/gender\" = \"Gender\"\n"
                             "uses \"http://hl7.org/fhir/StructureDefinition/QuestionnaireResponse\" as source\n"
                             "uses \"http://hl7.org/fhir/StructureDefinition/Patient\" as target\n"
                             "group main(source src : QuestionnaireResponse, target tgt : Patient) {\n"
                             "  src.item as item where (linkId = 'gender') then { item.answer as a -> tgt.gender = (a.value) \"g\"; };\n"
                             "}\n") o))))
(define transform (dynamic-require file 'transform))
(define (qr answer) (hasheq 'resourceType "QuestionnaireResponse" 'status "completed"
                            'item (list (hasheq 'linkId "gender" 'answer (list answer)))))

;; a string answer: fine
(check-equal? (hash-ref (transform (qr (hasheq 'valueString "female"))) 'gender) "female")
;; a Coding answer: refused at the rule, with the expression that fits
(define msg (with-handlers ([exn:fail? exn-message])
              (transform (qr (hasheq 'valueCoding (hasheq 'system "http://hl7.org/fhir/administrative-gender" 'code "female"))))
              "no error"))
(check-regexp-match #rx"gender[.]map:6:[0-9]+: .*`tgt[.]gender` is a code, but the rule assigns an object [{]code, system[}]: assign its `code`, `[(]a[.]value[.]code[)]`" msg)
(delete-directory/files dir)
