#lang racket/base

;;; validate-test.rkt — what a new user wanted from a map in a pipeline: a result that breaks a
;;; profile names the map's RULE, not only the element (`ensure-valid` after the map cannot), and a
;;; profile is given as the user has it — an FSH project or a canonical, not only an SD file.
;;;   (transform qr #:validate "<fsh dir>")        raises, each problem at the rule that wrote it
;;;   raco fml check map.fml --examples … --profile <fsh dir | canonical | sd.json>

(require rackunit racket/file racket/port racket/system racket/string json compiler/find-exe
         (only-in fml/check profiles-from check-map/examples)
         (only-in fml/parser/parse parse-fml)
         (only-in fhir/result/result ok-value)
         (only-in fhir/diagnostic diagnostic-message diagnostic-loc loc-line))

(define dir (make-temporary-directory "fml-validate-~a"))
(make-directory (build-path dir "fsh"))
(with-output-to-file (build-path dir "fsh" "intake.fsh")
  (lambda () (display "Profile: IntakePatient\nParent: Patient\nId: intake-patient\n* birthDate 1..1\n* gender 1..1\n")))
(define map-text #<<FML
#lang fml
map "http://example.org/StructureMap/intake" = "Intake"
uses "http://hl7.org/fhir/StructureDefinition/QuestionnaireResponse" as source
uses "http://hl7.org/fhir/StructureDefinition/Patient" as target

group main(source src : QuestionnaireResponse, target tgt : Patient) {
  src.item as item where (linkId = 'gender') -> tgt.gender = (item.answer.value) "gender";
}
FML
  )
(with-output-to-file (build-path dir "intake.rkt") (lambda () (display map-text)))
(define qr (hasheq 'resourceType "QuestionnaireResponse" 'status "completed"
                   'item (list (hasheq 'linkId "gender" 'answer (list (hasheq 'valueString "femme"))))))
(with-output-to-file (build-path dir "qr.json") (lambda () (write-json qr)))

;; the profiles of an FSH project
(define ps (profiles-from (build-path dir "fsh")))
(check-equal? (map (lambda (p) (hash-ref p 'url)) ps) '("http://example.org/StructureDefinition/intake-patient"))

;; transform #:validate: the gender rule wrote "femme" (line 7), no rule writes birthDate
(define transform (dynamic-require (build-path dir "intake.rkt") 'transform))
(check-equal? (transform qr) (hash 'resourceType "Patient" 'gender "femme"))   ; without, as before
(define msg (with-handlers ([exn:fail:user? exn-message]) (transform qr #:validate (path->string (build-path dir "fsh"))) "no error"))
(check-regexp-match #rx"^the map's Patient is invalid against IntakePatient [(]http://example.org/StructureDefinition/intake-patient[)] [(]FHIR R5 [(]5.0.0[)][)]:" msg)
(check-regexp-match #rx"intake[.]rkt:7:[0-9]+: .*Patient[.]gender" msg)
(check-regexp-match #rx"Patient[.]birthDate: .*1.*[(]no rule of main writes `birthDate`[)]" msg)
;; the gender rule is there but matched nothing (no gender item): said so, at the rule
(define no-gender (hash-set qr 'item '()))
(define msg2 (with-handlers ([exn:fail:user? exn-message]) (transform no-gender #:validate (path->string (build-path dir "fsh"))) "no error"))
(check-regexp-match #rx"intake[.]rkt:7:[0-9]+: .*Patient[.]gender: .*[(]the rule here wrote nothing: its source matched nothing in the input[)]" msg2)
;; the source matched, but the rule's `where` held for none of it (a gender answered under another
;; linkId, or a `where value.system = …` over an answer written as text): said with the filter and the
;; first item — "its source matched nothing in the input" blamed the input
(define other-item (hash-set qr 'item (list (hasheq 'linkId "sex" 'answer (list (hasheq 'valueString "female"))))))
(define msg2b (with-handlers ([exn:fail:user? exn-message]) (transform other-item #:validate (path->string (build-path dir "fsh"))) "no error"))
(check-regexp-match #rx"Patient[.]gender: .*[(]the rule here wrote nothing: its source matched 1 item, but `where [(]linkId = 'gender'[)]` held for none [(]the linkId there: sex[)][)]" msg2b)
;; the `where` of an outer rule held for none: said at that rule's line (7), where the fix is, not at
;; the nested rule that would have written birthDate (8) — a rerun's agent edited the wrong line
(with-output-to-file (build-path dir "nested.rkt")
  (lambda () (display (string-append "#lang fml\nmap \"http://example.org/StructureMap/nested\" = \"Nested\"\n"
                                     "uses \"http://hl7.org/fhir/StructureDefinition/QuestionnaireResponse\" as source\n"
                                     "uses \"http://hl7.org/fhir/StructureDefinition/Patient\" as target\n\n"
                                     "group main(source src : QuestionnaireResponse, target tgt : Patient) {\n"
                                     "  src.item as item where (linkId = 'birthDate') then {\n"
                                     "    item.answer as a -> tgt.birthDate = (a.value) \"birth\";\n"
                                     "  } \"birthItem\";\n}\n"))))
(define nested (dynamic-require (build-path dir "nested.rkt") 'transform))
(define msg-n (with-handlers ([exn:fail:user? exn-message])
                (nested (hash-set qr 'item (list (hasheq 'linkId "birthdate" 'answer (list (hasheq 'valueDate "1950-05-01")))))
                        #:validate (path->string (build-path dir "fsh")))
                "no error"))
(check-regexp-match #rx"nested[.]rkt:7:[0-9]+: .*Patient[.]birthDate: .*`where [(]linkId = 'birthDate'[)]` held for none" msg-n)
;; … and the value the `where` most likely meant (an agent's `birth_date` item showed only the first
;; item, the name)
(define near-item (hash-set qr 'item (list (hasheq 'linkId "name" 'answer (list (hasheq 'valueString "Ana")))
                                           (hasheq 'linkId "gendr" 'answer (list (hasheq 'valueString "female"))))))
(define msg2c (with-handlers ([exn:fail:user? exn-message]) (transform near-item #:validate (path->string (build-path dir "fsh"))) "no error"))
(check-regexp-match #rx"held for none [(]the linkId there: name, gendr — did you mean 'gendr'[?][)][)]" msg2c)
;; the item is there but its value cannot be read (a Coding written as a string): the message says
;; the read dropped it, where "matched nothing" sent the user looking for a missing item
(define unreadable (hash-set qr 'item (list (hasheq 'linkId "gender" 'answer (list (hasheq 'valueCoding "female"))))))
(define msg3 (parameterize ([current-error-port (open-output-string)])
               (with-handlers ([exn:fail:user? exn-message]) (transform unreadable #:validate (path->string (build-path dir "fsh"))) "no error")))
(check-regexp-match #rx"Patient[.]gender: .*its source matched nothing the map could read — item[[]0[]][.]answer[[]0[]][.]valueCoding [(]\"female\"[)] could not be read" msg3)
;; paths below the current directory are named from it
(check-regexp-match #rx"\n  intake[.]rkt:7:" (parameterize ([current-directory dir])
                                                (with-handlers ([exn:fail:user? exn-message]) (transform no-gender #:validate (path->string (build-path dir "fsh"))) "")))
;; by canonical, once the FSH has been read (its profiles are registered)
(check-exn #rx"birthDate" (lambda () (transform qr #:validate "http://example.org/StructureDefinition/intake-patient")))
;; the same error `ensure-valid` raises: one handler, `exn:fail:operation?`, catches an invalid
;; result from either (fhir-authoring's handler caught nothing from transform), its OperationOutcome
;; one issue per problem
(let ([e (with-handlers ([(dynamic-require 'fhir/operation 'exn:fail:operation?) values])
           (transform qr #:validate (path->string (build-path dir "fsh"))) #f)])
  (check-true (exn? e))
  (define oo ((dynamic-require 'fhir/operation 'exn:fail:operation-outcome) e))
  (check-equal? (hash-ref oo 'resourceType) "OperationOutcome")
  (check-true (for/or ([i (in-list (hash-ref oo 'issue))]) (regexp-match? #rx"birthDate" (hash-ref i 'diagnostics)))))
(check-exn #rx"is not a JSON file, an FSH project, or a canonical" (lambda () (transform qr #:validate "http://example.org/nope")))

;; the command line: --profile takes the FSH directory
(define out (open-output-string))
(define ok? (parameterize ([current-error-port out] [current-output-port out])
              (system* (find-exe) "-l-" "raco" "fml" "check" (path->string (build-path dir "intake.rkt"))
                       "--examples" (path->string (build-path dir "qr.json"))
                       "--profile" (path->string (build-path dir "fsh")))))
(check-false ok?)
;; each example named by its file, and the summary says what the results were held to (a clean run
;; said "1 example validated" and nothing of the profile)
(check-regexp-match #rx"qr[.]json: Patient.birthDate: .*no rule of main writes `birthDate`" (get-output-string out))
(check-regexp-match #rx"1 example validated [(][^)]*qr[.]json[)] against IntakePatient [(]http://[^)]*[)]" (get-output-string out))

;; the rule's source matched, but its value expression gave nothing for what it read (gender answered
;; as valueString, the rule reading `(a.value.code)`): said so with the value read — it said "its source
;; matched nothing in the input", and the item was there
(with-output-to-file (build-path dir "intake2.rkt")
  (lambda () (display (string-replace (string-replace map-text "StructureMap/intake" "StructureMap/intake2")
                                      "src.item as item where (linkId = 'gender') -> tgt.gender = (item.answer.value) \"gender\";"
                                      "src.item as item where (linkId = 'gender') then {\n    item.answer as a -> tgt.gender = (a.value.code) \"gender\";\n  } \"items\";"))))
(define transform2 (dynamic-require (build-path dir "intake2.rkt") 'transform))
(define as-string (hash-set qr 'item (list (hasheq 'linkId "gender" 'answer (list (hasheq 'valueString "female"))))))
(define msg4 (with-handlers ([exn:fail:user? exn-message]) (transform2 as-string #:validate (path->string (build-path dir "fsh"))) "no error"))
(check-regexp-match #rx"intake2[.]rkt:8:[0-9]+: .*Patient[.]gender: .*[(]the rule here wrote nothing: its source matched, but `[(]a[.]value[.]code[)]` was empty for {\"valueString\":\"female\"}[)]" msg4)
;; raco fml check says the same
(define found (check-map/examples (ok-value (parse-fml (string-replace (file->string (build-path dir "intake2.rkt")) "#lang fml\n" "")))
                                  (list as-string) #:profiles (profiles-from (build-path dir "fsh"))))
(check-true (for/or ([d (in-list found)]) (regexp-match? #rx"its source matched, but `[(]a[.]value[.]code[)]` was empty" (diagnostic-message d))))
;; raco fml --transform --indent prints the result indented, as a program's write-fhir-json does
(let ([out (open-output-string)])
  (parameterize ([current-output-port out] [current-error-port (open-output-nowhere)])
    (void (system* (find-exe) "-l-" "raco" "fml" (path->string (build-path dir "intake.rkt"))
                   "--transform" (path->string (build-path dir "qr.json")) "--indent")))
  (check-regexp-match #rx"^[{]\n  \"resourceType\": \"Patient\",\n  \"gender\": \"femme\"\n[}]" (get-output-string out)))
(delete-directory/files dir)
