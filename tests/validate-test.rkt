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
  src.item as item where linkId = 'gender' -> tgt.gender = (item.answer.value) "gender";
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
(delete-directory/files dir)
