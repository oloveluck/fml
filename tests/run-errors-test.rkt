#lang racket/base

;;; run-errors-test.rkt — four things a person writing the manual found by running maps:
;;;   1. a call to a group nothing defines failed at run time with a plain `error` (a stack trace at
;;;      the command line, no line in an OperationOutcome): it is the rule's error, at its line;
;;;   2. an `imports` nothing resolves silenced the unknown-group check (such a map checked clean,
;;;      whatever it called): the unresolved import is said, once;
;;;   3. `check` passed the specification's transforms this engine does not run (then escape, dateOp
;;;      and pointer; now dateOp alone), which then failed when the map ran;
;;;   4. `check --examples` said "its source matched nothing in the input" for a value the lenient
;;;      read had dropped (a date written 01/05/1950), where `transform #:validate` named it.

(require rackunit
         (only-in fhir/r5/schemas)
         (only-in fml/parser/parse parse-fml)
         (only-in fml/check check-map check-map/examples)
         (only-in fml/transform run-map)
         (only-in fhir/serialize/from-json from-json)
         (only-in fhir/result/result ok-value)
         (only-in fhir/diagnostic diagnostic-message diagnostic-severity diagnostic-loc loc-line
                  exn:fail:diagnostic? exn:fail:diagnostic-diagnostic))

(define (a-map . body)
  (ok-value (parse-fml (string-append "/// url = 'http://example.org/StructureMap/t'\n/// name = 't'\n"
                                      "uses \"http://hl7.org/fhir/StructureDefinition/QuestionnaireResponse\" as source\n"
                                      "uses \"http://hl7.org/fhir/StructureDefinition/Patient\" as target\n"
                                      (apply string-append body))
                       #:source "t.map")))
(define (group . rules)
  (string-append "group main(source src : QuestionnaireResponse, target tgt : Patient) {\n" (apply string-append rules) "}\n"))
(define qr (hasheq 'resourceType "QuestionnaireResponse" 'status "completed"
                   'item (list (hasheq 'linkId "dob" 'answer (list (hasheq 'valueDate "01/05/1950"))))))
(define (messages ds [severity #f])
  (for/list ([d (in-list ds)] #:when (or (not severity) (eq? (diagnostic-severity d) severity))) (diagnostic-message d)))

;; 1. an unknown group at run time: a diagnostic at the rule (line 6 of the text)
(let ([m (a-map (group "  src.item as item then nothere(item, tgt);\n"))])
  (define e (with-handlers ([exn:fail? values]) (run-map m (ok-value (from-json qr #:lenient? #t)))))
  (check-true (exn:fail:diagnostic? e))
  (check-regexp-match #rx"unknown group nothere [(]the groups of this map: main[)]" (exn-message e))
  (check-equal? (loc-line (diagnostic-loc (exn:fail:diagnostic-diagnostic e))) 6))

;; 2. an import nothing resolves is a warning, and the call is then not decided; with every import
;;    resolved (none here) the unknown group is the error it was
(let ([m (a-map "imports \"http://example.org/StructureMap/elsewhere\"\n" (group "  src.item as item then nothere(item, tgt);\n"))])
  (check-equal? (messages (check-map m) 'error) '())
  (check-regexp-match #rx"imports \"http://example.org/StructureMap/elsewhere\": no StructureMap with that url was found"
                      (car (messages (check-map m) 'warning))))
(check-regexp-match #rx"no group nothere in this map"
                    (car (messages (check-map (a-map (group "  src.item as item then nothere(item, tgt);\n"))) 'error)))

;; 3. a transform the engine does not run is an error of the check, at its rule (dateOp: the
;;    specification leaves its parameters "to be documented"; escape and pointer are run since)
(let ([found (check-map (a-map (group "  src.id as i -> tgt.id = dateOp(i, 'add');\n"
                                      "  src.id as i -> tgt.id = copy(i);\n")))])
  (check-equal? (length (messages found 'error)) 1)
  (check-regexp-match #rx"the transform dateOp is not one this engine runs" (car (messages found 'error)))
  (check-equal? (loc-line (diagnostic-loc (car found))) 6))

;; 4. the value the lenient read dropped is named where the example is checked
(let* ([m (a-map (group "  src.item as item where (linkId = 'dob') -> tgt then {\n"
                        "    item.answer as a -> tgt.birthDate = (a.value);\n  };\n"))]
       [profile (hasheq 'resourceType "StructureDefinition" 'id "dob" 'url "http://example.org/StructureDefinition/dob"
                        'name "Dob" 'type "Patient" 'baseDefinition "http://hl7.org/fhir/StructureDefinition/Patient"
                        'derivation "constraint"
                        'differential (hasheq 'element (list (hasheq 'id "Patient.birthDate" 'path "Patient.birthDate" 'min 1))))]
       [found (messages (check-map/examples m (list qr) #:profiles (list profile)) 'error)])
  (check-equal? (length found) 1)
  (check-regexp-match #rx"its source matched nothing the map could read — item\\[0\\].answer\\[0\\].valueDate [(]\"01/05/1950\"[)] could not be read"
                      (car found)))
