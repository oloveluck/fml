#lang racket/base

;;; guide-test.rkt — guide.md (the language guide agents read: fhir-mcp serves it as learn://fml,
;;; the fml skill points to it) keeps its promises: every ```fml block parses and checks with no
;;; error, the cheat sheet and the worked example give the results the text states, and the check
;;; reports the problems the text quotes, in its words.

(require rackunit racket/runtime-path racket/file racket/list racket/string json
         (only-in fml/parser/parse parse-fml)
         (only-in fml/check check-map)
         (only-in fml/transform run-map)
         (only-in fml/error fml-error-string)
         (only-in fml/workspace guide)
         (only-in fhir/diagnostic diagnostic-severity diagnostic-message)
         (only-in fhir/serialize/from-json from-json)
         (only-in fhir/result/result ok? ok-value err-error))

(dynamic-require 'fhir/r5/schemas #f)                 ; the engine runs over the R5 tower

(define-runtime-path guide-file "../guide.md")
(define text (file->string guide-file))
(define (blocks lang) (map cadr (regexp-match* (pregexp (string-append "```" lang "\n(.*?)```")) text #:match-select values)))
(define (map-named name) (for/first ([b (in-list (blocks "fml"))] #:when (regexp-match? (regexp-quote (format "/// name = '~a'" name)) b)) b))
(define (parse s) (let ([r (parse-fml s)]) (if (ok? r) (ok-value r) (error 'guide "~a" (fml-error-string (err-error r))))))
(define (transform map-text source) (run-map (parse map-text) (ok-value (from-json source #:lenient? #t))))

(test-case "the guide the workspace serves is this file"
  (check-equal? guide text))

(test-case "every fml block parses and checks with no error"
  (check-true (>= (length (blocks "fml")) 2))
  (for ([b (in-list (blocks "fml"))])
    (define r (parse-fml b))
    (check-true (ok? r) (format "parse: ~a" (if (ok? r) "" (fml-error-string (err-error r)))))
    (when (ok? r)
      (check-equal? (map diagnostic-message (check-map (ok-value r))) '() b))))

(test-case "the cheat sheet gives the Patient the text states"
  (define qr (string->jsexpr (car (blocks "json"))))
  (check-equal? (transform (map-named "QR2Patient") qr)
                (hash 'resourceType "Patient" 'gender "female" 'birthDate "1980-02-03")))

(test-case "the gender filter on the code system takes FHIR's codes only"
  (define rule (cadr (regexp-match #px"`(src[.]item as item where [(]linkId = 'gender'[)] then [{] item[.]answer as a where [(]value[.]system =\n'http://hl7[.]org/fhir/administrative-gender'[)] -> tgt[.]gender = [(]a[.]value[.]code[)]; [}];)`" (file->string guide-file))))
  (define m (string-replace (map-named "QR2Patient") "src.item as item where (linkId = 'gender') -> tgt.gender = (item.answer.value.code);"
                            (string-replace rule "\n" " ")))
  (define (qr system) (hasheq 'resourceType "QuestionnaireResponse" 'status "completed"
                              'item (list (hasheq 'linkId "gender" 'answer (list (hasheq 'valueCoding (hasheq 'system system 'code "female")))))))
  (check-equal? (hash-ref (transform m (qr "http://hl7.org/fhir/administrative-gender")) 'gender) "female")
  (check-false (hash-ref (transform m (qr "http://example.org/other")) 'gender #f)))

(test-case "the worked example gives the Observation the text describes"
  (define response (hasheq 'resourceType "QuestionnaireResponse" 'status "completed" 'subject (hasheq 'reference "Patient/p1")
                           'item (list (hasheq 'linkId "systolic" 'answer (list (hasheq 'valueInteger 120))))))
  (define obs (transform (map-named "BpIntake") response))
  (check-equal? (hash-ref obs 'status) "final")
  (check-equal? (hash-ref (car (hash-ref (hash-ref obs 'code) 'coding)) 'code) "85354-9")
  (check-equal? (hash-ref obs 'subject) (hash 'reference "Patient/p1"))
  (define comp (car (hash-ref obs 'component)))
  (check-equal? (hash-ref (car (hash-ref (hash-ref comp 'code) 'coding)) 'code) "8480-6")
  (check-equal? (hash-ref (hash-ref comp 'valueQuantity) 'value) 120)
  (check-equal? (hash-ref (hash-ref comp 'valueQuantity) 'unit) "mm[Hg]"))

(test-case "the name recipe splits the name as the text states"
  (define qr (hasheq 'resourceType "QuestionnaireResponse" 'status "completed"
                     'item (list (hasheq 'linkId "name" 'answer (list (hasheq 'valueString "Ann Marie Ng"))))))
  (check-equal? (hash-ref (transform (map-named "NameSplit") qr) 'name) (list (hash 'family "Ng" 'given '("Ann"))))
  (define middle (string-replace (map-named "NameSplit") "\"parts\";" "\"parts\";\n      a.value as full -> n.given = (full.split(' ')[1]) \"middle\";"))
  (check-equal? (hash-ref (car (hash-ref (transform middle qr) 'name)) 'given) '("Ann" "Marie")))

(test-case "the check reports the problems the text quotes"
  (define worked (map-named "BpIntake"))
  ;; an element the type does not have
  (define misspelt (check-map (parse (string-replace worked "src.subject as s" "src.subjct as s"))))
  (check-equal? (map diagnostic-message misspelt) '("`subjct` is not an element of QuestionnaireResponse (did you mean `subject`?)"))
  (check-equal? (map diagnostic-severity misspelt) '(error))
  ;; a required target element no rule assigns: a warning
  (define no-code (check-map (parse (string-replace worked "  src -> tgt.code = cc('http://loinc.org', '85354-9', 'Blood pressure panel') \"code\";\n" ""))))
  (check-equal? (map diagnostic-message no-code) '("Observation.code is required (min 1) but no rule of BpIntake assigns it"))
  (check-equal? (map diagnostic-severity no-code) '(warning)))

(test-case "a map applied to a source of another type is refused, as the text quotes"
  (check-exn (regexp (regexp-quote "the source is of type Observation, but group BpIntake takes src : QuestionnaireResponse"))
             (lambda () (transform (map-named "BpIntake") (hasheq 'resourceType "Observation" 'status "final" 'code (hasheq 'text "x"))))))

(test-case "the other forms of a rule run: type, cardinality, default, list mode, check, log, a constant, the short form"
  (define forms (map-named "Forms"))
  (define (qr . more) (apply hasheq 'resourceType "QuestionnaireResponse" 'id "q1" 'status "completed"
                             'item (list (hasheq 'linkId "weight" 'answer (list (hasheq 'valueDecimal 70)))) more))
  (check-equal? (string->jsexpr (jsexpr->string (transform forms (qr))))
                (string->jsexpr "{\"resourceType\": \"Observation\", \"id\": \"q1\", \"status\": \"final\", \"language\": \"en\",
                                  \"code\": {\"coding\": [{\"system\": \"http://loinc.org\", \"code\": \"29463-7\"}]},
                                  \"valueQuantity\": {\"value\": 70, \"unit\": \"kg\"}}"))
  ;; a language stated is copied; the default is for when there is none
  (check-equal? (hash-ref (transform forms (qr 'language "fr")) 'language) "fr")
  ;; `1..1`: a source with no status is an error at the rule
  (check-exn #rx"status has 0 values, and the rule takes 1..1"
             (lambda () (transform forms (hash-remove (qr) 'status)))))

(test-case "what is read beyond the specification is said, in the words the guide quotes"
  (define quoted (cadr (regexp-match #px"a\nwarning at the form: `` (`where linkId = 'x'`: the specification writes the expression in\nparentheses — `where [(]linkId = 'x'[)]`) ``" text)))
  (define bare (string-replace (map-named "QR2Patient") "where (linkId = 'gender')" "where linkId = 'x'"))
  (check-equal? (for/list ([d (in-list (check-map (parse bare)))]) (list (diagnostic-severity d) (diagnostic-message d)))
                (list (list 'warning (string-replace quoted "\n" " ")))))
