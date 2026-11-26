#lang racket/base

;;; reference-test.rkt — three behaviours found by running this engine beside the validator's
;;; StructureMap engine on the tutorial's maps changed in one small way (fhir-examples'
;;; value-experiment, H part 2). Each case fails without its fix in transform.rkt.

(require rackunit racket/file racket/string racket/runtime-path json
         (only-in fhir/r5/schemas)
         (only-in fhir/data-source current-data-source resources->data-source)
         (only-in fhir/serialize/from-json from-json)
         (only-in fhir/result/result ok-value)
         (only-in "../parser/parse.rkt" parse-fml)
         (only-in "../transform.rkt" run-map uses-types)
         (only-in "../check.rkt" check-map)
         (only-in fhir/diagnostic diagnostic-message diagnostic-severity))

(define-runtime-path tutorial "vendor/matchbox/tutorial")
(define-runtime-path matchbox "vendor/matchbox")

;; run : string string string (string -> string) -> jsexpr | string   a tutorial map, its text
;; changed, over a source of its step: the result, or the error's message
(define (run step map-name source-name change)
  (define sds (for/list ([f (in-list (directory-list (build-path tutorial step "logical") #:build? #t))])
                (call-with-input-file f read-json)))
  (define m (ok-value (parse-fml (change (file->string (build-path tutorial step "map" map-name))))))
  (with-handlers ([exn:fail? exn-message])
    (parameterize ([current-data-source (resources->data-source sds)])
      (uses-types m)
      (run-map m (ok-value (from-json (call-with-input-file (build-path tutorial step "source" source-name) read-json) #:lenient? #t))))))

;; a text that is no integer, put into an integer element, is an error at the rule (it was written
;; into the element as it stood: {"a21": "12345x"})
(check-regexp-match #rx"\"12345x\" is no integer"
                    (run "step4" "step4b2.map" "source4.json"
                         (lambda (t) (regexp-replace #rx"= a(?= *\")|= a(?=;)|= a(?= )" t "= append(a, 'x')"))))

;; a target that names a variable and has nothing after it creates its element and copies nothing
;; (the rule inside was removed: the source value "12345" was copied into tgt.e)
(let ([r (run "step11" "step11.map" "source11.json"
              (lambda (t) (regexp-replace #rx"[^\n]*rule_se_teg[^\n]*\n" t "")))])
  (check-true (hash? r))
  (check-equal? (for/list ([e (in-list (hash-ref r 'e))]) (and (hash? e) (list (hash-ref e 'f #f) (hash-ref e 'g #f))))
                '(("67890" "g2"))))

;; `check` is tested on every element the source selects, before its list mode picks among them
;; (with `not_first` over the one element, nothing was left to check and the map ran)
(check-regexp-match #rx"check condition failed"
                    (run "step9" "step9check.map" "source9b.json"
                         (lambda (t) (string-replace t "src.i as i" "src.i not_first as i"))))

;; a FHIRPath function that does not exist is said by the check, as a warning at its clause: run,
;; the expression gives nothing, silently (HL7's engine refuses to read the tutorial's step4b for it)
(let* ([m (ok-value (parse-fml (file->string (build-path tutorial "step4" "map" "step4b.map"))))]
       [found (filter (lambda (d) (regexp-match? #rx"isInteger\\(\\)` is not a FHIRPath function" (diagnostic-message d))) (check-map m))])
  (check-equal? (length found) 1)
  (check-eq? (diagnostic-severity (car found)) 'warning))

;; append and truncate take texts: of a complex value they are an error at the rule (append wrote
;; "#hasheq((ab . test))x", truncate handed the value on)
(check-regexp-match #rx"append joins texts"
                    (run "step10" "step10.map" "source10.json" (lambda (t) (string-replace t "tgt.aa = a;" "tgt.aa = append(a, 'x');"))))
(check-regexp-match #rx"truncate takes a text"
                    (run "step10" "step10.map" "source10.json" (lambda (t) (string-replace t "tgt.aa = a;" "tgt.aa = truncate(a, 3);"))))

;; `qty` with three arguments is not a form the specification has (a system comes with its code);
;; HL7's engine refuses the rule. This engine runs it, and the check says so at the target
;; (matchbox's own quantity.map is written that way: a warning, not an error)
(let* ([m (ok-value (parse-fml (file->string (build-path matchbox "quantity.map"))))]
       [found (filter (lambda (d) (regexp-match? #rx"qty with three arguments is not a form the specification has" (diagnostic-message d)))
                      (check-map m))])
  (check-equal? (length found) 1)
  (check-eq? (diagnostic-severity (car found)) 'warning))
(let* ([m (ok-value (parse-fml "/// url = 'http://x/q'\n/// name = 'q'\ngroup g(source src, target tgt) {\n  src.v as v -> tgt.value = qty(v, 'kg', 'http://unitsofmeasure.org', 'kg') \"r\";\n}\n"))])
  (check-false (ormap (lambda (d) (regexp-match? #rx"qty with" (diagnostic-message d))) (check-map m))))

;; ---- found on matchbox's own maps (the fourth pass) -------------------------------------------
;; run-matchbox : string string (string -> string) -> jsexpr | string   one of matchbox's top-level
;; maps, its text changed, over a source beside it
(define (run-matchbox map-name source-name [change values])
  (define m (ok-value (parse-fml (change (file->string (build-path matchbox map-name))))))
  (with-handlers ([exn:fail? exn-message])
    (uses-types m)
    (run-map m (ok-value (from-json (call-with-input-file (build-path matchbox source-name) read-json) #:lenient? #t)))))

;; translate(…, 'CodeableConcept') into a choice element writes the key of that type
;; (ExplanationOfBenefit.item.location[x]: it was written under the bare name `location`)
(let ([r (run-matchbox "translatecoding.map" "qr.json")])
  (check-true (hash? r))
  (define item (car (hash-ref r 'item)))
  (check-false (hash-has-key? item 'location))
  (check-equal? (hash-ref (car (hash-ref (hash-ref item 'locationCodeableConcept) 'coding)) 'code) "112283007"))

;; a transform's first argument may be a literal: copy(true) is true (looked up as a variable's
;; name it gave nothing, and the element kept the value it was copied with)
(define (second-active r) (hash-ref (hash-ref (cadr (hash-ref r 'entry)) 'resource) 'active))
(let ([r (run-matchbox "copyaliasingbug.map" "copyaliasingbug-source.json"
                       (lambda (t) (string-replace t "tgtRes.active = true" "tgtRes.active = copy(true)")))])
  (check-true (hash? r))
  (check-eq? (second-active r) #t))
(let ([r (run-matchbox "copyaliasingbug.map" "copyaliasingbug-source.json"
                       (lambda (t) (string-replace t "tgtRes.active = true" "tgtRes.active = cast(true, 'string')")))])
  (check-true (hash? r))
  (check-eq? (second-active r) #t))
;; append joins a boolean as FHIR writes it ("truex", not Racket's "#tx"); that text is no boolean
(check-regexp-match #rx"\"truex\" is no boolean"
                    (run-matchbox "copyaliasingbug.map" "copyaliasingbug-source.json"
                                  (lambda (t) (string-replace t "tgtRes.active = true" "tgtRes.active = append(true, 'x')"))))

;; FHIR's memberOf() in a `where` is answered by the terminology service over what is in context
;; (it was "not implemented", which a `where` reads as false: nothing was written and nothing said)
(define (gender-map vs)
  (ok-value (parse-fml (format "/// url = 'http://x/m'\n/// name = 'm'\nuses \"http://hl7.org/fhir/StructureDefinition/Patient\" alias Patient as source\nuses \"http://hl7.org/fhir/StructureDefinition/Patient\" alias Patient as target\ngroup g(source src : Patient, target tgt : Patient) {\n  src where (gender.memberOf('~a')) -> tgt.active = true \"r\";\n}\n" vs))))
(define (over-patient m gender)
  (uses-types m)
  (run-map m (ok-value (from-json (hasheq 'resourceType "Patient" 'gender gender) #:lenient? #t))))
(check-equal? (hash-ref (over-patient (gender-map "http://hl7.org/fhir/ValueSet/administrative-gender") "female") 'active #f) #t)
;; a value set nothing defines: cannot say, so the `where` does not hold
(check-false (hash-ref (over-patient (gender-map "http://nowhere.example/ValueSet/x") "female") 'active #f))
;; truncate cuts a primitive's text ("source must be stringy type"): true is "tru", which is no
;; boolean (it was handed on whole); cast(true, 'integer') fails (it handed the boolean on)
(check-regexp-match #rx"\"tru\" is no boolean"
                    (run-matchbox "copyaliasingbug.map" "copyaliasingbug-source.json"
                                  (lambda (t) (string-replace t "tgtRes.active = true" "tgtRes.active = truncate(true, 3)"))))
(check-regexp-match #rx"Failed to cast true to type \"integer\""
                    (run-matchbox "copyaliasingbug.map" "copyaliasingbug-source.json"
                                  (lambda (t) (string-replace t "tgtRes.active = true" "tgtRes.active = cast(true, 'integer')"))))

;; a `types` group for a primitive type is the copy of every value of that type (the tutorial's
;; step 1b): it was never run, the default copy taking the value as it stood
(check-equal? (hash-ref (run "step1" "step1b.map" "source1.json"
                             (lambda (t) (string-replace t "tgt.value = v" "tgt.value = append(v, 'x')")))
                        'a)
              "step1-demox")
(check-equal? (hash-ref (run "step1" "step1b.map" "source1.json" values) 'a) "step1-demo")
