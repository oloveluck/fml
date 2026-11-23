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
