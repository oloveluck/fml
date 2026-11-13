#lang racket/base

;;; spec-test.rkt — the mapping language as the specification states it
;;; (hl7.org/fhir/mapping-language.html and its formal grammar, mapping.g4): each form the
;;; specification gives is read, means what it says when run, and is carried by the StructureMap.
;;; The forms here were parsed and dropped (a source's type, cardinality and default; constants) or
;;; not read at all (rules after group invocations, the short form, a bare `share`, backticks, the
;;; g4's bare rule name and backticked url, metadata that is not a quoted value).

(require rackunit racket/string json
         (only-in fhir/r5/schemas)
         fml/parser/parse fml/parser/ast
         (only-in fml/transform run-map)
         (only-in fml/structuremap fml->structuremap structuremap->fml)
         (only-in fhir/serialize/from-json from-json)
         (only-in fhir/result/result ok? ok-value)
         (only-in fhir/diagnostic exn:fail:diagnostic? exn:fail:diagnostic-diagnostic diagnostic-message))

(define header
  (string-append "/// url = 'http://example.org/StructureMap/spec'\n/// name = 'Spec'\n"
                 "uses \"http://hl7.org/fhir/StructureDefinition/Patient\" alias Patient as source\n"
                 "uses \"http://hl7.org/fhir/StructureDefinition/Patient\" alias Patient as target\n"))
;; a-map : declarations rule … -> fml-map   a map of one group, Patient to Patient
(define (a-map #:before [before ""] . rules)
  (define r (parse-fml (string-append header before "group main(source src : Patient, target tgt : Patient) {\n"
                                      (string-join rules "\n") "\n}\n"
                                      "group names(source n, target t) { n.family as f -> t.family = f; }\n")))
  (unless (ok? r) (error 'spec-test "does not parse: ~a" r))
  (ok-value r))
(define (patient . kvs) (ok-value (from-json (apply hasheq 'resourceType "Patient" kvs) #:lenient? #t)))
(define (run m p) (string->jsexpr (jsexpr->string (run-map m p))))
(define (failure thunk)
  (with-handlers ([exn:fail:diagnostic? (lambda (e) (diagnostic-message (exn:fail:diagnostic-diagnostic e)))]
                  [exn:fail? exn-message])
    (thunk) #f))

;; ---- the header: metadata (the text) or `map` (the formal grammar) -----------------------------
(let ([m (a-map)])
  (check-equal? (fml-map-url m) "http://example.org/StructureMap/spec")
  (check-equal? (fml-map-name m) "Spec"))
(let ([m (ok-value (parse-fml "map `http://example.org/StructureMap/g4` = g4\nuses `http://hl7.org/fhir/StructureDefinition/Patient` as source\ngroup main(source src, target tgt) { src.gender -> tgt.gender; }\n"))])
  (check-equal? (fml-map-url m) "http://example.org/StructureMap/g4")
  (check-equal? (fml-map-name m) "g4")
  (check-equal? (fml-use-url (car (fml-map-uses m))) "http://hl7.org/fhir/StructureDefinition/Patient"))
;; metadata: FHIRPath literals, a complex element begun with no value, markdown over several lines;
;; a single jurisdiction is still a list (the resource's schema says it repeats)
(let* ([m (ok-value (parse-fml (string-append
                                "/// url = 'http://example.org/StructureMap/meta'\n/// name = 'Meta'\n/// title = 'R3 to R4'\n"
                                "/// experimental = true\n/// status = 'active'\n/// jurisdiction =\n/// jurisdiction.coding =\n"
                                "/// jurisdiction.coding.system = 'urn:iso:std:iso:3166'\n/// jurisdiction.coding.code = 'AQ'\n"
                                "/// description = \"\"\"\nYou should use my *awesome* map.\nIt does really cool things.\n\"\"\"\n"
                                "group main(source src, target tgt) { src.gender -> tgt.gender; }\n")))]
       [sm (fml->structuremap m)])
  (check-equal? (hash-ref sm 'title) "R3 to R4")
  (check-equal? (hash-ref sm 'experimental) #t)
  (check-equal? (hash-ref sm 'status) "active")
  (check-equal? (hash-ref sm 'jurisdiction) (list (hasheq 'coding (list (hasheq 'system "urn:iso:std:iso:3166" 'code "AQ")))))
  (check-equal? (hash-ref sm 'description) "You should use my *awesome* map.\nIt does really cool things.")
  ;; and read back from the resource, the same elements
  (check-equal? (fml->structuremap (structuremap->fml sm)) sm))
;; no status given: draft
(check-equal? (hash-ref (fml->structuremap (a-map "src.gender -> tgt.gender;")) 'status) "draft")

;; ---- a source: type, cardinality, default --------------------------------------------------------
;; `: type` selects the elements of that type
(let ([m (a-map "src.deceased : boolean as d -> tgt.deceased = d;")])
  (check-equal? (hash-ref (run m (patient 'deceasedBoolean #t)) 'deceasedBoolean) #t)
  (check-false (hash-has-key? (run m (patient 'deceasedDateTime "2020-01-01")) 'deceasedDateTime)))
(let ([m (a-map "src.contained : Patient as c -> tgt.contained = c;")])
  (check-equal? (length (hash-ref (run m (patient 'contained (list (hasheq 'resourceType "Patient" 'id "a")
                                                                   (hasheq 'resourceType "Observation" 'id "o" 'status "final"
                                                                           'code (hasheq 'text "x"))))) 'contained))
                1))
;; `min..max` is the element's cardinality in the source, or the transformation stops
(let ([m (a-map "src.name 1..1 as n -> tgt.name = n;")])
  (check-equal? (length (hash-ref (run m (patient 'name (list (hasheq 'family "Lee")))) 'name)) 1)
  (check-regexp-match #rx"src.name has 2 values, and the rule takes 1..1"
                      (failure (lambda () (run m (patient 'name (list (hasheq 'family "Lee") (hasheq 'family "Ng")))))))
  (check-regexp-match #rx"src.name has 0 values, and the rule takes 1..1" (failure (lambda () (run m (patient))))))
(check-false (failure (lambda () (run (a-map "src.name 0..* as n -> tgt.name = n;") (patient)))))
;; `default (value)` is used when the source has none, once
(let ([m (a-map "src.gender default ('unknown') as g -> tgt.gender = g;")])
  (check-equal? (hash-ref (run m (patient)) 'gender) "unknown")
  (check-equal? (hash-ref (run m (patient 'gender "female")) 'gender) "female"))
;; the specification's own source example reads whole
(let ([s (car (fml-rule-sources (car (fml-group-rules (car (fml-map-groups
          (a-map "src.value : integer 0..* default (10) first as vs0 where (value >= 10) check (value <= 100) log (value) -> tgt.value = vs0;")))))))])
  (check-equal? (list (fml-source-type s) (fml-source-min s) (fml-source-max s) (fml-source-list-mode s) (fml-source-variable s))
                '("integer" 0 "*" first "vs0"))
  (check-true (and (fml-fhirpath? (fml-source-default s)) (fml-fhirpath? (fml-source-where s))
                   (fml-fhirpath? (fml-source-check s)) (fml-fhirpath? (fml-source-log-expr s)))))

;; ---- constants -------------------------------------------------------------------------------
;; a constant is a FHIRPath expression, may name other constants, and a rule's variable hides it
(let ([m (a-map #:before "let half = 'un';\nlet whole = half & 'known';\n"
                "src -> tgt.gender = whole;"
                "src.birthDate as half -> tgt.birthDate = half;")])
  (define out (run m (patient 'birthDate "1990-01-02")))
  (check-equal? (hash-ref out 'gender) "unknown")
  (check-equal? (hash-ref out 'birthDate) "1990-01-02")
  (check-equal? (hash-ref (fml->structuremap m) 'const)
                (list (hasheq 'name "half" 'value "'un'") (hasheq 'name "whole" 'value "half & 'known'"))))
;; in a group that is called, and in a condition
(let ([m (ok-value (parse-fml (string-append header "let wanted = 'Lee';\n"
                                             "group main(source src : Patient, target tgt : Patient) { src.name as n -> tgt.name as t then names(n, t); }\n"
                                             "group names(source n, target t) { n.family as f where (f = wanted) -> t.family = f; }\n")))])
  (check-equal? (hash-ref (run m (patient 'name (list (hasheq 'family "Lee") (hasheq 'family "Ng")))) 'name)
                (list (hasheq 'family "Lee"))))
(check-regexp-match #rx"the constant (a|b) is defined in terms of itself"
                    (failure (lambda () (run (a-map #:before "let a = b;\nlet b = a;\n" "src -> tgt.gender = a;") (patient)))))

;; ---- rules -------------------------------------------------------------------------------------
;; group invocations, then a set of dependent rules: both run
(let ([m (a-map "src.name as n -> tgt.name as t then names(n, t) { n.text as x -> t.text = x; };")])
  (check-equal? (hash-ref (run m (patient 'name (list (hasheq 'family "Lee" 'text "Ann Lee")))) 'name)
                (list (hasheq 'family "Lee" 'text "Ann Lee"))))
;; the short form: `src -> tgt: a, b;` is `src.a -> tgt.a; src.b -> tgt.b;`
(let ([m (a-map "src -> tgt: gender, birthDate;")])
  (check-equal? (length (fml-group-rules (car (fml-map-groups m)))) 2)
  (check-equal? (fml-group-rules (car (fml-map-groups m)))
                (fml-group-rules (car (fml-map-groups (a-map "src.gender -> tgt.gender;" "src.birthDate -> tgt.birthDate;")))))
  (define out (run m (patient 'gender "female" 'birthDate "1990-01-02" 'active #t)))
  (check-equal? (list (hash-ref out 'gender) (hash-ref out 'birthDate) (hash-has-key? out 'active)) '("female" "1990-01-02" #f)))
;; a rule's name: in double quotes (the text), or a bare id (the formal grammar)
(check-equal? (map fml-rule-name (fml-group-rules (car (fml-map-groups (a-map "src.gender as g -> tgt.gender = g \"copyGender\";"
                                                                             "src.birthDate as b -> tgt.birthDate = b copyBirth;")))))
              '("copyGender" "copyBirth"))
;; the target list mode `share`, alone: the rules that share an element write into one
(let ([m (a-map "src -> tgt.name as t share, t.family = 'Lee';" "src -> tgt.name as t share, t.text = 'Ann Lee';")])
  (check-equal? (hash-ref (run m (patient)) 'name) (list (hasheq 'family "Lee" 'text "Ann Lee")))
  (define target (car (hash-ref (car (hash-ref (car (hash-ref (fml->structuremap m) 'group)) 'rule)) 'target)))
  (check-equal? (hash-ref target 'listMode) '("share"))
  (check-false (hash-has-key? target 'listRuleId)))
;; an element name in backticks
(check-equal? (hash-ref (run (a-map "src.`gender` as g -> tgt.`gender` = g;") (patient 'gender "male")) 'gender) "male")
;; every source list mode, and `queried`/`produced` structures
(for ([mode '("first" "not_first" "last" "not_last" "only_one")])
  (check-true (ok? (parse-fml (string-append header (format "group main(source src, target tgt) { src.name ~a as n -> tgt.name = n; }\n" mode))))))
(check-equal? (map fml-use-mode (fml-map-uses (ok-value (parse-fml "map \"http://x\" = x\nuses \"http://a\" as queried\nuses \"http://b\" as produced\ngroup g(source s, target t) { s.a -> t.a; }\n"))))
              '(queried produced))

;; ---- the StructureMap carries all of it ------------------------------------------------------
(let* ([m (a-map #:before "let limit = 100;\n"
                 "src.multipleBirth : integer 0..1 default (1) first as n where (n >= 1) check (n <= limit) -> tgt.multipleBirth = n;")]
       [sm (fml->structuremap m)]
       [source (car (hash-ref (car (hash-ref (car (hash-ref sm 'group)) 'rule)) 'source))])
  (check-equal? (for/list ([k '(type min max defaultValue listMode)]) (hash-ref source k)) '("integer" 0 "1" "1" "first"))
  (check-equal? (fml-map-groups (structuremap->fml sm)) (fml-map-groups m))
  (check-equal? (fml-map-constants (structuremap->fml sm)) (fml-map-constants m))
  ;; and the map read back from the resource runs the same
  (check-equal? (run (structuremap->fml sm) (patient 'multipleBirthInteger 2)) (run m (patient 'multipleBirthInteger 2)))
  (check-equal? (hash-ref (run m (patient)) 'multipleBirthInteger) 1))
