#lang racket/base

;;; alignment-test.rkt — the quality and spec-alignment pass of 2026-11-13: each behaviour it
;;; changed, held by a check that fails without the change. In four parts:
;;;   1. the text: what is read, what is refused, what is said to be beyond the specification;
;;;   2. the StructureMap: its parameters as its definition types them, the reference's forms read;
;;;   3. the engine: conditions as FHIRPath reads a collection for a Boolean, created types, the
;;;      specification's transforms (escape, pointer, cast's implicit type, reference, cp, id);
;;;   4. failures of a run as diagnostics at their place.
;;; The transform table is the specification's (the StructureMap page; the definitions quoted here
;;; are those of the R5 code system http://hl7.org/fhir/map-transform).

(require rackunit racket/string racket/list racket/file racket/runtime-path racket/match json
         (only-in fhir/r5/schemas)
         fml/parser/parse fml/parser/ast
         (only-in fml/error fml-error-diagnostic)
         (only-in fml/transform run-map transform-names current-empty-targets)
         (only-in fml/structuremap fml->structuremap structuremap->fml)
         (only-in fml/print fml->text)
         (only-in fml/check check-map validate-result)
         (only-in fhir/serialize/from-json from-json)
         (only-in fhir/result/result ok? ok-value err-error)
         (only-in fhir/diagnostic exn:fail:diagnostic? exn:fail:diagnostic-diagnostic diagnostic-message
                  diagnostic-severity diagnostic-loc loc-line loc-column))

;; ---- helpers -----------------------------------------------------------------------------------
(define (header source target)
  (string-append "/// url = 'http://example.org/StructureMap/a'\n/// name = 'A'\n"
                 (format "uses \"http://hl7.org/fhir/StructureDefinition/~a\" alias Source as source\n" source)
                 (format "uses \"http://hl7.org/fhir/StructureDefinition/~a\" alias Target as target\n" target)))
;; text-of-map : source-type target-type rule … -> string   one group `main` at line 5, its rules from line 6
(define (text-of-map source target #:flags [flags ""] #:after [after ""] . rules)
  (string-append (header source target)
                 "group main(source src : Source, target tgt : Target)" flags " {\n" (string-join rules "\n") "\n}\n" after))
(define (parsed text) (let ([r (parse-fml text #:source "a.map")]) (if (ok? r) (ok-value r) (error 'alignment-test "~a" (err-error r)))))
(define (a-map source target #:flags [flags ""] #:after [after ""] . rules)
  (parsed (apply text-of-map source target #:flags flags #:after after rules)))
(define (node j) (ok-value (from-json j #:lenient? #t)))
(define (patient . kvs) (node (apply hasheq 'resourceType "Patient" kvs)))
(define (run m src) (string->jsexpr (jsexpr->string (run-map m src))))
;; failure : thunk -> (cons message line) | #f   the diagnostic a run or a parse stops with
(define (failure thunk)
  (with-handlers ([exn:fail:diagnostic?
                   (lambda (e) (let ([d (exn:fail:diagnostic-diagnostic e)]) (cons (diagnostic-message d) (loc-line (diagnostic-loc d)))))])
    (thunk) #f))
(define (parse-failure text)
  (define r (parse-fml text #:source "a.map"))
  (and (not (ok? r)) (let ([d (fml-error-diagnostic (err-error r))]) (cons (diagnostic-message d) (loc-line (diagnostic-loc d))))))
(define (notes m) (for/list ([n (in-list (specification-notes m))]) (cons (loc-line (diagnostic-loc n)) (diagnostic-message n))))
(define (rules-of sm) (hash-ref (car (hash-ref sm 'group)) 'rule))

;; ==== 1. the text ================================================================================
;; a date literal in an expression is its text (the token's value was left a syntax object: the
;; StructureMap said `@#<syntax …>` and the expression evaluated to nothing)
(let ([m (a-map "Patient" "Patient"
                "src -> tgt.birthDate = (@2023-10-21);"
                "src.birthDate as b where (b < @2000-01-01) -> tgt.gender = 'male';")])
  (check-equal? (run m (patient 'birthDate "1990-01-02")) (hasheq 'resourceType "Patient" 'birthDate "2023-10-21" 'gender "male"))
  (check-equal? (hash-ref (car (hash-ref (car (hash-ref (car (rules-of (fml->structuremap m))) 'target)) 'parameter)) 'valueString) "@2023-10-21")
  (check-regexp-match #rx"where [(]b < @2000-01-01[)]" (fml->text m)))

;; an expression in parentheses is FHIRPath, whatever it is: `(iif(…))` and `(today())` call
;; FHIRPath's functions (a parenthesized call was read as the transform of that name: "the
;; transform iif is not one this engine runs"); without parentheses a call is still a transform
(let ([m (a-map "Patient" "Patient"
                "src.gender as g -> tgt.gender = (iif(g = 'female', 'male', 'female'));"
                "src -> tgt.birthDate = (today());"
                "src.id as i -> tgt.id = truncate(i, 2);")])
  (check-match (map fml-target-transform (append-map fml-rule-targets (fml-group-rules (car (fml-map-groups m)))))
               (list (? fml-fhirpath?) (? fml-fhirpath?) (fml-invoke "truncate" _)))
  (check-equal? (filter (lambda (d) (eq? (diagnostic-severity d) 'error)) (check-map m)) '())
  (define out (run m (patient 'id "abcdef" 'gender "female")))
  (check-equal? (list (hash-ref out 'gender) (hash-ref out 'id)) '("male" "ab"))
  (check-regexp-match #px"^[0-9]{4}-[0-9]{2}-[0-9]{2}$" (hash-ref out 'birthDate))
  ;; and it is written back in parentheses, the same map
  (check-regexp-match #rx"tgt.gender = [(]iif[(]g = 'female', 'male', 'female'[)][)];\n  src -> tgt.birthDate = [(]today[(][)][)];" (fml->text m))
  (check-equal? (fml->structuremap (parsed (fml->text m))) (fml->structuremap m))
  (check-equal? (fml-map-groups (structuremap->fml (fml->structuremap m))) (fml-map-groups m)))

;; without the parentheses it is an invocation, of a transform there is not: the check says how
(check-match (for/list ([d (in-list (check-map (a-map "Patient" "Patient" "src -> tgt.birthDate = today();")))]
                        #:when (eq? (diagnostic-severity d) 'error))
               (diagnostic-message d))
             (list (regexp #rx"the transform today is not one this engine runs.*; a FHIRPath function is written in parentheses: `= [(]today[(][)][)]`$")))

;; a name never takes the `+` after it: `(a+1)` is an addition (the lexer read `a+` as a name, for
;; the group flag `type+`), and the flag is still read
(let ([m (a-map "Patient" "Patient" #:flags " <<type+>>" "src.multipleBirth as n -> tgt.multipleBirth = (n+1);")])
  (check-equal? (hash-ref (run m (patient 'multipleBirthInteger 2)) 'multipleBirthInteger) 3)
  (check-equal? (fml-group-flags (car (fml-map-groups m))) '("type+"))
  (check-equal? (hash-ref (car (hash-ref (fml->structuremap m) 'group)) 'typeMode) "type-and-types"))
(check-equal? (fml-group-flags (car (fml-map-groups (a-map "Patient" "Patient" #:flags " <<types>>" "src.gender -> tgt.gender;")))) '("types"))
;; `type` is still a name: an element, a variable
(check-equal? (hash-ref (run (a-map "Observation" "Observation" "src.status as type -> tgt.status = type;")
                             (node (hasheq 'resourceType "Observation" 'status "final" 'code (hasheq 'text "x"))))
                        'status)
              "final")
;; a group's type mode is `types` or `type+`: anything else was read and dropped
(check-match (parse-failure (text-of-map "Patient" "Patient" #:flags " <<nonsense words>>" "src.gender -> tgt.gender;"))
             (cons (regexp #rx"unexpected identifier `nonsense`") 5))
;; a cardinality is whole numbers (`1.5..2` was read, and written to the StructureMap as min 1.5)
(check-match (parse-failure (text-of-map "Patient" "Patient" "src.name 1.5..2 as n -> tgt.name = n;"))
             (cons (regexp #rx"a cardinality is whole numbers.*`1[.]5` is not one") 6))
(check-false (parse-failure (text-of-map "Patient" "Patient" "src.name 0..* as n -> tgt.name = n;")))

;; what the specified language does not have is said, at the form: an expression as a transform
;; without parentheses, a rule's name in single quotes, an expression given to a group
(check-match (notes (a-map "Patient" "Patient" #:after "group names(source n, target t) { n.family as f -> t.family = f; }\n"
                           "src.name as n -> tgt.name as t, t.text = n.family & '!';"
                           "src.gender as g -> tgt.gender = g 'copyGender';"
                           "src.name as n -> tgt.name as t then names(n.first(), t);"))
             (list (cons 6 (regexp #rx"^`= n.family & '!'`: the specification writes an expression in parentheses — `= [(]n.family & '!'[)]`$"))
                   (cons 7 (regexp #rx"^'copyGender': the specification writes a rule's name in double quotes — \"copyGender\"$"))
                   (cons 8 (regexp #rx"^`names[(]… n.first[(][)] …[)]`: a group is given variables and literals"))))
;; on a file with Windows line ends too: the note quotes its own text (a position counts `\r\n` as
;; one character; the text was read some lines up — "`where then {`")
(check-match (notes (parsed (string-replace (text-of-map "Patient" "Patient"
                                                         "src.name as n where n.family = 'Lee' -> tgt.name = n;"
                                                         "src.gender as g where g = 'female' -> tgt.gender = g 'named';")
                                            "\n" "\r\n")))
             (list (cons 6 (regexp #rx"^`where n.family = 'Lee'`: the specification writes the expression in parentheses"))
                   (cons 7 (regexp #rx"^`where g = 'female'`: "))
                   (cons 7 (regexp #rx"^'named': the specification writes a rule's name in double quotes"))))
;; and not the forms it has: a literal of any kind, a variable, a path of names, an invocation, an
;; expression in parentheses; a variable and a literal given to a group
(check-equal? (notes (a-map "Patient" "Patient" #:after "group names(source n, source s, target t) { n.family as f -> t.family = f; }\n"
                            "src -> tgt.active = true, tgt.gender = 'female', tgt.birthDate = @1990-01-02, tgt.multipleBirth = 2;"
                            "src.name as n -> tgt.name as t, t.text = n.family, t.family = (n.family & '!'), t.use = cast(n, 'code');"
                            "src.name as n -> tgt.name as t then names(n, 'x', t) \"named\";"))
              '())
;; the printer writes none of them: a map that had all three reads back without a note
(let ([m (a-map "Patient" "Patient" "src.name as n -> tgt.name as t, t.text = n.family & '!' 'named';")])
  (check-equal? (notes (parsed (fml->text m))) '()))

;; ==== 2. the StructureMap =========================================================================
;; a group invocation's parameters are a transform's: a variable is valueId, a string valueString
;; (its text, not the FHIRPath that writes it), a number valueInteger; and they read back as given
(let* ([m (a-map "Patient" "Patient"
                 #:after "group label(source n, source text, source count, target t) { n -> t.family = text, t.text = (count.toString()); }\n"
                 "src.name as n -> tgt.name as t then label(n, 'Lee', 5, t);")]
       [sm (fml->structuremap m)]
       [dependent (car (hash-ref (car (rules-of sm)) 'dependent))])
  (check-equal? (hash-ref dependent 'parameter)
                (list (hasheq 'valueId "n") (hasheq 'valueString "Lee") (hasheq 'valueInteger 5) (hasheq 'valueId "t")))
  (check-equal? (fml-map-groups (structuremap->fml sm)) (fml-map-groups m))
  (check-equal? (hash-ref (run m (patient 'name (list (hasheq 'given (list "Ann"))))) 'name) (list (hasheq 'family "Lee" 'text "5")))
  (check-equal? (run (structuremap->fml sm) (patient 'name (list (hasheq 'given (list "Ann")))))
                (run m (patient 'name (list (hasheq 'given (list "Ann")))))))
;; a transform with no parameters has no `parameter` (`uuid()` wrote `"parameter": []`: not FHIR JSON)
(let ([target (car (hash-ref (car (rules-of (fml->structuremap (a-map "Patient" "Patient" "src -> tgt.id = uuid();")))) 'target))])
  (check-equal? (hash-ref target 'transform) "uuid")
  (check-false (hash-has-key? target 'parameter)))

;; the reference's StructureMaps, read: R4's `dependent.variable` (the names; the call lost its
;; arguments), and its form of the simple rule `src.a -> tgt.a` — a source and a target sharing a
;; synthetic variable, the target a `create` of nothing (run as written it copied nothing)
(define (reference-map . rules)
  (hasheq 'resourceType "StructureMap" 'url "http://example.org/StructureMap/ref" 'name "ref" 'status "draft"
          'structure (list (hasheq 'url "http://hl7.org/fhir/StructureDefinition/Patient" 'mode "source" 'alias "P")
                           (hasheq 'url "http://hl7.org/fhir/StructureDefinition/Patient" 'mode "target" 'alias "P"))
          'group (list (hasheq 'name "main" 'typeMode "none"
                               'input (list (hasheq 'name "src" 'type "P" 'mode "source") (hasheq 'name "tgt" 'type "P" 'mode "target"))
                               'rule rules)
                       (hasheq 'name "names" 'typeMode "none"
                               'input (list (hasheq 'name "n" 'mode "source") (hasheq 'name "t" 'mode "target"))
                               'rule (list (hasheq 'name "family" 'source (list (hasheq 'context "n" 'element "family" 'variable "f"))
                                                   'target (list (hasheq 'context "t" 'element "family" 'transform "copy"
                                                                         'parameter (list (hasheq 'valueId "f"))))))))))
(let ([m (structuremap->fml
          (reference-map (hasheq 'name "names" 'source (list (hasheq 'context "src" 'element "name" 'variable "n"))
                                 'target (list (hasheq 'context "tgt" 'contextType "variable" 'element "name" 'variable "t"))
                                 'dependent (list (hasheq 'name "names" 'variable (list "n" "t"))))
                         (hasheq 'name "gender" 'source (list (hasheq 'context "src" 'element "gender" 'variable "vvv"))
                                 'target (list (hasheq 'context "tgt" 'contextType "variable" 'element "gender" 'variable "vvv"
                                                       'transform "create")))))])
  (check-equal? (run m (patient 'gender "female" 'name (list (hasheq 'family "Lee" 'text "Ann Lee"))))
                (hasheq 'resourceType "Patient" 'gender "female" 'name (list (hasheq 'family "Lee"))))
  (check-regexp-match #rx"then names[(]n, t[)] \"names\";\n  src.gender -> tgt.gender \"gender\";" (fml->text m)))

;; HL7's own example (StructureMap-supplyrequest-transform, in the core package) writes a constant
;; as an `evaluate` of a quoted string, and a `copy` with no parameter: its text means the resource
;; (the constant came back as `('draft')`, which reads as the literal — another StructureMap)
(let* ([m (structuremap->fml
           (reference-map (hasheq 'name "status" 'source (list (hasheq 'context "src" 'element "id" 'variable "a"))
                                  'target (list (hasheq 'context "tgt" 'element "gender" 'transform "evaluate"
                                                        'parameter (list (hasheq 'valueString "'female'")))))
                          (hasheq 'name "birth" 'source (list (hasheq 'context "src" 'element "birthDate" 'variable "a"))
                                  'target (list (hasheq 'context "tgt" 'element "birthDate" 'transform "copy")))))]
       [text (fml->text m)])
  (check-regexp-match #rx"src.id as a -> tgt.gender = 'female' \"status\";\n  src.birthDate as a -> tgt.birthDate = copy[(][)] \"birth\";" text)
  (check-equal? (fml->structuremap (parsed text)) (fml->structuremap m))
  (check-equal? (hash-ref (run m (patient 'id "p1")) 'gender) "female"))

;; ==== 3. the engine ===============================================================================
;; a condition is read as FHIRPath reads a collection where a Boolean is expected: one Boolean is
;; its value, one item of another kind is true, nothing or several items is not. (Every condition
;; that was not exactly `true` was false: matchbox's `where $this.resource.ofType(FHIR.Patient)`
;; maps wrote nothing.)
(define bundle
  (node (hasheq 'resourceType "Bundle" 'type "collection"
                'entry (list (hasheq 'resource (hasheq 'resourceType "Patient" 'id "p1" 'active #f))
                             (hasheq 'resource (hasheq 'resourceType "Observation" 'id "o1" 'status "final" 'code (hasheq 'text "x")))))))
(let ([m (a-map "Bundle" "Bundle" "src.entry as e where ($this.resource.ofType(FHIR.Patient)) -> tgt.entry = e;")])
  (check-equal? (map (lambda (e) (hash-ref (hash-ref e 'resource) 'id)) (hash-ref (run m bundle) 'entry)) '("p1")))
(let ([holds (lambda (condition)
               (hash-has-key? (run (a-map "Patient" "Patient" (format "src.gender as g where (~a) -> tgt.gender = g;" condition))
                                   (patient 'gender "female" 'active #f 'name (list (hasheq 'family "Lee") (hasheq 'family "Ng"))))
                              'gender))])
  (check-true (holds "g = 'female'"))
  (check-false (holds "g = 'male'"))
  (check-true (holds "src.id.empty()"))
  (check-false (holds "src.active"))                      ; one Boolean, false
  (check-true (holds "src.gender"))                       ; one item that is no Boolean
  (check-false (holds "src.id"))                          ; nothing
  (check-false (holds "src.name")))                       ; several items

;; matchbox's own maps of that kind now write what they say (they ran, and wrote nothing): the
;; Patient entry copied twice, the second copy alone made active; the sections of the reports a
;; Composition refers to
(define-runtime-path matchbox "vendor/matchbox")
(define (matchbox-run map-file source-file)
  (run (parsed (file->string (build-path matchbox map-file)))
       (node (call-with-input-file (build-path matchbox source-file) read-json))))
(check-equal? (for/list ([e (in-list (hash-ref (matchbox-run "copyaliasingbug.map" "copyaliasingbug-source.json") 'entry))])
                (hash-ref (hash-ref e 'resource) 'active))
              '(#f #t))
(check-equal? (map (lambda (s) (hash-ref s 'title)) (hash-ref (matchbox-run "bundle-resolve.map" "bundle-resolve.src.json") 'section))
              '("lab report 2" "lab report 5"))

;; what is created into an element of an abstract type has its own type's elements: a Patient
;; created into Bundle.entry.resource has a `name` that repeats (it was written as one object)
(let ([m (a-map "Patient" "Bundle"
                "src -> tgt.entry as e, e.resource = create('Patient') as p then {"
                "  src.name as n -> p.name = n;"
                "  src.telecom as t -> p.telecom = t;"
                "};")])
  (check-equal? (hash-ref (car (hash-ref (run m (patient 'name (list (hasheq 'family "Lee")) 'telecom (list (hasheq 'value "1") (hasheq 'value "2")))) 'entry)) 'resource)
                (hasheq 'resourceType "Patient" 'name (list (hasheq 'family "Lee")) 'telecom (list (hasheq 'value "1") (hasheq 'value "2")))))

;; an expression's value picks a choice element's key, as a variable's does (`tgt.value = (a + 1)`
;; wrote the key `value`, which no resource has)
(let ([m (a-map "Patient" "Patient"
                "src.multipleBirth as n -> tgt.multipleBirth = (n + 1);"
                "src.deceased as d -> tgt.deceased = (d + 1 day);")])
  (check-equal? (run m (patient 'multipleBirthInteger 2 'deceasedDateTime "2020-01-01T10:00:00Z"))
                (hasheq 'resourceType "Patient" 'multipleBirthInteger 3 'deceasedDateTime "2020-01-02T10:00:00Z")))

;; the specification's transforms: all but dateOp ("Parameters to be documented") are run
(check-equal? (transform-names)
              '("append" "c" "cast" "cc" "copy" "cp" "create" "escape" "evaluate" "id" "pointer" "qty" "reference"
                "translate" "truncate" "uuid"))

;; escape(source, fmt1, fmt2): "change source from one kind of escaping to another (plain, java, xml, json)"
(define (escaped text from to)
  (hash-ref (car (hash-ref (run (a-map "Patient" "Patient" (format "src.name as n then { n.text as x -> tgt.name as t, t.text = escape(x, '~a', '~a'); };" from to))
                                (patient 'name (list (hasheq 'text text))))
                           'name))
            'text))
(check-equal? (escaped "say \"hi\" \\ Ω\n" "plain" "json") "say \\\"hi\\\" \\\\ Ω\\n")
(check-equal? (escaped "say \"hi\" \\ Ω\n" "plain" "java") "say \\\"hi\\\" \\\\ \\u03A9\\n")
(check-equal? (escaped "a < b & \"c\"" "plain" "xml") "a &lt; b &amp; &quot;c&quot;")
(check-equal? (escaped "a &lt; b &amp; &#937; &#x3A9;" "xml" "plain") "a < b & Ω Ω")
(check-equal? (escaped "tab\\there \\u03A9 \\uD83D\\uDE00" "json" "plain") "tab\there Ω 😀")
(check-equal? (escaped "a\\nb &amp; c" "json" "xml") "a\nb &amp;amp; c")
(check-equal? (escaped "unchanged" "plain" "plain") "unchanged")
(check-match (failure (lambda () (escaped "x" "plain" "html")))
             (cons (regexp #rx"escape: \"html\" is not a kind of escaping [(]java, json, plain, xml[)]") 6))
(check-equal? (filter (lambda (d) (eq? (diagnostic-severity d) 'error))
                      (check-map (a-map "Patient" "Patient" "src.id as i -> tgt.id = escape(i, 'plain', 'json'), tgt.implicitRules = pointer(src);")))
              '())

;; reference(resource) and pointer(resource): the resource's own id; a resource being built that
;; has none is given one (a random UUID, as the reference implementation gives it), so the
;; reference points at what the map writes — it was `Patient/`
(let* ([m (a-map "Patient" "Bundle"
                 "src -> tgt.entry as e, e.fullUrl = pointer(src), e.resource = create('Patient') as p, e.link as l, l.relation = 'self', l.url = reference(p);"
                 "src -> tgt.entry as e, e.resource = create('Patient') as p, p.id = 'given', e.fullUrl = pointer(p), e.link as l, l.url = reference(src);")]
       [entries (hash-ref (run m (patient 'id "p1")) 'entry)]
       [made (hash-ref (hash-ref (car entries) 'resource) 'id)])
  (check-regexp-match #px"^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$" made)
  (check-equal? (hash-ref (car (hash-ref (car entries) 'link)) 'url) (string-append "Patient/" made))
  (check-equal? (hash-ref (car entries) 'fullUrl) "urn:uuid:p1")
  (check-equal? (hash-ref (cadr entries) 'fullUrl) "urn:uuid:given")
  (check-equal? (hash-ref (car (hash-ref (cadr entries) 'link)) 'url) "Patient/p1"))
;; a source resource with no id has nothing to refer to (it was `Patient/#f`); an element is no resource
(check-match (failure (lambda () (run (a-map "Patient" "Patient" "src -> tgt.implicitRules = reference(src);") (patient))))
             (cons (regexp #rx"reference[(]src[)]: the Patient has no id to refer to") 6))
(check-match (failure (lambda () (run (a-map "Patient" "Patient" "src.name as n -> tgt.implicitRules = pointer(n);") (patient 'name (list (hasheq 'family "Lee"))))))
             (cons (regexp #rx"pointer[(][)] needs a resource, n is not one") 6))

;; cast(source, type?): "Target type can be left as implicit if there is one and only one target type known"
(let ([m (a-map "Patient" "Patient" "src.id as i -> tgt.active = cast(i), tgt.multipleBirth = cast(i, 'integer');")])
  (check-match (failure (lambda () (run m (patient 'id "true")))) (cons (regexp #rx"Failed to cast \"true\" to type \"integer\"") 6)))
(check-equal? (hash-ref (run (a-map "Patient" "Patient" "src.id as i -> tgt.active = cast(i);") (patient 'id "true")) 'active) #t)
(check-match (failure (lambda () (run (a-map "Patient" "Patient" "src.id as i -> tgt.active = cast(i);") (patient 'id "perhaps"))))
             (cons (regexp #rx"Failed to cast \"perhaps\" to type \"boolean\"") 6))
(check-match (failure (lambda () (run (a-map "Patient" "Patient" "src.id as i -> tgt.name = cast(i);") (patient 'id "x"))))
             (cons (regexp #rx"cast[(]i[)]: the target's type is not known here — name it, cast[(]i, 'integer'[)]") 6))

;; cp(value): "If no system is provided, the system should be inferred from the content of the value";
;; id(system, value, type): "type is a code from the identifier type value set"
(let* ([m (a-map "Patient" "Patient"
                 "src.telecom as t then { t.value as v -> tgt.telecom = cp(v); };"
                 "src.id as i -> tgt.identifier = id('http://example.org/mrn', i, 'MR'), tgt.telecom = cp('fax', i);")]
       [out (run m (patient 'id "12" 'telecom (for/list ([v '("ann@example.org" "+1 (555) 010-9999" "https://example.org/ann" "room 3")]) (hasheq 'value v))))])
  (check-equal? (hash-ref out 'telecom)
                (list (hasheq 'system "email" 'value "ann@example.org") (hasheq 'system "phone" 'value "+1 (555) 010-9999")
                      (hasheq 'system "url" 'value "https://example.org/ann") (hasheq 'value "room 3")
                      (hasheq 'system "fax" 'value "12")))
  (check-equal? (hash-ref out 'identifier)
                (list (hasheq 'system "http://example.org/mrn" 'value "12"
                              'type (hasheq 'coding (list (hasheq 'system "http://terminology.hl7.org/CodeSystem/v2-0203" 'code "MR")))))))

;; the target type from `uses … as target` when the parameter names none: the |version is no part
;; of the type's name
(let ([m (parsed (string-append "/// url = 'http://example.org/StructureMap/v'\n/// name = 'V'\n"
                                "uses \"http://hl7.org/fhir/StructureDefinition/Patient|5.0.0\" as source\n"
                                "uses \"http://hl7.org/fhir/StructureDefinition/Patient|5.0.0\" as target\n"
                                "group main(source src, target tgt) { src.gender as g -> tgt.gender = g; }\n"))])
  (check-equal? (run m (patient 'gender "female")) (hasheq 'resourceType "Patient" 'gender "female")))

;; ==== 4. a run that fails is a diagnostic, at its place ===========================================
;; a group called with the wrong number of arguments (a plain `error`: a stack trace at the command line)
(check-match (failure (lambda () (run (a-map "Patient" "Patient" #:after "group names(source n, target t) { n.family as f -> t.family = f; }\n"
                                             "src.name as n -> tgt.name as t then names(n);")
                                      (patient 'name (list (hasheq 'family "Lee"))))))
             (cons (regexp #rx"^Rule \"src.name\": group names takes 2 parameters, called with 1$") 6))
;; a constant defined in terms of itself; a target whose type nothing states
(check-true (exn:fail:diagnostic?
             (with-handlers ([exn:fail? values])
               (run (parsed (string-append (header "Patient" "Patient") "let a = b;\nlet b = a;\n"
                                           "group main(source src : Source, target tgt : Target) { src -> tgt.gender = a; }\n"))
                    (patient)))))
(check-match (failure (lambda () (run (parsed "/// url = 'http://example.org/StructureMap/t'\n/// name = 'T'\ngroup main(source src, target tgt) { src.gender as g -> tgt.gender = g; }\n")
                                      (patient 'gender "female"))))
             (cons (regexp #rx"cannot determine the target type: group main's target parameter has no type") 3))

;; a result checked with nothing recorded about the run says only what it knows (it said "its
;; source matched, but its value was empty" of a rule whose source matched nothing)
(let* ([m (a-map "Patient" "Patient" "src.birthDate as b -> tgt.birthDate = b;")]
       [profile (hasheq 'resourceType "StructureDefinition" 'id "dob" 'url "http://example.org/StructureDefinition/dob"
                        'name "Dob" 'type "Patient" 'baseDefinition "http://hl7.org/fhir/StructureDefinition/Patient"
                        'derivation "constraint"
                        'differential (hasheq 'element (list (hasheq 'id "Patient.birthDate" 'path "Patient.birthDate" 'min 1))))]
       [messages (lambda () (map diagnostic-message (validate-result m (run-map m (patient)) #:profiles (list profile))))])
  (check-match (messages) (list (regexp #rx"[(]the rule here wrote nothing[)]$")))
  (check-match (parameterize ([current-empty-targets (make-hasheq)]) (messages))
               (list (regexp #rx"[(]the rule here wrote nothing: its source matched nothing in the input[)]$"))))
