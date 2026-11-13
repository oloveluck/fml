#lang racket/base

;;; print.rkt — the FML AST as text: the mapping language as the specification writes it.
;;;
;;; The other direction of parser/parse.rkt, so a StructureMap from anywhere (a package, a server,
;;; a store) reads as the language (structuremap.rkt reads the resource into the AST). The text is
;;; canonical, not the author's: one declaration and one rule to a line, no comments.
;;;
;;; Only the specified language is written (hl7.org/fhir/mapping-language.html, and its formal
;;; grammar mapping.g4 where the two agree or the text is silent). Where they disagree the TEXT is
;;; followed — it is what the specification's own examples, and every published map, are written in:
;;;   the header      `/// url = '…'` and `/// name = '…'` metadata lines (the g4: `map url = name`)
;;;   a url           "double-quoted"                                   (the g4: `backticked`)
;;;   a rule's name   "double-quoted"                                   (the g4: a bare id)
;;; and always: a FHIRPath expression of `where`, `check`, `log` and `default` in parentheses (the
;;; specification: "MUST be surrounded by a set of parentheses"), a string as a FHIRPath literal
;;; ('single-quoted'), an element name that is no simple name or is one of the grammar's keywords in
;;; backticks.
;;;
;;; Two things a StructureMap can hold have NO syntax in the specified language; a map that has one
;;; is written with the reference implementation's form for it, under a comment that says so:
;;;   an embedded ConceptMap   the R4 language's `conceptmap "name" { … }` block
;;;   a target's listRuleId    `share <id>`
;;;
;;; Laws (tests/print-test.rkt, over the corpora): the text parses; it means the map
;;; (fml->structuremap of the parsed text is fml->structuremap of the map); printed again it is the
;;; same text; and lint.rkt finds nothing in it but those two forms.

(require racket/contract racket/list racket/match racket/string
         "parser/ast.rkt"
         (only-in fhirpath/print fhirpath->string)
         (only-in fhirpath/parser/ast fp-lit fp-lit? fp-lit-value))

(provide
 (contract-out
  ;; fml->text : fml-map -> string
  [fml->text (-> fml-map? string?)]))

;; ---- names and literals ------------------------------------------------------------------------
;; the formal grammar's literal tokens: as an element's name, one of these is written in backticks
(define keywords
  '("map" "uses" "as" "alias" "imports" "let" "group" "extends" "default" "where" "check" "log" "then"
    "true" "false" "types" "first" "not_first" "last" "not_last" "only_one" "share" "single"
    "source" "target" "queried" "produced"))

;; element-name : string -> string   one step of a context or element path
(define (element-name step)
  (if (and (regexp-match? #px"^[A-Za-z_][A-Za-z0-9_]*$" step) (not (member step keywords)))
      step
      (string-append "`" (regexp-replace* #rx"[\\\\`]" step "\\\\&") "`")))
;; element-path : dotted-path -> string
(define (element-path path) (string-join (map element-name (string-split path ".")) "."))

;; quoted : string -> string   a FHIRPath string literal
(define (quoted s) (fhirpath->string (fp-lit s)))
;; url-text, name-text : string -> string   "double-quoted", as the specification's text writes them
(define (double-quoted s) (string-append "\"" (regexp-replace* #rx"[\\\\\"]" s "\\\\&") "\""))

;; expression : fml-fhirpath -> string
(define (expression f) (fhirpath->string (fml-fhirpath-ast f)))
;; literal-expression? : any -> boolean   a FHIRPath literal that is not a string (10, true, @2020)
(define (literal-expression? x)
  (and (fml-fhirpath? x) (fp-lit? (fml-fhirpath-ast x)) (not (string? (fp-lit-value (fml-fhirpath-ast x))))))

;; ---- the map -----------------------------------------------------------------------------------
;; Each part is its lines; the parts are separated by a blank line.
(define (fml->text m)
  (define parts
    (list (metadata-lines m)
          (append-map conceptmap-lines (fml-map-conceptmaps m))
          (map use-line (fml-map-uses m))
          (for/list ([url (in-list (fml-map-imports m))]) (format "imports ~a" (double-quoted url)))
          (for/list ([c (in-list (fml-map-constants m))]) (format "let ~a = ~a;" (car c) (expression (cdr c))))))
  (define blocks (append (filter pair? parts) (map group-lines (fml-map-groups m))))
  (string-append (string-join (map (lambda (lines) (string-join lines "\n")) blocks) "\n\n") "\n"))

;; metadata-lines : fml-map -> (listof string)   the url and the name, then every other element
(define (metadata-lines m)
  (define (line path value)
    (format "/// ~a =~a" path
            (match value
              ['empty ""]
              [(? string? s) (if (regexp-match? #rx"\n" s) (format " \"\"\"\n~a\n\"\"\"" s) (string-append " " (quoted s)))]
              [#t " true"]
              [#f " false"]
              [(? number? n) (format " ~a" n)])))
  (append (if (fml-map-url m) (list (line "url" (fml-map-url m))) '())
          (if (fml-map-name m) (list (line "name" (fml-map-name m))) '())
          (for/list ([md (in-list (fml-map-metadata m))] #:unless (member (car md) '("" "url" "name")))
            (line (car md) (cdr md)))))

(define (use-line u)
  (format "uses ~a~a as ~a" (double-quoted (fml-use-url u))
          (if (fml-use-alias u) (format " alias ~a" (fml-use-alias u)) "")
          (fml-use-mode u)))

;; conceptmap-lines : fml-conceptmap -> (listof string)   no syntax in the specified language: the
;; R4 language's block, each system under a prefix
(define (conceptmap-lines cm)
  (define systems (remove-duplicates (append-map (lambda (m) (list (cm-mapping-source m) (cm-mapping-target m)))
                                                 (fml-conceptmap-mappings cm))))
  ;; the prefixes the map was written with, else s1, s2 … (a StructureMap keeps none)
  (define named (for/hash ([(prefix system) (in-hash (fml-conceptmap-prefixes cm))]) (values system prefix)))
  (define prefix-of
    (for/hash ([system (in-list systems)] [i (in-naturals 1)])
      (values system (hash-ref named system (lambda () (format "s~a" i))))))
  (define (code c) (if (regexp-match? #px"^[A-Za-z_][A-Za-z0-9_]*$" c) c (double-quoted c)))
  (append
   (list "// not in the specified language (an embedded concept map, in the R4 language's form)"
         (format "conceptmap ~a {" (double-quoted (fml-conceptmap-name cm))))
   (for/list ([system (in-list systems)])
     (format "  prefix ~a = ~a" (hash-ref prefix-of system) (double-quoted system)))
   (for/list ([m (in-list (fml-conceptmap-mappings cm))])
     (format "  ~a:~a ~a ~a:~a" (hash-ref prefix-of (cm-mapping-source m)) (code (cm-mapping-code m)) (cm-mapping-op m)
             (hash-ref prefix-of (cm-mapping-target m)) (code (cm-mapping-target-code m))))
   (list "}")))

;; ---- groups and rules --------------------------------------------------------------------------
(define (group-lines g)
  (append
   (list (format "group ~a(~a)~a~a {"
                 (fml-group-name g)
                 (string-join (for/list ([p (in-list (fml-group-params g))])
                                (format "~a ~a~a" (fml-param-mode p) (fml-param-name p)
                                        (if (fml-param-type p) (format " : ~a" (fml-param-type p)) "")))
                              ", ")
                 (if (fml-group-extends g) (format " extends ~a" (fml-group-extends g)) "")
                 (cond [(member "type+" (fml-group-flags g)) " <<type+>>"]
                       [(member "types" (fml-group-flags g)) " <<types>>"]
                       [else ""])))
   (append-map (lambda (r) (rule-lines r "  ")) (fml-group-rules g))
   (list "}")))

;; rule-lines : fml-rule indent -> (listof string)
;;   sources [-> targets] [then invocations [{ rules }] | then { rules }] ["name"];
(define (rule-lines r indent)
  (define head
    (string-append indent
                   (string-join (map source-text (fml-rule-sources r)) ", ")
                   (if (pair? (fml-rule-targets r))
                       (string-append " -> " (string-join (map target-text (fml-rule-targets r)) ", "))
                       "")
                   (if (pair? (fml-rule-dependents r))
                       (string-append " then " (string-join (map call-text (fml-rule-dependents r)) ", "))
                       "")))
  (define name (if (fml-rule-name r) (string-append " " (double-quoted (fml-rule-name r))) ""))
  (cond
    [(null? (fml-rule-nested r)) (list (string-append head name ";"))]
    [else (append (list (string-append head (if (pair? (fml-rule-dependents r)) " {" " then {")))
                  (append-map (lambda (n) (rule-lines n (string-append indent "  "))) (fml-rule-nested r))
                  (list (string-append indent "}" name ";")))]))

;; source-text : fml-source -> string
;;   context.element [: type] [min..max] [default (value)] [list-mode] [as variable]
;;   [where (condition)] [check (condition)] [log (expression)]
(define (source-text s)
  (define (clause keyword f) (and f (format "~a (~a)" keyword (expression f))))
  (string-join
   (filter values
           (list (string-append (element-name (fml-source-context s))
                                (if (fml-source-element s) (string-append "." (element-path (fml-source-element s))) ""))
                 (and (fml-source-type s) (format ": ~a" (fml-source-type s)))
                 (and (or (fml-source-min s) (fml-source-max s))
                      (format "~a..~a" (or (fml-source-min s) 0) (or (fml-source-max s) "*")))
                 (clause "default" (fml-source-default s))
                 (and (fml-source-list-mode s) (symbol->string (fml-source-list-mode s)))
                 (and (fml-source-variable s) (format "as ~a" (fml-source-variable s)))
                 (clause "where" (fml-source-where s))
                 (clause "check" (fml-source-check s))
                 (clause "log" (fml-source-log-expr s))))
   " "))

;; target-text : fml-target -> string
;;   context.element [= transform] [as variable] [list-mode]   |   invocation [as variable]
(define (target-text t)
  (string-join
   (filter values
           (list (cond [(fml-target-context t)
                        (string-append (element-name (fml-target-context t))
                                       (if (fml-target-element t) (string-append "." (element-path (fml-target-element t))) "")
                                       (if (fml-target-transform t) (string-append " = " (transform-text (fml-target-transform t))) ""))]
                       [else (transform-text (fml-target-transform t))])
                 (and (fml-target-variable t) (format "as ~a" (fml-target-variable t)))
                 (and (fml-target-list-mode t) (symbol->string (fml-target-list-mode t)))
                 (match (fml-target-share t)
                   [#f #f]
                   [#t "share"]
                   [id (format "share ~a" id)])))          ; (a listRuleId: no syntax in the specified language)
   " "))

;; transform-text : Transform -> string   a literal, a variable (the copy transform), an
;; invocation, or a FHIRPath expression in parentheses (the short hand for `evaluate`)
(define (transform-text tr)
  (match tr
    [(fml-literal v) (quoted v)]
    [(fml-var name) name]
    [(fml-invoke name args) (format "~a(~a)" name (string-join (map argument-text args) ", "))]
    [(? literal-expression?) (expression tr)]
    [(? fml-fhirpath?) (format "(~a)" (expression tr))]))

;; argument-text : Arg -> string   a variable by its name, a literal, an expression
(define (argument-text a)
  (match a
    [(? string? name) name]
    [(fml-literal v) (quoted v)]
    [(? fml-fhirpath?) (expression a)]))

(define (call-text c)
  (format "~a(~a)" (fml-call-name c) (string-join (map argument-text (fml-call-args c)) ", ")))

(module+ test
  (require rackunit (only-in "parser/parse.rkt" parse-fml) (only-in fhir/result/result ok-value ok?)
           (only-in "structuremap.rkt" fml->structuremap structuremap->fml))
  (define (parsed text) (let ([r (parse-fml text)]) (if (ok? r) (ok-value r) (error 'print-test "~a" r))))
  ;; the specification's own examples, each form once; read (in whichever spelling) and written back
  (define written
    (string-append
     "/// url = 'http://example.org/StructureMap/all'\n"
     "/// name = 'All'\n"
     "/// title = 'Every form'\n"
     "/// status = 'active'\n"
     "/// jurisdiction =\n"
     "/// jurisdiction.coding =\n"
     "/// jurisdiction.coding.code = 'AQ'\n"
     "/// description = \"\"\"\nYou should use my *awesome* map.\nIt does really cool things.\n\"\"\"\n"
     "\n"
     "uses \"http://hl7.org/fhir/3.0/StructureDefinition/CodeSystem\" alias CodeSystemR3 as source\n"
     "uses \"http://hl7.org/fhir/StructureDefinition/CodeSystem\" as target\n"
     "\n"
     "imports \"http://hl7.org/fhir/StructureMap/*3to4\"\n"
     "\n"
     "let name = 'example';\n"
     "\n"
     "group CodeSystem(source src : CodeSystemR3, target tgt : CodeSystem) extends DomainResource <<type+>> {\n"
     "  src.value : integer 0..* default (10) first as vs0 where (value >= 10) check (value <= 100) log (value) -> tgt.value = vs0;\n"
     "  src.value : code as vs0 -> tgt.value = create('code') as vt0 then code(vs0, vt0) \"valueCode\";\n"
     "  src.concept as c -> tgt.concept as t first then concept(c, t), designation(c, t) {\n"
     "    c.code as x -> t.code = x;\n"
     "    c -> t.display = (c.display & name), t.definition = 'text', t.inactive = true share;\n"
     "  } \"concepts\";\n"
     "  src.`not-found` as nf -> tgt.`group` = nf;\n"
     "  src.item as item -> evaluate(item, answer.first()) as ans, tgt.count = 2;\n"
     "  src.type -> tgt.type;\n"
     "}\n"
     "\n"
     "group concept(source c, target t) {\n"
     "  c.property as p then {\n"
     "    p.code as x -> t.code = x;\n"
     "  };\n"
     "}\n"))
  (check-equal? (fml->text (parsed written)) written)
  ;; the other spellings of the same map come out in the one written form
  (define other
    (string-append
     "map \"http://example.org/StructureMap/all\" = \"All\"\n"
     "uses \"http://hl7.org/fhir/StructureDefinition/Patient\" as source\n"
     "group g(source src, target tgt) {\n"
     "  src.item as item where linkId = 'x' check item.exists() -> tgt.a = \"text\" named;\n"   ; bare where, a bare rule name
     "  src -> tgt: type, subtype;\n"                                                             ; the short form
     "}\n"))
  (check-equal? (fml->text (parsed other))
                (string-append
                 "/// url = 'http://example.org/StructureMap/all'\n/// name = 'All'\n\n"
                 "uses \"http://hl7.org/fhir/StructureDefinition/Patient\" as source\n\n"
                 "group g(source src, target tgt) {\n"
                 "  src.item as item where (linkId = 'x') check (item.exists()) -> tgt.a = 'text' \"named\";\n"
                 "  src.type -> tgt.type;\n"
                 "  src.subtype -> tgt.subtype;\n"
                 "}\n"))
  ;; what the specified language has no syntax for is written in the reference implementation's form,
  ;; under a comment (an embedded concept map) — and reads back
  (define with-conceptmap
    (parsed (string-append "map \"http://x/m\" = \"m\"\n"
                           "conceptmap \"cm\" { prefix s = \"http://s\" prefix t = \"http://t\" s:a == t:b s:\"c d\" - t:e }\n"
                           "group g(source src, target tgt) { src.a as a -> tgt.a = translate(a, '#cm', 'code') as x share shared; }\n")))
  (check-regexp-match #rx"// not in the specified language [^\n]*\nconceptmap \"cm\" [{]\n  prefix s = \"http://s\"\n  prefix t = \"http://t\"\n  s:a == t:b\n  s:\"c d\" - t:e\n[}]\n"
                      (fml->text with-conceptmap))
  (check-regexp-match #rx"as x share shared;" (fml->text with-conceptmap))
  (check-equal? (fml->structuremap (parsed (fml->text with-conceptmap))) (fml->structuremap with-conceptmap))
  ;; from a StructureMap: its text means the resource, and the text is a fixpoint
  (define sm (fml->structuremap (parsed written)))
  (define text (fml->text (structuremap->fml sm)))
  (check-equal? (fml->structuremap (parsed text)) sm)
  (check-equal? (fml->text (parsed text)) text))
