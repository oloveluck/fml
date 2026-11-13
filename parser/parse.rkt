#lang racket/base

;;; parse.rkt — the FML parse API: text -> (ok FmlMap) | (err …), via brag.
;;;
;;; string -> tokens (lexer.rkt) -> brag datum (grammar.rkt, the official mapping.g4 ported with
;;; the FHIRPath cascade inlined) -> the prefab FML AST. Embedded FHIRPath subtrees share the
;;; fhirpath grammar's rule names, so fhirpath/parser/ast.rkt's `lower` turns them into REAL
;;; FHIRPath ASTs at parse time — `fml-fhirpath` carries the AST, not text (transform.rkt evaluates
;;; it directly; structuremap.rkt prints it back with fhirpath/print.rkt).

(require racket/contract racket/match racket/string racket/list
         (prefix-in g: "grammar.rkt")
         "lexer.rkt"
         "ast.rkt"
         (only-in fhirpath/parser/ast lower)
         (only-in fhir/result/result ok err ok? err?)
         (only-in "../error.rkt" fml-error)
         (only-in fhirpath/print fhirpath->string)
         (only-in fhir/diagnostic exn->diagnostic diagnostic->string attach-loc! loc diagnostic diagnostic? raise-diagnostic))

(provide (contract-out
          ;; (ok fml-map) | (err fml-error) — a parse error carries its diagnostic (positions in `source`)
          [parse-fml (->* (string?) (#:source (or/c string? path? #f)) (or/c ok? err?))]
          ;; specification-notes : fml-map -> (listof diagnostic)   what the map's text wrote that the
          ;; specified language does not have (warnings, each at its place); none for a map not parsed here
          [specification-notes (-> fml-map? (listof diagnostic?))]))

(define (parse-fml src #:source [source #f])
  (with-handlers ([exn:fail? (lambda (e)
                               (define d (exn->diagnostic e 'fml #:source source))
                               (err (fml-error 'parse (diagnostic->string d) d)))])
    (define-values (d locs) (syntax->datum/locs (g:parse (tokenize (open-input-string src)))))
    (parameterize ([current-locs locs] [current-source (and source (if (path? source) (path->string source) source))])
      (define m (lower-doc d))
      (hash-set! notes m (beyond-specification d src))
      (ok m))))

;; ---- positions ------------------------------------------------------------------
;; The brag tree keeps the tokens' positions; rules, sources and targets remember theirs
;; (fhir/diagnostic attach-loc!, as FSH rules and CQL definitions do), so a check can point at them.
(define current-locs (make-parameter (hasheq)))
(define current-source (make-parameter #f))
(define located-tags '(fgroup frule rule-source rule-target
                       where-clause check-clause target-list-mode conceptmap metadata
                       transform gcall rule-name))
;; syntax->datum/locs : syntax -> (values datum (hasheq datum-node -> (list line col pos span)))
(define (syntax->datum/locs stx)
  (define locs (make-hasheq))
  (define (walk s)
    (define l (syntax->list s))
    (cond [l (define d (map walk l))
             (when (and (pair? d) (memq (car d) located-tags) (syntax-line s))
               (hash-set! locs d (list (syntax-line s) (syntax-column s) (syntax-position s) (syntax-span s))))
             d]
          ;; a token's value, whole: a date literal is a struct, whose fields `syntax-e` would
          ;; leave as syntax (`@2020-01-01` then printed, and evaluated, as a syntax object)
          [else (syntax->datum s)]))
  (define d (walk stx))
  (values d locs))
;; loc-of : datum-node -> loc | #f   where a located node was parsed (1-based line and column)
(define (loc-of d)
  (match (hash-ref (current-locs) d #f)
    [(list line col pos span) (loc (current-source) line (and col (add1 col)) pos span)]
    [#f #f]))
;; located! : ast-node datum-node -> ast-node   the node remembers where its datum was parsed
(define (located! node d)
  (define l (loc-of d))
  (when l (attach-loc! node l))
  node)
;; parse-error : datum-node format args … -> raises   a text the grammar reads and the language does
;; not have, refused at its place (the lowering's own parse errors)
(define (parse-error d fmt . args)
  (raise-diagnostic (diagnostic 'fml 'parse 'error (apply format fmt args) (or (loc-of d) (loc (current-source) #f #f #f #f)))))

;; ---- beyond the specification ------------------------------------------------------
;; Some forms are read that the specified language does not have (grammar.rkt's header says why
;; each is read). A map that writes one is told so, at the form: print.rkt writes none of them but
;; `share <id>` and the `conceptmap` block, and those only for a StructureMap that holds what they say.
(define notes (make-weak-hasheq))
(define (specification-notes m) (hash-ref notes m '()))

;; beyond-specification : datum source-text -> (listof diagnostic)
(define (beyond-specification d src)
  (define (note node fmt . args)
    (diagnostic 'fml 'check 'warning (apply format fmt args) (or (loc-of node) (loc (current-source) #f #f #f #f))))
  ;; the text a located node was parsed from
  (define (text-of node)
    (match (hash-ref (current-locs) node #f)
      [(list _ _ (? number? pos) (? number? span))
       #:when (<= (+ (sub1 pos) span) (string-length src))
       (substring src (sub1 pos) (+ (sub1 pos) span))]
      [_ #f]))
  (define (clause-notes node keyword)
    (define text (text-of node))
    (define expression (and text (string-trim (substring text (min (string-length text) (string-length keyword))))))
    (if (and expression (not (parenthesized? expression)))
        (list (note node "`~a ~a`: the specification writes the expression in parentheses — `~a (~a)`"
                    keyword expression keyword expression))
        '()))
  (define (walk x)
    (match x
      [(cons 'rule-source parts)
       (define tags (map (lambda (p) (and (pair? p) (car p))) parts))
       (define alias-after-where (let ([w (memq 'where-clause tags)]) (and w (memq 'salias w))))
       (append (if alias-after-where
                   (list (note (kid x 'where-clause) "`where` before `as ~a`: the specification names the variable first — `as ~a where (…)`"
                               (id-of (cadr (last (kids x 'salias)))) (id-of (cadr (last (kids x 'salias))))))
                   '())
               (append-map walk parts))]
      [(cons 'where-clause _) (clause-notes x "where")]
      [(cons 'check-clause _) (clause-notes x "check")]
      [(list 'target-list-mode "share" (list 'share-id id))
       (list (note x "`share ~a`: the specified language has no syntax for the rule that shares (StructureMap's listRuleId); it has `share` alone" id))]
      [(cons 'conceptmap _)
       (list (note x "an embedded `conceptmap` is not in the specified language (the R4 language's block, read as the reference implementation reads it); a ConceptMap resource and `translate(v, '<its url>', 'code')` say the same"))]
      [(list 'metadata raw)
       (if (equal? (car (lower-metadata x)) "")
           (list (note x "`///~a` states no element: the specification's metadata is `/// <element> = <value>` (a comment is `//`)" raw))
           '())]
      ;; a transform is a literal, a variable (a path of names), an invocation, or an expression in
      ;; parentheses (the specification's short hand for `evaluate`); a "double-quoted" one is a literal
      [(list 'transform (? pair? expression))
       (define text (text-of x))
       (if (or (plain-value? (lower expression)) (not text) (parenthesized? (string-trim text)))
           '()
           (list (note x "`= ~a`: the specification writes an expression in parentheses — `= (~a)`" (string-trim text) (string-trim text))))]
      ;; an invocation's parameter is a variable or a literal: nothing else has a place in a StructureMap
      [(cons 'gcall _)
       (define arguments (let ([al (kid x 'arg-list)]) (if al (cdr al) '())))
       (for/list ([e (in-list arguments)] #:unless (plain-argument? (lower e)))
         (define text (fhirpath->string (lower e)))
         (note x "`~a(… ~a …)`: a group is given variables and literals; as a StructureMap this argument is the text of the expression, not its value — name it first (`… as v`) and pass `v`"
               (id-of (cadr x)) text))]
      [(list 'rule-name _)
       (define text (text-of x))
       (if (and text (regexp-match? #rx"^'" text))
           (list (note x "~a: the specification writes a rule's name in double quotes — ~s" text (cadr x)))
           '())]
      [(cons (or 'fp-expr 'function-call) _) '()]                        ; no FML form inside an expression
      [(cons (? symbol?) parts) (append-map walk (filter pair? parts))]
      [_ '()]))
  (walk d))

;; parenthesized? : string -> boolean   one pair of parentheses holds the whole text
;; ("(a) or (b)" is not; a parenthesis inside a 'string' or "string" is text)
(define (parenthesized? text)
  (and (regexp-match? #rx"^[(]" text)
       (let loop ([i 0] [depth 0] [in-string #f])
         (cond
           [(= i (string-length text)) #f]
           [else
            (define c (string-ref text i))
            (cond
              [in-string (cond [(char=? c #\\) (loop (+ i 2) depth in-string)]
                           [(char=? c in-string) (loop (add1 i) depth #f)]
                           [else (loop (add1 i) depth in-string)])]
              [(memv c '(#\' #\")) (loop (add1 i) depth c)]
              [(char=? c #\() (loop (add1 i) (add1 depth) #f)]
              [(char=? c #\)) (if (= depth 1) (= i (sub1 (string-length text))) (loop (add1 i) (sub1 depth) #f))]
              [else (loop (add1 i) depth #f)])]))))

;; ---- helpers over brag datum trees ------------------------------------------
(define ((tagged? tag) x) (and (pair? x) (eq? (car x) tag)))
(define (kids d tag) (filter (tagged? tag) (cdr d)))
(define (kid d tag) (let ([k (kids d tag)]) (and (pair? k) (car k))))
(define (id-of x)      ; (identifier IDENTIFIER|"as"|…) -> string
  (match x
    [(list 'identifier a) (if (symbol? a) (symbol->string a) a)]
    [(? symbol? s) (symbol->string s)]
    [(? string? s) s]))
(define (fp-of x)      ; (fp-expr …) subtree -> a FHIRPath AST via fhirpath's own lowering
  (fml-fhirpath (lower x)))

;; ---- lowering ---------------------------------------------------------------
(define (lower-doc d)
  (define metadata (map lower-metadata (kids d 'metadata)))
  (define md (kid d 'map-decl))
  (define (stated key) (let ([v (assoc-ref metadata key)]) (and (string? v) v)))
  ;; the map's url and name: the `map` declaration (the formal grammar), else the metadata (the text)
  (define-values (url name)
    (if md
        (values (cadr (kid md 'url)) (let ([n (caddr md)]) (if (pair? n) (id-of n) n)))
        (values (stated "url") (stated "name"))))
  (fml-map url name metadata
           (map lower-structure (kids d 'structure))
           (for/list ([i (in-list (kids d 'imports))]) (cadr (kid i 'url)))
           (for/list ([c (in-list (kids d 'const))]) (cons (id-of (cadr c)) (fp-of (caddr c))))
           (map lower-group (kids d 'fgroup))
           (map lower-conceptmap (kids d 'conceptmap))))

;; lower-metadata : (metadata raw) -> Metadata   `name{.property}* = value`: the value a FHIRPath
;; literal ('text', true, 12), markdown between """ … """ (several lines), or nothing — a complex
;; element begun. ("double-quoted" text is read too.) A line that is none of these keeps its text
;; under the path "" (reported by `specification-notes`).
(define (lower-metadata m)
  (define raw (cadr m))
  (match (regexp-match #px"^\\s*([A-Za-z][A-Za-z0-9]*(?:[.][A-Za-z][A-Za-z0-9]*)*)\\s*=\\s*(.*)$" (regexp-replace #px"\\s+$" raw ""))
    [(list _ path value)
     (cons path
           (match value
             ["" 'empty]
             [(pregexp #px"^(?s:\"\"\"(.*)\"\"\")$" (list _ text)) (string-trim text "\n" #:repeat? #f)]
             [(pregexp #px"^'((?:[^'\\\\]|\\\\.)*)'$" (list _ text)) (unescape-literal text)]
             [(pregexp #px"^\"((?:[^\"\\\\]|\\\\.)*)\"$" (list _ text)) (unescape-literal text)]
             ["true" #t]
             ["false" #f]
             [(pregexp #px"^-?[0-9]+(?:[.][0-9]+)?$") (string->number value)]
             [_ value]))]
    [_ (cons "" raw)]))
(define (unescape-literal s)
  (regexp-replace* #px"\\\\(.)" s (lambda (_ c) (case c [("n") "\n"] [("t") "\t"] [("r") "\r"] [else c]))))

;; an embedded concept map: its prefixes resolve the systems of each mapping
(define (lower-conceptmap c)
  (define prefixes
    (for/hash ([p (in-list (kids c 'cm-prefix))]) (values (id-of (cadr p)) (caddr p))))
  (define (code-of cc) (let ([x (cadr cc)]) (if (string? x) x (id-of x))))
  (define (system-of id) (hash-ref prefixes (id-of id) (id-of id)))
  (fml-conceptmap (cadr c) prefixes
                  (for/list ([m (in-list (kids c 'cm-mapping))])
                    (match (cdr m)
                      [(list s sc op t tc)
                       (cm-mapping (system-of s) (code-of sc) (apply string-append (cdr op)) (system-of t) (code-of tc))]))))
(define (assoc-ref al k) (cond [(assoc k al) => cdr] [else #f]))

(define (lower-structure s)
  (fml-use (cadr (kid s 'url))
           (let ([a (kid s 'structure-alias)]) (and a (id-of (cadr a))))
           (string->symbol (cadr (kid s 'model-mode)))))

(define (lower-group g) (located! (lower-group* g) g))
(define (lower-group* g)
  (fml-group (id-of (cadr g))
             (for/list ([p (in-list (kids (kid g 'parameters) 'parameter))])
               (fml-param (string->symbol (cadr (kid p 'input-mode)))
                          (id-of (caddr p))
                          (let ([t (kid p 'param-type)]) (and t (id-of (cadr t))))))
             (let ([e (kid g 'extends-clause)]) (and e (id-of (cadr e))))
             (match (kid g 'type-mode)                  ; the flags as written: `types` | `type+`
               [(list _ "types") '("types")]
               [(list _ "type" "+") '("type+")]
               [#f '()])
             (append* (for/list ([r (in-list (cdr g))] #:when (or ((tagged? 'frule) r) ((tagged? 'short-rule) r)))
                        (if ((tagged? 'frule) r) (list (lower-rule r)) (lower-short-rule r))))))

;; lower-short-rule : `src -> tgt: a, b;` -> the identity rules `src.a -> tgt.a; src.b -> tgt.b;`
(define (lower-short-rule r)
  (match (cdr r)
    [(list (list 'rule-context source-ids ...) (list 'rule-context target-ids ...) elements ...)
     (define (path context element) (string-join (append (cdr context) (list element)) "."))
     (define source (map id-of source-ids))
     (define target (map id-of target-ids))
     (for/list ([e (in-list (map id-of elements))])
       (fml-rule (list (fml-source (car source) (path source e) #f #f #f #f #f #f #f #f #f))
                 (list (fml-target (car target) (path target e) #f #f #f #f))
                 '() '() #f))]))

(define (lower-rule r) (located! (lower-rule* r) r))
(define (lower-rule* r)
  (define-values (dependents nested)
    (let ([dep (kid r 'dependent)])
      (cond [(not dep) (values '() '())]
            [else (values (map lower-gcall (kids dep 'gcall))
                          (let ([block (kid dep 'rule-block)]) (if block (map lower-rule (kids block 'frule)) '())))])))
  (fml-rule (map lower-source (kids (kid r 'rule-sources) 'rule-source))
            (let ([ts (kid r 'rule-targets)]) (if ts (map lower-target (kids ts 'rule-target)) '()))
            dependents nested
            (let ([n (kid r 'rule-name)]) (and n (let ([v (cadr n)]) (if (symbol? v) (symbol->string v) v))))))

(define (lower-source s)
  (define ctx-ids (map id-of (cdr (kid s 'rule-context))))
  (located! (lower-source* s ctx-ids) s))
(define (lower-source* s ctx-ids)
  (define cardinality (kid s 'source-cardinality))
  ;; `min..max`: whole numbers (the lexer's NUMBER is also a decimal), max a number or `*`
  (when cardinality
    (for ([bound (in-list (list (cadr cardinality) (cadr (caddr cardinality))))]
          #:unless (regexp-match? #px"^(?:[0-9]+|[*])$" bound))
      (parse-error s "a cardinality is whole numbers, `0..1` or `1..*`: `~a` is not one" bound)))
  (fml-source (car ctx-ids)
              (if (pair? (cdr ctx-ids)) (string-join (cdr ctx-ids) ".") #f)
              (let ([t (kid s 'source-type)]) (and t (id-of (cadr t))))
              (and cardinality (string->number (cadr cardinality)))
              (and cardinality (cadr (caddr cardinality)))
              (let ([d (kid s 'source-default)]) (and d (fp-of (cadr d))))
              (let ([m (kid s 'source-list-mode)]) (and m (string->symbol (cadr m))))
              (let ([a (kid s 'salias)]) (and a (id-of (cadr a))))
              (let ([w (kid s 'where-clause)]) (and w (fp-of (cadr w))))
              (let ([c (kid s 'check-clause)]) (and c (fp-of (cadr c))))
              (let ([l (kid s 'log-clause)]) (and l (fp-of (cadr l))))))

(define (lower-target t) (located! (lower-target* t) t))
(define (lower-target* t)
  (define rc (kid t 'rule-context))
  (define ctx-ids (if rc (map id-of (cdr rc)) '(#f)))
  (define-values (variable share mode)
    (let ([a (kid t 'salias)] [lm (kid t 'target-list-mode)])
      ;; `share` alone is the list mode; `share <id>` names the rule that shares (listRuleId)
      (values (and a (id-of (cadr a)))
              (and lm (equal? (cadr lm) "share")
                   (match (cddr lm) [(list (list 'share-id id)) (symbol->string id)] [_ #t]))
              (and lm (member (cadr lm) '("first" "last" "single")) (string->symbol (cadr lm))))))
  (fml-target (car ctx-ids)                              ; #f: a bare invocation target
              (if (pair? (cdr ctx-ids)) (string-join (cdr ctx-ids) ".") #f)
              (let ([tr (kid t 'transform)] [fc (kid t 'function-call)])
                (cond [tr (lower-transform tr)] [fc (lower-transform (list 'transform fc))] [else #f]))
              variable share mode))

;; a transform is a lowered FHIRPath — recognise the special shapes on the AST:
;; a bare string literal, a bare variable reference, or a named invocation (create/reference/…)
(define (lower-transform tr)
  (match (cadr tr)
    [(? string? delim) (fml-literal delim)]              ; DELIM double-quoted literal
    [fp-datum
     (define ast (lower fp-datum))
     (match ast
       [(fp-lit* v) #:when (string? v) (fml-literal v)]
       [(fp-inv* #f name) (fml-var (symbol->string name))]
       [(fp-callv #f 'copy (list (fp-inv* #f name))) (fml-var (symbol->string name))]   ; copy(v) = v
       [(fp-callv #f name args) (fml-invoke (symbol->string name) (map invoke-arg args))]
       [_ (fml-fhirpath ast)])]))

;; pattern helpers over fhirpath's prefab structs (avoid a hard require cycle on struct names)
(require (only-in fhirpath/parser/ast
                  fp-lit fp-lit? fp-lit-value fp-invoke fp-invoke? fp-invoke-target fp-invoke-name
                  fp-call fp-call? fp-call-target fp-call-name fp-call-args))
(define-match-expander fp-lit*
  (syntax-rules () [(_ v) (? fp-lit? (app fp-lit-value v))]))
(define-match-expander fp-inv*
  (syntax-rules () [(_ t n) (? fp-invoke? (and (app fp-invoke-target t) (app fp-invoke-name n)))]))
(define-match-expander fp-callv
  (syntax-rules () [(_ t n a) (? fp-call? (and (app fp-call-target t) (app fp-call-name n) (app fp-call-args a)))]))

;; plain-value? : fhirpath-ast -> boolean   what a transform may be without parentheses: a literal,
;; a variable or a path of names, an invocation
(define (plain-value? ast)
  (match ast
    [(? fp-lit?) #t]
    [(fp-inv* #f _) #t]
    [(fp-inv* target _) (plain-value? target)]
    [(fp-callv #f _ _) #t]
    [_ #f]))
;; plain-argument? : fhirpath-ast -> boolean   a group invocation's parameter: a variable or a literal
(define (plain-argument? ast)
  (match ast
    [(? fp-lit?) #t]
    [(fp-inv* #f _) #t]
    [_ #f]))

;; invoke-arg : fhirpath-ast -> Arg   a variable by its name, a string literal, else the expression
(define (invoke-arg a)
  (match a
    [(fp-lit* v) #:when (string? v) (fml-literal v)]
    [(fp-inv* #f name) (symbol->string name)]            ; a variable argument
    [ast (fml-fhirpath ast)]))

;; lower-gcall : (gcall name arg-list?) -> fml-call   its arguments as an invocation's: a variable
;; by name, a string literal, else the expression
(define (lower-gcall c)
  (fml-call (id-of (cadr c))
            (let ([al (kid c 'arg-list)])
              (if al (map (lambda (e) (invoke-arg (lower e))) (cdr al)) '()))))

(module+ test
  (require (only-in fhir/diagnostic attached-loc loc-line loc-column loc-source diagnostic-loc diagnostic-severity diagnostic-message))
  (require rackunit (only-in fhir/result/result ok-value)
           (only-in fhirpath/parser/ast fp-binop fp-invoke fp-lit))
  (define m (ok-value (parse-fml #<<EOF
map "http://x" = "demo"
uses "http://hl7.org/fhir/StructureDefinition/QuestionnaireResponse" alias QR as source
uses "http://hl7.org/fhir/StructureDefinition/Patient" alias Patient as target

group entry(source src : QR, target tgt : Patient) {
  src -> tgt.gender = 'female' "Simple";
  src.item as item where linkId.value in ('patient.sex') -> tgt.gender = (item.answer.valueString);
  src.item as item then item(item, tgt);
  src.rest as r -> tgt.rest as t then {
    r.code as code -> t.code = code;
  } "nested";
}
EOF
)))
  (check-equal? (fml-map-url m) "http://x")
  (check-equal? (length (fml-map-uses m)) 2)
  (define g (car (fml-map-groups m)))
  (check-equal? (fml-group-name g) "entry")
  (check-equal? (map fml-param-mode (fml-group-params g)) '(source target))
  (check-equal? (length (fml-group-rules g)) 4)
  ;; transforms lower to REAL shapes: a literal, a fhirpath AST, a var
  (check-equal? (fml-target-transform (car (fml-rule-targets (car (fml-group-rules g)))))
                (fml-literal "female"))
  (define r2 (cadr (fml-group-rules g)))
  (check-true (fml-fhirpath? (fml-source-where (car (fml-rule-sources r2)))))
  (check-true (fml-fhirpath? (fml-target-transform (car (fml-rule-targets r2)))))
  (define r4 (cadddr (fml-group-rules g)))
  (check-equal? (length (fml-rule-nested r4)) 1)
  (check-equal? (fml-target-transform (car (fml-rule-targets (car (fml-rule-nested r4)))))
                (fml-var "code"))
  ;; rules, sources and targets remember their positions (1-based line and column)
  (define l2 (attached-loc r2))
  (check-equal? (list (loc-line l2) (loc-column l2)) '(7 3))
  (check-equal? (loc-line (attached-loc (car (fml-rule-sources r2)))) 7)
  (check-true (> (loc-column (attached-loc (car (fml-rule-targets r2)))) 50))
  (check-equal? (loc-line (attached-loc (car (fml-rule-nested r4)))) 10)
  ;; what the specified language does not have is said, at the form (the map above: one bare where)
  (define (notes-of text) (for/list ([n (specification-notes (ok-value (parse-fml text)))])
                            (list (loc-line (diagnostic-loc n)) (diagnostic-severity n) (diagnostic-message n))))
  (check-match (specification-notes m)
               (list (app diagnostic-message
                          (regexp #rx"^`where linkId.value in [(]'patient.sex'[)]`: the specification writes the expression in parentheses — `where [(]linkId.value in [(]'patient.sex'[)][)]`$"))))
  (check-equal? (loc-line (diagnostic-loc (car (specification-notes m)))) 7)
  (check-match (notes-of (string-append "map \"http://x\" = \"m\"\n/// just words\n"
                                        "conceptmap \"cm\" { prefix s = \"http://s\" prefix t = \"http://t\" s:a == t:b }\n"
                                        "group g(source src, target tgt) {\n"
                                        "  src.a where (x = ')') or (y = 2) as a check a.exists() -> tgt.a = a share shared;\n"
                                        "}\n"))
               (list (list 2 'warning (regexp #rx"states no element"))
                     (list 3 'warning (regexp #rx"embedded `conceptmap` is not in the specified language"))
                     (list 5 'warning (regexp #rx"`where` before `as a`"))
                     (list 5 'warning (regexp #rx"^`where [(]x = '[)]'[)] or [(]y = 2[)]`"))
                     (list 5 'warning (regexp #rx"^`check a.exists[(][)]`"))
                     (list 5 'warning (regexp #rx"^`share shared`"))))
  ;; the specified forms, in either of the specification's spellings, are not remarked on
  (check-equal? (notes-of (string-append "/// url = 'http://x'\n/// name = 'm'\n/// title = \"\"\"\nA\nB\n\"\"\"\n"
                                         "uses \"http://hl7.org/fhir/StructureDefinition/Patient\" as source\n"
                                         "let k = 'v';\n"
                                         "group g(source src, target tgt) {\n"
                                         "  src.a : string 0..1 default ('x') first as a where ((a = '(') or true) check (a.exists()) log (a) -> tgt.a = a share \"named\";\n"
                                         "  src -> tgt: b, c;\n"
                                         "}\n"))
                '()))
