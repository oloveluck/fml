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
         (only-in cql/error cql-error)
         (only-in fhir/diagnostic exn->diagnostic diagnostic->string))

(provide (contract-out
          ;; (ok fml-map) | (err cql-error) — a parse error carries its diagnostic (positions in `source`)
          [parse-fml (->* (string?) (#:source (or/c string? path? #f)) (or/c ok? err?))]))

(define (parse-fml src #:source [source #f])
  (with-handlers ([exn:fail? (lambda (e)
                               (define d (exn->diagnostic e 'fml #:source source))
                               (err (cql-error 'parse (diagnostic->string d) d)))])
    (ok (lower-doc (g:parse-to-datum (tokenize (open-input-string src)))))))

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
  (define metadata (for/list ([m (in-list (kids d 'metadata))])
                     (match (regexp-match #px"^\\s*(\\w+)\\s*=\\s*\"([^\"]*)\"" (cadr m))
                       [(list _ k v) (cons k v)]
                       [_ (cons "" (cadr m))])))
  (define md (kid d 'map-decl))
  (define-values (url name)
    (if md
        (values (cadr (kid md 'url)) (let ([n (caddr md)]) (if (pair? n) (id-of n) n)))
        (values (assoc-ref metadata "url") (assoc-ref metadata "name"))))
  (fml-map url name metadata
           (map lower-structure (kids d 'structure))
           (for/list ([i (in-list (kids d 'imports))]) (cadr (kid i 'url)))
           (map lower-group (kids d 'fgroup))
           (map lower-conceptmap (kids d 'conceptmap))))

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
                       (vector (system-of s) (code-of sc) (apply string-append (cdr op)) (system-of t) (code-of tc))]))))
(define (assoc-ref al k) (cond [(assoc k al) => cdr] [else #f]))

(define (lower-structure s)
  (fml-use (cadr (kid s 'url))
           (let ([a (kid s 'structure-alias)]) (and a (id-of (cadr a))))
           (string->symbol (cadr (kid s 'model-mode)))))

(define (lower-group g)
  (fml-group (id-of (cadr g))
             (for/list ([p (in-list (kids (kid g 'parameters) 'parameter))])
               (fml-param (string->symbol (cadr (kid p 'input-mode)))
                          (id-of (caddr p))
                          (let ([t (kid p 'param-type)]) (and t (id-of (cadr t))))))
             (let ([e (kid g 'extends-clause)]) (and e (id-of (cadr e))))
             (let ([tm (kid g 'type-mode)]) (if tm (map id-of (cdr tm)) '()))
             (map lower-rule (kids g 'frule))))

(define (lower-rule r)
  (define-values (dependents nested)
    (let ([dep (kid r 'dependent)])
      (cond [(not dep) (values '() '())]
            [else (values (map lower-gcall (kids dep 'gcall))
                          (map lower-rule (kids dep 'frule)))])))
  (fml-rule (map lower-source (kids (kid r 'rule-sources) 'rule-source))
            (let ([ts (kid r 'rule-targets)]) (if ts (map lower-target (kids ts 'rule-target)) '()))
            dependents nested
            (let ([n (kid r 'rule-name)]) (and n (cadr n)))))

(define (lower-source s)
  (define ctx-ids (map id-of (cdr (kid s 'rule-context))))
  (fml-source (car ctx-ids)
              (if (pair? (cdr ctx-ids)) (string-join (cdr ctx-ids) ".") #f)
              (let ([a (kid s 'salias)]) (and a (id-of (cadr a))))
              (let ([w (kid s 'where-clause)]) (and w (fp-of (cadr w))))
              (let ([c (kid s 'check-clause)]) (and c (fp-of (cadr c))))
              (let ([l (kid s 'log-clause)]) (and l (fp-of (cadr l))))
              (let ([m (kid s 'source-list-mode)]) (and m (string->symbol (cadr m))))))

(define (lower-target t)
  (define rc (kid t 'rule-context))
  (define ctx-ids (if rc (map id-of (cdr rc)) '(#f)))
  (define-values (variable share)
    (let ([a (kid t 'salias)] [lm (kid t 'target-list-mode)])
      (values (and a (id-of (cadr a)))
              (and lm (equal? (cadr lm) "share") (id-of (caddr lm))))))
  (fml-target (car ctx-ids)                              ; #f: a bare invocation target
              (if (pair? (cdr ctx-ids)) (string-join (cdr ctx-ids) ".") #f)
              (let ([tr (kid t 'transform)] [fc (kid t 'function-call)])
                (cond [tr (lower-transform tr)] [fc (lower-transform (list 'transform fc))] [else #f]))
              variable share))

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

(define (invoke-arg a)
  (match a
    [(fp-lit* v) #:when (string? v) (fml-literal v)]
    [(fp-inv* #f name) (symbol->string name)]            ; a variable argument
    [ast (fml-fhirpath ast)]))

(define (lower-gcall c)
  (fml-call (id-of (cadr c))
            (let ([al (kid c 'arg-list)])
              (if al (for/list ([e (in-list (cdr al))])
                       (match (lower e)
                         [(fp-inv* #f name) (symbol->string name)]
                         [ast (fml-fhirpath ast)]))
                  '()))))

(module+ test
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
                (fml-var "code")))
