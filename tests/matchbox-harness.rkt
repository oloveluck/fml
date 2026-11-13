#lang racket/base

;;; matchbox-harness.rkt — the second FML corpus: the maps the matchbox engine (ahdis) tests itself
;;; with, vendored under tests/vendor/matchbox (see its SOURCE.txt). Three kinds of check:
;;;
;;;   parse     every .map in the corpus parses to a StructureMap (top level + the tutorial's)
;;;   tutorial  the FHIR mapping-language TUTORIAL (step1 .. step13): each step's logical models
;;;             (logical/*.json) are the data source the map's `uses` resolve in; each
;;;             result/<map>.<source>.json is the map run over source/<source>.json; an expected
;;;             OperationOutcome means the run must FAIL (a `check` that does not hold)
;;;   runs      the top-level maps over the sources that fit them run without an error (no
;;;             expected output in the corpus: the check is that the engine takes them)

(require racket/contract racket/file racket/list racket/string racket/path json
         (only-in fhir/r5/schemas)                       ; the tower the sources parse into
         (only-in fhir/data-source current-data-source resources->data-source)
         (only-in "../parser/parse.rkt" parse-fml specification-notes)
         (only-in "../print.rkt" fml->text)
         (only-in fhir/diagnostic diagnostic-message)
         (only-in "../parser/ast.rkt" fml-map fml-map? fml-map-url fml-map-conceptmaps fml-conceptmap fml-conceptmap-prefixes)
         (only-in "../structuremap.rkt" fml->structuremap structuremap->fml)
         (only-in "../transform.rkt" run-map uses-types resolve-structure-map)
         (only-in "../check.rkt" check-map)
         (only-in fhir/diagnostic diagnostic-severity diagnostic->string)
         (only-in fhirpath/parser/parse parse-fhirpath)
         (only-in fhirpath/print fhirpath->string)
         (only-in fhir/serialize/from-json from-json)
         (only-in fhir/result/result ok? ok-value err-error))

;; A Tally is (tally passed total failures) — failures: (listof (list name reason))
;; An Outcome is 'pass | 'skip (not counted) | a string saying why the case failed
(struct tally (passed total failures) #:transparent)
(define empty-tally (tally 0 0 '()))
(define (tally-add t name outcome [weight 1])
  (case outcome
    [(skip) t]
    [(pass) (tally (+ (tally-passed t) weight) (+ (tally-total t) weight) (tally-failures t))]
    [else (tally (tally-passed t) (+ (tally-total t) weight) (append (tally-failures t) (list (list name outcome))))]))
;; run-cases : cases (case -> name) (case -> Outcome) -> tally   every case, an exception its reason
(define (run-cases cases name-of outcome-of)
  (for/fold ([t empty-tally]) ([c (in-list cases)])
    (tally-add t (name-of c)
               (with-handlers ([exn:fail? (lambda (e) (format "error: ~a" (exn-message e)))]) (outcome-of c)))))
;; the corpus-relative name of a map file
(define ((relative-to dir) f) (path->string (find-relative-path (simple-form-path dir) (simple-form-path f))))

(provide (contract-out
          [struct tally ([passed exact-nonnegative-integer?] [total exact-nonnegative-integer?]
                         [failures list?])]
          ;; result files the reference's own tests do not use as expectations (with the evidence)
          [reference-overrides (listof (list/c string? (or/c 'skip 'error) string?))]
          ;; parse-corpus : dir -> tally   every .map under dir (recursively) parses
          [parse-corpus (-> path-string? tally?)]
          ;; run-tutorial : matchbox-dir -> tally   the tutorial steps' expected results
          [run-tutorial (-> path-string? tally?)]
          ;; run-smoke : matchbox-dir -> tally   the top-level maps run over their sources
          [run-smoke (-> path-string? tally?)]
          ;; roundtrip-corpus : dir -> tally   every .map: parse, emit the StructureMap, read it back,
          ;; same AST (up to the FML-only surface)
          [roundtrip-corpus (-> path-string? tally?)]
          ;; run-tutorial/structuremap : matchbox-dir -> tally   the tutorial again, each map taken
          ;; from the reference's StructureMap resource (map/<step>.json) by canonical — no FML read
          ;; (several of those resources predate their .map: stale conditions, a missing rule)
          [run-tutorial/structuremap (-> path-string? tally?)]
          ;; run-tutorial/own-structuremap : matchbox-dir -> tally   the tutorial from OUR emitted
          ;; StructureMap resources, resolved by canonical: the resource is the executable form
          [run-tutorial/own-structuremap (-> path-string? tally?)]
          ;; check-corpus : dir -> tally   every map checked against its structures: a map passes when
          ;; the check reports no error (the tutorial's within its step's logical models)
          [check-corpus (-> path-string? tally?)]
          ;; print-corpus : dir -> tally   every .map as print.rkt writes it: the text parses, means the
          ;; map (the same StructureMap), is written the same again, and holds nothing the specified
          ;; language lacks but a `conceptmap` block or a `share <id>` (what the map itself holds)
          [print-corpus (-> path-string? tally?)]
          ;; print-structuremaps : dir -> tally   the same for every StructureMap RESOURCE under dir
          ;; (the reference's, R4-flavoured): resource -> text -> resource
          [print-structuremaps (-> path-string? tally?)]
          ;; compare-reference-structuremaps : matchbox-dir -> tally   our StructureMap for each
          ;; tutorial map against the reference's, rule by rule (metadata and R4-only fields dropped,
          ;; FHIRPath text normalised through parse and print)
          [compare-reference-structuremaps (-> path-string? tally?)]))

;; ---- parse ------------------------------------------------------------------
(define (map-files dir)
  (sort (for/list ([f (in-directory dir)] #:when (regexp-match? #px"\\.map$" (path->string f))) f)
        string<? #:key path->string))

;; parse-map-file : file [source-name] -> (ok fml-map) | (err …)   each corpus file parsed once per
;; name its positions carry: nine checks read the same maps, and parsing was 82% of the whole run
;; (a parsed map is not changed by running, checking or printing it)
(define parsed-files (make-hash))
(define (parse-map-file f [source #f])
  (hash-ref! parsed-files (cons (path->string (simple-form-path f)) source)
             (lambda () (parse-fml (file->string f) #:source source))))

(define (parse-corpus dir)
  (run-cases (map-files dir) (relative-to dir)
             (lambda (f) (let ([r (parse-map-file f)]) (if (ok? r) 'pass (format "parse: ~a" (err-error r)))))))

;; ---- tutorial ---------------------------------------------------------------
(define (read-json-file f) (call-with-input-file f read-json))

(define (run-loaded m source-file)
  (uses-types m)                                   ; registers the step's logical models
  (define sr (from-json (read-json-file source-file) #:lenient? #t))
  (unless (ok? sr) (error 'source "~a does not parse as its type" source-file))
  (run-map m (ok-value sr)))

;; ---- the StructureMap as the interchange form --------------------------------------
(define (ast-key m)
  (struct-copy fml-map m [metadata '()]
               [conceptmaps (for/list ([c (fml-map-conceptmaps m)]) (struct-copy fml-conceptmap c [prefixes (hash)]))]))

(define (roundtrip-corpus dir)
  (run-cases (map-files dir) (relative-to dir)
             (lambda (f)
               (define r (parse-map-file f))
               (cond [(not (ok? r)) 'skip]                                   ; parse failures are counted elsewhere
                     [(equal? (ast-key (structuremap->fml (fml->structuremap (ok-value r)))) (ast-key (ok-value r))) 'pass]
                     [else "read back differs from the parsed map"]))))

;; ---- the map as text (print.rkt) ------------------------------------------------------
;; printed-outcome : fml-map -> Outcome   the laws of print.rkt, for one map
(define (printed-outcome m)
  (define text (fml->text m))
  (define r (parse-fml text))
  (cond
    [(not (ok? r)) (format "the printed text does not parse: ~a" (err-error r))]
    [(not (equal? (fml->structuremap (ok-value r)) (fml->structuremap m))) "the printed text is another map"]
    [(not (equal? (fml->text (ok-value r)) text)) "printed again, the text differs"]
    [else
     (define beyond
       (for/list ([n (in-list (specification-notes (ok-value r)))]
                  #:unless (regexp-match? #rx"^(`share |an embedded `conceptmap`)" (diagnostic-message n)))
         (diagnostic-message n)))
     (if (null? beyond) 'pass (format "the printed text is not the specified language: ~a" (car beyond)))]))

(define (print-corpus dir)
  (run-cases (map-files dir) (relative-to dir)
             (lambda (f)
               (define r (parse-map-file f))
               (if (ok? r) (printed-outcome (ok-value r)) 'skip))))       ; parse failures are counted elsewhere

(define (print-structuremaps dir)
  (define resources
    (for*/list ([f (in-list (sort (for/list ([f (in-directory dir)] #:when (regexp-match? #px"\\.json$" (path->string f))) f)
                                  string<? #:key path->string))]
                [j (in-value (with-handlers ([exn:fail? (lambda (_) #f)]) (read-json-file f)))]
                #:when (and (hash? j) (equal? (hash-ref j 'resourceType #f) "StructureMap")))
      (cons f j)))
  (run-cases resources (lambda (r) ((relative-to dir) (car r)))
             (lambda (r) (printed-outcome (structuremap->fml (cdr r))))))

;; the logical models beside a tutorial map (map/x.map -> ../logical/*.json), none elsewhere
(define (logical-models-beside f)
  (define d (build-path (path-only f) 'up "logical"))
  (if (directory-exists? d)
      (for/list ([g (directory-list d #:build? #t)] #:when (regexp-match? #px"\\.json$" (path->string g))) (read-json-file g))
      '()))

(define (check-corpus dir)
  (run-cases (map-files dir) (relative-to dir)
             (lambda (f)
               (define name ((relative-to dir) f))
               (define r (parse-map-file f name))
               (cond
                 [(not (ok? r)) 'skip]
                 [else
                  (define errors
                    (parameterize ([current-data-source (resources->data-source (logical-models-beside f))])
                      (for/list ([d (in-list (check-map (ok-value r) #:source name))] #:when (eq? (diagnostic-severity d) 'error))
                        (diagnostic->string d))))
                  (if (null? errors) 'pass (string-join errors "\n    "))]))))

;; the reference's StructureMap (matchbox, R4-flavoured) and ours, comparable: ids/metadata and
;; R4-only fields dropped, unnamed rules unnamed on both sides, FHIRPath text re-printed
(define (normalise-sm sm)
  (define (fp s) (if (string? s) (with-handlers ([exn:fail? (lambda (_) s)]) (fhirpath->string (parse-fhirpath s))) s))
  (define (param p)
    (for/hasheq ([(k v) (in-hash p)]) (values k (if (eq? k 'valueString) (fp v) v))))
  (define (target t)
    (for/hasheq ([(k v) (in-hash t)] #:unless (memq k '(contextType)))
      (values k (cond [(eq? k 'parameter) (map param v)] [else v]))))
  (define (source s)
    (for/hasheq ([(k v) (in-hash s)]) (values k (if (memq k '(condition check logMessage)) (fp v) v))))
  (define (dependent d)                                        ; R4 wrote `variable`, R5 `parameter`
    (for/hasheq ([(k v) (in-hash d)])
      (if (eq? k 'variable) (values 'parameter (map (lambda (id) (hasheq 'valueId id)) v)) (values k v))))
  (define (rule r)
    (for/hasheq ([(k v) (in-hash r)] #:unless (memq k '(name documentation)))
      (values k (case k [(source) (map source v)] [(target) (map target v)] [(rule) (map rule v)] [(dependent) (map dependent v)] [else v]))))
  (define (group g)
    (for/hasheq ([(k v) (in-hash g)] #:unless (or (memq k '(documentation)) (and (eq? k 'typeMode) (equal? v "none"))))
      (values k (if (eq? k 'rule) (map rule v) v))))
  (for/hasheq ([(k v) (in-hash sm)] #:when (memq k '(structure import group)))
    (values k (if (eq? k 'group) (map group v) v))))

(define (compare-reference-structuremaps dir)
  (define pairs (for/list ([f (in-list (map-files (build-path dir "tutorial")))]
                           #:when (file-exists? (path-replace-extension f #".json")))
                  f))
  ;; a case per RULE: ours against theirs, in order (a rule one side lacks counts as a failure)
  (define (rules sm) (append-map (lambda (g) (hash-ref g 'rule '())) (hash-ref sm 'group '())))
  (define cases
    (for*/list ([f (in-list pairs)]
                [r (in-value (parse-map-file f))] #:when (ok? r)
                [ours (in-value (rules (normalise-sm (fml->structuremap (ok-value r)))))]
                [theirs (in-value (rules (normalise-sm (read-json-file (path-replace-extension f #".json")))))]
                [i (in-range (max (length ours) (length theirs)))])
      (list ((relative-to dir) f) (and (< i (length ours)) (list-ref ours i)) (and (< i (length theirs)) (list-ref theirs i)))))
  (run-cases cases car
             (lambda (c)
               (define a (cadr c)) (define b (caddr c))
               (if (and a b (jsexpr=? a b)) 'pass
                   (format "rule differs\n    ours   ~v\n    theirs ~v" a b)))))

(define (expected-error? j) (and (hash? j) (equal? (hash-ref j 'resourceType #f) "OperationOutcome")))

;; Two result files are not expectations of the reference engine. The evidence is matchbox's own
;; test class (matchbox-engine/src/test/java/ch/ahdis/matchbox/engine/tests/FhirMappingLanguageTests.java
;; at the vendored commit):
;;   step4b.map is never run there — the map's own comment says `a.isInteger()` "will not work, isInteger
;;   is not a FHIRPath function"; the result file is a leftover, so the case is skipped;
;;   step6b over source6b: the test expects the transform to THROW (`only_one` over two elements —
;;   "Rule rule_a23b: Check condition failed: the collection has more than one item"), so the run must
;;   fail here too, whatever the stale result file says.
(define reference-overrides
  '(("step4/step4b.source4.json" skip
     "the reference never runs step4b.map; its comment says isInteger() is not a FHIRPath function")
    ("step6/step6b.source6b.json" error
     "the reference test expects an exception: only_one over two elements")))
(define (override-of name) (let ([o (assoc name reference-overrides)]) (and o (cadr o))))

(define (run-tutorial dir) (run-tutorial-with dir map-from-file))
(define (run-tutorial/structuremap dir) (run-tutorial-with dir map-from-structuremap))
(define (run-tutorial/own-structuremap dir) (run-tutorial-with dir map-from-own-structuremap))
;; the map emitted as a StructureMap resource, then resolved by canonical from the data source
(define (map-from-own-structuremap map-file sds)
  (define-values (m0 _) (map-from-file map-file sds))
  (define sm (fml->structuremap m0))
  (define m (parameterize ([current-data-source (resources->data-source (cons sm sds))])
              (resolve-structure-map (hash-ref sm 'url))))
  (unless m (error 'structuremap "~a: not resolved by canonical" map-file))
  (values m (list sm)))

;; a map loader : map-file sds -> (values fml-map extra-resources)   the FML file, or the reference's
;; StructureMap next to it resolved by its canonical from the data source
(define (map-from-file map-file sds)
  (define mr (parse-map-file map-file))
  (unless (ok? mr) (error 'parse "~a" (err-error mr)))
  (values (ok-value mr) '()))
(define (map-from-structuremap map-file sds)
  (define sm (read-json-file (path-replace-extension map-file #".json")))
  (define m (parameterize ([current-data-source (resources->data-source (cons sm sds))])
              (resolve-structure-map (hash-ref sm 'url))))
  (unless m (error 'structuremap "~a: not resolved by canonical" map-file))
  (values m (list sm)))

;; A TutorialCase is (tcase name step map-file source-file expected): one result file of one step,
;; with the step's logical models beside it
(struct tcase (name step map-file source-file expected) #:transparent)
(define (tutorial-cases dir)
  (define steps (sort (for/list ([d (directory-list (build-path dir "tutorial") #:build? #t)]
                                 #:when (directory-exists? (build-path d "result")))
                        d)
                      < #:key (lambda (d) (string->number (regexp-replace #rx"^step" (path->string (file-name-from-path d)) "")))))
  (for*/list ([step (in-list steps)]
              [result (in-list (sort (directory-list (build-path step "result")) string<? #:key path->string))])
    (define parts (regexp-match #px"^(.*)\\.(.*)\\.json$" (path->string result)))
    (define name (format "~a/~a" (path->string (file-name-from-path step)) (path->string result)))
    (tcase name step
           (build-path step "map" (string-append (cadr parts) ".map"))
           (build-path step "source" (string-append (caddr parts) ".json"))
           ;; the reference's expectation: the result file, unless its own test expects an error
           (if (eq? (override-of name) 'error)
               (hasheq 'resourceType "OperationOutcome" 'issue (list (hasheq 'diagnostics (caddr (assoc name reference-overrides)))))
               (read-json-file (build-path step "result" result))))))

;; the outcome of one case: the map (loaded as `load-map` says) run over the source, against what the
;; reference expects — a result, or an error
(define (run-tutorial-with dir load-map)
  (run-cases (tutorial-cases dir) tcase-name
             (lambda (c)
               (define sds (logical-models-beside (tcase-map-file c)))
               (define expected (tcase-expected c))
               (define actual
                 (with-handlers ([exn:fail? (lambda (e) (list 'error (exn-message e)))])
                   (define-values (m extra) (load-map (tcase-map-file c) sds))
                   (parameterize ([current-data-source (resources->data-source (append extra sds))])
                     (run-loaded m (tcase-source-file c)))))
               (define failed? (and (pair? actual) (eq? (car actual) 'error)))
               (cond [(eq? (override-of (tcase-name c)) 'skip) 'skip]
                     [(expected-error? expected)
                      (if failed? 'pass (format "expected an error (~a), got ~v" (hash-ref (car (hash-ref expected 'issue)) 'diagnostics "") actual))]
                     [failed? (cadr actual)]
                     [(jsexpr=? actual expected) 'pass]
                     [else (format "mismatch\n    expected ~v\n    actual   ~v" expected actual)]))))

(define (jsexpr=? a b)
  (cond [(and (hash? a) (hash? b))
         (and (= (hash-count a) (hash-count b))
              (for/and ([(k v) (in-hash a)]) (and (hash-has-key? b k) (jsexpr=? v (hash-ref b k)))))]
        [(and (list? a) (list? b)) (and (= (length a) (length b)) (andmap jsexpr=? a b))]
        [(and (number? a) (number? b)) (= a b)]
        [else (equal? a b)]))

;; ---- the top-level maps -------------------------------------------------------
;; each map with the corpus source its `uses … as source` fits (the Java tests pair them the same way)
(define smoke-pairs
  '(("bundle-resolve.map" "bundle-resolve.src.json")
    ("bundleobs.map" "copyaliasingbug-source.json")
    ("bundlets.map" "qr.json")
    ("cast.map" "qr.json")
    ("conformsto.map" "pat.json")
    ("conformstoneg.map" "pat.json")
    ("copyaliasingbug.map" "copyaliasingbug-source.json")
    ("memberof.map" "pat.json")
    ("narrative.map" "pat.json")
    ("qr2patfordates.map" "qr.json")
    ("qr2patgender.map" "qr.json")
    ("qr2patgender_pin.map" "qr.json")
    ("quantity.map" "qr.json")
    ("stringtocoding.map" "qr.json")
    ("stringtocodingwithcc.map" "qr.json")
    ("translatecoding.map" "qr.json")
    ("whereclause.map" "capabilitystatement-example.json")))

(define (run-smoke dir)
  (run-cases smoke-pairs car
             (lambda (p)
               (parameterize ([current-data-source (resources->data-source '())])
                 (define-values (m _) (map-from-file (build-path dir (car p)) '()))
                 (run-loaded m (build-path dir (cadr p)))
                 'pass))))

(module+ test
  (require rackunit racket/runtime-path)
  (define-runtime-path vendor "vendor/matchbox")
  (define p (parse-corpus vendor))
  (check-equal? (tally-total p) (length (map-files vendor)))
  (check-true (> (tally-total p) 40))
  ;; step1 is the simplest map there is: one rule, one string copied
  (define t (run-tutorial vendor))
  (check-false (assoc "step1/step1.source1.json" (tally-failures t)) "tutorial step 1")
  ;; every case the reference asserts agrees (the overrides above are the reference's own)
  (check-equal? (tally-failures t) '())
  (check-equal? (tally-total t) 37)
  (check-true (>= (tally-passed (run-smoke vendor)) 17))
  ;; the StructureMap as the interchange form: every map reads back, step 1 runs from the resource
  (define rt (roundtrip-corpus vendor))
  (check-equal? (tally-failures rt) '())
  (define ts (run-tutorial/structuremap vendor))
  (check-false (assoc "step1/step1.source1.json" (tally-failures ts)) "tutorial step 1 from its StructureMap")
  ;; from our own StructureMap resources every case agrees, as from the FML
  (check-equal? (tally-failures (run-tutorial/own-structuremap vendor)) '()))
