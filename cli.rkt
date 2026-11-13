#lang racket/base

;;; cli.rkt — `raco fml`: the command line for the FHIR Mapping Language package.
;;;
;;;   raco fml <file.map>                         parse an FML map and print its StructureMap JSON
;;;                                               (a StructureMap .json is accepted wherever a .map is)
;;;   raco fml <file.map> --transform <src.json>  run the map over a source resource (R5), print the target
;;;       [--structures <file|dir>]               StructureDefinitions the map's `uses` resolve in
;;;                                               (logical models: a JSON file, a Bundle, or a directory)
;;;   raco fml check <file.map> [--structures <file|dir>] [--examples <file|dir>] [--profile <sd.json|fsh-dir|canonical>]...
;;;                                               the map against its structures: findings positioned in
;;;                                               the FML; with examples, each result validated
;;;   raco fml --decompile <StructureMap.json>    a StructureMap resource as the mapping language (print.rkt)
;;;   raco fml --parse-corpus <dir>               parse every .map under a directory and write each back, report the failures
;;;   raco fml --tck                              run the vendored corpora (fhir-test-cases, matchbox, tutorial)
;;; Exit codes: 0 ok · 1 FML error / regression · 2 usage.

(require json racket/string
         (only-in racket/file file->string)
         (only-in fhir/serialize/fhir-json fhir-json->string)
         (only-in fml/parser/parse parse-fml)
         (only-in fml/structuremap fml->structuremap structuremap->fml)
         (only-in fml/print fml->text)
         (only-in fml/transform run-map uses-types)
         (only-in fml/check check-map check-map/examples profiles-from)
         (only-in racket/list append-map)
         (only-in fhir/diagnostic diagnostic-severity)
         (only-in "error.rkt" fml-error-string fml-error-diagnostic fml-error-message)
         (only-in fhir/diagnostic diagnostic diagnostic-loc loc loc-source emit-diagnostics format-flag
                  exn:fail:diagnostic? exn:fail:diagnostic-diagnostic)
         (only-in fhir/serialize/from-json from-json)
         (only-in fhir/serialize/lenient lenient-drops drop->string drops-message)
         (only-in fhir/data-source current-data-source resources->data-source directory->data-source)
         (only-in fhir/result/result ok? ok-value err-error validation-error-message))

(provide compile-file transform-file parse-corpus-dir check-file decompile-file)

;; parse-file : path-string -> fml-map   (or exit 1)
;; report : diagnostic text -> exit 1   (format from --format)
(define current-format (make-parameter 'text))
(define (report d text)
  (emit-diagnostics (list d) #:format (current-format) #:text-of (lambda (_) text))
  (exit 1))
(define (with-source d source)
  (define l (diagnostic-loc d))
  (struct-copy diagnostic d [loc (if l (struct-copy loc l [source source]) (loc source #f #f #f #f))]))

(define (parse-file file)
  (if (regexp-match? #px"\\.json$" (if (path? file) (path->string file) file))
      (let ([sm (call-with-input-file file read-json)])
        (unless (and (hash? sm) (equal? (hash-ref sm 'resourceType #f) "StructureMap"))
          (eprintf "~a: not a StructureMap\n" file) (exit 1))
        (structuremap->fml sm))
      (parse-map-text file)))
(define (parse-map-text file)
  (define src (regexp-replace #px"^#lang[^\n]*" (file->string file) ""))   ; keep line numbers
  (define r (parse-fml src #:source file))
  (unless (ok? r)
    (define e (err-error r))
    (report (or (fml-error-diagnostic e) (diagnostic 'fml 'parse 'error (fml-error-message e) (loc file #f #f #f #f))) src))
  (ok-value r))

;; compile-file : path-string -> void   print the StructureMap JSON
(define (compile-file file)
  (displayln (jsexpr->string (fml->structuremap (parse-file file)))))

;; transform-file : path-string path-string [path-string|#f] -> void
;; run the map over the R5 resource in src-file; `structures` (a JSON file or a directory of them) holds
;; the StructureDefinitions of the map's logical models, resolved before the source is read
(define (transform-file file src-file [structures #f] #:indent [indent #f])
  (dynamic-require 'fhir/r5/schemas #f)                 ; the source resource must build as a typed node
  (define m (parse-file file))
  (parameterize ([current-data-source
                  (cond [(not structures) (current-data-source)]
                        [(directory-exists? structures) (directory->data-source structures)]
                        [else (resources->data-source (list (call-with-input-file structures read-json)))])])
    (uses-types m)
    (define src (call-with-input-file src-file read-json))
    (define r (from-json src #:lenient? #t))
    (unless (ok? r) (eprintf "~a: ~a\n" src-file (validation-error-message (err-error r))) (exit 1))
    ;; what the lenient read left out is said: the map never sees it
    (let ([drops (lenient-drops src (ok-value r))])
      (unless (null? drops)
        (eprintf "~a: warning: ~a\n" src-file (drops-message drops (hash-ref src 'resourceType "its type")))))
    ;; a run-time error (a check, a source of the wrong type, ...) is a diagnostic at its rule or group
    (with-handlers ([exn:fail:diagnostic?
                     (lambda (e)
                       (define d (exn:fail:diagnostic-diagnostic e))
                       (define l (diagnostic-loc d))
                       (report d (if (and l (equal? (loc-source l) file)) (file->string file) "")))])
      ;; resourceType first, as FHIR writes it; `--indent`: indented as a program's write-fhir-json prints
      (displayln (fhir-json->string (run-map m (ok-value r)) #:indent indent)))))

;; check-file : map-file options -> void   the static findings, then the examples validated; exit 1 on an error
(define (check-file file opts)
  (dynamic-require 'fhir/r5/schemas #f)
  (define m (parse-file file))
  (define src (file->string file))
  (define structures (hash-ref opts "--structures" #f))
  ;; each example with its file, the name findings and the summary give it
  (define example-files
    (let ([e (hash-ref opts "--examples" #f)])
      (cond [(not e) '()]
            [(directory-exists? e) (for/list ([f (in-directory e)] #:when (regexp-match? #px"\\.json$" (path->string f)))
                                     (path->string f))]
            [else (list e)])))
  (define examples (for/list ([f (in-list example-files)]) (call-with-input-file f read-json)))
  ;; a profile is a StructureDefinition file, an FSH project (its profiles), or a canonical
  (define (profiles) (append-map profiles-from (hash-ref opts "--profile" '())))
  (parameterize ([current-data-source
                  (cond [(not structures) (current-data-source)]
                        [(directory-exists? structures) (directory->data-source structures)]
                        [else (resources->data-source (list (call-with-input-file structures read-json)))])])
    (define held (profiles))
    (define ds (append (check-map m #:source file)
                       (if (null? examples) '() (check-map/examples m examples #:profiles held #:source file #:names example-files))))
    (emit-diagnostics ds #:format (current-format) #:text-of (lambda (_) src))
    (when (eq? (current-format) 'text)
      (eprintf "~a: ~a finding~a~a\n" file (length ds) (if (= 1 (length ds)) "" "s")
               (if (null? examples) ""
                   ;; which examples, and what they were held to (a clean run said neither)
                   (format ", ~a example~a validated (~a) against ~a" (length examples) (if (= 1 (length examples)) "" "s")
                           (string-join example-files ", ")
                           (if (null? held) "the result's base type and the profiles it claims"
                               (string-join (for/list ([sd (in-list held)])
                                              (format "~a (~a)" (hash-ref sd 'name "?") (hash-ref sd 'url "?")))
                                            ", "))))))
    (when (ormap (lambda (d) (eq? (diagnostic-severity d) 'error)) ds) (exit 1))))

;; parse-corpus-dir : path-string -> void   every .map under dir parses, and is written back in the
;; specified language (print.rkt's laws); the failures, the counts; exit 1 on any
(define (parse-corpus-dir dir)
  (define (harness name) (dynamic-require 'fml/tests/matchbox-harness name))
  (define (report t what)
    (for ([f (in-list ((harness 'tally-failures) t))]) (printf "  FAIL ~a: ~a\n" (car f) (cadr f)))
    (printf "~a: ~a/~a maps ~a\n" dir ((harness 'tally-passed) t) ((harness 'tally-total) t) what)
    (null? ((harness 'tally-failures) t)))
  (define parsed? (report ((harness 'parse-corpus) dir) "parse"))
  (define printed? (report ((harness 'print-corpus) dir) "printed in the specified language and read back"))
  (unless (and parsed? printed?) (exit 1)))

;; decompile-file : path-string -> void   a StructureMap resource (JSON) as the mapping language
(define (decompile-file file)
  (define resource (call-with-input-file file read-json))
  (unless (and (hash? resource) (equal? (hash-ref resource 'resourceType #f) "StructureMap"))
    (eprintf "fml: ~a is not a StructureMap resource\n" file)
    (exit 2))
  (write-string (fml->text (structuremap->fml resource)))
  (void))

(define (usage [code 2])                             ; --help: stdout, exit 0
  ((if (zero? code) printf eprintf)
   "usage: raco fml <file.map> [--transform <source.json> [--structures <file|dir>] [--indent]]\n       raco fml check <file.map> [--structures <file|dir>] [--examples <file|dir>] [--profile <sd.json|fsh-dir|canonical>]...\n       raco fml --decompile <StructureMap.json>\n       raco fml --parse-corpus <dir>\n       raco fml --tck\n")
  (exit code))

(module+ main
  (define-values (fmt args) (format-flag (vector->list (current-command-line-arguments))))
  (current-format fmt)
  ;; an unreadable input file is a one-line error (exit 2), not a stack trace
  (let ([default (uncaught-exception-handler)])
    (uncaught-exception-handler
     (lambda (e)
       (cond [(exn:fail:filesystem? e)
              (define m (regexp-match #px"path: ([^\n]*)" (exn-message e)))
              (eprintf "fml: cannot read ~a\n" (if m (cadr m) (exn-message e)))
              (exit 2)]
             [else (default e)]))))
  (cond
    [(ormap (lambda (a) (member a '("-h" "--help"))) args) (usage 0)]
    [(null? args) (usage)]
    [(equal? (car args) "--tck") (dynamic-require '(submod fml/tests/run main) #f)]
    [(and (equal? (car args) "--parse-corpus") (= (length args) 2)) (parse-corpus-dir (cadr args))]
    [(and (equal? (car args) "--decompile") (= (length args) 2)) (decompile-file (cadr args))]
    [(and (equal? (car args) "check") (pair? (cdr args)) (not (regexp-match? #rx"^-" (cadr args))))
     (define opts (let loop ([as (cddr args)] [h (hash)])
                    (cond [(null? as) h]
                          [(and (member (car as) '("--structures" "--examples")) (pair? (cdr as))) (loop (cddr as) (hash-set h (car as) (cadr as)))]
                          [(and (equal? (car as) "--profile") (pair? (cdr as))) (loop (cddr as) (hash-update h "--profile" (lambda (l) (append l (list (cadr as)))) '()))]
                          [else (usage)])))
     (check-file (cadr args) opts)]
    [(regexp-match? #rx"^-" (car args)) (usage)]
    [(= (length args) 1) (compile-file (car args))]
    [else
     ;; <file.map> --transform <src> [--structures <sds>]
     (define opts (let loop ([as (cdr args)] [h (hash)])
                    (cond [(null? as) h]
                          [(and (member (car as) '("--transform" "--structures")) (pair? (cdr as)))
                           (loop (cddr as) (hash-set h (car as) (cadr as)))]
                          [(equal? (car as) "--indent") (loop (cdr as) (hash-set h "--indent" 2))]
                          [else (usage)])))
     (unless (hash-ref opts "--transform" #f) (usage))
     (transform-file (car args) (hash-ref opts "--transform") (hash-ref opts "--structures" #f)
                     #:indent (hash-ref opts "--indent" #f))]))
