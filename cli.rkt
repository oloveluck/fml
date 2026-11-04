#lang racket/base

;;; cli.rkt — `raco fml`: the command line for the FHIR Mapping Language package.
;;;
;;;   raco fml <file.map>                         parse an FML map and print its StructureMap JSON
;;;   raco fml <file.map> --transform <src.json>  run the map over a source resource (R5), print the target
;;;       [--structures <file|dir>]               StructureDefinitions the map's `uses` resolve in
;;;                                               (logical models: a JSON file, a Bundle, or a directory)
;;;   raco fml --parse-corpus <dir>               parse every .map under a directory, report the failures
;;;   raco fml --tck                              run the vendored corpora (fhir-test-cases, matchbox, tutorial)
;;; Exit codes: 0 ok · 1 FML error / regression · 2 usage.

(require json
         (only-in racket/file file->string)
         (only-in fml/parser/parse parse-fml)
         (only-in fml/structuremap fml->structuremap)
         (only-in fml/transform run-map uses-types)
         (only-in cql/error cql-error-string cql-error-diagnostic cql-error-message)
         (only-in fhir/diagnostic diagnostic diagnostic-loc loc emit-diagnostics format-flag)
         (only-in fhir/serialize/from-json from-json)
         (only-in fhir/data-source current-data-source resources->data-source directory->data-source)
         (only-in fhir/result/result ok? ok-value err-error validation-error-message))

(provide compile-file transform-file parse-corpus-dir)

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
  (define src (regexp-replace #px"^#lang[^\n]*" (file->string file) ""))   ; keep line numbers
  (define r (parse-fml src #:source file))
  (unless (ok? r)
    (define e (err-error r))
    (report (or (cql-error-diagnostic e) (diagnostic 'fml 'parse 'error (cql-error-message e) (loc file #f #f #f #f))) src))
  (ok-value r))

;; compile-file : path-string -> void   print the StructureMap JSON
(define (compile-file file)
  (displayln (jsexpr->string (fml->structuremap (parse-file file)))))

;; transform-file : path-string path-string [path-string|#f] -> void
;; run the map over the R5 resource in src-file; `structures` (a JSON file or a directory of them) holds
;; the StructureDefinitions of the map's logical models, resolved before the source is read
(define (transform-file file src-file [structures #f])
  (dynamic-require 'fhir/r5/schemas #f)                 ; the source resource must build as a typed node
  (define m (parse-file file))
  (parameterize ([current-data-source
                  (cond [(not structures) (current-data-source)]
                        [(directory-exists? structures) (directory->data-source structures)]
                        [else (resources->data-source (list (call-with-input-file structures read-json)))])])
    (uses-types m)
    (define r (from-json (call-with-input-file src-file read-json) #:lenient? #t))
    (unless (ok? r) (eprintf "~a: ~a\n" src-file (validation-error-message (err-error r))) (exit 1))
    (displayln (jsexpr->string (run-map m (ok-value r))))))

;; parse-corpus-dir : path-string -> void   parse every .map under dir; the failures, a count; exit 1 on any
(define (parse-corpus-dir dir)
  (define t ((dynamic-require 'fml/tests/matchbox-harness 'parse-corpus) dir))
  (define failures ((dynamic-require 'fml/tests/matchbox-harness 'tally-failures) t))
  (for ([f (in-list failures)]) (printf "  FAIL ~a: ~a\n" (car f) (cadr f)))
  (printf "~a: ~a/~a maps parse\n" dir
          ((dynamic-require 'fml/tests/matchbox-harness 'tally-passed) t)
          ((dynamic-require 'fml/tests/matchbox-harness 'tally-total) t))
  (unless (null? failures) (exit 1)))

(define (usage [code 2])                             ; --help: stdout, exit 0
  ((if (zero? code) printf eprintf)
   "usage: raco fml <file.map> [--transform <source.json> [--structures <file|dir>]]\n       raco fml --parse-corpus <dir>\n       raco fml --tck\n")
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
    [(regexp-match? #rx"^-" (car args)) (usage)]
    [(= (length args) 1) (compile-file (car args))]
    [else
     ;; <file.map> --transform <src> [--structures <sds>]
     (define opts (let loop ([as (cdr args)] [h (hash)])
                    (cond [(null? as) h]
                          [(and (member (car as) '("--transform" "--structures")) (pair? (cdr as)))
                           (loop (cddr as) (hash-set h (car as) (cadr as)))]
                          [else (usage)])))
     (unless (hash-ref opts "--transform" #f) (usage))
     (transform-file (car args) (hash-ref opts "--transform") (hash-ref opts "--structures" #f))]))
