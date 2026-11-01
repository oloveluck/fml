#lang racket/base

;;; cli.rkt — `raco fml`: the command line for the FHIR Mapping Language package.
;;;
;;;   raco fml <file.map>                         parse an FML map and print its StructureMap JSON
;;;   raco fml <file.map> --transform <src.json>  run the map over a source resource (R5), print the target
;;;   raco fml --tck                              run the vendored fhir-test-cases structure-mapping suite
;;; Exit codes: 0 ok · 1 FML error / regression · 2 usage.

(require json
         (only-in racket/file file->string)
         (only-in fml/parser/parse parse-fml)
         (only-in fml/structuremap fml->structuremap)
         (only-in fml/transform run-map)
         (only-in cql/error cql-error-string)
         (only-in fhir/serialize/from-json from-json)
         (only-in fhir/result/result ok? ok-value err-error validation-error-message))

(provide compile-file transform-file)

;; parse-file : path-string -> fml-map   (or exit 1)
(define (parse-file file)
  (define r (parse-fml (regexp-replace #px"^#lang[^\n]*\n" (file->string file) "")))
  (unless (ok? r) (eprintf "~a: ~a\n" file (cql-error-string (err-error r))) (exit 1))
  (ok-value r))

;; compile-file : path-string -> void   print the StructureMap JSON
(define (compile-file file)
  (displayln (jsexpr->string (fml->structuremap (parse-file file)))))

;; transform-file : path-string path-string -> void   run the map over the R5 resource in src-file
(define (transform-file file src-file)
  (dynamic-require 'fhir/r5/schemas #f)                 ; the source resource must build as a typed node
  (define m (parse-file file))
  (define r (from-json (call-with-input-file src-file read-json) #:lenient? #t))
  (unless (ok? r) (eprintf "~a: ~a\n" src-file (validation-error-message (err-error r))) (exit 1))
  (displayln (jsexpr->string (run-map m (ok-value r)))))

(define (usage)
  (eprintf "usage: raco fml <file.map> [--transform <source.json>]\n       raco fml --tck\n")
  (exit 2))

(module+ main
  (define args (vector->list (current-command-line-arguments)))
  (cond
    [(null? args) (usage)]
    [(equal? (car args) "--tck") (dynamic-require '(submod fml/tests/run main) #f)]
    [(regexp-match? #rx"^-" (car args)) (usage)]
    [(and (= (length args) 3) (equal? (cadr args) "--transform")) (transform-file (car args) (caddr args))]
    [(= (length args) 1) (compile-file (car args))]
    [else (usage)]))
