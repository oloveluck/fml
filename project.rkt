#lang racket/base

;;; project.rkt — maps as members of a project of structures (fsh/project.rkt loads this on demand
;;; when a project holds .map files; it must not depend on fsh). Three entry points, each over the
;;; current SD context (the project's own package first, so `uses` finds the project's structures):
;;;   parse-map-file   : path -> (values map|#f diagnostics)      parse errors positioned in the file
;;;   map-structuremap : map -> StructureMap jsexpr               what the package carries
;;;   check-project-map: map examples profiles #:source -> diagnostics
;;;       the static check, then every example whose type is the map's source structure
;;;       transformed and validated against the target structure (a profile among `profiles`
;;;       when the map targets one)

(require racket/contract racket/list racket/string racket/file
         "parser/ast.rkt"
         (only-in "parser/parse.rkt" parse-fml)
         (only-in "structuremap.rkt" fml->structuremap)
         (only-in "check.rkt" check-map check-map/examples)
         (only-in fhir/sd/logical resolve-structure-definition/data)
         (only-in fhir/diagnostic diagnostic diagnostic? loc)
         (only-in fhir/result/result ok? ok-value err-error)
         (only-in "error.rkt" fml-error-diagnostic fml-error-message))

(provide (contract-out
          [parse-map-file (-> path-string? (values (or/c fml-map? #f) (listof diagnostic?)))]
          [map-structuremap (-> fml-map? hash?)]
          [check-project-map (->* (fml-map? (listof hash?) (listof hash?)) (#:source (or/c string? path? #f)) (listof diagnostic?))]))

(define (parse-map-file file)
  (define src (regexp-replace #px"^#lang[^\n]*" (file->string file) ""))
  (define r (parse-fml src #:source file))
  (cond [(ok? r) (values (ok-value r) '())]
        [else (define e (err-error r))
              (values #f (list (or (fml-error-diagnostic e)
                                   (diagnostic 'fml 'parse 'error (fml-error-message e)
                                               (loc (if (path? file) (path->string file) file) #f #f #f #f)))))]))

(define (map-structuremap m) (fml->structuremap m))

(define (check-project-map m examples profiles #:source [source #f])
  (define static (check-map m #:source source))
  (define (use-of mode) (findf (lambda (u) (eq? (fml-use-mode u) mode)) (fml-map-uses m)))
  (define (structure-type url)                       ; the resource type a `uses` url names
    (define sd (resolve-structure-definition/data url))
    (or (and sd (hash-ref sd 'type #f)) (last (string-split (car (regexp-split #rx"[|]" url)) "/"))))
  (define src-use (use-of 'source))
  (define tgt-use (use-of 'target))
  (define mine (if src-use
                   (let ([t (structure-type (fml-use-url src-use))])
                     (filter (lambda (e) (equal? (hash-ref e 'resourceType #f) t)) examples))
                   '()))
  (define target-profiles
    (if tgt-use
        (let ([u (car (regexp-split #rx"[|]" (fml-use-url tgt-use)))])
          (filter (lambda (sd) (equal? (hash-ref sd 'url #f) u)) profiles))
        '()))
  (append static
          (if (null? mine) '() (check-map/examples m mine #:profiles target-profiles #:source source))))

(module+ test
  (require rackunit (only-in fhir/r5/schemas))
  (define dir (make-temporary-directory))
  (define f (build-path dir "t.map"))
  (call-with-output-file f (lambda (o) (write-string "map \"http://x/m\" = \"m\"\nuses \"http://hl7.org/fhir/StructureDefinition/Patient\" alias P as source\nuses \"http://hl7.org/fhir/StructureDefinition/Patient\" alias P as target\ngroup g(source s : P, target t : P) { s.gender as g -> t.gender = g; s.nope as n -> t.active = n; }\n" o)))
  (define-values (m ds) (parse-map-file f))
  (check-true (fml-map? m))
  (check-equal? ds '())
  (check-equal? (hash-ref (map-structuremap m) 'resourceType) "StructureMap")
  (define found (check-project-map m (list (hasheq 'resourceType "Patient" 'gender "female")) '() #:source f))
  (check-equal? (length found) 1)                     ; `nope` is not an element of Patient
  (define-values (m2 ds2) (parse-map-file (let ([b (build-path dir "bad.map")]) (call-with-output-file b (lambda (o) (write-string "map oops" o))) b)))
  (check-false m2)
  (check-equal? (length ds2) 1)
  (delete-directory/files dir))
