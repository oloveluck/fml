#lang racket/base

;;; support.rkt — what FML offers the family's editor hub (fhir/lang/support): the
;;; diagnostics of a map (parse, then the check against its structures), the maps beside it, its
;;; declarations as an outline (the map, its uses aliases, imports, concept maps, groups), hover and go
;;; to definition (an imported url leads to the map that declares it), and completion of declared
;;; names and keywords. What an agent runs is workspace.rkt.

(require racket/contract racket/string racket/list
         fhir/lang/support fhir/lang/declared
         (only-in fhir/result/result ok? err? ok-value err-error)
         (only-in "parser/parse.rkt" parse-fml)
         (only-in "check.rkt" check-map)
         (only-in "error.rkt" fml-error-diagnostic fml-error-message))

(provide (contract-out [support lang-support?]))

(define (path-string p) (if (path? p) (path->string p) p))

;; diagnose : text path -> diagnostics   parse, then what the two StructureDefinitions decide
(define (diagnose text path)
  (define r (parse-fml (blank-lang-line text) #:source (path-string path)))
  (cond
    [(err? r) (define e (err-error r))
              (list (or (fml-error-diagnostic e) (positionless 'fml 'parse (fml-error-message e) path)))]
    [else (check-map (ok-value r) #:source (path-string path))]))

;; the declarations: the map is its name with the url as its id, so `imports "url"` finds it
(define declarations
  (list (list "map"        #px"^map\\s+\"([^\"]*)\"\\s*=\\s*\"?([^\"\\s]+)\"?" (lambda (gs) (cons (cadr gs) (car gs))))
        (list "uses"       #px"^uses\\s+\"[^\"]+\"\\s+alias\\s+(\\w+)")
        (list "imports"    #px"^imports\\s+\"([^\"]+)\"")
        (list "conceptmap" #px"^conceptmap\\s+\"([^\"]+)\"")
        (list "group"      #px"^group\\s+(\\w+)")))

(define keywords
  '("map" "uses" "alias" "as" "source" "target" "queried" "produced" "imports" "let" "conceptmap" "prefix"
    "group" "extends" "types" "type+" "default" "where" "check" "log" "then" "share" "first" "last"
    "not_first" "not_last" "only_one" "create" "copy" "truncate" "escape" "cast" "append" "translate"
    "reference" "dateOp" "uuid" "pointer" "evaluate" "cc" "c" "qty" "id" "cp" "equivalent" "equal"
    "wider" "narrower" "unmatched" "true" "false"))

(define (item-kind kind) (case kind [("group") 3] [("map" "uses" "imports") 9] [("conceptmap") 12] [else 1]))

;; entities : text file -> (listof entity)   the declarations; the map by either header the
;; specification has: `map "url" = "name"`, or the metadata lines `/// url = '…'`, `/// name = '…'`
;; (the entity is at the name's line, its id the url stated on another)
(define (entities text file)
  (define url (cond [(regexp-match #px"(?m:^///\\s*url\\s*=\\s*['\"]([^'\"]*)['\"])" text) => cadr] [else #f]))
  (declared-entities (cons (list "map" #px"^///\\s*name\\s*=\\s*['\"]([^'\"]*)['\"]" (lambda (gs) (cons (car gs) url)))
                           declarations)
                     text file))
(define (project-files path) (files-beside path '("map" "fml")))

;; definition : project text position -> entity|#f   a group, a uses alias, or the map an imports url
;; names (by its id) in this map or one beside it
(define (definition pr text pos)
  (define n (name-at text pos))
  (define es (project-entities pr))
  (define e (and n (find-entity es n)))
  (cond
    [(and e (equal? (entity-kind e) "imports"))
     (or (findf (lambda (x) (and (equal? (entity-kind x) "map") (equal? (entity-id x) (entity-name e)))) es) e)]
    [else e]))
(define (hover pr text pos)
  (define e (definition pr text pos))
  (and e (declaration-hover e ((project-text-of pr) (entity-file e)) "fml")))
(define (completion pr text pos) (prefix-completions (project-entities pr) keywords item-kind text pos))

(define support (lang-support "fml" diagnose project-files entities hover definition completion))

(module+ test
  (require rackunit)
  (define fml "map \"http://x/StructureMap/m\" = \"MyMap\"\nuses \"http://hl7.org/fhir/StructureDefinition/Patient\" alias Patient as source\nimports \"http://x/StructureMap/other\"\ngroup main(source src : Patient, target tgt) {\n  src -> tgt then helper(src, tgt);\n}\ngroup helper(source s, target t) {\n}\n")
  (define es (entities fml "/tmp/nowhere/m.map"))
  (check-equal? (map entity-name es) '("MyMap" "Patient" "http://x/StructureMap/other" "main" "helper"))
  (check-equal? (entity-id (car es)) "http://x/StructureMap/m")
  ;; the header as metadata lines: the same entity, at the name's line
  (define other "/// url = 'http://x/StructureMap/other'\n/// name = 'Other'\n/// status = 'draft'\n\ngroup o(source s, target t) {\n}\n")
  (check-equal? (map (lambda (e) (list (entity-kind e) (entity-name e) (entity-id e) (entity-line e))) (entities other "/tmp/nowhere/other.map"))
                '(("map" "Other" "http://x/StructureMap/other" 1) ("group" "o" #f 4)))
  (define pr (project "/tmp/nowhere/m.map" (append es (entities other "/tmp/nowhere/other.map"))
                      (lambda (f) (if (regexp-match? #rx"other" (path-string f)) other fml))))
  (check-equal? (entity-line (definition pr fml (cons 4 20))) 6)                                    ; helper
  (check-equal? (entity-file (definition pr fml (cons 2 20))) "/tmp/nowhere/other.map")           ; the import
  (check-regexp-match #rx"group helper" (hover pr fml (cons 4 20)))
  (check-equal? (map (lambda (i) (hash-ref i 'label)) (completion pr fml (cons 4 20))) '("helper"))
  (check-true (pair? (diagnose "map \"http://x\" = \"M\"\ngroup g(source s, target t) {\n  s -> \n" "/tmp/nowhere/bad.map"))))
