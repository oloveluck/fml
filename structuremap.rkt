#lang racket/base

;;; structuremap.rkt — FML AST -> the StructureMap resource (jsexpr).
;;;
;;; The canonical compiled form of a mapping (the FHIR analog of ELM for CQL): groups/inputs/rules
;;; with source/target/dependent parts, transform codes per the spec's StructureMap model.

(require racket/contract racket/list
         "parser/ast.rkt"
         (only-in fhirpath/print fhirpath->string)
         (only-in fhir/serialize/jsexpr-util drop-false))

(provide (contract-out [fml->structuremap (-> fml-map? hash?)]))

(define (fml->structuremap m)
  (drop-false
   (hasheq 'resourceType "StructureMap"
           'url (fml-map-url m)
           'name (fml-map-name m)
           'status "active"
           'structure (let ([us (fml-map-uses m)])
                        (and (pair? us)
                             (for/list ([u (in-list us)])
                               (drop-false (hasheq 'url (fml-use-url u)
                                                   'mode (symbol->string (fml-use-mode u))
                                                   'alias (fml-use-alias u))))))
           'import (let ([is (fml-map-imports m)]) (and (pair? is) is))
           'contained (let ([cms (fml-map-conceptmaps m)])
                        (and (pair? cms) (map conceptmap->jsexpr cms)))
           'group (for/list ([g (in-list (fml-map-groups m))]) (group->jsexpr g)))))

;; an embedded concept map as a contained ConceptMap (url #name), one group per system pair
(define (conceptmap->jsexpr cm)
  (define by-pair
    (for/fold ([h (hash)]) ([m (in-list (fml-conceptmap-mappings cm))])
      (hash-update h (cons (vector-ref m 0) (vector-ref m 3)) (lambda (l) (append l (list m))) '())))
  (hasheq 'resourceType "ConceptMap" 'id (fml-conceptmap-name cm) 'url (string-append "#" (fml-conceptmap-name cm))
          'status "active"
          'group (for/list ([(pair ms) (in-hash by-pair)])
                   (hasheq 'source (car pair) 'target (cdr pair)
                           'element (for/list ([m (in-list ms)])
                                      (hasheq 'code (vector-ref m 1)
                                              'target (list (hasheq 'code (vector-ref m 4)
                                                                    'relationship (relationship-of (vector-ref m 2))))))))))
(define (relationship-of op)
  (case op [("==" "=") "equivalent"] [("-" "~") "related-to"] [("<=" "<") "source-is-narrower-than-target"]
           [(">=" ">") "source-is-broader-than-target"] [("!=") "not-related-to"] [else "related-to"]))

(define (group->jsexpr g)
  (drop-false
   (hasheq 'name (fml-group-name g)
           'extends (fml-group-extends g)
           'typeMode (cond [(member "type+" (fml-group-flags g)) "type-and-types"]
                           [(member "types" (fml-group-flags g)) "types"]
                           [else #f])
           'input (for/list ([p (in-list (fml-group-params g))])
                    (drop-false (hasheq 'name (fml-param-name p)
                                        'type (fml-param-type p)
                                        'mode (symbol->string (fml-param-mode p)))))
           'rule (for/list ([r (in-list (fml-group-rules g))] [i (in-naturals)])
                   (rule->jsexpr r i)))))

(define (rule->jsexpr r i)
  (drop-false
   (hasheq 'name (or (fml-rule-name r) (format "rule-~a" i))
           'source (for/list ([s (in-list (fml-rule-sources r))]) (source->jsexpr s))
           'target (let ([ts (fml-rule-targets r)])
                     (and (pair? ts) (for/list ([t (in-list ts)]) (target->jsexpr t))))
           'rule (let ([ns (fml-rule-nested r)])
                   (and (pair? ns) (for/list ([n (in-list ns)] [j (in-naturals)]) (rule->jsexpr n j))))
           'dependent (let ([ds (fml-rule-dependents r)])
                        (and (pair? ds)
                             (for/list ([d (in-list ds)])
                               (hasheq 'name (fml-call-name d)
                                       'parameter (for/list ([a (in-list (fml-call-args d))])
                                                    (if (string? a)
                                                        (hasheq 'valueId a)
                                                        (hasheq 'valueString (fhirpath->string (fml-fhirpath-ast a))))))))))))

;; fp-text : (fml-fhirpath ast)|#f -> the printed expression string (for condition/check/log)
(define (fp-text f) (and f (fhirpath->string (fml-fhirpath-ast f))))

(define (source->jsexpr s)
  (drop-false
   (hasheq 'context (fml-source-context s)
           'element (fml-source-element s)
           'variable (fml-source-variable s)
           'condition (fp-text (fml-source-where s))
           'check (fp-text (fml-source-check s))
           'logMessage (fp-text (fml-source-log-expr s))
           'listMode (and (fml-source-list-mode s) (symbol->string (fml-source-list-mode s))))))

(define (target->jsexpr t)
  (define-values (transform params)
    (let ([tr (fml-target-transform t)])
      (cond
        [(not tr) (values #f #f)]
        [(fml-literal? tr) (values "copy" (list (hasheq 'valueString (fml-literal-value tr))))]
        [(fml-fhirpath? tr) (values "evaluate" (list (hasheq 'valueString (fhirpath->string (fml-fhirpath-ast tr)))))]
        [(fml-var? tr) (values "copy" (list (hasheq 'valueId (fml-var-name tr))))]
        [(fml-invoke? tr)
         (values (fml-invoke-name tr)
                 (for/list ([a (in-list (fml-invoke-args tr))])
                   (cond [(fml-literal? a) (hasheq 'valueString (fml-literal-value a))]
                         [(fml-fhirpath? a) (hasheq 'valueString (fhirpath->string (fml-fhirpath-ast a)))]
                         [else (hasheq 'valueId a)])))]
        [else (values #f #f)])))
  (drop-false
   (hasheq 'context (fml-target-context t)
           'element (fml-target-element t)
           'variable (fml-target-variable t)
           'transform transform
           'parameter params
           'listMode (and (fml-target-share t) (list "share"))
           'listRuleId (fml-target-share t))))

(module+ test
  (require rackunit (only-in "parser/parse.rkt" parse-fml) (only-in fhir/result/result ok-value))
  (define sm (fml->structuremap
              (ok-value (parse-fml "map \"http://x\" = \"t\"\nuses \"http://h/P\" alias P as target\ngroup g(source s, target t : P) { s -> t.gender = 'female' \"r\"; }"))))
  (check-equal? (hash-ref sm 'resourceType) "StructureMap")
  (define rule (car (hash-ref (car (hash-ref sm 'group)) 'rule)))
  (check-equal? (hash-ref (car (hash-ref rule 'target)) 'transform) "copy"))
