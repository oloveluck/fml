#lang racket/base

;;; ast.rkt — the FHIR Mapping Language AST (prefab data definitions).
;;;
;;; An FmlMap is (fml-map url name metadata uses imports groups):
;;;   metadata : (Listof (cons key-string value-string))   from /// key = "value" headers
;;;   uses     : (Listof fml-use)     — declared source/target models
;;;   groups   : (Listof fml-group)   — the first group is the entry point
;;; A FmlGroup is (fml-group name params extends flags rules):
;;;   params   : (Listof fml-param)   — (mode 'source|'target, name, type|#f)
;;; A FmlRule is (fml-rule sources targets dependents nested name):
;;;   sources    : (Listof fml-source)  — usually one
;;;   targets    : (Listof fml-target)  — the comma-chained target ops (may be empty)
;;;   dependents : (Listof fml-call)    — `then group(args)` invocations
;;;   nested     : (Listof fml-rule)    — `then { … }` block
;;; A FmlSource is (fml-source context element variable where check log-expr list-mode):
;;;   where/check/log-expr : (fml-fhirpath ast)|#f ; list-mode : 'first|'last|'only_one|#f
;;; A FmlTarget is (fml-target context element transform variable share):
;;;   transform : #f (plain create/copy of the element)
;;;             | (fml-literal value)          — 'text' / "text" literal assignment
;;;             | (fml-fhirpath ast)           — = (expr) : a REAL FHIRPath AST (fp-* prefabs),
;;;                                              produced by the inlined cascade at parse time
;;;             | (fml-invoke name args)       — create('T') / reference(v) / cast(v,'T') / copy(v)…
;;;             | (fml-var name)               — = boundVar
;;; An arg in fml-invoke/fml-call is a string (identifier/var), (fml-literal v), or (fml-fhirpath ast).

(require racket/contract)

(provide (struct-out fml-map) (struct-out fml-use) (struct-out fml-group) (struct-out fml-param)
         (struct-out fml-rule) (struct-out fml-source) (struct-out fml-target)
         (struct-out fml-call) (struct-out fml-literal) (struct-out fml-fhirpath)
         (struct-out fml-invoke) (struct-out fml-var))

(struct fml-map      (url name metadata uses imports groups) #:prefab)
(struct fml-use      (url alias mode)                        #:prefab) ; mode 'source|'target
(struct fml-group    (name params extends flags rules)       #:prefab)
(struct fml-param    (mode name type)                        #:prefab)
(struct fml-rule     (sources targets dependents nested name) #:prefab)
(struct fml-source   (context element variable where check log-expr list-mode) #:prefab)
(struct fml-target   (context element transform variable share) #:prefab)
(struct fml-call     (name args)                             #:prefab)
(struct fml-literal  (value)                                 #:prefab)
(struct fml-fhirpath (ast)                                   #:prefab)
(struct fml-invoke   (name args)                             #:prefab)
(struct fml-var      (name)                                  #:prefab)
