#lang racket/base

;;; ast.rkt — the FHIR Mapping Language AST (prefab data definitions).
;;;
;;; An FmlMap is (fml-map url name metadata uses imports constants groups conceptmaps):
;;;   metadata : (Listof Metadata)    — the `/// name{.property}* = value` headers, in order
;;;   uses     : (Listof fml-use)     — declared source/target models
;;;   constants: (Listof (cons name fml-fhirpath)) — `let name = expression;`
;;;   groups   : (Listof fml-group)   — the first group is the entry point
;;; A Metadata is (cons path value): path the dotted element name ("jurisdiction.coding.code"),
;;;   value a string | a boolean | a number | 'empty (a complex element begun: `/// jurisdiction =`)
;;;   conceptmaps : (Listof fml-conceptmap) — the R4-era embedded `conceptmap "name" { … }` blocks
;;; A FmlConceptmap is (fml-conceptmap name prefixes mappings):
;;;   prefixes : (Hash prefix-string -> system-url) ; mappings : (Listof CmMapping)
;;; A CmMapping is (cm-mapping source code op target target-code): one `s:code op t:code` line, the
;;;   systems resolved through the prefixes, `op` the operator as written ("==", "=", "-", …)
;;; A FmlGroup is (fml-group name params extends flags rules):
;;;   params   : (Listof fml-param)   — (mode 'source|'target, name, type|#f)
;;; A FmlRule is (fml-rule sources targets dependents nested name):
;;;   sources    : (Listof fml-source)  — usually one
;;;   targets    : (Listof fml-target)  — the comma-chained target ops (may be empty)
;;;   dependents : (Listof fml-call)    — `then group(args)` invocations
;;;   nested     : (Listof fml-rule)    — `then { … }` block
;;; A FmlSource is (fml-source context element type min max default list-mode variable where check
;;;   log-expr), in the order the language writes them:
;;;   type : a type name | #f (`: code` — only elements of that type are selected)
;;;   min  : an integer | #f ; max : a string ("1", "*") | #f   (`0..*`: the element's cardinality, checked)
;;;   default : (fml-fhirpath ast) | #f  (`default (10)`: the value when the source has none)
;;;   list-mode : 'first | 'not_first | 'last | 'not_last | 'only_one | #f
;;;   where/check/log-expr : (fml-fhirpath ast) | #f
;;; A FmlTarget is (fml-target context element transform variable share list-mode):
;;;   share : #f | #t (the list mode `share`) | a rule id (`share <id>`: the StructureMap's listRuleId)
;;   list-mode : 'first|'last|'single|#f (the target list modes other than share; accepted, kept)
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
         (struct-out fml-invoke) (struct-out fml-var) (struct-out fml-conceptmap) (struct-out cm-mapping))

(struct fml-map      (url name metadata uses imports constants groups conceptmaps) #:prefab)
(struct fml-conceptmap (name prefixes mappings)                 #:prefab)
(struct cm-mapping   (source code op target target-code)          #:prefab)
(struct fml-use      (url alias mode)                        #:prefab) ; mode 'source|'target
(struct fml-group    (name params extends flags rules)       #:prefab)
(struct fml-param    (mode name type)                        #:prefab)
(struct fml-rule     (sources targets dependents nested name) #:prefab)
(struct fml-source   (context element type min max default list-mode variable where check log-expr) #:prefab)
(struct fml-target   (context element transform variable share list-mode) #:prefab)
(struct fml-call     (name args)                             #:prefab)
(struct fml-literal  (value)                                 #:prefab)
(struct fml-fhirpath (ast)                                   #:prefab)
(struct fml-invoke   (name args)                             #:prefab)
(struct fml-var      (name)                                  #:prefab)
