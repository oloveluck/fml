#lang brag

;; FHIR Mapping Language grammar: the language the specification defines — its text
;; (hl7.org/fhir/mapping-language.html) and its formal grammar (mapping.g4, vendored alongside).
;; Where the g4 leaves FHIRPath as a stub ("fhirPath : literal // insert reference to FhirPath
;; grammar here") we INLINE the repo's FHIRPath cascade (fhirpath/parser/grammar.rkt, same rule
;; names, so fhirpath's `lower` applies to the subtrees unchanged).
;;
;; The specification's text and its g4 disagree in four places; both forms are read here:
;;   the header      text: `/// url = '…'`, `/// name = '…'` metadata ; g4: `map url = name`
;;   a url           text: "double-quoted"                           ; g4: `backticked`
;;   a rule name     text: "double-quoted"                           ; g4: a bare id
;;   a string        text: 'single' and "double" quoted              ; g4: 'single' quoted
;; and the text alone defines the metadata lines and the short form `src -> tgt: a, b;`.
;;
;; Read here and NOT in the specification (parse.rkt's `specification-notes` reports each;
;; print.rkt writes the first two never, the last two only for a StructureMap that holds them):
;;   `where`/`check` with an unparenthesized expression (the specification: "MUST be surrounded by
;;   a set of parentheses"), `where` before the alias, `share <id>` (the StructureMap's listRuleId,
;;   which the language has no syntax for), and the R4 language's `conceptmap` block.

fml-doc         : metadata* map-decl? metadata* (conceptmap | structure | imports | const)* fgroup*

;; the R4 mapping language's embedded concept map (dropped from the R5 g4, still written and run by
;; the reference implementation): prefixes name code systems, mappings pair codes
conceptmap      : /"conceptmap" (DELIM | STRING) /"{" (cm-prefix | cm-unmapped | cm-mapping)* /"}"
cm-prefix       : /"prefix" identifier /"=" (DELIM | STRING)
cm-unmapped     : /"unmapped" /"for" identifier /"=" identifier
cm-mapping      : identifier /":" cm-code cm-op identifier /":" cm-code
cm-code         : identifier | DELIM | STRING
cm-op           : "=" "=" | "=" | "!=" | "<=" | ">=" | "-" | "~" | "<" | ">" 

metadata        : METADATA
map-decl        : /"map" url /"=" (DELIM | identifier)
url             : DELIM | BDELIM
structure       : /"uses" url structure-alias? /"as" model-mode
structure-alias : /"alias" identifier
imports         : /"imports" url
const           : /"let" identifier /"=" fp-expr /";"
model-mode      : "source" | "queried" | "target" | "produced"

fgroup          : /"group" identifier parameters extends-clause? type-mode? /"{" (frule | short-rule)* /"}"
parameters      : /"(" parameter (/"," parameter)* /")"
parameter       : input-mode identifier param-type?
param-type      : /":" identifier
input-mode      : "source" | "target"
extends-clause  : /"extends" identifier
type-mode       : /"<<" ("types" | "type" "+") /">>"

frule           : rule-sources (/"->" rule-targets)? dependent? rule-name? /";"
;; the text writes a rule's name in double quotes, the g4 as a bare id (never a keyword)
rule-name       : DELIM | STRING | IDENTIFIER
;; the text's short form of the identity transform: `src -> tgt: type, subtype;` is
;; `src.type -> tgt.type; src.subtype -> tgt.subtype;`
short-rule      : rule-context /"->" rule-context /":" identifier (/"," identifier)* /";"
rule-sources    : rule-source (/"," rule-source)*
;; `where` may precede the alias (the R3/R4 inter-version maps write `where … as v`)
rule-source     : rule-context source-type? source-cardinality? source-default?
                  source-list-mode? salias? where-clause? salias? check-clause? log-clause?
rule-context    : identifier (/"." identifier)*
source-type     : /":" identifier
source-cardinality : NUMBER /".." upper-bound
upper-bound     : NUMBER | "*"
source-default  : /"default" /"(" fp-expr /")"
source-list-mode : "first" | "not_first" | "last" | "not_last" | "only_one"
salias          : /"as" identifier
where-clause    : /"where" fp-expr
check-clause    : /"check" fp-expr
log-clause      : /"log" /"(" fp-expr /")"

;; group invocations, the last of which may be followed by a set of dependent rules
dependent       : /"then" (gcall (/"," gcall)* rule-block? | rule-block)
rule-block      : /"{" frule* /"}"

rule-targets    : rule-target (/"," rule-target)*
rule-target     : rule-context (/"=" transform)? salias? target-list-mode?
                | function-call salias?
target-list-mode : "first" | "last" | "single" | "share" share-id?
share-id        : IDENTIFIER
;; literal | copy-context | invocation — all FHIRPath primaries (incl. the FML dialect's
;; double-quoted strings, which the fp literal below admits as DELIM)
transform       : fp-expr

;; a dependent group call (`then item(item, tgt)`) — distinct from fp's function-call `invocation`
gcall           : identifier /"(" arg-list? /")"

;; ---- the inlined FHIRPath cascade (fhirpath/parser/grammar.rkt, verbatim rule names) ----------
fp-expr             : implies-expr
implies-expr        : or-expr             (@implies-op or-expr)*
or-expr             : and-expr            (@or-op and-expr)*
and-expr            : membership-expr     (@and-op membership-expr)*
membership-expr     : equality-expr       (@membership-op equality-expr)*
equality-expr       : inequality-expr     (@equality-op inequality-expr)*
inequality-expr     : union-expr          (@inequality-op union-expr)*
;; NOTE: the fp `as` OPERATOR is omitted here — `create('X') as var` must read as a target
;; alias, not a type cast (the .as() FUNCTION form remains); `is` stays (`where $this is Quantity`)
union-expr          : type-expr           (@union-op type-expr)*
type-expr           : additive-expr       (@type-op type-spec)*
type-op             : "is"
type-spec           : identifier (/"." identifier)*
additive-expr       : multiplicative-expr (@additive-op multiplicative-expr)*
multiplicative-expr : unary-expr          (@multiplicative-op unary-expr)*
unary-expr          : @polarity-op? postfix-expr

postfix-expr        : primary postfix-tail*
postfix-tail        : invocation | indexer
invocation          : /"." (function-call | member | special-var)
indexer             : /"[" fp-expr /"]"

primary             : literal
                    | function-call
                    | member
                    | special-var
                    | external-constant
                    | /"(" fp-expr /")"

function-call       : identifier /"(" arg-list? /")"
arg-list            : fp-expr (/"," fp-expr)*
member              : identifier
special-var         : "$this" | "$index" | "$total"
external-constant   : EXTERNAL

;; keyword tokens usable as plain identifiers inside expressions and FML contexts
identifier          : IDENTIFIER | BDELIM | "as" | "is" | "in" | "contains" | "where" | "check" | "log"
                    | "first" | "last" | "single" | "share" | "default" | "div" | "mod"
                    | "source" | "target" | "map" | "uses" | "alias" | "let" | "group" | "extends"
                    | "conceptmap" | "prefix" | "types" | "type" | "queried" | "produced" | "unmapped" | "for"
                    | "not_first" | "not_last" | "only_one"

;; the FML dialect also admits "double-quoted" strings inside expressions (qr2cda-eval's iif)
literal             : quantity | STRING | DELIM | BOOL | DATE | DATETIME | TIME | "{" "}"
quantity            : NUMBER unit?
unit                : identifier | STRING

implies-op          : "implies"
and-op              : "and"
union-op            : "|"
or-op               : "or" | "xor"
membership-op       : "in" | "contains"
equality-op         : "=" | "!=" | "~" | "!~"
inequality-op       : "<=" | ">=" | "<" | ">"
polarity-op         : "+" | "-"
additive-op         : "+" | "-" | "&"
multiplicative-op   : "*" | "/" | "div" | "mod"
