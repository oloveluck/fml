#lang brag

;; FHIR Mapping Language grammar, ported to brag from the OFFICIAL hl7.org/fhir/R5/mapping.g4
;; (vendored alongside as mapping.g4). Where the g4 leaves FHIRPath as a stub
;; ("fhirPath : literal // insert reference to FhirPath grammar here") we INLINE the repo's
;; FHIRPath cascade (fhirpath/parser/grammar.rkt, same rule names, so fhirpath's `lower` applies
;; to the subtrees unchanged). Deviations from the g4 follow the reference implementation and the
;; vendored fhir-test-cases suite where the g4 lags reality: `where`/`check` accept a bare (not
;; only parenthesized) expression, `share` carries a listRuleId, and a rule name may be a
;; double-quoted string.

fml-doc         : metadata* map-decl? structure* imports* const* fgroup+

metadata        : METADATA
map-decl        : /"map" url /"=" (DELIM | identifier)
url             : DELIM
structure       : /"uses" url structure-alias? /"as" model-mode
structure-alias : /"alias" identifier
imports         : /"imports" url
const           : /"let" identifier /"=" fp-expr /";"
model-mode      : "source" | "queried" | "target" | "produced"

fgroup          : /"group" identifier parameters extends-clause? type-mode? /"{" frule* /"}"
parameters      : /"(" parameter (/"," parameter)* /")"
parameter       : input-mode identifier param-type?
param-type      : /":" identifier
input-mode      : "source" | "target"
extends-clause  : /"extends" identifier
type-mode       : /"<<" identifier+ /">>"

frule           : rule-sources (/"->" rule-targets)? dependent? rule-name? /";"
rule-name       : DELIM | STRING
rule-sources    : rule-source (/"," rule-source)*
rule-source     : rule-context source-type? source-cardinality? source-default?
                  source-list-mode? salias? where-clause? check-clause? log-clause?
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

dependent       : /"then" (gcall (/"," gcall)* | /"{" frule* /"}")

rule-targets    : rule-target (/"," rule-target)*
rule-target     : rule-context (/"=" transform)? salias? target-list-mode?
target-list-mode : "first" | "last" | "single" | "share" identifier
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
;; NOTE: the fp `is`/`as` OPERATOR level is omitted here — `create('X') as var` must read as a
;; target alias, not a type cast (the suite uses the .is()/.as() FUNCTION forms, which remain)
union-expr          : type-expr           (@union-op type-expr)*
type-expr           : additive-expr
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
identifier          : IDENTIFIER | "as" | "is" | "in" | "contains" | "where" | "check" | "log"
                    | "first" | "last" | "single" | "share" | "default" | "div" | "mod"
                    | "source" | "target" | "map" | "uses" | "alias" | "let" | "group" | "extends"

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
