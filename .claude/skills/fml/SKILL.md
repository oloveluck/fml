---
name: fml
description: Write, check and run FHIR Mapping Language (FML) maps without an MCP server — `raco fml` (compile to a StructureMap, transform a resource, check a map against its structures and validate its results), logical models via --structures, StructureMaps read back as maps, `#lang fml` modules, maps by canonical and StructureMap/$transform as an operation. Use when writing or debugging a .map/.fml file, converting FHIR or logical-model data with a map, or reading a StructureMap.
---

# FML without MCP

Read [`guide.md`](../../../guide.md) first: the language in brief, a checked cheat sheet and worked
example, what the checker reports, and the common problems (that guide is what fhir-mcp serves as
`learn://fml`). This skill is the workflow around it: the commands, the loop, and the Racket API.

`fml` and `raco fml` are the same tool (the standalone build is `dist/bin/fml`).

## The loop
1. **Check against the structures.** `raco fml check map.map` reports, at each rule's line, an
   element a type does not have, a value that cannot go into an element, a wrong group call, and (a
   warning) a required target element no rule assigns:
   ```
   bad.fml:8:3: fml check error: `subjct` is not an element of QuestionnaireResponse (did you mean `subject`?)
   nocode.map:5:1: fml check warning: Observation.code is required (min 1) but no rule of main assigns it
   ```
   Exit 1 on an error, 0 with only warnings. `--format json` puts the findings on stdout as JSON
   (`line`, `column`, `message`, `severity`); `--format outcome` as an OperationOutcome.
2. **Transform one source.** `raco fml map.map --transform source.json` prints the target JSON. The
   engine reads R5; a source from another version is read leniently.
3. **Validate the results.** `raco fml check map.map --examples ex/ [--profile profile.json]`
   transforms each example (a file or a directory of them) and validates the result against the
   target structure and the profile; a problem is traced to the rule that wrote the element:
   ```
   nocode.map: fml validate error: example 1: Observation.code: minimum required = 1, but found 0
   nocode.map: 2 findings, 1 example validated
   ```
4. **Narrow it down.** A target element missing from the result means its rule's source matched
   nothing: transform once with the rule's `where` removed, or split the rule, and compare.

**Logical models.** Give their StructureDefinitions with `--structures <file|dir>` (to `check` and
`--transform` alike); the source instance names its type in `resourceType`:
```
cd tests/vendor/matchbox/tutorial/step1
raco fml map/step1.map --transform source/source1.json --structures logical
{"a":"step1-demo","resourceType":"TRight"}
```

**The StructureMap form.** `raco fml map.map` prints the StructureMap; a StructureMap `.json` is
read back wherever a `.map` is (`raco fml sm.json --transform src.json`, `raco fml check sm.json`).

## From Racket
A map starting with `#lang fml` is a module; requiring it registers its StructureMap by canonical:
```racket
#lang racket/base
(require json "qr2pat.map")
(transform (call-with-input-file "qr.json" read-json))    ; '#hash((gender . "female") ...)
(hash-ref structuremap 'url)
```
`racket qr2pat.map` prints the StructureMap. A map by canonical, and the FHIR operation:
```racket
(require fml/registry fml/operations fhir/operation (only-in fhir/conformance/operations ensure-valid))
(define to-patient (structure-map "http://example.org/StructureMap/qr2patient"))   ; registered, data source, packages
(list (smap-source to-patient) (smap-target to-patient))   ; '(QuestionnaireResponse Patient)
(ensure-valid (to-patient qr))                             ; transform, then validate
(apply-operation "StructureMap/$transform" qr #:srcMap fml-text)
(apply-operation "StructureMap/$transform" qr #:source "http://example.org/StructureMap/qr2patient")
```
`(compose-maps a b)` is `b` after `a`, refused unless `a`'s target is `b`'s source. Lower level:
`fml/parser/parse` (`parse-fml`), `fml/structuremap` (`fml->structuremap`, `structuremap->fml`),
`fml/check` (`check-map`, `check-map/examples`), `fml/transform` (`run-map` over a typed node; load
`fhir/r5/schemas` first, and call `uses-types` before reading a logical-model source).

The entry group's source type is checked: an Observation given to a map whose group takes a
QuestionnaireResponse is an error at the group (`the source is of type Observation, but group
BpIntake takes src : QuestionnaireResponse`), exit 1.

## Working on the engine itself
`CLAUDE.md` is the architecture. `raco test .` runs everything; `raco fml --tck` runs the corpora
with their floors (fhir-test-cases, matchbox's maps, the tutorial, the StructureMap round trip, the
check); `TESTING.md` has the numbers. `tests/guide-test.rkt` keeps guide.md's examples true and
`tests/docs-test.rkt` the manual's references.
