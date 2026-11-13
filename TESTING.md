# Testing fml

Every corpus is vendored with its source and license (`tests/vendor/*/SOURCE.txt`) and held to a
floor that only rises. `raco fml --tck` runs them with their reports.

## Results

| Corpus | What it checks | Result |
|---|---|---|
| fhir-test-cases `r5/structure-mapping` | the executable JSON tests run and match; every map parses | 6/6 run, 15/15 parse |
| matchbox's mapping-language tests (ahdis/matchbox) | every map parses; the top-level maps run over their sources | 43/43 parse, 17/17 run |
| the FHIR mapping-language tutorial (13 steps over logical models) | each step's results against the expected ones | 37/37 |
| … run from the StructureMaps this package emits, by canonical | the same results, from the resources rather than the FML | 37/37 |
| … run from the reference's StructureMap resources | the same results, from matchbox's own `map/*.json` | 29/37 |
| every corpus map through its StructureMap and back | FML → StructureMap → FML gives the same map | 58/58 |
| our StructureMap against the reference's, rule by rule | names, documentation and R4-isms normalised | 21/31 |
| every corpus map, and every StructureMap resource matchbox ships, written as the specified language (`fml/print`) | the text parses, means the same StructureMap, is written the same again | 58/58 maps, 25/25 resources |
| HL7's own StructureMap examples (the R4 and R5 core packages and CRMI, when fhir-spec is installed; `tests/print-test.rkt`) | the same laws | 8/8 |
| `check` over every corpus map | no error finding on maps that fit their structures | 55/58 |
| the FHIR R3/R4 inter-version maps (FHIR/interversion, CI only, not vendored) | every map parses | 332/332 |
| The specification's forms and transforms (`tests/spec-test.rkt`, `tests/alignment-test.rkt`) | each form the specification gives is read, runs and is carried by the StructureMap; each transform of its table but `dateOp` does what its definition says; what is read beyond it is said | passes |
| The guide (`tests/guide-test.rkt`) | every FML example in guide.md parses and checks clean; the cheat sheet and worked example give the results the text states; the quoted findings are the checker's | passes |
| The manual (`tests/docs-test.rkt`) | scribblings/fml.scrbl renders with no broken reference to its own sections or bindings | passes |

## The misses, named

- **Two tutorial result files are not the reference's expectations**, and the harness says why with
  the evidence from matchbox's test class (`tests/matchbox-harness.rkt`, `reference-overrides`):
  `step4b.map` is never run there, and `step6b` over `source6b` is expected to throw.
- **From the reference's StructureMaps, 29/37**: several of those resources predate their `.map`
  (stale conditions, a missing rule). It was 30 until a condition was read as FHIRPath reads a
  collection where a Boolean is expected (one item that is no Boolean holds): `step4b.json`'s
  condition is a quoted string, `'a21.isInteger()'`, so its rule now copies, as it would in the
  reference; the count had included it while every condition that was not exactly `true` was false
  (which kept matchbox's `where $this.resource.ofType(FHIR.Patient)` maps from writing anything).
  The reference never runs `step4b`.
- **Rule by rule, 21/31**: the rest are the same stale conditions and the reference's synthetic
  variables.
- **`check`, 55/58**: the three findings are real. `contributor` and `Composition.code` are R4
  elements that R5 dropped, and `identifer` is a typo in `syntax.map`.

## Running them

```sh
raco test .                      # unit tests
raco fml --tck                   # all the corpora, with the floors
raco fml --parse-corpus dir/     # parse every .map under a directory (the inter-version maps in CI)
```
