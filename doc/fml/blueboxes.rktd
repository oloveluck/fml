1260
((3) 0 () 5 ((q lib "fml/check.rkt") (q lib "fml/project.rkt") (q lib "fml/transform.rkt") (q lib "fml/registry.rkt") (q lib "fml/structuremap.rkt")) () (h ! (equal) ((c def c (c (? . 0) q check-map/examples)) q (1974 . 9)) ((c def c (c (? . 2) q run-map)) q (906 . 4)) ((c def c (c (? . 0) q check-map)) q (1834 . 4)) ((q def ((lib "fml/operations.rkt") $transform)) q (2881 . 2)) ((c def c (c (? . 3) q smap-target)) q (1638 . 3)) ((c def c (c (? . 4) q fml->structuremap)) q (778 . 3)) ((c def c (c (? . 1) q parse-map-file)) q (2353 . 3)) ((c def c (c (? . 1) q check-project-map)) q (2523 . 9)) ((c def c (c (? . 2) q register-structure-map!)) q (1163 . 3)) ((c def c (c (? . 1) q map-structuremap)) q (2461 . 3)) ((c def c (c (? . 2) q current-fml-log)) q (1044 . 4)) ((c def c (c (? . 3) q smap-source)) q (1570 . 3)) ((c def c (c (? . 4) q structuremap->fml)) q (841 . 3)) ((c def c (c (? . 3) q structure-map)) q (1387 . 4)) ((c def c (c (? . 2) q uses-types)) q (988 . 3)) ((c def c (c (? . 3) q smap-ast)) q (1706 . 3)) ((c def c (c (? . 2) q map-imports)) q (1317 . 3)) ((c def c (c (? . 2) q resolve-structure-map)) q (1232 . 3)) ((q def ((lib "fml/parser/parse.rkt") parse-fml)) q (639 . 4)) ((c def c (c (? . 3) q compose-maps)) q (1760 . 4))))

raco fml map.fml                                                     the StructureMap JSON
raco fml map.fml –transform src.json [–structures sds/]              run the map; print the target
raco fml check map.fml [–structures sds/] [–examples ex/] [–profile  the map against its structures; examples transformed and the results
sd.json]                                                             validated
raco fml –parse-corpus dir/                                          parse every .map under a directory
raco fml –tck                                                        the vendored corpora, with floors
procedure
(parse-fml src [#:source source]) -> (or/c ok? err?)
  src : string?
  source : (or/c string? path? #f) = #f
procedure
(fml->structuremap m) -> hash?
  m : fml-map?
procedure
(structuremap->fml sm) -> fml-map?
  sm : hash?
procedure
(run-map m source) -> hash?
  m : fml-map?
  source : fhir?
procedure
(uses-types m) -> hash?
  m : fml-map?
parameter
(current-fml-log) -> (-> string? any?)
(current-fml-log log) -> void?
  log : (-> string? any?)
procedure
(register-structure-map! m) -> void?
  m : fml-map?
procedure
(resolve-structure-map url) -> (or/c fml-map? #f)
  url : string?
procedure
(map-imports m) -> (listof fml-map?)
  m : fml-map?
procedure
(structure-map ref [#:structures structures]) -> smap?
  ref : (or/c string? fml-map? hash?)
  structures : (or/c #f data-source? (listof hash?)) = #f
procedure
(smap-source m) -> (or/c symbol? #f)
  m : smap?
procedure
(smap-target m) -> (or/c symbol? #f)
  m : smap?
procedure
(smap-ast m) -> fml-map?
  m : smap?
procedure
(compose-maps a b) -> smap?
  a : smap?
  b : smap?
procedure
(check-map m [#:source source]) -> (listof diagnostic?)
  m : fml-map?
  source : (or/c string? path? #f) = #f
procedure
(check-map/examples  m                       
                     examples                
                    [#:profiles profiles     
                     #:source source])   -> (listof diagnostic?)
  m : fml-map?
  examples : (listof hash?)
  profiles : (listof hash?) = '()
  source : (or/c string? path? #f) = #f
procedure
(parse-map-file file) -> (or/c fml-map? #f) (listof diagnostic?)
  file : path-string?
procedure
(map-structuremap m) -> hash?
  m : fml-map?
procedure
(check-project-map  m                     
                    examples              
                    profiles              
                   [#:source source]) -> (listof diagnostic?)
  m : fml-map?
  examples : (listof hash?)
  profiles : (listof hash?)
  source : (or/c string? path? #f) = #f
value
$transform : procedure?
