# modelling-tasks [![Haskell CI](https://github.com/fmidue/modelling-tasks/workflows/Haskell%20CI/badge.svg)](https://github.com/fmidue/modelling-tasks/actions?query=workflow%3A%22Haskell+CI%22+branch%3Amaster)

This repository contains exercise tasks for modelling lecture contents.

The diagram types covered are

* Activity diagram (UML)
* Class diagram (UML)
* Object diagram (UML)
* Petri net

## Compatibility

On Windows, you may have to use `SAT4J` instead of `MiniSat`.
In order to do so you can change the provided flag in `stack.yaml` or `stack-*.yaml` to:

``` yaml
    alloy-use-sat4j: true
```

Or provide it as argument to each call of `stack`, e.g. `stack build --flag modelling-tasks:alloy-use-sat4j`.

LaTeX and Graphviz need to be installed on the system.

## Generating instances in GHCi

Task types usually have a consistent naming scheme.
Therefore it should be possible to perform the following steps accordingly for all other tasks in order to test a certain task.
The example here is for `NameCdError` (i.e. module `Modelling.CdOd.NameCdError`).
For German versions, change `English` to `German`.

``` sh
stack ghci --stack-yaml=stack-examples.yaml  --package=autotool-capabilities-io-instances
```

``` haskell
:m + Capabilities.Alloy.IO Capabilities.Cache.IO Capabilities.Diagrams.IO Capabilities.Graphviz.IO Capabilities.PlantUml.IO
:m + Control.OutputCapable.Blocks Control.OutputCapable.Blocks.Generic
inst <- nameCdErrorGenerate defaultNameCdErrorConfig 0 0
runLangMReport (return ()) (>>) (nameCdErrorTask "/tmp/" inst) >>= \(Just (), x) -> (x English :: IO ())
runLangMReport (return ()) (>>) (nameCdErrorSyntax inst NameCdErrorAnswer {reason = 'b', dueTo = [1,2,4]}) >>= \(Just (), x) -> (x English :: IO ())
runLangMReport (return ()) (>>) (nameCdErrorEvaluation inst NameCdErrorAnswer {reason = 'b', dueTo = [1,2,4]}) >>= \(r, x) -> (x English :: IO ()) >> return r :: IO (Maybe Rational)
```

For running all steps at once in `ghci`, the following approach is also possible:

``` haskell
:m + Control.OutputCapable.Blocks.Debug System.Random Text.Parsec
let getLines = init <$> getLines' where getLines' = do { x <- getLine; if null x then pure [] else (\l -> x ++ '\n' : l) <$> getLines' }
testTask Nothing English (randomRIO (0,1000) >>= nameCdErrorGenerate defaultNameCdErrorConfig 0) (nameCdErrorTask "/tmp/") nameCdErrorSyntax nameCdErrorEvaluation (either (error . show) id . parse parseNameCdErrorAnswer "" <$> getLines)
```

Please also note that the `..Task`, `..Syntax`, and `..Evaluation` functions sometimes require arguments for a directory (above `"/tmp/"`) and sometimes don't.

In order to view configurations and instances formatted more nicely, you may use `pPrint`, e.g.:

``` haskell
:m + Text.Pretty.Simple
pPrint defaultNameCdErrorConfig
inst <- nameCdErrorGenerate defaultNameCdErrorConfig 0 0
pPrint inst
```
