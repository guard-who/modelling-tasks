# modelling-tasks [![Haskell CI](https://github.com/fmidue/modelling-tasks/workflows/Haskell%20CI/badge.svg)](https://github.com/fmidue/modelling-tasks/actions?query=workflow%3A%22Haskell+CI%22+branch%3Amaster)

This repository contains exercise tasks for modelling lecture contents.

The diagram types covered are

* Activity diagram
* Class diagram
* Object diagram
* Petri net

## Generate instance in GHCi

Task types usually have a consistent naming scheme.
Therefore it should be possible to perform the following steps accordingly for all other tasks in order to test a certain task.
The example her is for `NameCdError` (i.e. module `Modelling.CdOd.NameCdError`).
For German versions change `English` to `German`.

``` sh
stack ghci
```

``` haskell
:m +Control.Monad.Output Control.Monad.Output.Generic
inst <- nameCdErrorGenerate defaultNameCdErrorConfig 0 0
runLangMReport (return ()) (>>) (nameCdErrorTask "/tmp/" inst) >>= \(Just (), x) -> (x English :: IO ())
runLangMReport (return ()) (>>) (nameCdErrorSyntax inst NameCdErrorAnswer {reason = 'b', dueTo = [1,2,4]}) >>= \(Just (), x) -> (x English :: IO ())
runLangMReport (return ()) (>>) (nameCdErrorEvaluation inst NameCdErrorAnswer {reason = 'b', dueTo = [1,2,4]}) >>= \(r, x) -> (x English :: IO ()) >> return r :: IO (Maybe Rational)
```

For running all steps using ghci, this approach is also possible:

``` haskell
:m +Control.Monad.Output.Debug System.Random Text.Parsec
let getLines = init <$> getLines' where getLines' = do { x <- getLine; if null x then pure [] else (\l -> x ++ '\n' : l) <$> getLines' }
testTask English (randomRIO (0,1000) >>= nameCdErrorGenerate defaultNameCdErrorConfig 0) (nameCdErrorTask "/tmp/") nameCdErrorSyntax nameCdErrorEvaluation (either (error . show) id . parse parseNameCdErrorAnswer "" <$> getLines)
```

Please also note, that the `..Task`, `..Syntax`, and `..Evaluation` functions sometimes require arguments for a directory (above `"/tmp/"`) and sometimes don't.
