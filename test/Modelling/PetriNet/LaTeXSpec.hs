module Modelling.PetriNet.LaTeXSpec where

import Modelling.PetriNet.LaTeX
import Modelling.PetriNet.Types         (defaultPetri)

import Control.Concurrent
  (forkIO, killThread, newEmptyMVar, putMVar, takeMVar)
import Control.Monad
import System.Exit                      (ExitCode (..))
import System.IO                        (hGetContents)
import System.Process
  (CreateProcess (..), StdStream (..), createProcess, proc, waitForProcess)
import Test.Hspec
import Text.LaTeX                       (renderFile)

spec :: Spec
spec =
  describe "uebung" $
    context "takes a parsed PetriNet, chosen Task and Task-Type" $
      it " and creates coresponding LaTeX-Data for the mathematical notation" $ do
        out <- renderFile "test.tex" (uebung defaultPetri 1 True) >> renderPdf "test.tex"
        out `shouldContain` "Output written on test.pdf"

renderPdf :: String -> IO String
renderPdf file = do
  let callLatex = proc "pdflatex" ["-halt-on-error", file]
  (_, Just hout, Just herr, ph) <- createProcess callLatex {
      std_out = CreatePipe,
      std_err = CreatePipe
    }
  pout <- listenForOutput hout
  perr <- listenForOutput herr
  out <- getOutput pout
  err <- getOutput perr
  code <- waitForProcess ph
  unless (null err) $ errorMessage err
  (if code == ExitSuccess
    then return
    else errorMessage) out
  where
    listenForOutput h = do
      mvar <- newEmptyMVar
      pid <- forkIO $ hGetContents h >>= putMVar mvar
      return (pid, mvar)
    getOutput (pid, mvar) = do
      output <- takeMVar mvar
      killThread pid
      return output
    errorMessage err = error $
      "Failed creating pdf for LaTeX-File: " <> file <> "\n" <> err
