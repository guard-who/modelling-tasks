{-# LANGUAGE NamedFieldPuns #-}

module Modelling.PetriNet.BasicNetFunctions where

import Modelling.PetriNet.Types

import Control.Concurrent
  (forkIO, killThread, newEmptyMVar, putMVar, takeMVar)
import Control.Monad
import System.Exit                      (ExitCode (..))
import System.IO                        (hGetContents)
import System.Process
  (CreateProcess (..), StdStream (..), createProcess, proc, waitForProcess)


instanceInput :: IO Int
instanceInput = do
  putStr "Index of wanted Instance: "
  read <$> getLine

checkBasicConfig :: BasicConfig -> Maybe String
checkBasicConfig BasicConfig{places,transitions,atLeastActive
                   , minTokensOverall,maxTokensOverall,maxTokensPerPlace
                   , minFlowOverall,maxFlowOverall,maxFlowPerEdge}
 | places <= 0
  = Just "The number of places must be positive."
 | places > 8
  = Just "Cannot deal with more than 8 places."
 | transitions <= 0
  = Just "The number of transitions must be positive."
 | transitions > 8
  = Just "Cannot deal with more than 8 transitions."
 | atLeastActive < 0
  = Just "The parameter 'atLeastActive' must be non-negative."
 | atLeastActive > transitions
  = Just "There cannot be more active transitions than there are transitions."
 | minTokensOverall < 0
  = Just "The parameter 'minTokensOverall' must be non-negative."
 | maxTokensOverall < minTokensOverall
  = Just "The parameter 'minTokensOverall' must not be larger than 'maxTokensOverall'."
 | maxTokensPerPlace < 0
  = Just "The parameter 'maxTokensPerPlace' must be non-negative."
 | maxTokensPerPlace > maxTokensOverall
  = Just "The parameter 'maxTokensPerPlace' must not be larger than 'maxTokensOverall'."
 | maxTokensOverall > places * maxTokensPerPlace
  = Just "The parameter 'maxTokensOverall' is set unreasonably high, given the per-place parameter."
 | minFlowOverall < 0
  = Just "The parameter 'minFlowOverall' must be non-negative."
 | maxFlowOverall < minFlowOverall
  = Just "The parameter 'minFlowOverall' must not be larger than 'maxFlowOverall'."
 | maxFlowPerEdge <= 0
  = Just "The parameter 'maxFlowPerEdge' must be positive."
 | maxFlowOverall < maxFlowPerEdge
  = Just "The parameter 'maxFlowPerEdge' must not be larger than 'maxFlowOverall'."
 | maxFlowOverall > 2 * places * transitions * maxFlowPerEdge
  = Just "The parameter 'maxFlowOverall' is set unreasonably high, given the other parameters."
 | transitions + places > 1 + minFlowOverall 
  = Just "The number of transitions and places exceeds 'minFlowOverall' too much to create a connected net."
 | otherwise
  = Nothing
  
checkChangeConfig :: BasicConfig -> ChangeConfig -> Maybe String
checkChangeConfig BasicConfig
                   {places,transitions
                   , minTokensOverall,maxTokensOverall,maxTokensPerPlace
                   , minFlowOverall,maxFlowOverall,maxFlowPerEdge}
                ChangeConfig
                   {tokenChangeOverall, flowChangeOverall
                   , maxFlowChangePerEdge, maxTokenChangePerPlace}
                 
 | tokenChangeOverall < 0
  = Just "The parameter 'tokenChangeOverall' must be non-negative."
 | maxTokenChangePerPlace < 0
  = Just "The parameter 'maxTokenChangePerPlace' must be non-negative."
 | maxTokenChangePerPlace > tokenChangeOverall
  = Just "The parameter 'maxTokenChangePerPlace' must not be larger than 'tokenChangeOverall'."
 | maxTokenChangePerPlace > maxTokensPerPlace
  = Just "The parameter 'maxTokenChangePerPlace' must not be larger than 'maxTokensPerPlace'."
 | tokenChangeOverall > maxTokensOverall - minTokensOverall
  = Just "With 'tokenChangeOverall', stay within the range of tokens overall."
 | maxTokenChangePerPlace * places < tokenChangeOverall
  = Just "The parameter 'tokenChangeOverall' is set unreasonably high, given the per-place parameter."
 | flowChangeOverall < 0
  = Just "The parameter 'flowChangeOverall' must be non-negative."
 | maxFlowChangePerEdge < 0
  = Just "The parameter 'maxFlowChangePerEdge' must be non-negative."
 | maxFlowChangePerEdge > flowChangeOverall
  = Just "The parameter 'maxFlowChangePerEdge' must not be larger than 'flowChangeOverall'."
 | maxFlowChangePerEdge > maxFlowPerEdge
  = Just "The parameter 'maxFlowChangePerEdge' must not be larger than 'maxFlowPerEdge'."
 | flowChangeOverall > maxFlowOverall - minFlowOverall
  = Just "With 'flowChangeOverall', stay within the range of flow overall."
 | 2 * places * transitions * maxFlowChangePerEdge < flowChangeOverall
  = Just "The parameter 'flowChangeOverall' is set unreasonably high, given the other parameters."
 | otherwise
  = Nothing
  
checkCConfig :: BasicConfig  -> Maybe String
checkCConfig BasicConfig{atLeastActive}
 | atLeastActive < 2
  = Just "The parameter 'atLeastActive' must be at least 2 to create the task." 
 | otherwise = Nothing
 
renderPdfFile :: String -> IO String
renderPdfFile file = do
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