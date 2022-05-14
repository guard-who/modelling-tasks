module Main where

import qualified Language.Alloy.Debug as AD (parseInstance)
import qualified Data.ByteString as B (readFile)

import System.Environment (getArgs, withArgs)
import Text.Pretty.Simple (pPrint)

import AD_Instance (parseInstance)
import AD_PlantUMLConverter(convertToPlantUML)
import AD_Petrinet (PetriKey(..), convertToPetrinet)

import Modelling.PetriNet.Diagram (cacheNet)
import Data.GraphViz.Commands (GraphvizCommand(..))
import Control.Monad.Except(runExceptT)

main :: IO ()
main = do
  xs <- getArgs
  case xs of
    ownscope:incscope:f:path:xs' -> do
      inst <- B.readFile f
      let ad = failWith id . parseInstance ownscope incscope . failWith show
            $ AD.parseInstance inst
          petri = convertToPetrinet ad
      pPrint ad
      pPrint petri
      _ <- runExceptT $ cacheNet path (show . label) petri False False False Dot
      pPrint $ convertToPlantUML ad
    _ -> error "usage: three parameters required: String (scope of source) String (scope of include) FilePath (Alloy instance)"

failWith :: (a -> String) -> Either a c -> c
failWith f = either (error . f) id