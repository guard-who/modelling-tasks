module Modelling.CdOd.OutputSpec where

import Modelling.CdOd.CdAndChanges.Instance (
  GenericClassDiagramInstance (..),
  fromInstance,
  )

import Capabilities.Diagrams.IO         ()
import Capabilities.Graphviz.IO         ()
import Modelling.CdOd.Output            (drawCd, drawOdFromInstance)
import Modelling.CdOd.Types             (defaultCdDrawSettings)
import Modelling.Common                 (withUnitTestsUsingPath)

import Control.Monad                    (void)
import Control.Monad.Except             (runExceptT)
import Control.Monad.Random             (evalRandT)
import Data.ByteString.Char8            (pack)
import Data.GraphViz                    (DirType (Back))
import Test.Hspec                       (Spec)
import Test.Similarity                  (Deviation (..), shouldReturnSimilar)
import System.IO.Extra                  (withTempFile)
import System.Random                    (mkStdGen)
import Language.Alloy.Debug             (parseInstance)

spec :: Spec
spec = do
  withUnitTestsUsingPath "drawCd" (draws "class") dir "svg"
    $ \file -> shouldReturnSimilar' (Just file) . drawCdInstance
  withUnitTestsUsingPath "drawOd" (draws "object") dir "svg"
    $ \file -> shouldReturnSimilar' (Just file) . drawOdInstance
  where
    shouldReturnSimilar' f = shouldReturnSimilar
      f
      200
      Deviation {absoluteDeviation = 20, relativeDeviation = 0.2}
    draws what = "draws roughly the expected " ++ what ++ " diagram"
    dir = "test/unit/Modelling/CdOd/Output"
    drawCdInstance alloy = withTempFile $ \file -> do
      Right alloyInstance <- runExceptT $ parseInstance (pack alloy)
      Right cd <- return $ instanceClassDiagram <$> fromInstance alloyInstance
      void $ drawCd defaultCdDrawSettings mempty cd file
      readFile file
    drawOdInstance alloy = withTempFile $ \file -> do
      Right alloyInstance <- runExceptT $ parseInstance (pack alloy)
      void $ flip evalRandT
        (mkStdGen 0)
        $ drawOdFromInstance alloyInstance undefined (Just 1) Back True file
      readFile file
