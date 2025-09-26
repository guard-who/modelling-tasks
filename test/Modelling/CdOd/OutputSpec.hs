module Modelling.CdOd.OutputSpec where

import Modelling.CdOd.CdAndChanges.Instance (
  GenericClassDiagramInstance (..),
  fromInstance,
  )

import qualified Data.ByteString.Char8            as BS (
  pack,
  readFile,
  writeFile,
  unpack
  )
import Capabilities.Diagrams.IO         ()
import Capabilities.Graphviz.IO         ()
import Capabilities.WriteFile.IO        ()
import Modelling.CdOd.Output            (drawCd, drawOdFromInstance)
import Modelling.CdOd.Types             (defaultCdDrawSettings)
import Modelling.Common                 (withUnitTestsUsingPath)

import Control.Monad                    (void)
import Control.Monad.Except             (runExceptT)
import Control.Monad.Random             (evalRandT)
import Data.GraphViz                    (DirType (Forward))
import Test.Hspec                       (Spec)
import Test.Similarity                  (Deviation (..), shouldReturnSimilar)
import System.IO.Extra                  (withTempFile)
import System.Random                    (mkStdGen)
import Language.Alloy.Debug             (parseInstance)

spec :: Spec
spec = do
  withUnitTestsUsingPath "drawCd" (draws "class") dir "svg"
    $ \file -> shouldReturnSimilar' (Just file) . fmap BS.unpack . drawCdInstance
  withUnitTestsUsingPath "drawOd" (draws "object") dir "svg"
    $ \file -> shouldReturnSimilar' (Just file) . fmap BS.unpack . drawOdInstance
  where
    shouldReturnSimilar' f = shouldReturnSimilar
      f
      200
      Deviation {absoluteDeviation = 20, relativeDeviation = 0.2}
    draws what = "draws roughly the expected " ++ what ++ " diagram"
    dir = "test/unit/Modelling/CdOd/Output"
    drawCdInstance alloy = withTempFile $ \file -> do
      Right alloyInstance <- runExceptT $ parseInstance (BS.pack alloy)
      Right cd <- return $ instanceClassDiagram <$> fromInstance alloyInstance
      renderedCd <- drawCd defaultCdDrawSettings mempty cd
      BS.writeFile file renderedCd
      BS.readFile file
    drawOdInstance alloy = withTempFile $ \file -> do
      Right alloyInstance <- runExceptT $ parseInstance (BS.pack alloy)
      let possibleLinks = map (: []) ['w'..'y']
      void $ flip evalRandT
        (mkStdGen 0)
        $ drawOdFromInstance
          alloyInstance
          Nothing
          possibleLinks
          (Just 1)
          Forward
          True
          file
      BS.readFile file
