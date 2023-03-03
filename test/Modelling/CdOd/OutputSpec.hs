module Modelling.CdOd.OutputSpec where

import Modelling.CdOd.CdAndChanges.Instance (
  GenericClassDiagramInstance (..),
  fromInstance,
  )
import Modelling.CdOd.Output            (drawCd, drawOdFromInstance)
import Modelling.Common                 (withUnitTests)

import Control.Monad                    (void)
import Control.Monad.Except             (runExceptT)
import Control.Monad.Random             (evalRandT)
import Data.ByteString.Char8            (pack)
import Data.GraphViz                    (DirType (Back))
import Test.Hspec
import System.IO.Extra                  (withTempFile)
import System.Random                    (mkStdGen)
import Language.Alloy.Debug             (parseInstance)

spec :: Spec
spec = do
  withUnitTests "drawCd" does dir "svg" $ shouldReturn . drawCdInstance
  withUnitTests "drawOd" does dir "svg" $ shouldReturn . drawOdInstance
  where
    does = "draws expected class diagram"
    dir = "test/unit/Modelling/CdOd/Output"
    drawCdInstance alloy = withTempFile $ \file -> do
      Right alloyInstance <- runExceptT $ parseInstance (pack alloy)
      Right cd <- return $ instanceClassDiagram <$> fromInstance alloyInstance
      void $ drawCd True True mempty cd file
      readFile file
    drawOdInstance alloy = withTempFile $ \file -> do
      Right alloyInstance <- runExceptT $ parseInstance (pack alloy)
      void $ flip evalRandT
        (mkStdGen 0)
        $ drawOdFromInstance alloyInstance (Just 1) Back True file
      readFile file
