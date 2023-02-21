module Modelling.CdOd.OutputSpec where

import Modelling.CdOd.CdAndChanges.Instance (
  ClassDiagramInstance (..), fromInstance)
import Modelling.CdOd.Output            (drawCd)
import Modelling.Common                 (withUnitTests)

import Control.Monad                    (void)
import Control.Monad.Except             (runExceptT)
import Data.ByteString.Char8            (pack)
import Test.Hspec
import System.IO.Extra                  (withTempFile)
import Language.Alloy.Debug             (parseInstance)

spec :: Spec
spec = do
  withUnitTests "drawCd" does dir "svg" $ shouldReturn . getResult
  where
    does = "draws expected class diagram"
    dir = "test/unit/Modelling/CdOd/Output"
    getResult alloy = withTempFile $ \file -> do
      Right alloyInstance <- runExceptT $ parseInstance (pack alloy)
      Right cd <- return $ instanceClassDiagram <$> fromInstance alloyInstance
      void $ drawCd True True mempty cd file
      readFile file
