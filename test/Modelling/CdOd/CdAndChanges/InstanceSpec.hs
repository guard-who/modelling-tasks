module Modelling.CdOd.CdAndChanges.InstanceSpec where

import qualified Data.ByteString.Char8            as BS (pack)

import Modelling.Common                 (withUnitTests)
import Modelling.CdOd.CdAndChanges.Instance (
  fromInstanceWithNameOverlap,
  nameClassDiagramInstance,
  )
import Control.Monad                    ((>=>))
import Language.Alloy.Debug             (parseInstance)
import Test.Hspec

spec :: Spec
spec =
  withUnitTests "getChangesAndCds" does dir "hs" $ shouldReturn . getResult
  where
    does = "generates expected class diagrams"
    dir = "test/unit/Modelling/CdOd/CdAndChanges/Instance"
    getResult = parseInstance
      . BS.pack
      >=> fmap show . (fromInstanceWithNameOverlap >=> nameClassDiagramInstance)
