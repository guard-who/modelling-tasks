module Modelling.CdOd.Generate (
  generateCds,
  instanceToAnyCd,
  instanceToCd,
  ) where

import qualified Data.Bimap                       as BM (
  fromList,
  )

import Capabilities.Alloy               (MonadAlloy, getInstances)
import Modelling.CdOd.CdAndChanges.Instance (
  GenericClassDiagramInstance (..),
  fromInstance,
  )
import Modelling.CdOd.CdAndChanges.Transform (
  transformNoChanges,
  )
import Modelling.CdOd.Types (
  AnyCd,
  AnyClassDiagram (..),
  Cd,
  ClassConfig (..),
  RelationshipProperties,
  anyClassNames,
  anyRelationshipName,
  renameClassesAndRelationships,
  toValidCd,
  )

import Control.Monad.Catch              (MonadThrow)
import Control.Monad.Random             (MonadRandom)
import Data.Maybe                       (mapMaybe)
import Language.Alloy.Call              (AlloyInstance)
import System.Random.Shuffle            (shuffleM)

generateCds
  :: (MonadAlloy m, MonadRandom m)
  => Maybe Bool
  -> ClassConfig
  -> RelationshipProperties
  -> Maybe Integer
  -> Maybe Int
  -> m [AlloyInstance]
generateCds withNonTrivialInheritance config props maxInstances to = do
  let alloyCode = transformNoChanges config props withNonTrivialInheritance
  alloyInstances <- getInstances maxInstances to alloyCode
  shuffleM alloyInstances

instanceToAnyCd :: MonadThrow m => AlloyInstance -> m AnyCd
instanceToAnyCd alloyInstance = do
  cd <- instanceClassDiagram <$> fromInstance alloyInstance
  let classRenamingMap = BM.fromList $ zip (anyClassNames cd) $ map pure ['A'..]
      relationshipNames = mapMaybe anyRelationshipName $ anyRelationships cd
      relationshipRenamingMap =
        BM.fromList $ zip relationshipNames $ map pure ['z', 'y' ..]
  renameClassesAndRelationships classRenamingMap relationshipRenamingMap cd

instanceToCd :: MonadThrow m => AlloyInstance -> m Cd
instanceToCd alloyInstance = instanceToAnyCd alloyInstance >>= toValidCd
