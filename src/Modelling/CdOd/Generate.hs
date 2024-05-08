module Modelling.CdOd.Generate (
  generateCds,
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
  Cd,
  ClassConfig (..),
  ClassDiagram (..),
  RelationshipProperties,
  relationshipName,
  renameClassesAndRelationships,
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
  instas <- getInstances maxInstances to alloyCode
  shuffleM instas

instanceToCd :: MonadThrow m => AlloyInstance -> m Cd
instanceToCd alloyIntance = do
  cd <- instanceClassDiagram <$> fromInstance alloyIntance
  let classRenamingMap = BM.fromList $ zip (classNames cd) $ map pure ['A'..]
      relationshipNames = mapMaybe relationshipName $ relationships cd
      relationshipRenamingMap =
        BM.fromList $ zip relationshipNames $ map pure ['z', 'y' ..]
  renameClassesAndRelationships classRenamingMap relationshipRenamingMap cd
