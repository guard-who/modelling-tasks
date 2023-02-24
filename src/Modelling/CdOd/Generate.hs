module Modelling.CdOd.Generate (
  generateCds,
  instanceToCd,
  ) where

import qualified Data.Bimap                       as BM (
  fromList,
  )

import Modelling.CdOd.Auxiliary.Util    (getInstances)
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
  renameClassesAndRelationshipsInCd,
  )

import Control.Monad.IO.Class           (MonadIO (liftIO))
import Control.Monad.Random             (MonadRandom)
import Data.Bifunctor                   (first)
import Data.Maybe                       (mapMaybe)
import Language.Alloy.Call              (AlloyInstance)
import System.Random.Shuffle            (shuffleM)

generateCds
  :: (MonadIO m, MonadRandom m)
  => Maybe Bool
  -> ClassConfig
  -> RelationshipProperties
  -> Maybe Integer
  -> Maybe Int
  -> m [AlloyInstance]
generateCds withNonTrivialInheritance config props maxInsts to = do
  let alloyCode = transformNoChanges config props withNonTrivialInheritance
  instas  <- liftIO $ getInstances maxInsts to alloyCode
  shuffleM instas

instanceToCd :: AlloyInstance -> Either String Cd
instanceToCd rinsta = do
  cd <- instanceClassDiagram <$> fromInstance rinsta
  let cns = BM.fromList $ zip (classNames cd) $ map pure ['A'..]
      relationshipNames = mapMaybe relationshipName $ relationships cd
      rns = BM.fromList $ zip relationshipNames $ map pure ['z', 'y' ..]
  first show $ renameClassesAndRelationshipsInCd cns rns cd
