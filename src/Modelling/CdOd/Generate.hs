module Modelling.CdOd.Generate (
  generateCds,
  instanceToCd,
  ) where

import qualified Data.Bimap                       as BM (
  fromList,
  )

import qualified Control.Monad                    as Debug
import qualified Data.ByteString                  as Debug
import qualified Data.ByteString.Lazy.UTF8        as Debug
import qualified Language.Alloy.Call              as Debug
import qualified Language.Alloy.Debug             as Debug
import qualified Data.Digest.Pure.SHA             as Debug (sha1, showDigest)

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
  renameClassesAndRelationships,
  )

import Control.Monad.IO.Class           (MonadIO (liftIO))
import Control.Monad.Random             (MonadRandom)
import Data.Bifunctor                   (first)
import Data.Maybe                       (mapMaybe)
import Language.Alloy.Call              (AlloyInstance)
import System.Random.Shuffle            (shuffleM)

debug :: Bool
debug = False

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
  Debug.when debug $ liftIO $ Debug.getRawInstancesWith Debug.defaultCallAlloyConfig {
    Debug.maxInstances = maxInsts,
    Debug.timeout = to
    } alloyCode >>= Debug.zipWithM_
    (Debug.writeFile . (\x -> "debug/debug-raw-" ++ Debug.showDigest (Debug.sha1 $ Debug.fromString alloyCode) ++ show x ++ ".als"))
    [1 :: Integer ..]
  instas  <- liftIO $ getInstances maxInsts to alloyCode
  shuffleM instas

instanceToCd :: AlloyInstance -> Either String Cd
instanceToCd rinsta = do
  cd <- instanceClassDiagram <$> fromInstance rinsta
  let cns = BM.fromList $ zip (classNames cd) $ map pure ['A'..]
      relationshipNames = mapMaybe relationshipName $ relationships cd
      rns = BM.fromList $ zip relationshipNames $ map pure ['z', 'y' ..]
  first show $ renameClassesAndRelationships cns rns cd
