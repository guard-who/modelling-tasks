module Modelling.CdOd.Generate (
  generateCds,
  instanceToEdges,
  nameEdges,
  ) where

import qualified Data.Bimap                       as BM (
  fromList,
  keysR,
  )

import Modelling.CdOd.Auxiliary.Util    (getInstances)
import Modelling.CdOd.CdAndChanges.Instance (
  ClassDiagramInstance (..),
  fromInstance,
  )
import Modelling.CdOd.CdAndChanges.Transform (
  transformNoChanges,
  )
import Modelling.CdOd.Edges             (
  Connection (..),
  DiagramEdge,
  isInheritanceEdge,
  relationshipToEdge,
  renameClasses,
  )
import Modelling.CdOd.Types (
  ClassConfig (..),
  ClassDiagram (..),
  RelationshipProperties,
  )

import Control.Monad.IO.Class           (MonadIO (liftIO))
import Control.Monad.Random             (MonadRandom)
import Data.List                        (partition)
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

instanceToEdges :: AlloyInstance -> Either String ([String], [DiagramEdge])
instanceToEdges rinsta = do
  cd <- instanceClassDiagram <$> fromInstance rinsta
  let cns = BM.fromList $ zip (classNames cd) $ map pure ['A'..]
  return (BM.keysR cns, nameEdges $ renameClasses cns $ map relationshipToEdge (relationships cd))

nameEdges :: [DiagramEdge] -> [DiagramEdge]
nameEdges es =
     ihs
  ++ [(s, e, Assoc k [n] m1 m2 b)
     | (n, (s, e, Assoc k _ m1 m2 b)) <- zip ['z', 'y' ..] ass]
  where
    (ihs, ass) = partition isInheritanceEdge es
