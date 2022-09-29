{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}

module Modelling.PetriNet.Pick (
  PickInstance (..),
  checkConfigForPick,
  pickGenerate,
  pickEvaluation,
  pickSolution,
  pickSyntax,
  pickTaskInstance,
  renderPick,
  wrong,
  wrongInstances,
  ) where

import qualified Data.Bimap                       as BM (fromList, lookup)
import qualified Data.Map                         as M (
  elems,
  filter,
  fromList,
  keys,
  foldrWithKey,
  insert,
  )

import Modelling.Auxiliary.Common (
  Object,
  )
import Modelling.PetriNet.Diagram       (getDefaultNet, getNet, renderWith)
import Modelling.PetriNet.Types         (
  BasicConfig (..),
  ChangeConfig (..),
  PetriLike,
  PetriNet,
  checkBasicConfig,
  checkChangeConfig,
  checkGraphLayouts,
  manyRandomDrawSettings,
  placeNames,
  randomDrawSettings,
  transitionNames,
  traversePetriLike,
  )

import Control.Applicative              (Alternative ((<|>)))
import Control.Arrow                    (Arrow (second))
import Control.Monad.Output (
  LangM',
  LangM,
  OutputMonad (..),
  Rated,
  english,
  german,
  singleChoice,
  singleChoiceSyntax,
  translations,
  )
import Control.Monad.Random (
  RandT,
  StdGen,
  evalRandT,
  mkStdGen
  )
import Control.Monad.IO.Class           (MonadIO)
import Control.Monad.Trans              (MonadTrans (lift))
import Control.Monad.Trans.Except       (ExceptT)
import Data.Bitraversable               (bimapM)
import Data.Containers.ListUtils        (nubOrd)
import Data.Map                         (Map)
import Data.Maybe                       (isJust)
import GHC.Generics                     (Generic)
import Language.Alloy.Call (
  AlloyInstance
  )
import System.Random.Shuffle            (shuffleM)

data PickInstance = PickInstance {
  nets :: !(Map Int (Bool, PetriNet)),
  showSolution :: !Bool
  }
  deriving (Generic, Read, Show)

-- TODO: replace 'wrong' in 'pickGenerate' by 'wrongInstances'
-- if this value might be greater than 1 on task generation.
wrongInstances :: PickInstance -> Int
wrongInstances inst = length [False | (False, _) <- M.elems (nets inst)]

wrong :: Int
wrong = 1

pickTaskInstance
  :: (MonadTrans m, Traversable t)
  => (AlloyInstance -> Either String (t Object))
  -> AlloyInstance
  -> m (ExceptT String IO) [(PetriLike String, Maybe (t String))]
pickTaskInstance parseF inst = lift $ do
  confl <- second Just <$> getNet parseF inst
  net   <- (,Nothing) <$> getDefaultNet inst
  return [confl,net]

pickGenerate
  :: (c -> Int -> RandT StdGen (ExceptT String IO) [(PetriLike String, Maybe a)])
  -> (c -> BasicConfig)
  -> (c -> Bool)
  -> (c -> Bool)
  -> c
  -> Int
  -> Int
  -> ExceptT String IO PickInstance
pickGenerate pick bc useDifferent withSol config segment seed = flip evalRandT (mkStdGen seed) $ do
  ns <- pick config segment
  ns'  <- shuffleM ns
  let ts = nubOrd $ concat $ transitionNames . fst <$> ns'
      ps = nubOrd $ concat $ placeNames . fst <$> ns'
  ts' <- shuffleM ts
  ps' <- shuffleM ps
  let mapping = BM.fromList $ zip (ps ++ ts) (ps' ++ ts')
  ns'' <- lift $ bimapM (traversePetriLike (`BM.lookup` mapping)) return `mapM` ns'
  s <- randomDrawSettings (bc config)
  ns''' <- addDrawingSettings s ns''
  return $ PickInstance {
    nets = M.fromList $ zip [1 ..] [(isJust m, (n, d)) | ((n, m), d) <- ns'''],
    showSolution = withSol config
    }
  where
    addDrawingSettings s ps = zip ps <$>
      if useDifferent config
      then manyRandomDrawSettings (bc config) (wrong + 1)
      else return $ replicate (wrong + 1) s

pickSyntax
  :: OutputMonad m
  => PickInstance
  -> Int
  -> LangM m
pickSyntax task = singleChoiceSyntax withSol options
  where
    options = M.keys $ nets task
    withSol = showSolution (task :: PickInstance)

pickEvaluation
  :: OutputMonad m
  => PickInstance
  -> Int
  -> Rated m
pickEvaluation task = do
  let what = translations $ do
        english "petri net"
        german "Petrinetz"
  singleChoice what msolutionString solution
  where
    msolutionString =
      if withSol
      then Just $ show solution
      else Nothing
    solution = pickSolution task
    withSol = showSolution (task :: PickInstance)

pickSolution :: PickInstance -> Int
pickSolution = head . M.keys . M.filter fst . nets

renderPick
  :: (MonadIO m, OutputMonad m)
  => String
  -> String
  -> PickInstance
  -> LangM' m (Map Int (Bool, String))
renderPick path task config =
  M.foldrWithKey render' (return mempty) $ nets config
  where
    render' x (b, (net, ds)) ns = do
      file <- renderWith path (task ++ '-' : show x) net ds
      M.insert x (b, file) <$> ns

checkConfigForPick :: Bool -> Int -> BasicConfig -> ChangeConfig -> Maybe String
checkConfigForPick useDifferent numWrongInstances basic change
  = checkBasicConfig basic
  <|> checkChangeConfig basic change
  <|> checkGraphLayouts useDifferent numWrongInstances basic
