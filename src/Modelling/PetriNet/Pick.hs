{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

import Capabilities.Cache               (MonadCache)
import Capabilities.Diagrams            (MonadDiagrams)
import Capabilities.Graphviz            (MonadGraphviz)
import Modelling.Auxiliary.Common (
  Object,
  )
import Modelling.PetriNet.Diagram       (getDefaultNet, getNet, renderWith)
import Modelling.PetriNet.Types         (
  BasicConfig (..),
  ChangeConfig (..),
  Drawable,
  GraphConfig (..),
  Net (..),
  checkBasicConfig,
  checkChangeConfig,
  checkGraphLayouts,
  manyRandomDrawSettings,
  placeNames,
  randomDrawSettings,
  transitionNames,
  )

import Control.Applicative              (Alternative ((<|>)))
import Control.Arrow                    (Arrow (second))
import Control.Monad.Catch              (MonadThrow)
import Control.Monad.Output (
  LangM,
  OutputMonad,
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
import Control.Monad.Trans              (MonadTrans (lift))
import Data.Bitraversable               (bimapM)
import Data.Containers.ListUtils        (nubOrd)
import Data.Map                         (Map)
import Data.Maybe                       (isJust)
import GHC.Generics                     (Generic)
import Language.Alloy.Call (
  AlloyInstance
  )
import System.Random.Shuffle            (shuffleM)

data PickInstance n = PickInstance {
  nets :: !(Map Int (Bool, Drawable n)),
  showSolution :: !Bool
  }
  deriving (Generic, Read, Show)

-- TODO: replace 'wrong' in 'pickGenerate' by 'wrongInstances'
-- if this value might be greater than 1 on task generation.
wrongInstances :: PickInstance n -> Int
wrongInstances inst = length [False | (False, _) <- M.elems (nets inst)]

wrong :: Int
wrong = 1

pickTaskInstance
  :: (MonadThrow m, Net p n, Traversable t)
  => (AlloyInstance -> m (t Object))
  -> AlloyInstance
  -> m [(p n String, Maybe (t String))]
pickTaskInstance parseF inst = do
  confl <- second Just <$> getNet parseF inst
  net   <- (,Nothing) <$> getDefaultNet inst
  return [confl,net]

pickGenerate
  :: (MonadThrow m, Net p n, Ord b)
  => (c
    -> Int
    -> RandT StdGen m [(p n b, Maybe a)]
    )
  -> (c -> GraphConfig)
  -> (c -> Bool)
  -> (c -> Bool)
  -> c
  -> Int
  -> Int
  -> m (PickInstance (p n b))
pickGenerate pick gc useDifferent withSol config segment seed
  = flip evalRandT (mkStdGen seed) $ do
  ns <- pick config segment
  ns'  <- shuffleM ns
  let ts = nubOrd $ concatMap (transitionNames . fst) ns'
      ps = nubOrd $ concatMap (placeNames . fst) ns'
  ts' <- shuffleM ts
  ps' <- shuffleM ps
  let mapping = BM.fromList $ zip (ps ++ ts) (ps' ++ ts')
  ns'' <- lift $ bimapM (traverseNet (`BM.lookup` mapping)) return `mapM` ns'
  s <- randomDrawSettings (gc config)
  ns''' <- addDrawingSettings s ns''
  return $ PickInstance {
    nets = M.fromList $ zip [1 ..] [(isJust m, (n, d)) | ((n, m), d) <- ns'''],
    showSolution = withSol config
    }
  where
    addDrawingSettings s ps = zip ps <$>
      if useDifferent config
      then manyRandomDrawSettings (gc config) (wrong + 1)
      else return $ replicate (wrong + 1) s

pickSyntax
  :: OutputMonad m
  => PickInstance n
  -> Int
  -> LangM m
pickSyntax task = singleChoiceSyntax withSol options
  where
    options = M.keys $ nets task
    withSol = showSolution task

pickEvaluation
  :: OutputMonad m
  => PickInstance n
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
    withSol = showSolution task

pickSolution :: PickInstance n -> Int
pickSolution = head . M.keys . M.filter fst . nets

renderPick
  :: (MonadCache m, MonadDiagrams m, MonadGraphviz m, MonadThrow m, Net p n)
  => String
  -> String
  -> PickInstance (p n String)
  -> m (Map Int (Bool, String))
renderPick path task config =
  M.foldrWithKey render' (pure mempty) $ nets config
  where
    render' x (b, (net, ds)) ns =
      renderWith path (task ++ '-' : show x) net ds
      >>= \file -> M.insert x (b, file) <$> ns

checkConfigForPick
  :: Bool
  -> Int
  -> BasicConfig
  -> ChangeConfig
  -> GraphConfig
  -> Maybe String
checkConfigForPick useDifferent numWrongInstances basic change graph
  = checkBasicConfig basic
  <|> checkChangeConfig basic change
  <|> checkGraphLayouts useDifferent numWrongInstances graph
