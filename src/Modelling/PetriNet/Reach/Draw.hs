module Modelling.PetriNet.Reach.Draw (drawToFile) where

import qualified Data.Map                         as M (fromList)
import qualified Data.Set                         as S (toList)

import Capabilities.Cache               (MonadCache)
import Capabilities.Diagrams            (MonadDiagrams)
import Capabilities.Graphviz            (MonadGraphviz)
import Modelling.PetriNet.Diagram       (cacheNet)
import Modelling.PetriNet.Reach.Type (
  Net (connections, places, start, transitions),
  mark,
  )
import Modelling.PetriNet.Types (
  DrawSettings (..),
  PetriLike (PetriLike),
  SimpleNode (SimplePlace, SimpleTransition),
  )

import Control.Monad.Catch              (MonadThrow)
import Data.GraphViz                    (GraphvizCommand)
import Data.List                        (group, sort)

drawToFile
  :: (
    Ord s,
    Ord t,
    Show s,
    Show t,
    MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    MonadThrow m
    )
  => Bool
  -> FilePath
  -> GraphvizCommand
  -> Int
  -> Net s t
  -> m FilePath
drawToFile hidePlaceNames path cmd x net = cacheNet
    (path ++ "graph" ++ show x)
    id
    (toPetriLike show show net)
    DrawSettings {
      withAnnotatedLabels = False,
      with1Weights = False,
      withPlaceNames = not hidePlaceNames,
      withTransitionNames = True,
      withGraphvizCommand = cmd
      }

{-|
Requires two functions that provide unique ids for places and nodes.
This function does not check if resulting ids overlap.
-}
toPetriLike
  :: (Ord a, Ord s, Ord t)
  => (s -> a)
  -> (t -> a)
  -> Net s t
  -> PetriLike SimpleNode a
toPetriLike fp ft n = PetriLike $ M.fromList $ ps ++ ts
  where
    ps = do
      p <- S.toList $ places n
      let i = mark (start n) p
          filterC f = filter f $ connections n
          countP = length . filter (p ==)
          outcome =
            [ (ft t, countP xs)
            | (xs,t,_) <- filterC (\(from,_,_) -> p `elem` from)
            ]
      return (fp p, SimplePlace i (M.fromList outcome))
    ts = do
      t <- S.toList $ transitions n
      let filterC = filter (\(_,x,_) -> x == t) $ connections n
          outcome =
            [ (fp $ head xs', length xs')
            | (_,_,xs) <- filterC,
              xs' <- group $ sort xs
            ]
      return (ft t, SimpleTransition (M.fromList outcome))
