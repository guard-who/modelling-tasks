module Modelling.PetriNet.Reach.Draw (drawToFile, toPetriLike) where

import qualified Data.Map                         as M (fromList)
import qualified Data.Set                         as S (toList)

import Modelling.Auxiliary.Output       (LangM')
import Modelling.PetriNet.Diagram       (cacheNet)
import Modelling.PetriNet.Reach.Type (
  Net (connections, places, start, transitions),
  mark,
  )
import Modelling.PetriNet.Types (
  PetriLike (PetriLike),
  Node (PlaceNode, TransitionNode),
  )

import Control.Monad.IO.Class           (MonadIO (liftIO))
import Control.Monad.Trans.Class        (MonadTrans (lift))
import Control.Monad.Trans.Except       (runExceptT)
import Data.GraphViz                    (GraphvizCommand)
import Data.List                        (group, sort)

drawToFile
  :: (MonadIO m, Ord s, Ord t, Show s, Show t)
  => Bool
  -> FilePath
  -> GraphvizCommand
  -> Int
  -> Net s t
  -> LangM' m FilePath
drawToFile hidePNames path cmd x net = fmap (either error id) $
  lift $ liftIO $ runExceptT $ cacheNet
    (path ++ "graph" ++ show x)
    id
    (toPetriLike show show net)
    hidePNames
    False
    True
    cmd

{-|
Requires two functions that provide unique ids for places and nodes.
This function does not check if resulting ids overlap.
-}
toPetriLike
  :: (Ord a, Ord s, Ord t)
  => (s -> a)
  -> (t -> a)
  -> Net s t
  -> PetriLike a
toPetriLike fp ft n = PetriLike $ M.fromList $ ps ++ ts
  where
    ps = do
      p <- S.toList $ places n
      let i = mark (start n) p
          filterC f = filter f $ connections n
          countP = length . filter (p ==)
          fin = [ (ft t, countP xs)
                | (_,t,xs) <- filterC (\(_,_,to) -> p `elem` to)]
          fout = [ (ft t, countP xs)
                 | (xs,t,_) <- filterC (\(from,_,_) -> p `elem` from)]
      return (fp p, PlaceNode i (M.fromList fin) (M.fromList fout))
    ts = do
      t <- S.toList $ transitions n
      let filterC = filter (\(_,x,_) -> x == t) $ connections n
          fin =
            [ (fp $ head xs', length xs')
            | (xs,_,_) <- filterC,
              xs' <- group $ sort xs
            ]
          fout =
            [ (fp $ head xs', length xs')
            | (_,_,xs) <- filterC,
              xs' <- group $ sort xs
            ]
      return (ft t, TransitionNode (M.fromList fin) (M.fromList fout))
