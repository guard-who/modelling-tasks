module Modelling.PetriNet.Reach.Draw (drawToFile, toPetriLike) where

import qualified Data.Map                         as M (fromList)
import qualified Data.Set                         as S (toList)

import Modelling.Auxiliary.Output       (LangM')
import Modelling.PetriNet.Diagram       (drawNet)
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
import Control.Monad.Trans.Except       (ExceptT, runExceptT)
import Data.GraphViz                    (GraphvizCommand (Neato))
import Data.List                        (group, sort)
import Diagrams                         (Diagram)
import Diagrams.Backend.SVG             (B, renderSVG)
import Diagrams.Prelude                 (mkWidth)

drawToFile
  :: (MonadIO m, Ord s, Ord t, Show s, Show t)
  => Bool
  -> FilePath
  -> Int
  -> Net s t
  -> LangM' m FilePath
drawToFile hidePNames path x net = do
  graph <- lift $ liftIO $ runExceptT $ drawPetriWithDefaults net hidePNames
  lift $ liftIO $ writeGraph path (show x) $ either error id graph

writeGraph
  :: FilePath
  -> String
  -> Diagram B
  -> IO FilePath
writeGraph path index d = do
  let file = path ++ "graph" ++ index ++ ".svg"
  renderSVG file (mkWidth 250) d
  return file

drawPetriWithDefaults
  :: (Ord s, Ord t, Show s, Show t)
  => Net s t
  -> Bool
  -> ExceptT String IO (Diagram B)
drawPetriWithDefaults p hidePNames = drawPetri p hidePNames False True Neato

drawPetri
  :: (Ord s, Ord t, Show s, Show t)
  => Net s t
  -> Bool
  -> Bool
  -> Bool
  -> GraphvizCommand
  -> ExceptT String IO (Diagram B)
drawPetri = drawNet id . toPetriLike show show

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
