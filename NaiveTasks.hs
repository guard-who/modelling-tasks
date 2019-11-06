module NaiveTasks where

import qualified Alloy (getInstances)

import qualified Data.Bimap as BM (fromList)

import Edges     (fromEdges, renameEdges)
import Generate  (generate)
import Output    (drawCdFromSyntax)
import Transform (transform)
import Types     (ClassConfig (..), Syntax)
import Util

import Control.Monad          (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random   (MonadRandom, RandomGen, RandT)
import Data.Bimap             (Bimap)
import Data.GraphViz          (GraphvizOutput (Pdf))
import Data.Maybe             (fromMaybe)
import System.Random.Shuffle  (shuffleM)

debug :: Bool
debug = False

getDifferentNamesTask
  :: RandomGen g
  => ClassConfig
  -> Int
  -> Int
  -> Int
  -> RandT g IO (Syntax, String, Bimap String String)
getDifferentNamesTask config maxObjects searchSpace maxInstances = do
  config' <- withMinimalLabels 2 config
  (names, edges) <- generate config' searchSpace
  let cd0    = fromEdges names edges
      parts1 = transform cd0 "0" ""
      labels = [l | (_, l, _, _, _, _) <- snd cd0]
  when debug . liftIO $ drawCdFromSyntax True (Just redColor) cd0 "debug-0" Pdf
  instances  <- liftIO $ Alloy.getInstances maxInstances (combineParts parts1)
  instances' <- shuffleM instances
  case instances' of
    []      -> getDifferentNamesTask config maxObjects searchSpace maxInstances
    (od1:_) -> do
      labels' <- shuffleM labels
      let bm  = BM.fromList $ zip labels' $ (:[]) <$> ['a', 'b' ..]
          cd1 = fromEdges names $ renameEdges bm edges
      return (cd1, od1, bm)
  where
    combineParts (p1, p2, p3, p4, p5) = p1 ++ p2 ++ p3 ++ p4 ++ p5

withMinimalLabels :: MonadRandom m => Int -> ClassConfig -> m ClassConfig
withMinimalLabels n config
  | n <= lowerLimit = return config
  | otherwise       = head <$> shuffleM
    [ config {
        aggregations = (Just aggrs, snd (aggregations config)),
        associations = (Just assos, snd (associations config)),
        compositions = (Just comps, snd (compositions config))
      }
    | aggrs <- range aggregations  0                   n
    , assos <- range associations (0 + aggrs)         (n - aggrs)
    , comps <- range compositions (0 + aggrs + assos) (n - aggrs - assos)]
  where
    lowerLimit = 0
      +. fst (aggregations config)
      +. fst (associations config)
      +. fst (compositions config)
    x +. y = x + fromMaybe 0 y
    min' l1 Nothing   = l1
    min' l1 (Just l2) = min l1 l2
    range f low high  = [low +. fst (f config) .. min' high (snd $ f config)]
  
