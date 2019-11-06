module NaiveTasks where

import qualified Alloy (getInstances)

import qualified Data.Bimap as BM (fromList)

import Edges     (fromEdges, renameEdges)
import Generate  (generate)
import Output    (drawCdFromSyntax)
import Transform (createRunCommand, mergeParts, transform)
import Types     (ClassConfig (..), Syntax)
import Util

import Control.Monad          (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random   (MonadRandom, RandomGen, RandT)
import Data.Bimap             (Bimap)
import Data.GraphViz          (GraphvizOutput (Pdf))
import Data.List              (permutations)
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
  configs <- withMinimalLabels 3 config
  continueWithHead configs $ \config' -> do
    (names, edges) <- generate config' searchSpace
    let cd0    = (0 :: Integer, fromEdges names edges)
        parts0 = extractFourParts cd0
        labels = [l | (_, l, _, _, _, _) <- snd $ snd cd0]
        cds    = fromEdges names
          . flip renameEdges edges . BM.fromList . zip labels
          <$> drop 1 (permutations labels)
        cds'   = zip [1 :: Integer ..] cds
        partss = extractFourParts <$> cds'
        runCmd = foldr (\(n, _) -> (++ " and (not cd" ++ show n ++ ")")) "cd0" cds'
        onlyCd0 = createRunCommand runCmd (length names) maxObjects
        partss' = foldr mergeParts parts0 partss
    when debug . liftIO $ drawCd cd0
    when debug . liftIO $ drawCd `mapM_` cds'
    instances  <- liftIO
      $ Alloy.getInstances maxInstances (combineParts partss' ++ onlyCd0)
    instances' <- shuffleM instances
    continueWithHead instances' $ \od1 -> do
      labels' <- shuffleM labels
      let bm  = BM.fromList $ zip labels' $ (:[]) <$> ['a', 'b' ..]
          cd1 = fromEdges names $ renameEdges bm edges
      return (cd1, od1, bm)
  where
    extractFourParts (n, cd) = case transform cd (show n) "" of
      (p1, p2, p3, p4, _) -> (p1, p2, p3, p4)
    combineParts (p1, p2, p3, p4) = p1 ++ p2 ++ p3 ++ p4
    drawCd (n, cd) =
      drawCdFromSyntax True (Just redColor) cd ("debug-" ++ show n) Pdf
    continueWithHead []    _ =
      getDifferentNamesTask config maxObjects searchSpace maxInstances
    continueWithHead (x:_) f = f x

withMinimalLabels :: MonadRandom m => Int -> ClassConfig -> m [ClassConfig]
withMinimalLabels n config
  | n <= lowerLimit = return [config]
  | otherwise       = shuffleM
    [ config {
        aggregations = (Just aggrs, snd (aggregations config)),
        associations = (Just assos, snd (associations config)),
        compositions = (Just comps, snd (compositions config))
      }
    | aggrs <- range aggregations  0                           n
    , assos <- range associations  0                          (n - aggrs)
    , comps <- range compositions (max 0 $ n - aggrs - assos) (n - aggrs - assos)]
  where
    lowerLimit = 0
      +. fst (aggregations config)
      +. fst (associations config)
      +. fst (compositions config)
    x +. y = x + fromMaybe 0 y
    min' l1 Nothing   = l1
    min' l1 (Just l2) = min l1 l2
    range f low high  = [low +. fst (f config) .. min' high (snd $ f config)]
  
