{-# LANGUAGE TupleSections #-}
module MatchCdOd where

import qualified CdAndChanges.Transform           as Changes (transform)

import qualified Data.Bimap                       as BM (fromList, keysR, size)
import qualified Data.Map                         as M (fromList, lookup)
import qualified Language.Alloy.Call              as Alloy (getInstances)

import CD2Alloy.Transform               (createRunCommand, mergeParts, transform)
import CdAndChanges.Instance            (fromInstance)
import Auxiliary.Util                   (redColor)
import Edges (
  DiagramEdge,
  anyMarkedEdge,
  checkMultiEdge,
  fromEdges,
  renameClasses,
  renameEdges,
  )
import Generate                         (generate)
import Mutation
  (Target (..), getAllMutationResults, nonTargets)
import Output                           (drawCdFromSyntax)
import Types
  (ClassConfig (..), Change (..), Connection (..), Syntax, defaultProperties)

import Control.Arrow                    (first, second)
import Control.Monad                    (when)
import Control.Monad.Fail               (MonadFail)
import Control.Monad.IO.Class           (liftIO)
import Control.Monad.Random             (MonadRandom, RandomGen, RandT)
import Data.GraphViz                    (GraphvizOutput (Pdf))
import Data.List                        (delete)
import Data.Map                         (Map)
import Data.Maybe                       (fromJust, isJust, listToMaybe)
import Data.Set                         (singleton)
import Language.Alloy.Call              (AlloyInstance)
import System.Random.Shuffle            (shuffleM)

debug :: Bool
debug = False

getRandomTask
  :: RandomGen g
  => ClassConfig
  -> Int
  -> Int
  -> Maybe Integer
  -> RandT g IO (Map Int Syntax, [([Int], AlloyInstance)])
getRandomTask config maxObjects searchSpace maxInstances = do
  (cd1, cd2, cd3, numClasses) <- getRandomCDs config searchSpace
  instas <- liftIO $ getODInstances maxObjects maxInstances cd1 cd2 cd3 numClasses
  mrinstas <- takeRandomInstances instas
  case mrinstas of
    Nothing      -> getRandomTask config maxObjects searchSpace maxInstances
    Just rinstas -> return (M.fromList [(1, cd1), (2, cd2)], rinstas)

getRandomTask'
  :: RandomGen g
  => ClassConfig
  -> Int
  -> Int
  -> Maybe Integer
  -> RandT g IO (Map Int Syntax, [([Int], AlloyInstance)])
getRandomTask' config maxObjects searchSpace maxInstances = do
  let code = Changes.transform config defaultProperties
  instas <- liftIO $ Alloy.getInstances (Just 6000) code
  when debug $ liftIO $ print $ length instas
  rinstas <- shuffleM instas
  ods <- getODsFor maxObjects maxInstances rinstas
  maybe (getRandomTask' config maxObjects searchSpace maxInstances) return ods

getODsFor
  :: RandomGen g
  => Int
  -> Maybe Integer
  -> [AlloyInstance]
  -> RandT g IO (Maybe (Map Int Syntax, [([Int], AlloyInstance)]))
getODsFor _          _            []       = return Nothing
getODsFor maxObjects maxInstances (cd:cds) = do
  (_, [(_, cd1), (_, cd2), (_, cd3)], numClasses) <- applyChanges cd
  instas <- liftIO $ getODInstances maxObjects maxInstances cd1 cd2 cd3 numClasses
  mrinstas <- takeRandomInstances instas
  case mrinstas of
    Nothing      -> getODsFor maxObjects maxInstances cds
    Just rinstas -> return $ Just (M.fromList [(1, cd1), (2, cd2)], rinstas)

applyChanges
  :: RandomGen g
  => AlloyInstance
  -> RandT g IO (Syntax, [(Change DiagramEdge, Syntax)], Int)
applyChanges insta = do
  (names, edges0, changes) <- either error return $ fromInstance insta
  let (cs, es) = names
  cs' <- shuffleM cs
  es' <- shuffleM es
  let bme = BM.fromList $ zip es' $ (:[]) <$> ['z', 'y' ..]
      bmc = BM.fromList $ zip cs' $ (:[]) <$> ['A' ..]
      cd  = getSyntax bmc bme edges0 $ Change Nothing Nothing
      cds = getSyntax bmc bme edges0 <$> changes
  let changes' = fmap (head . renameClasses bmc . renameEdges bme . (:[])) <$> changes
  return (cd, zip changes' cds, BM.size bmc)
  where
    getSyntax bmc bme es c =
      fromEdges (BM.keysR bmc) $ renameClasses bmc $ renameEdges bme $ performChange c es
    performChange c es =
      let rs = maybe es (`delete` es) $ remove c
          add' = case (remove c, add c) of
            (Just (from, to, Assoc t n _ _ _), Just (from', to', Assoc t' _ m1 m2 b))
              | t == t', from == from' && to == to' || from == to' && to == from' ->
                Just (from', to', Assoc t' n m1 m2 b)
            _ -> add c
      in maybe rs (: rs) add'

getRandomCDs :: RandomGen g => ClassConfig -> Int -> RandT g IO (Syntax, Syntax, Syntax, Int)
getRandomCDs config searchSpace = do
  (names, edges) <- generate config searchSpace
  let cd0 = fromEdges names edges
  -- continueIf (not (anyMarkedEdge cd0)) $ do
  when debug . liftIO $ drawCdFromSyntax False True (Just redColor) cd0 "debug-0" Pdf
  mutations <- shuffleM $ getAllMutationResults config names edges
  let medges1 = getFirstValidSatisfying (not . anyMarkedEdge) names mutations
  continueIf (isJust medges1) $ do
    mutations' <- shuffleM mutations
    let Just edges1 = medges1
        Just edges2 = getFirstValidSatisfying (const True) names mutations'
    continueIf (not $ null $ nonTargets (singleton TInheritance) $ edges1 ++ edges2) $ do
      [cd1, cd2] <- shuffleM [fromEdges names edges1, fromEdges names edges2]
      mutations'' <- shuffleM mutations
      let Just edges3 = getFirstValidSatisfying (not . anyMarkedEdge) names mutations''
          cd3         = fromEdges names edges3
      when debug . liftIO $ drawCdFromSyntax False True (Just redColor) cd3 "debug-3" Pdf
      return (cd1, cd2, cd3, length names)
  where
    continueIf True  m = m
    continueIf False _ = getRandomCDs config searchSpace

getODInstances
  :: Int
  -> Maybe Integer
  -> Syntax
  -> Syntax
  -> Syntax
  -> Int
  -> IO (Map [Int] [AlloyInstance])
getODInstances maxObjects maxInstances cd1 cd2 cd3 numClasses = do
  -- TODO remove `toOldSyntax`
  let parts1 = case transform (toOldSyntax cd1) "1" "" of (p1, p2, p3, p4, _) -> (p1, p2, p3, p4)
      parts2 = case transform (toOldSyntax cd2) "2" "" of (p1, p2, p3, p4, _) -> (p1, p2, p3, p4)
      parts1and2 = mergeParts parts1 parts2
      parts3 = case transform (toOldSyntax cd3) "3" "" of (p1, p2, p3, p4, _) -> (p1, p2, p3, p4)
      cd1not2 = createRunCommand "cd1 and (not cd2)" numClasses maxObjects
      cd2not1 = createRunCommand "cd2 and (not cd1)" numClasses maxObjects
      cd1and2 = createRunCommand "cd1 and cd2" numClasses maxObjects
      cdNot1not2 = createRunCommand "(not cd1) and (not cd2) and cd3" numClasses maxObjects
  instances1not2 <- Alloy.getInstances maxInstances (combineParts parts1and2 ++ cd1not2)
  instances2not1 <- Alloy.getInstances maxInstances (combineParts parts1and2 ++ cd2not1)
  instances1and2 <- Alloy.getInstances maxInstances (combineParts parts1and2 ++ cd1and2)
  instancesNot1not2 <- Alloy.getInstances maxInstances (combineParts (mergeParts parts1and2 parts3) ++ cdNot1not2)
  when debug . print $ length instances1not2
  when debug . print $ length instances2not1
  when debug . print $ length instances1and2
  when debug . print $ length instancesNot1not2
  return $ (M.fromList [([1]  , instances1not2),
                        ([2]  , instances2not1),
                        ([1,2], instances1and2),
                        ([]   , instancesNot1not2)])
  where
    combineParts (p1, p2, p3, p4) = p1 ++ p2 ++ p3 ++ p4
    toOldSyntax = first (second listToMaybe <$>)

takeRandomInstances :: (MonadRandom m, MonadFail m) => Map [Int] [a] -> m (Maybe [([Int], a)])
takeRandomInstances instas = do
  let takes = [ [takeL [1] x, takeL [2] y, takeL [1,2] z, takeL [] u]
              | x <- [0 .. min 2 (length $ fromJust $ M.lookup [1]   instas)]
              , y <- [0 .. min 2 (length $ fromJust $ M.lookup [2]   instas)]
              , z <- [0 .. min 2 (length $ fromJust $ M.lookup [1,2] instas)]
              , u <- [0 .. min 2 (length $ fromJust $ M.lookup []    instas)]
              , 5 == x + y + z + u ]
      takeL k n = take n . fmap (k,) . fromJust . M.lookup k
  case takes of
    []  -> return Nothing
    _:_ -> Just <$> do
      rinstas <- mapM shuffleM instas
      ts:_    <- shuffleM takes
      shuffleM $ concat $ fmap ($ rinstas) ts

getFirstValidSatisfying :: (Syntax -> Bool) -> [String] -> [[DiagramEdge]] -> Maybe [DiagramEdge]
getFirstValidSatisfying _ _     []
  = Nothing
getFirstValidSatisfying p names (x:xs)
  | checkMultiEdge x, p (fromEdges names x)
  = Just x
  | otherwise
  = getFirstValidSatisfying p names xs
