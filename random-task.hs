{-# LANGUAGE TupleSections #-}
module Main (main) where

import Edges
import Generate  (generate)
import Mutation  (Target (..), getAllMutationResults, nonTargets)
import Output    (drawCdFromSyntax, drawOdFromInstance)
import Transform (createRunCommand, transform)
import Types     (ClassConfig (..), Syntax)

import qualified Alloy (getInstances)

import Control.Monad.Fail         (MonadFail)
import Control.Monad.Random.Class (MonadRandom)
import Data.Function              (on)
import Data.GraphViz              (GraphvizOutput (Pdf))
import Data.List                  ((\\), groupBy, intercalate, sortBy, union)
import Data.Map                   (Map)
import Data.Maybe                 (fromJust, isJust)
import Data.Set                   (singleton)
import System.Random.Shuffle      (shuffleM)

import qualified Data.Map as M (fromList, lookup, traverseWithKey)

main :: IO ()
main = do
  let config = ClassConfig {
          classes      = (Just 4, Just 4),
          aggregations = (Nothing, Just 2),
          associations = (Nothing, Just 2),
          compositions = (Nothing, Just 1),
          inheritances = (Just 1, Just 2)
        }
  let maxObjects = 4
  task <- getRandomTask config maxObjects 10 (-1)
  (\i cd -> drawCdFromSyntax True False cd (output ++ '-' : show i) Pdf) `M.traverseWithKey` fst task
  uncurry drawOd `mapM_` concat (zip [1 :: Int ..] <$> groupBy ((==) `on` fst) (sortBy (compare `on` fst) $ snd task))
  where
    output = "output"
    drawOd x (y, insta) =
      drawOdFromInstance True insta (output ++ '-' : toDescription y 2 ++ '-' : show x) Pdf
    toDescription :: [Int] -> Int -> String
    toDescription x n =
      intercalate "and" (show <$> x) ++ foldr ((++) . ("not" ++) . show) [] ([1..n] \\ x)

getRandomTask
  :: ClassConfig
     -> Int -> Int -> Int -> IO (Map Int Syntax, [([Int], String)])
getRandomTask config maxObjects searchSpace maxInstances = do
  (cds, instas) <- getRandomInstances config maxObjects searchSpace maxInstances
  mrinstas <- takeRandomInstances instas
  case mrinstas of
    Nothing      -> getRandomTask config maxObjects searchSpace maxInstances
    Just rinstas -> return (cds, rinstas)

getRandomInstances :: ClassConfig -> Int -> Int -> Int -> IO (Map Int Syntax, Map [Int] [String])
getRandomInstances config maxObjects searchSpace maxInstances = do
  (names, edges) <- generate config searchSpace
  mutations <- shuffleM $ getAllMutationResults config names edges
  let medges1 = getFirstValidSatisfying (not . anyRedEdge) names mutations
  continueIf (isJust medges1) $ do
    mutations' <- shuffleM mutations
    let Just edges1 = medges1
        Just edges2 = getFirstValidSatisfying (const True) names mutations'
    continueIf (not $ null $ nonTargets (singleton TInheritance) $ edges1 ++ edges2) $ do
      [cd1, cd2] <- shuffleM [fromEdges names edges1, fromEdges names edges2]
      mutations'' <- shuffleM mutations
      let Just edges3 = getFirstValidSatisfying (not . anyRedEdge) names mutations''
          cd3 = fromEdges names edges3
          parts1 = case transform cd1 "1" "" of (p1, p2, p3, p4, _) -> (p1, p2, p3, p4)
          parts2 = case transform cd2 "2" "" of (p1, p2, p3, p4, _) -> (p1, p2, p3, p4)
          parts1and2 = mergeParts parts1 parts2
          parts3 = case transform cd3 "3" "" of (p1, p2, p3, p4, _) -> (p1, p2, p3, p4)
          cd1not2 = createRunCommand "cd1 and (not cd2)" (length names) maxObjects
          cd2not1 = createRunCommand "cd2 and (not cd1)" (length names) maxObjects
          cd1and2 = createRunCommand "cd1 and cd2" (length names) maxObjects
          cdNot1not2 = createRunCommand "(not cd1) and (not cd2) and cd3" (length names) maxObjects
      instances1not2 <- Alloy.getInstances maxInstances (combineParts parts1and2 ++ cd1not2)
      instances2not1 <- Alloy.getInstances maxInstances (combineParts parts1and2 ++ cd2not1)
      instances1and2 <- Alloy.getInstances maxInstances (combineParts parts1and2 ++ cd1and2)
      instancesNot1not2 <- Alloy.getInstances maxInstances (combineParts (mergeParts parts1and2 parts3) ++ cdNot1not2)
      return $ ((M.fromList [(1, cd1), (2, cd2)]),
                M.fromList [([1]  , instances1not2),
                            ([2]  , instances2not1),
                            ([1,2], instances1and2),
                            ([]   , instancesNot1not2)])
  where
    continueIf True  m = m
    continueIf False _ = getRandomInstances config maxObjects searchSpace maxInstances
    combineParts (p1, p2, p3, p4) = p1 ++ p2 ++ p3 ++ p4

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

mergeParts
  :: (String, String, String, String)
  -> (String, String, String, String)
  -> (String, String, String, String)
mergeParts (p1, p2, p3, p4) (_, p2', p3', p4') =
  (p1, p2 `unionL` p2', p3 `unionL` p3', p4 ++ p4')
  where
    unionL x y = unlines $ (++ [""]) $ filter (not . null) $ lines x `union` lines y
