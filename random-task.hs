module Main (main) where

import Edges
import Generate  (generate)
import Mutation  (Target (..), getAllMutationResults, nonTargets)
import Output    (drawCdFromSyntax, drawOdFromInstance)
import Transform (createRunCommand, transform)
import Types     (ClassConfig (..), Syntax)

import qualified Alloy (getInstances)

import Data.List             (union)
import Data.GraphViz         (GraphvizOutput (Pdf))
import Data.Maybe            (isJust)
import Data.Set              (singleton)
import System.Random.Shuffle (shuffleM)

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
  getRandomTask config maxObjects "output" 10 (-1)

getRandomTask :: ClassConfig -> Int -> String ->  Int -> Int -> IO ()
getRandomTask config maxObjects output searchSpace maxInstances = do
  (names, edges) <- generate config searchSpace
  mutations <- shuffleM $ getAllMutationResults config names edges
  let medges1 = getFirstSatisfying (not . anyRedEdge) names mutations
  continueIf (isJust medges1) $ do
    mutations' <- shuffleM mutations
    mutations'' <- shuffleM mutations
    let Just edges1 = medges1
        Just edges2 = getFirstSatisfying (not . anyRedEdge) names mutations'
        Just edges3 = getFirstSatisfying (not . anyRedEdge) names mutations''
        cd1 = fromEdges names edges1
        cd2 = fromEdges names edges2
        cd3 = fromEdges names edges3
        parts1 = case transform cd1 "1" "" of (p1, p2, p3, p4, _) -> (p1, p2, p3, p4)
        parts2 = case transform cd2 "2" "" of (p1, p2, p3, p4, _) -> (p1, p2, p3, p4)
        parts1and2 = mergeParts parts1 parts2
        parts3 = case transform cd3 "3" "" of (p1, p2, p3, p4, _) -> (p1, p2, p3, p4)
        cd1not2 = createRunCommand "cd1 and (not cd2)" (length names) maxObjects
        cd2not1 = createRunCommand "cd2 and (not cd1)" (length names) maxObjects
        cd1and2 = createRunCommand "cd1 and cd2" (length names) maxObjects
        cdNot1not2 = createRunCommand "(not cd1) and (not cd2) and cd3" (length names) maxObjects
    continueIf (not $ null $ nonTargets (singleton TInheritance) $ edges1 ++ edges2) $ do
      instances1not2 <- Alloy.getInstances maxInstances (combineParts parts1and2 ++ cd1not2)
      instances2not1 <- Alloy.getInstances maxInstances (combineParts parts1and2 ++ cd2not1)
      instances1and2 <- Alloy.getInstances maxInstances (combineParts parts1and2 ++ cd1and2)
      instancesNot1not2 <- Alloy.getInstances maxInstances (combineParts (mergeParts parts1and2 parts3) ++ cdNot1not2)
      let takes = [ (take x, take y, take z, take u)
                  | x <- [0 .. min 3 (length instances1not2)]
                  , y <- [0 .. min 3 (length instances2not1)]
                  , z <- [0 .. min 2 (length instances1and2)]
                  , u <- [0 .. min 2 (length instancesNot1not2)]
                  , 5 == x + y + z + u ]
      continueIf (not $ null takes) $ do
        (take1not2, take2not1, take1and2, takeNot1not2) <- head <$> shuffleM takes
        shuffled1not2 <- take1not2 <$> shuffleM instances1not2
        shuffled2not1 <- take2not1 <$> shuffleM instances2not1
        shuffled1and2 <- take1and2 <$> shuffleM instances1and2
        shuffledNot1not2 <- takeNot1not2 <$> shuffleM instancesNot1not2
        drawCdFromSyntax True True cd1 (output ++ '-' : "1") Pdf
        drawCdFromSyntax True True cd2 (output ++ '-' : "2") Pdf
        mapM_ (uncurry $ drawOd "1not2") $ zip [1 :: Integer ..] shuffled1not2
        mapM_ (uncurry $ drawOd "2not1") $ zip [1 :: Integer ..] shuffled2not1
        mapM_ (uncurry $ drawOd "1and2") $ zip [1 :: Integer ..] shuffled1and2
        mapM_ (uncurry $ drawOd "not1not2") $ zip [1 :: Integer ..] shuffledNot1not2
  where
    continueIf True  m = m
    continueIf False _ = getRandomTask config maxObjects output searchSpace maxInstances
    drawOd x y insta   =
      drawOdFromInstance True insta (output ++ '-' : x ++ '-' : show y) Pdf
    combineParts (p1, p2, p3, p4) = p1 ++ p2 ++ p3 ++ p4

getFirstSatisfying :: (Syntax -> Bool) -> [String] -> [[DiagramEdge]] -> Maybe [DiagramEdge]
getFirstSatisfying _ _     []
  = Nothing
getFirstSatisfying p names (x:xs)
  | checkMultiEdge x, p (fromEdges names x)
  = Just x
  | otherwise
  = getFirstSatisfying p names xs

mergeParts
  :: (String, String, String, String)
  -> (String, String, String, String)
  -> (String, String, String, String)
mergeParts (p1, p2, p3, p4) (_, p2', p3', p4') =
  (p1, p2 `unionL` p2', p3 `unionL` p3', p4 ++ p4')
  where
    unionL x y = unlines $ (++ [""]) $ filter (not . null) $ lines x `union` lines y
