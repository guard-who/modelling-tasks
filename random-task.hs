module Main (main) where

import Edges
import Generate  (generate)
import Mutation  (Target (..), getAllMutationResults, nonTargets)
import Output    (drawCdFromSyntax, drawOdFromInstance)
import Transform (createRunCommand, transform)
import Types     (ClassConfig (..))

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
  let medges2 = getFirstValid names mutations
  continueIf (isJust medges2) $ do
    mutations' <- shuffleM mutations
    mutations'' <- shuffleM mutations
    let Just edges2 = medges2
        Just edges3 = getFirstValid names mutations'
        Just edges4 = getFirstValid names mutations''
        cd2 = fromEdges names edges2
        cd3 = fromEdges names edges3
        cd4 = fromEdges names edges4
        parts2 = case transform cd2 "2" "" of (p1, p2, p3, p4, _) -> (p1, p2, p3, p4)
        parts3 = case transform cd3 "3" "" of (p1, p2, p3, p4, _) -> (p1, p2, p3, p4)
        parts2and3 = mergeParts parts2 parts3
        parts4 = case transform cd4 "4" "" of (p1, p2, p3, p4, _) -> (p1, p2, p3, p4)
        cd2not3 = createRunCommand "cd2 and (not cd3)" (length names) maxObjects
        cd3not2 = createRunCommand "cd3 and (not cd2)" (length names) maxObjects
        cd2and3 = createRunCommand "cd2 and cd3" (length names) maxObjects
        cdNot2not3 = createRunCommand "(not cd2) and (not cd3) and cd4" (length names) maxObjects
    continueIf (not $ null $ nonTargets (singleton TInheritance) $ edges2 ++ edges3) $ do
      instances2not3 <- Alloy.getInstances maxInstances (combineParts parts2and3 ++ cd2not3)
      instances3not2 <- Alloy.getInstances maxInstances (combineParts parts2and3 ++ cd3not2)
      instances2and3 <- Alloy.getInstances maxInstances (combineParts parts2and3 ++ cd2and3)
      instancesNot2not3 <- Alloy.getInstances maxInstances (combineParts (mergeParts parts2and3 parts4) ++ cdNot2not3)
      let takes = [ (take x, take y, take z, take u)
                  | x <- [0 .. min 3 (length instances2not3)]
                  , y <- [0 .. min 3 (length instances3not2)]
                  , z <- [0 .. min 2 (length instances2and3)]
                  , u <- [0 .. min 2 (length instancesNot2not3)]
                  , 5 == x + y + z + u ]
      continueIf (not $ null takes) $ do
        (take2not3, take3not2, take2and3, takeNot2not3) <- head <$> shuffleM takes
        shuffled2not3 <- take2not3 <$> shuffleM instances2not3
        shuffled3not2 <- take3not2 <$> shuffleM instances3not2
        shuffled2and3 <- take2and3 <$> shuffleM instances2and3
        shuffledNot2not3 <- takeNot2not3 <$> shuffleM instancesNot2not3
        drawCdFromSyntax True cd2 (output ++ '-' : "2") Pdf
        drawCdFromSyntax True cd3 (output ++ '-' : "3") Pdf
        mapM_ (uncurry $ drawOd "2not3") $ zip [1 :: Integer ..] shuffled2not3
        mapM_ (uncurry $ drawOd "3not2") $ zip [1 :: Integer ..] shuffled3not2
        mapM_ (uncurry $ drawOd "2and3") $ zip [1 :: Integer ..] shuffled2and3
        mapM_ (uncurry $ drawOd "not2not3") $ zip [1 :: Integer ..] shuffledNot2not3
  where
    continueIf True  m = m
    continueIf False _ = getRandomTask config maxObjects output searchSpace maxInstances
    drawOd x y insta   =
      drawOdFromInstance True insta (output ++ '-' : x ++ '-' : show y) Pdf
    combineParts (p1, p2, p3, p4) = p1 ++ p2 ++ p3 ++ p4

getFirstValid :: [String] -> [[DiagramEdge]] -> Maybe [DiagramEdge]
getFirstValid _     []
  = Nothing
getFirstValid names (x:xs)
  | checkMultiEdge x, not (anyRedEdge $ fromEdges names x)
  = Just x
  | otherwise
  = getFirstValid names xs

mergeParts
  :: (String, String, String, String)
  -> (String, String, String, String)
  -> (String, String, String, String)
mergeParts (p1, p2, p3, p4) (_, p2', p3', p4') =
  (p1, p2 `unionL` p2', p3 `unionL` p3', p4 ++ p4')
  where
    unionL x y = unlines $ (++ [""]) $ filter (not . null) $ lines x `union` lines y
