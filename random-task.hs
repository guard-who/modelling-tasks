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
          aggregations = (Nothing, Nothing),
          associations = (Nothing, Nothing),
          compositions = (Nothing, Nothing),
          inheritances = (Nothing, Nothing)
        }
  getRandomTask config "output" 10 (-1)

getRandomTask :: ClassConfig -> String ->  Int -> Int -> IO ()
getRandomTask config output searchSpace maxInstances = do
  (names, edges) <- generate config searchSpace
  mutations <- shuffleM $ getAllMutationResults config names edges
  let medges2 = getFirstValid names mutations
  continueIf (isJust medges2) $ do
    mutations' <- shuffleM mutations
    let Just edges2 = medges2
        Just edges3 = getFirstValid names mutations'
        cd2 = fromEdges names edges2
        cd3 = fromEdges names edges3
        parts2 = transform cd2 "2" ""
        parts3 = transform cd3 "3" ""
        cd2not3 = createRunCommand "cd2 and (not cd3)" (length names) 5
        cd3not2 = createRunCommand "cd3 and (not cd2)" (length names) 5
        cd2and3 = createRunCommand "cd2 and cd3" (length names) 5
    continueIf (not $ null $ nonTargets (singleton TInheritance) $ edges2 ++ edges3) $ do
      instances2not3 <- getInstancesOfMerged parts2 parts3 cd2not3
      instances3not2 <- getInstancesOfMerged parts2 parts3 cd3not2
      instances2and3 <- getInstancesOfMerged parts2 parts3 cd2and3
      let takes = [ (take x, take y, take z)
                  | x <- [0 .. min 3 (length instances2not3)]
                  , y <- [0 .. min 3 (length instances3not2)]
                  , z <- [0 .. min 3 (length instances2and3)]
                  , 5 == x + y + z ]
      continueIf (not $ null takes) $ do
        (take2not3, take3not2, take2and3) <- head <$> shuffleM takes
        shuffled2not3 <- take2not3 <$> shuffleM instances2not3
        shuffled3not2 <- take3not2 <$> shuffleM instances3not2
        shuffled2and3 <- take2and3 <$> shuffleM instances2and3
        drawCdFromSyntax True cd2 (output ++ '-' : "2") Pdf
        drawCdFromSyntax True cd3 (output ++ '-' : "3") Pdf
        mapM_ (uncurry $ drawOd "2not3") $ zip [1 :: Integer ..] shuffled2not3
        mapM_ (uncurry $ drawOd "3not2") $ zip [1 :: Integer ..] shuffled3not2
        mapM_ (uncurry $ drawOd "2and3") $ zip [1 :: Integer ..] shuffled2and3
  where
    continueIf True  m = m
    continueIf False _ = getRandomTask config output searchSpace maxInstances
    drawOd x y insta   =
      drawOdFromInstance True insta (output ++ '-' : x ++ '-' : show y) Pdf
    getInstancesOfMerged x y =
      Alloy.getInstances maxInstances . combineParts . mergeParts x y
    combineParts (p1, p2, p3, p4, p5) = p1 ++ p2 ++ p3 ++ p4 ++ p5

getFirstValid :: [String] -> [[DiagramEdge]] -> Maybe [DiagramEdge]
getFirstValid _     []
  = Nothing
getFirstValid names (x:xs)
  | checkMultiEdge x, not (anyRedEdge $ fromEdges names x)
  = Just x
  | otherwise
  = getFirstValid names xs

mergeParts
  :: (String, String, String, String, String)
  -> (String, String, String, String, String)
  -> String
  -> (String, String, String, String, String)
mergeParts (p1, p2, p3, p4, _) (_, p2', p3', p4', _) command =
  (p1, p2 `unionL` p2', p3 `unionL` p3', p4 ++ p4', command)
  where
    unionL x y = unlines $ (++ [""]) $ filter (not . null) $ lines x `union` lines y
