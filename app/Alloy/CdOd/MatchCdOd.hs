{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
module Alloy.CdOd.MatchCdOd where

import qualified Alloy.CdOd.CdAndChanges.Transform as Changes (transform)

import qualified Data.Bimap                       as BM (fromList, keysR, size)
import qualified Data.Map                         as M
  (alter, empty, foldrWithKey, fromList, lookup, traverseWithKey)
import qualified Language.Alloy.Call              as Alloy (getInstances)

import Alloy.CdOd.CD2Alloy.Transform    (createRunCommand, mergeParts, transform)
import Alloy.CdOd.CdAndChanges.Instance (fromInstance)
import Alloy.CdOd.Auxiliary.Util        (redColor)
import Alloy.CdOd.Edges (
  DiagramEdge,
  anyMarkedEdge,
  checkMultiEdge,
  fromEdges,
  renameClasses,
  renameEdges,
  )
import Alloy.CdOd.Generate              (generate)
import Alloy.CdOd.Mutation
  (Target (..), getAllMutationResults, nonTargets)
import Alloy.CdOd.Output                (drawCdFromSyntax, drawOdFromInstance)
import Alloy.CdOd.Types
  (ClassConfig (..), Change (..), Connection (..), Syntax, defaultProperties)

import Control.Arrow                    (first, second)
import Control.Monad                    (void, when)
import Control.Monad.Fail               (MonadFail)
import Control.Monad.IO.Class           (liftIO)
import Control.Monad.Random
  (MonadRandom, RandomGen, RandT, evalRandT, mkStdGen)
import Data.GraphViz                    (GraphvizOutput (Pdf, Svg))
import Data.List                        ((\\), intercalate, nub, sort)
import Data.List                        (delete)
import Data.Map                         (Map)
import Data.Maybe                       (fromJust, isJust, listToMaybe)
import Data.Set                         (singleton)
import Data.String.Interpolate          (i)
import GHC.Generics                     (Generic)
import Language.Alloy.Call              (AlloyInstance)
import System.Random.Shuffle            (shuffleM)

debug :: Bool
debug = False

data MatchCdOdInstance = MatchCdOdInstance {
    diagrams  :: Map Int FilePath,
    instances :: Map Char ([Int], FilePath)
  } deriving (Generic, Show)

data MatchCdOdConfig = MatchCdOdConfig {
    classConfig  :: ClassConfig,
    maxObjects   :: Int,
    maxInstances :: Maybe Integer,
    searchSpace  :: Int
  } deriving Generic

defaultMatchCdOdConfig :: MatchCdOdConfig
defaultMatchCdOdConfig = MatchCdOdConfig {
    classConfig  = ClassConfig {
        classes      = (4, 4),
        aggregations = (0, Just 2),
        associations = (0, Just 2),
        compositions = (0, Just 1),
        inheritances = (1, Just 2)
      },
    maxObjects   = 4,
    maxInstances = Nothing,
    searchSpace  = 10
  }

instancesOfMatch :: MatchCdOdInstance -> Map Int String
instancesOfMatch task = nub . sort <$>
  M.foldrWithKey
  (\o (cs, _) m -> foldr (\c -> M.alter (Just . maybe [o] (o:)) c) m cs)
  M.empty
  (instances task)

matchCdOd :: MatchCdOdConfig -> FilePath -> Int -> Int -> IO MatchCdOdInstance
matchCdOd config path segment seed = do
  let g = mkStdGen $ (segment +) $ (4 *) seed
  (cds, ods) <- evalRandT (getRandomTask (classConfig config) (maxObjects config) (searchSpace config) (maxInstances config)) g
  cds' <- (\k c -> drawCdFromSyntax True False Nothing c (cdFilename k) Svg) `M.traverseWithKey` cds
  ods' <- (\k (is,o) -> (is,) <$> drawOdFromInstance o M.empty True (odFilename k is) Svg) `M.traverseWithKey` ods
  return $ MatchCdOdInstance cds' ods'
  where
    cdFilename :: Int -> String
    cdFilename n    = [i|#{path}output-cd#{n}|]
    odFilename :: Char -> [Int] -> String
    odFilename n is = [i|#{path}output-od-#{n}-#{toDescription is 2}|]
    toDescription x n =
      intercalate "and" (show <$> x) ++ foldr ((++) . ("not" ++) . show) [] ([1..n] \\ x)

getRandomTask
  :: RandomGen g
  => ClassConfig
  -> Int
  -> Int
  -> Maybe Integer
  -> RandT g IO (Map Int Syntax, Map Char ([Int], AlloyInstance))
getRandomTask config maxObs search maxIs = do
  (cd1, cd2, cd3, numClasses) <- getRandomCDs config search
  instas <- liftIO $ getODInstances maxObs maxIs cd1 cd2 cd3 numClasses
  mrinstas <- takeRandomInstances instas
  case mrinstas of
    Nothing      -> getRandomTask config maxObs search maxIs
    Just rinstas -> return (M.fromList [(1, cd1), (2, cd2)], M.fromList $ zip ['a' ..] rinstas)

getRandomTask'
  :: RandomGen g
  => ClassConfig
  -> Int
  -> Int
  -> Maybe Integer
  -> RandT g IO (Map Int Syntax, [([Int], AlloyInstance)])
getRandomTask' config maxObs search maxIs = do
  let code = Changes.transform config defaultProperties
  instas <- liftIO $ Alloy.getInstances (Just 6000) code
  when debug $ liftIO $ print $ length instas
  rinstas <- shuffleM instas
  ods <- getODsFor maxObs maxIs rinstas
  maybe (getRandomTask' config maxObs search maxIs) return ods

getODsFor
  :: RandomGen g
  => Int
  -> Maybe Integer
  -> [AlloyInstance]
  -> RandT g IO (Maybe (Map Int Syntax, [([Int], AlloyInstance)]))
getODsFor _          _            []       = return Nothing
getODsFor maxObs maxIs (cd:cds) = do
  (_, [(_, cd1), (_, cd2), (_, cd3)], numClasses) <- applyChanges cd
  instas <- liftIO $ getODInstances maxObs maxIs cd1 cd2 cd3 numClasses
  mrinstas <- takeRandomInstances instas
  case mrinstas of
    Nothing      -> getODsFor maxObs maxIs cds
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
getRandomCDs config search = do
  (names, edges) <- generate config search
  let cd0 = fromEdges names edges
  -- continueIf (not (anyMarkedEdge cd0)) $ do
  when debug . liftIO . void $ drawCdFromSyntax False True (Just redColor) cd0 "debug-0" Pdf
  mutations <- shuffleM $ getAllMutationResults config names edges
  let medges1 = getFirstValidSatisfying (not . anyMarkedEdge) names mutations
  continueIf (isJust medges1) $ do
    mutations' <- shuffleM mutations
    let Just edges1 = medges1
        medges2     = getFirstValidSatisfying (const True) names mutations'
        Just edges2 = medges2
        notOnlyInhs = not $ null $ nonTargets (singleton TInheritance) $ edges1 ++ edges2
    continueIf (isJust medges2 && notOnlyInhs) $ do
      [cd1, cd2] <- shuffleM [fromEdges names edges1, fromEdges names edges2]
      mutations'' <- shuffleM mutations
      let Just edges3 = getFirstValidSatisfying (not . anyMarkedEdge) names mutations''
          cd3         = fromEdges names edges3
      when debug . void. liftIO $ drawCdFromSyntax False True (Just redColor) cd3 "debug-3" Pdf
      return (cd1, cd2, cd3, length names)
  where
    continueIf True  m = m
    continueIf False _ = getRandomCDs config search

getODInstances
  :: Int
  -> Maybe Integer
  -> Syntax
  -> Syntax
  -> Syntax
  -> Int
  -> IO (Map [Int] [AlloyInstance])
getODInstances maxObs maxIs cd1 cd2 cd3 numClasses = do
  -- TODO remove `toOldSyntax`
  let parts1 = case transform (toOldSyntax cd1) "1" "" of (p1, p2, p3, p4, _) -> (p1, p2, p3, p4)
      parts2 = case transform (toOldSyntax cd2) "2" "" of (p1, p2, p3, p4, _) -> (p1, p2, p3, p4)
      parts1and2 = mergeParts parts1 parts2
      parts3 = case transform (toOldSyntax cd3) "3" "" of (p1, p2, p3, p4, _) -> (p1, p2, p3, p4)
      cd1not2 = createRunCommand "cd1 and (not cd2)" numClasses maxObs
      cd2not1 = createRunCommand "cd2 and (not cd1)" numClasses maxObs
      cd1and2 = createRunCommand "cd1 and cd2" numClasses maxObs
      cdNot1not2 = createRunCommand "(not cd1) and (not cd2) and cd3" numClasses maxObs
  instances1not2 <- Alloy.getInstances maxIs (combineParts parts1and2 ++ cd1not2)
  instances2not1 <- Alloy.getInstances maxIs (combineParts parts1and2 ++ cd2not1)
  instances1and2 <- Alloy.getInstances maxIs (combineParts parts1and2 ++ cd1and2)
  instancesNot1not2 <- Alloy.getInstances maxIs (combineParts (mergeParts parts1and2 parts3) ++ cdNot1not2)
  when debug . print $ length instances1not2
  when debug . print $ length instances2not1
  when debug . print $ length instances1and2
  when debug . print $ length instancesNot1not2
  return $ M.fromList [([1]  , instances1not2),
                       ([2]  , instances2not1),
                       ([1,2], instances1and2),
                       ([]   , instancesNot1not2)]
  where
    combineParts (p1, p2, p3, p4) = p1 ++ p2 ++ p3 ++ p4
    toOldSyntax = first (second listToMaybe <$>)

takeRandomInstances
  :: (MonadRandom m, MonadFail m) => Map [Int] [a] -> m (Maybe [([Int], a)])
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
      shuffleM $ concatMap ($ rinstas) ts

getFirstValidSatisfying
  :: (Syntax -> Bool) -> [String] -> [[DiagramEdge]] -> Maybe [DiagramEdge]
getFirstValidSatisfying _ _     []
  = Nothing
getFirstValidSatisfying p names (x:xs)
  | checkMultiEdge x, p (fromEdges names x)
  = Just x
  | otherwise
  = getFirstValidSatisfying p names xs
