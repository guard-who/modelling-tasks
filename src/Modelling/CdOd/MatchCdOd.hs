{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
module Modelling.CdOd.MatchCdOd (
  MatchCdOdConfig (..),
  MatchCdOdInstance (..),
  applyChanges,
  debug,
  defaultMatchCdOdConfig,
  getRandomTask',
  getRandomTask,
  instancesOfMatch,
  matchCdOd,
  matchCdOdEvaluation,
  matchCdOdTask,
  newMatchCdOdInstances,
  ) where

import qualified Modelling.CdOd.CdAndChanges.Transform as Changes (transform)

import qualified Data.Bimap                       as BM (fromList, keysR, size)
import qualified Data.Map                         as M (
  alter,
  differenceWith,
  empty,
  foldrWithKey,
  fromAscList,
  fromList,
  lookup,
  toList,
  traverseWithKey,
  union,
  )

import Modelling.Auxiliary.Output (
  OutputMonad (..),
  directionsAdvice,
  hoveringInformation,
  simplifiedInformation,
  LangM,
  )
import Modelling.CdOd.CD2Alloy.Transform (createRunCommand, mergeParts, transform)
import Modelling.CdOd.CdAndChanges.Instance (fromInstance)
import Modelling.CdOd.Auxiliary.Util (
  alloyInstanceToOd,
  getInstances,
  redColor,
  )
import Modelling.CdOd.Edges (
  DiagramEdge,
  anyMarkedEdge,
  checkMultiEdge,
  fromEdges,
  renameClasses,
  renameEdges,
  toEdges,
  )
import Modelling.CdOd.Generate          (generate)
import Modelling.CdOd.Mutation
  (Target (..), getAllMutationResults, nonTargets)
import Modelling.CdOd.Output
  (drawCdFromSyntax, drawOdFromNodesAndEdges, getDirs)
import Modelling.CdOd.Types (
  ClassConfig (..),
  Change (..),
  Connection (..),
  Letters (Letters, lettersList),
  Od,
  Syntax,
  associationNames,
  classNames,
  defaultProperties,
  linkNames,
  renameAssocsInCd,
  renameClassesInCd,
  renameClassesInOd,
  renameLinksInOd,
  toOldSyntax,
  )

import Control.Monad                    (void, when)
import Control.Monad.Catch              (MonadThrow)
import Control.Monad.Except             (runExceptT)
#if __GLASGOW_HASKELL__ < 808
import Control.Monad.Fail               (MonadFail)
#endif
import Control.Monad.IO.Class           (MonadIO (liftIO))
import Control.Monad.Random
  (MonadRandom (getRandom), RandT, RandomGen, evalRandT, mkStdGen)
import Control.Monad.Trans              (MonadTrans (lift))
import Data.Bitraversable               (bimapM)
import Data.GraphViz                    (GraphvizOutput (Pdf, Svg))
import Data.List (
  (\\),
  delete,
  intercalate,
  nub,
  permutations,
  sort,
  )
import Data.Map                         (Map)
import Data.Maybe                       (fromJust)
import Data.Set                         (singleton)
import Data.String.Interpolate          (i)
import GHC.Generics                     (Generic)
import Language.Alloy.Call              (AlloyInstance)
import System.Random.Shuffle            (shuffleM)

debug :: Bool
debug = False

data MatchCdOdInstance = MatchCdOdInstance {
    diagrams       :: Map Int Syntax,
    generatorValue :: Int,
    instances      :: Map Char ([Int], Od)
  } deriving (Generic, Read, Show)

data MatchCdOdConfig = MatchCdOdConfig {
    allowSelfLoops   :: Maybe Bool,
    classConfig      :: ClassConfig,
    maxObjects       :: Int,
    maxInstances     :: Maybe Integer,
    searchSpace      :: Int,
    timeout          :: Maybe Int
  } deriving (Generic, Read, Show)

defaultMatchCdOdConfig :: MatchCdOdConfig
defaultMatchCdOdConfig = MatchCdOdConfig {
    allowSelfLoops   = Nothing,
    classConfig  = ClassConfig {
        classes      = (4, 4),
        aggregations = (0, Just 2),
        associations = (0, Just 2),
        compositions = (0, Just 1),
        inheritances = (1, Just 2)
      },
    maxObjects       = 4,
    maxInstances     = Nothing,
    searchSpace      = 10,
    timeout          = Nothing
  }

instancesOfMatch :: MatchCdOdInstance -> Map Int Letters
instancesOfMatch task = Letters . nub . sort <$>
  M.foldrWithKey
  (\o (cs, _) m -> foldr (M.alter (Just . maybe [o] (o:))) m cs)
  M.empty
  (instances task)

matchCdOdTask
  :: (MonadIO m, OutputMonad m)
  => FilePath
  -> MatchCdOdInstance
  -> LangM m
matchCdOdTask path task = do
  let dirs = foldr (M.union . getDirs . toEdges) M.empty $ diagrams task
      anonymous o = length (fst o) `div` 3
  cds <- lift $ liftIO $
    (\k c -> drawCdFromSyntax True True Nothing c (cdFilename k) Svg)
    `M.traverseWithKey` diagrams task
  ods <- lift $ liftIO $ flip evalRandT (mkStdGen $ generatorValue task) $
    (\k (is,o) -> (is,) <$> uncurry drawOdFromNodesAndEdges
      o (anonymous o) dirs True (odFilename k is) Svg)
    `M.traverseWithKey` instances task
  paragraph $ text "Consider the following two class diagrams."
  images show id cds
  paragraph $ text
    [i|Which of the following five object diagrams conform to which class diagram?
An object diagram can conform to neither, either, or both class diagrams.|]
  images (:[]) snd ods
  paragraph $ do
    text [i|Please state your answer by giving a list of pairs, each comprising of a class diagram number and a string of object diagram letters.
Each pair indicates that the mentioned object diagrams conform to the respective class diagram.
For example, |]
    code $ show matchCdOdInitial
    text [i| expresses that among the offered choices exactly the object diagrams a and b are instances of class diagram 1 and that none of the offered object diagrams are instances of class diagram 2.|]
  paragraph simplifiedInformation
  paragraph directionsAdvice
  paragraph hoveringInformation
  where
    cdFilename :: Int -> String
    cdFilename n    = [i|#{path}output-cd#{n}|]
    odFilename :: Char -> [Int] -> String
    odFilename n is = [i|#{path}output-od-#{n}-#{toDescription is 2}|]
    toDescription x n =
      intercalate "and" (map show x) ++ concatMap (("not" ++) . show) ([1..n] \\ x)

matchCdOdInitial :: [(Int, Letters)]
matchCdOdInitial = [(1, Letters "ab"), (2, Letters "")]

matchCdOdEvaluation
  :: (OutputMonad m, Foldable t, Functor t)
  => MatchCdOdInstance
  -> t (Int, Letters)
  -> LangM m
matchCdOdEvaluation task is' = do
  paragraph $ text "Remarks on your solution:"
  let is = Letters . nub . sort
        <$> foldr
          (\(c, o) -> M.alter (Just . maybe o (o++)) c)
          M.empty
          (fmap lettersList <$> is')
  assertion (null $ notInstanceOf is) $ text "Given instances are correct?"
  assertion (is == instancesOfMatch task) $ text "Given instances are exhaustive?"
  where
    notInstanceOf :: Map Int Letters -> Map Int Letters
    notInstanceOf is = M.differenceWith
      (\f s -> Letters <$> maybeList (lettersList f \\ lettersList s))
      is
      $ instancesOfMatch task
    maybeList [] = Nothing
    maybeList l  = Just l

matchCdOd :: MatchCdOdConfig -> Int -> Int -> IO MatchCdOdInstance
matchCdOd config segment seed = do
  let g = mkStdGen $ (segment +) $ 4 * seed
  (g', (cds, ods)) <- flip evalRandT g $ (,) <$> getRandom <*> getRandomTask
    (classConfig config)
    (allowSelfLoops config)
    (maxObjects config)
    (searchSpace config)
    (maxInstances config)
    (timeout config)
  ods' <- either error id <$> runExceptT (mapM (mapM alloyInstanceToOd) ods)
  let names  = nub $ concat $ classNames <$> cds
      assocs = nub $ concat (associationNames <$> cds)
        ++ concat (linkNames . snd <$> ods')
  names'  <- shuffleM names
  assocs' <- shuffleM assocs
  let inst = MatchCdOdInstance cds g' ods'
  renameInstance inst names' assocs'

newMatchCdOdInstances
  :: (MonadFail m, MonadRandom m, MonadThrow m)
  => MatchCdOdInstance
  -> m [MatchCdOdInstance]
newMatchCdOdInstances inst = do
  let names = nub $ concat $ classNames <$> diagrams inst
      assocs = nub $ concat (associationNames <$> diagrams inst)
        ++ concat (linkNames . snd <$> instances inst)
  names'  <- shuffleM $ tail $ permutations names
  assocs' <- shuffleM $ tail $ permutations assocs
  sequence
    [ renameInstance inst ns as >>= shuffleInstance
    | (ns, as) <- zip names' (concat $ replicate 3 assocs')
    ]

shuffleInstance
  :: (MonadFail m, MonadRandom m)
  => MatchCdOdInstance
  -> m MatchCdOdInstance
shuffleInstance inst = do
  cds <- shuffleM $ M.toList $ diagrams inst
  ods <- shuffleM $ M.toList $ instances inst
  let changeId x (y, cd) = ((y, x), (x, cd))
      (idMap, cds') = unzip $ zipWith changeId [1..] cds
      replaceId x (_, od) = (x, od)
      rename = maybe (fail "invalid match-cd-od instance") return
        . (`lookup` idMap)
  ods' <- mapM (mapM $ bimapM (mapM rename) return)
    $ zipWith replaceId ['a'..] ods
  return $ MatchCdOdInstance {
    diagrams = M.fromAscList cds',
    generatorValue = generatorValue inst,
    instances = M.fromAscList ods'
    }

renameInstance
  :: MonadThrow m
  => MatchCdOdInstance
  -> [String]
  -> [String]
  -> m MatchCdOdInstance
renameInstance inst names' assocs' = do
  let cds = diagrams inst
      ods = instances inst
      names = nub $ concat $ classNames <$> diagrams inst
      assocs = nub $ concat (associationNames <$> diagrams inst)
        ++ concat (linkNames . snd <$> instances inst)
      bmNames  = BM.fromList $ zip names names'
      bmAssocs = BM.fromList $ zip assocs assocs'
      renameCd cd = renameClassesInCd bmNames cd >>= renameAssocsInCd bmAssocs
      renameOd od = renameClassesInOd bmNames od >>= renameLinksInOd bmAssocs
  cds' <- renameCd `mapM` cds
  ods' <- mapM renameOd `mapM` ods
  return $ MatchCdOdInstance {
    diagrams = cds',
    generatorValue = generatorValue inst,
    instances = ods'
    }

getRandomTask
  :: RandomGen g
  => ClassConfig
  -> Maybe Bool
  -> Int
  -> Int
  -> Maybe Integer
  -> Maybe Int
  -> RandT g IO (Map Int Syntax, Map Char ([Int], AlloyInstance))
getRandomTask config selfLoops maxObs search maxIs to = do
  (cd1, cd2, cd3, numClasses) <- getRandomCDs config search
  instas <- liftIO $ getODInstances maxObs maxIs to cd1 cd2 cd3 numClasses selfLoops
  mrinstas <- takeRandomInstances instas
  case mrinstas of
    Nothing      -> getRandomTask config selfLoops maxObs search maxIs to
    Just rinstas -> return (M.fromList [(1, cd1), (2, cd2)], M.fromList $ zip ['a' ..] rinstas)

getRandomTask'
  :: RandomGen g
  => ClassConfig
  -> Maybe Bool
  -> Int
  -> Int
  -> Maybe Integer
  -> RandT g IO (Map Int Syntax, [([Int], AlloyInstance)])
getRandomTask' config selfLoops maxObs search maxIs = do
  let alloyCode = Changes.transform config defaultProperties
  instas <- liftIO $ getInstances (Just 6000) Nothing alloyCode
  when debug $ liftIO $ print $ length instas
  rinstas <- shuffleM instas
  ods <- getODsFor maxObs maxIs Nothing selfLoops rinstas
  maybe (getRandomTask' config selfLoops maxObs search maxIs) return ods

getODsFor
  :: RandomGen g
  => Int
  -> Maybe Integer
  -> Maybe Int
  -> Maybe Bool
  -> [AlloyInstance]
  -> RandT g IO (Maybe (Map Int Syntax, [([Int], AlloyInstance)]))
getODsFor _      _     _  _         []       = return Nothing
getODsFor maxObs maxIs to selfLoops (cd:cds) = do
  (_, [(_, cd1), (_, cd2), (_, cd3)], numClasses) <- applyChanges cd
  instas <- liftIO $ getODInstances maxObs maxIs to cd1 cd2 cd3 numClasses selfLoops
  mrinstas <- takeRandomInstances instas
  case mrinstas of
    Nothing      -> getODsFor maxObs maxIs to selfLoops cds
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
  let bme = BM.fromList $ zip es' $ map (:[]) ['z', 'y' ..]
      bmc = BM.fromList $ zip cs' $ map (:[]) ['A' ..]
      cd  = getSyntax bmc bme edges0 $ Change Nothing Nothing
      cds = map (getSyntax bmc bme edges0) changes
  let changes' = map (fmap $ head . renameClasses bmc . renameEdges bme . (:[])) changes
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
  (names, edges) <- generate Nothing config search
  let cd0 = fromEdges names edges
  -- continueIf (not (anyMarkedEdge cd0)) $ do
  when debug . liftIO . void $ drawCdFromSyntax False True (Just redColor) cd0 "debug-0" Pdf
  mutations <- shuffleM $ getAllMutationResults config names edges
  let medges1 = getFirstValidSatisfying (not . anyMarkedEdge) names mutations
  continueWithJust medges1 (const True) $ \edges1 -> do
    mutations' <- shuffleM mutations
    let medges2     = getFirstValidSatisfying (const True) names mutations'
        notOnlyInhs = not . null . nonTargets (singleton TInheritance) . (edges1 ++)
    continueWithJust medges2 notOnlyInhs $ \edges2 -> do
      [cd1, cd2] <- shuffleM [fromEdges names edges1, fromEdges names edges2]
      mutations'' <- shuffleM mutations
      let medges3 = getFirstValidSatisfying (not . anyMarkedEdge) names mutations''
      continueWithJust medges3 (const True) $ \edges3 -> do
        let cd3         = fromEdges names edges3
        when debug . void . liftIO
          $ drawCdFromSyntax False True (Just redColor) cd3 "debug-3" Pdf
        return (cd1, cd2, cd3, length names)
  where
    continueWithJust mx p m
      | Just x <- mx, p x = m x
      | otherwise         = getRandomCDs config search

getODInstances
  :: Int
  -> Maybe Integer
  -> Maybe Int
  -> Syntax
  -> Syntax
  -> Syntax
  -> Int
  -> Maybe Bool
  -> IO (Map [Int] [AlloyInstance])
getODInstances maxObs maxIs to cd1 cd2 cd3 numClasses selfLoops = do
  -- TODO remove `toOldSyntax`
  let parts1 = getFourParts cd1 "1"
      parts2 = getFourParts cd2 "2"
      parts1and2 = mergeParts parts1 parts2
      parts3 = getFourParts cd3 "3"
      cd1not2 = runCommand "cd1 and (not cd2)"
      cd2not1 = runCommand "cd2 and (not cd1)"
      cd1and2 = runCommand "cd1 and cd2"
      cdNot1not2 = runCommand "(not cd1) and (not cd2) and cd3"
  instances1not2 <- getInstances maxIs to (combineParts parts1and2 ++ cd1not2)
  instances2not1 <- getInstances maxIs to (combineParts parts1and2 ++ cd2not1)
  instances1and2 <- getInstances maxIs to (combineParts parts1and2 ++ cd1and2)
  instancesNot1not2 <- getInstances maxIs to (combineParts (mergeParts parts1and2 parts3) ++ cdNot1not2)
  when debug . print $ length instances1not2
  when debug . print $ length instances2not1
  when debug . print $ length instances1and2
  when debug . print $ length instancesNot1not2
  return $ M.fromList [([1]  , instances1not2),
                       ([2]  , instances2not1),
                       ([1,2], instances1and2),
                       ([]   , instancesNot1not2)]
  where
    getFourParts cd nr = case transform (toOldSyntax cd) selfLoops False nr "" of
      (p1, p2, p3, p4, _) -> (p1, p2, p3, p4)
    runCommand x = createRunCommand selfLoops x numClasses maxObs
    combineParts (p1, p2, p3, p4) = p1 ++ p2 ++ p3 ++ p4

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
