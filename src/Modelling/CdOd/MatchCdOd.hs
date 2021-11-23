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
  defaultMatchCdOdInstance,
  getRandomTask',
  getRandomTask,
  instancesOfMatch,
  matchCdOd,
  matchCdOdEvaluation,
  matchCdOdSyntax,
  matchCdOdTask,
  newMatchCdOdInstances,
  ) where

import qualified Modelling.CdOd.CdAndChanges.Transform as Changes (transform)

import qualified Data.Bimap                       as BM (fromList, keysR, size)
import qualified Data.Map                         as M (
  alter,
  empty,
  foldrWithKey,
  fromAscList,
  fromList,
  insert,
  keys,
  lookup,
  toList,
  traverseWithKey,
  union,
  )

import Modelling.Auxiliary.Output (
  LangM,
  OutputMonad (..),
  Rated,
  addPretext,
  directionsAdvice,
  english,
  german,
  hoveringInformation,
  multipleChoice,
  simplifiedInformation,
  translate,
  translations,
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
  AssociationType (Association, Composition),
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
  showLetters,
  toOldSyntax,
  )

import Control.Monad                    (void, when)
import Control.Monad.Catch              (MonadThrow)
import Control.Monad.Except             (runExceptT)
#if __GLASGOW_HASKELL__ < 808
import Control.Monad.Fail               (MonadFail)
#endif
import Control.Monad.IO.Class           (MonadIO (liftIO))
import Control.Monad.Random (
  MonadRandom (getRandom),
  RandT,
  RandomGen,
  evalRandT,
  mapRandT,
  mkStdGen,
  )
import Control.Monad.Trans              (MonadTrans (lift))
import Data.Bifunctor                   (Bifunctor (second))
import Data.Bitraversable               (bimapM)
import Data.Containers.ListUtils        (nubOrd)
import Data.GraphViz                    (GraphvizOutput (Pdf, Svg))
import Data.List (
  (\\),
  delete,
  insert,
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
  paragraph $ translate $ do
    english "Consider the following two class diagrams."
    german "Betrachten Sie die folgenden zwei Klassendiagramme."
  images show id cds
  paragraph $ translate $ do
    english [i|Which of the following five object diagrams conform to which class diagram?
An object diagram can conform to neither, either, or both class diagrams.|]
    german [i|Welche der folgenden fünf Objektdiagramme passen zu welchem Klassendiagramm?
Ein Objektdiagramm kann zu keinem, einem oder beiden Klassendiagrammen passen.|]
  images (:[]) snd ods
  paragraph $ do
    translate $ do
      english [i|Please state your answer by giving a list of pairs, each comprising of a class diagram number and object diagram letters.
Each pair indicates that the mentioned object diagrams conform to the respective class diagram.
For example, |]
      english [i|Bitte geben Sie Ihre Antwort in Form einer Liste von Paaren an, die jeweils aus einer Klassendiagrammnummer und aus Objektdiagrammbuchstaben bestehen.
Jedes Paar gibt an, dass die genannten Objektdiagramme zu dem jeweiligen Klassendiagramm passen.
Zum Beispiel drückt |]
    code $ showMatching matchCdOdInitial
    translate $ do
      english [i|expresses that among the offered choices exactly the object diagrams a and b are instances of class diagram 1 and that none of the offered object diagrams are instances of class diagram 2.|]
      english [i|aus, dass unter den angebotenen Auswahlmöglichkeiten genau die Objektdiagramme a und b Instanzen des Klassendiagramms 1 sind und dass keines der angebotenen Objektdiagramme Instanz des Klassendiagramms 2 ist.|]
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

newtype ShowLetters = ShowLetters { showLetters' :: Letters }

instance Show ShowLetters where
  show = showLetters . showLetters'

showMatching :: [(Int, Letters)] -> String
showMatching = show . fmap (second ShowLetters)

matchCdOdInitial :: [(Int, Letters)]
matchCdOdInitial = [(1, Letters "ab"), (2, Letters "")]

matchCdOdSyntax
  :: (OutputMonad m, Foldable t, Functor t)
  => MatchCdOdInstance
  -> t (Int, Letters)
  -> LangM m
matchCdOdSyntax task sub = addPretext $ do
  assertion (all availableCd $ fst <$> sub) $ translate $ do
    english "Referenced class diagrams were provided within task"
    german "Referenzierte Klassendiagramme sind Bestandteil der Aufgabenstellung"
  assertion (all (all availableOd) $ lettersList . snd <$> sub) $ translate $ do
    english "Referenced object diagrams were provided within task"
    german "Referenced Objektdiagramme sind Bestandteil der Aufgabenstellung"
  where
    availableCd = (`elem` M.keys (diagrams task))
    availableOd = (`elem` M.keys (instances task))

matchCdOdEvaluation
  :: (OutputMonad m, Foldable t)
  => MatchCdOdInstance
  -> t (Int, Letters)
  -> Rated m
matchCdOdEvaluation task sub' = do
  let sub = toMatching sub'
      correct = foldr (`M.insert` True) M.empty $ toMatching $ M.toList
        $ instancesOfMatch task
      what = translations $ do
        english "instances"
        german "Instanzen"
  multipleChoice what Nothing correct sub
  where
    toMatching :: (Ord x, Foldable f) => f (x, Letters) -> [(x, Char)]
    toMatching = nubOrd .
      foldr (\(c, ys) xs -> foldr (insert . (c,)) xs (lettersList ys)) []

matchCdOd :: MatchCdOdConfig -> Int -> Int -> IO MatchCdOdInstance
matchCdOd config segment seed = do
  let g = mkStdGen $ (segment +) $ 4 * seed
  evalRandT (getMatchCdOdTask config) g

getMatchCdOdTask
  :: (RandomGen g, MonadIO m, MonadFail m, MonadThrow m)
  => MatchCdOdConfig
  -> RandT g m MatchCdOdInstance
getMatchCdOdTask config = do
  (cds, ods) <- mapRandT liftIO $ getRandomTask
    (classConfig config)
    (allowSelfLoops config)
    (maxObjects config)
    (searchSpace config)
    (maxInstances config)
    (timeout config)
  ods' <- runExceptT (mapM (mapM alloyInstanceToOd) ods)
    >>= either fail return
  let names  = nubOrd $ concat $ classNames <$> cds
      assocs = nubOrd $ concat (associationNames <$> cds)
        ++ concat (linkNames . snd <$> ods')
  names'  <- shuffleM names
  assocs' <- shuffleM assocs
  g' <- getRandom
  let inst = MatchCdOdInstance cds g' ods'
  lift $ renameInstance inst names' assocs'

defaultMatchCdOdInstance :: MatchCdOdInstance
defaultMatchCdOdInstance = MatchCdOdInstance {
  diagrams = M.fromList [
    (1,(
      [("C",[]),("A",["C"]),("D",[]),("B",[])],
      [(Association,"z",(2,Just 2),"B","C",(1,Just 2)),
       (Association,"y",(0,Just 2),"B","D",(0,Just 2)),
       (Composition,"x",(1,Just 1),"C","D",(0,Just 2))
      ]
    )),
    (2,(
      [("C",[]),("A",["C"]),("D",[]),("B",[])],
      [(Composition,"x",(1,Just 1),"C","D",(1,Just 2)),
       (Association,"y",(0,Just 2),"B","D",(0,Just 2))
      ]
    ))
    ],
  generatorValue = 7777369639206507645,
  instances = M.fromList [
    ('a',([1],(
      ["B$0","B$1","C$0","D$0"],
      [(0,2,"z"),(1,2,"z"),(1,3,"y"),(2,3,"x")]
    ))),
    ('b',([1],(
      ["B$0","B$1","C$0","D$0"],
      [(0,2,"z"),(1,2,"z"),(2,3,"x")]
    ))),
    ('c',([2],(
      ["A$0","C$0","D$0","D$1"],
      [(0,2,"x"),(1,3,"x")]
    ))),
    ('d',([2],(
      ["A$0","B$0","D$0"],
      [(0,2,"x")]
    ))),
    ('e',([],(
      ["A$0","B$0","D$0"],
      [(0,2,"x"),(1,0,"z"),(1,2,"y")]
    )))
    ]
  }

newMatchCdOdInstances
  :: (MonadFail m, MonadRandom m, MonadThrow m, RandomGen g)
  => MatchCdOdInstance
  -> RandT g m [MatchCdOdInstance]
newMatchCdOdInstances inst = do
  let names = nub $ concat $ classNames <$> diagrams inst
      assocs = nub $ concat (associationNames <$> diagrams inst)
        ++ concat (linkNames . snd <$> instances inst)
  names'  <- shuffleM $ tail $ permutations names
  assocs' <- shuffleM $ tail $ permutations assocs
  sequence
    [ lift $ renameInstance inst ns as >>= shuffleInstance
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
