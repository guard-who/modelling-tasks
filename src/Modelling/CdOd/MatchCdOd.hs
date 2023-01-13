{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Modelling.CdOd.MatchCdOd (
  MatchCdOdConfig (..),
  MatchCdOdInstance (..),
  applyChanges,
  checkMatchCdOdConfig,
  debug,
  defaultMatchCdOdConfig,
  defaultMatchCdOdInstance,
  getMatchCdOdTask,
  getODInstances,
  matchCdOd,
  matchCdOdEvaluation,
  matchCdOdSolution,
  matchCdOdSyntax,
  matchCdOdTask,
  matchingShow,
  newMatchCdOdInstances,
  takeRandomInstances,
  ) where

import qualified Modelling.CdOd.CdAndChanges.Transform as Changes (transform)

import qualified Data.Bimap                       as BM (fromList, keysR, size)
import qualified Data.Map                         as M (
  adjust,
  empty,
  foldrWithKey,
  fromAscList,
  fromList,
  keys,
  lookup,
  toList,
  traverseWithKey,
  union,
  )

import Modelling.Auxiliary.Common (
  Randomise (randomise),
  RandomiseLayout (randomiseLayout),
  shuffleEverything,
  )
import Modelling.Auxiliary.Output (
  addPretext,
  directionsAdvice,
  hoveringInformation,
  simplifiedInformation,
  )
import Modelling.CdOd.CD2Alloy.Transform (
  combineParts,
  createRunCommand,
  mergeParts,
  transform,
  )
import Modelling.CdOd.CdAndChanges.Instance (fromInstance)
import Modelling.CdOd.Auxiliary.Util (
  alloyInstanceToOd,
  getInstances,
  )
import Modelling.CdOd.Edges (
  DiagramEdge,
  fromEdges,
  renameClasses,
  renameEdges,
  toEdges,
  )
import Modelling.CdOd.Output
  (cacheCd, cacheOd, getDirs)
import Modelling.CdOd.Types (
  AssociationType (Association, Composition),
  ClassConfig (..),
  Change (..),
  Connection (..),
  Letters (Letters, lettersList),
  ObjectConfig (..),
  Od,
  Syntax,
  associationNames,
  checkClassConfigWithProperties,
  classNames,
  defaultProperties,
  linkNames,
  renameAssocsInCd,
  renameClassesInCd,
  renameClassesInOd,
  renameLinksInOd,
  showLetters,
  shuffleClassAndConnectionOrder,
  shuffleObjectAndLinkOrder,
  toOldSyntax,
  )

import Control.Monad                    ((>=>), when)
import Control.Monad.Catch              (MonadThrow)
import Control.Monad.Except             (runExceptT)
#if __GLASGOW_HASKELL__ < 808
import Control.Monad.Fail               (MonadFail)
#endif
import Control.Monad.IO.Class           (MonadIO (liftIO))
import Control.Monad.Output (
  LangM,
  OutputMonad (..),
  Rated,
  english,
  german,
  multipleChoice,
  translate,
  translations,
  )
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
import Data.List (
  delete,
  permutations,
  )
import Data.Map                         (Map)
import Data.Maybe                       (fromJust)
import Data.String.Interpolate          (iii)
import GHC.Generics                     (Generic)
import Language.Alloy.Call              (AlloyInstance)
import System.Random.Shuffle            (shuffleM)

debug :: Bool
debug = False

data MatchCdOdInstance = MatchCdOdInstance {
    diagrams       :: Map Int Syntax,
    generatorValue :: Int,
    instances      :: Map Char ([Int], Od),
    showSolution   :: Bool
  } deriving (Generic, Read, Show)

data MatchCdOdConfig = MatchCdOdConfig {
    classConfig      :: ClassConfig,
    maxInstances     :: Maybe Integer,
    objectConfig     :: ObjectConfig,
    presenceOfLinkSelfLoops :: Maybe Bool,
    printSolution    :: Bool,
    searchSpace      :: Int,
    timeout          :: Maybe Int
  } deriving (Generic, Read, Show)

{-# DEPRECATED searchSpace "because Modelling.Cd.generate' is not used anymore and will be removed soon" #-}

defaultMatchCdOdConfig :: MatchCdOdConfig
defaultMatchCdOdConfig = MatchCdOdConfig {
    classConfig  = ClassConfig {
        classes      = (4, 4),
        aggregations = (0, Just 2),
        associations = (0, Just 2),
        compositions = (0, Just 1),
        inheritances = (1, Just 2),
        relationships = (4, Just 6)
      },
    maxInstances     = Nothing,
    objectConfig = ObjectConfig {
      links          = (4, Just 10),
      linksPerObject = (0, Just 4),
      objects        = (2, 4)
      },
    presenceOfLinkSelfLoops = Nothing,
    printSolution    = False,
    searchSpace      = 10,
    timeout          = Nothing
  }

toMatching :: Map Char [Int] -> Map (Int, Char) Bool
toMatching m =
  M.fromList [((cd, od), any (cd `elem`) $ M.lookup od m) | cd <- cds, od <- ods]
  where
    cds = take 2 [1 ..]
    ods = take 5 ['a' ..]

checkMatchCdOdConfig :: MatchCdOdConfig -> Maybe String
checkMatchCdOdConfig config = checkClassConfigWithProperties
  (classConfig config)
  defaultProperties

matchCdOdTask
  :: (MonadIO m, OutputMonad m)
  => FilePath
  -> MatchCdOdInstance
  -> LangM m
matchCdOdTask path task = do
  let dirs = foldr (M.union . getDirs . toEdges) M.empty $ diagrams task
      anonymous o = length (fst o) `div` 3
  cds <- lift $ liftIO $
    (\_ c -> cacheCd True True mempty c path)
    `M.traverseWithKey` diagrams task
  ods <- lift $ liftIO $ flip evalRandT (mkStdGen $ generatorValue task) $
    (\_ (is,o) -> (is,) <$> uncurry cacheOd
      o (anonymous o) dirs True path)
    `M.traverseWithKey` instances task
  paragraph $ translate $ do
    english "Consider the following two class diagrams."
    german "Betrachten Sie die folgenden zwei Klassendiagramme."
  images show id cds
  paragraph $ translate $ do
    english [iii|
      Which of the following five object diagrams conform to which class diagram?
      \n
      An object diagram can conform to neither, either, or both class diagrams.
      |]
    german [iii|
      Welche der folgenden fünf Objektdiagramme
      passen zu welchem Klassendiagramm?
      \n
      Ein Objektdiagramm kann zu keinem,
      einem oder beiden Klassendiagrammen passen.
      |]
  images (:[]) snd ods
  paragraph $ do
    translate $ do
      english [iii|
        Please state your answer by giving a list of pairs,
        each comprising of a class diagram number and object diagram letters.
        \n
        Each pair indicates that the mentioned object diagrams conform to the
        respective class diagram.
        \n
        For example,#{" "}|]
      german [iii|
        Bitte geben Sie Ihre Antwort in Form einer Liste von Paaren an,
        die jeweils aus einer Klassendiagrammnummer und
        aus Objektdiagrammbuchstaben bestehen.
        \n
        Jedes Paar gibt an, dass die genannten Objektdiagramme
        zu dem jeweiligen Klassendiagramm passen.
        \n
        Zum Beispiel drückt#{" "}|]
    code . show $ matchingShow matchCdOdInitial
    translate $ do
      english [iii|
        expresses that among the offered choices exactly
        the object diagrams a and b are instances of class diagram 1 and
        that none of the offered object diagrams
        are instances of class diagram 2.
        |]
      german [iii|
        aus, dass unter den angebotenen Auswahlmöglichkeiten
        genau die Objektdiagramme a und b Instanzen des Klassendiagramms 1 sind
        und dass keines der angebotenen Objektdiagramme
        Instanz des Klassendiagramms 2 ist.
        |]
  paragraph simplifiedInformation
  paragraph directionsAdvice
  paragraph hoveringInformation

newtype ShowLetters = ShowLetters { showLetters' :: Letters }

instance Show ShowLetters where
  show = showLetters . showLetters'

matchingShow :: [(Int, Letters)] -> [(Int, ShowLetters)]
matchingShow = fmap (second ShowLetters)

matchCdOdInitial :: [(Int, Letters)]
matchCdOdInitial = [(1, Letters "ab"), (2, Letters "")]

matchCdOdSyntax
  :: (OutputMonad m, Foldable t, Functor t)
  => MatchCdOdInstance
  -> t (Int, Letters)
  -> LangM m
matchCdOdSyntax task sub = addPretext $ do
  assertion (all availableCd $ fst <$> sub) $ translate $ do
    english "Referenced class diagrams were provided within task?"
    german [iii|
      Referenzierte Klassendiagramme sind Bestandteil der Aufgabenstellung?
      |]
  assertion (all (all availableOd) $ lettersList . snd <$> sub) $ translate $ do
    english "Referenced object diagrams were provided within task?"
    german "Referenzierte Objektdiagramme sind Bestandteil der Aufgabenstellung?"
  where
    availableCd = (`elem` M.keys (diagrams task))
    availableOd = (`elem` M.keys (instances task))

matchCdOdEvaluation
  :: (OutputMonad m, Foldable t)
  => MatchCdOdInstance
  -> t (Int, Letters)
  -> Rated m
matchCdOdEvaluation task sub' = do
  let sub = toMatching' sub'
      sol = fst <$> instances task
      matching = toMatching sol
      what = translations $ do
        english "instances"
        german "Instanzen"
      solution =
        if showSolution task
        then Just . show . matchingShow $ matchCdOdSolution task
        else Nothing
  multipleChoice what solution matching sub
  where
    toMatching' :: Foldable f => f (Int, Letters) -> [(Int, Char)]
    toMatching' =
      foldr (\(c, ys) xs -> foldr ((:) . (c,)) xs (lettersList ys)) []

matchCdOdSolution :: MatchCdOdInstance -> [(Int, Letters)]
matchCdOdSolution = M.toList . reverseMapping . fmap fst . instances
  where
    reverseMapping :: Map Char [Int] -> Map Int Letters
    reverseMapping = fmap (fmap Letters) . M.foldrWithKey
      (\x ys xs -> foldr (M.adjust (x:)) xs ys)
      $ M.fromList [(1, []), (2, [])]

matchCdOd :: MatchCdOdConfig -> Int -> Int -> IO MatchCdOdInstance
matchCdOd config segment seed = do
  let g = mkStdGen $ (segment +) $ 4 * seed
  inst <- evalRandT (getMatchCdOdTask getRandomTask config) g
  shuffleEverything inst

getMatchCdOdTask
  :: (MonadIO m, MonadFail m)
  => (MatchCdOdConfig
    -> RandT g IO (Map Int Syntax, Map Char ([Int], AlloyInstance)))
  -> MatchCdOdConfig
  -> RandT g m MatchCdOdInstance
getMatchCdOdTask f config = do
  (cds, ods) <- mapRandT liftIO $ f config
  ods' <- runExceptT (mapM (mapM alloyInstanceToOd) ods)
    >>= either fail return
  return $ MatchCdOdInstance {
        diagrams       = cds,
        generatorValue = 0,
        instances      = ods',
        showSolution   = printSolution config
        }

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
    ],
  showSolution = False
  }

classAndAssocNames :: MatchCdOdInstance -> ([String], [String])
classAndAssocNames inst =
  let names = nubOrd $ concatMap classNames (diagrams inst)
      assocs = nubOrd $ concatMap associationNames (diagrams inst)
        ++ concatMap (linkNames . snd) (instances inst)
  in (names, assocs)

newMatchCdOdInstances
  :: (MonadFail m, MonadRandom m, MonadThrow m, RandomGen g)
  => MatchCdOdInstance
  -> RandT g m [MatchCdOdInstance]
newMatchCdOdInstances inst = do
  let (names, assocs) = classAndAssocNames inst
  names'  <- shuffleM $ tail $ permutations names
  assocs' <- shuffleM $ tail $ permutations assocs
  sequence
    [ lift $ renameInstance inst ns as >>= shuffleInstance
    | (ns, as) <- zip names' (concat $ replicate 3 assocs')
    ]

instance Randomise MatchCdOdInstance where
  randomise inst = do
    let (names, assocs) = classAndAssocNames inst
    names'  <- shuffleM names
    assocs' <- shuffleM assocs
    renameInstance inst names' assocs'
      >>= shuffleInstance
      >>= changeGeneratorValue

instance RandomiseLayout MatchCdOdInstance where
  randomiseLayout = shuffleNodesAndEdges

shuffleNodesAndEdges
  :: MonadRandom m
  => MatchCdOdInstance
  -> m MatchCdOdInstance
shuffleNodesAndEdges MatchCdOdInstance {..} = do
  cds <- mapM shuffleClassAndConnectionOrder diagrams
  ods <- mapM (mapM shuffleObjectAndLinkOrder) instances
  return MatchCdOdInstance {
    diagrams = cds,
    generatorValue = generatorValue,
    instances = ods,
    showSolution = showSolution
    }

changeGeneratorValue
  :: MonadRandom m
  => MatchCdOdInstance
  -> m MatchCdOdInstance
changeGeneratorValue inst = do
  r <- getRandom
  return inst { generatorValue = r }

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
    instances = M.fromAscList ods',
    showSolution = showSolution inst
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
      (names, assocs) = classAndAssocNames inst
      bmNames  = BM.fromList $ zip names names'
      bmAssocs = BM.fromList $ zip assocs assocs'
      renameCd = renameClassesInCd bmNames >=> renameAssocsInCd bmAssocs
      renameOd = renameClassesInOd bmNames >=> renameLinksInOd bmAssocs
  cds' <- renameCd `mapM` cds
  ods' <- mapM renameOd `mapM` ods
  return $ MatchCdOdInstance {
    diagrams = cds',
    generatorValue = generatorValue inst,
    instances = ods',
    showSolution = showSolution inst
    }

getRandomTask
  :: RandomGen g
  => MatchCdOdConfig
  -> RandT g IO (Map Int Syntax, Map Char ([Int], AlloyInstance))
getRandomTask config = do
  let alloyCode = Changes.transform (classConfig config) defaultProperties
  instas <- liftIO
    $ getInstances (maxInstances config) (timeout config) alloyCode
  when debug $ liftIO $ print $ length instas
  rinstas <- shuffleM instas
  ods <- getODsFor config { timeout = Nothing } rinstas
  maybe (error "could not find instance") return ods

getODsFor
  :: RandomGen g
  => MatchCdOdConfig
  -> [AlloyInstance]
  -> RandT g IO (Maybe (Map Int Syntax, Map Char ([Int], AlloyInstance)))
getODsFor _      []       = return Nothing
getODsFor config (cd:cds) = do
  (_, [(_, cd1), (_, cd2), (_, cd3)], numClasses) <- applyChanges cd
  instas <- liftIO $ getODInstances config cd1 cd2 cd3 numClasses
  mrinstas <- takeRandomInstances instas
  case mrinstas of
    Nothing      -> getODsFor config cds
    Just rinstas -> return $ Just (
      M.fromList [(1, cd1), (2, cd2)],
      M.fromList $ zip ['a' ..] rinstas
      )

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

getODInstances
  :: MatchCdOdConfig
  -> Syntax
  -> Syntax
  -> Syntax
  -> Int
  -> IO (Map [Int] [AlloyInstance])
getODInstances config cd1 cd2 cd3 numClasses = do
  -- TODO remove `toOldSyntax`
  let parts1 = alloyFor cd1 "1"
      parts2 = alloyFor cd2 "2"
      parts1and2 = mergeParts parts1 parts2
      combined1and2 = combineParts parts1and2
      parts3 = alloyFor cd3 "3"
      parts1to3 = mergeParts parts1and2 parts3
      cd1not2 = runCommand "cd1 and (not cd2)" parts1and2
      cd2not1 = runCommand "cd2 and (not cd1)" parts1and2
      cd1and2 = runCommand "cd1 and cd2" parts1and2
      cdNot1not2 = runCommand "(not cd1) and (not cd2) and cd3" parts1to3
  instances1not2 <- getInstances maxIs to (combined1and2 ++ cd1not2)
  instances2not1 <- getInstances maxIs to (combined1and2 ++ cd2not1)
  instances1and2 <- getInstances maxIs to (combined1and2 ++ cd1and2)
  instancesNot1not2 <-
    getInstances maxIs to (combineParts parts1to3 ++ cdNot1not2)
  when debug . print $ length instances1not2
  when debug . print $ length instances2not1
  when debug . print $ length instances1and2
  when debug . print $ length instancesNot1not2
  return $ M.fromList [([1]  , instances1not2),
                       ([2]  , instances2not1),
                       ([1,2], instances1and2),
                       ([]   , instancesNot1not2)]
  where
    alloyFor cd nr = transform
      (toOldSyntax cd)
      []
      (objectConfig config)
      (presenceOfLinkSelfLoops config)
      False
      nr
      ""
    to = timeout config
    maxIs = maxInstances config
    runCommand x = createRunCommand
      x
      numClasses
      (objectConfig config)

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
