{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Modelling.CdOd.MatchCdOd (
  MatchCdOdConfig (..),
  MatchCdOdInstance (..),
  checkMatchCdOdConfig,
  checkMatchCdOdInstance,
  defaultMatchCdOdConfig,
  defaultMatchCdOdInstance,
  getChangesAndCds,
  getMatchCdOdTask,
  getODInstances,
  matchCdOd,
  matchCdOdEvaluation,
  matchCdOdSolution,
  matchCdOdSyntax,
  matchCdOdTask,
  matchingShow,
  takeRandomInstances,
  ) where

import qualified Modelling.CdOd.CdAndChanges.Transform as Changes (transform)

import qualified Data.Bimap                       as BM (fromList)
import qualified Data.Map                         as M (
  adjust,
  elems,
  foldrWithKey,
  fromAscList,
  fromList,
  keys,
  lookup,
  toList,
  traverseWithKey,
  )

import Capabilities.Alloy               (MonadAlloy, getInstances)
import Capabilities.Cache               (MonadCache)
import Capabilities.Diagrams            (MonadDiagrams)
import Capabilities.Graphviz            (MonadGraphviz)
import Modelling.Auxiliary.Common (
  Randomise (isRandomisable, randomise),
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
import Modelling.CdOd.CdAndChanges.Instance (
  ChangeAndCd (..),
  ClassDiagramInstance,
  GenericClassDiagramInstance (..),
  fromInstance,
  renameClassesAndRelationshipsInCdInstance,
  )
import Modelling.CdOd.Auxiliary.Util (
  alloyInstanceToOd,
  )
import Modelling.CdOd.Output            (cacheCd, cacheOd)
import Modelling.CdOd.Types (
  Cd,
  ClassConfig (..),
  ClassDiagram (..),
  LimitedLinking (..),
  Link (..),
  Object (..),
  ObjectConfig (..),
  ObjectDiagram (..),
  ObjectProperties (..),
  Od,
  Relationship (..),
  associationNames,
  checkClassConfigWithProperties,
  checkObjectDiagram,
  classNames,
  defaultProperties,
  isObjectDiagramRandomisable,
  linkNames,
  relationshipName,
  renameClassesAndRelationships,
  renameObjectsWithClassesAndLinksInOd,
  reverseAssociation,
  shuffleClassAndConnectionOrder,
  shuffleObjectAndLinkOrder,
  )
import Modelling.Types (
  Change (..),
  Letters (Letters, lettersList),
  showLetters,
  )

import Control.Exception                (Exception)
import Control.Monad.Catch              (MonadThrow, throwM)
#if __GLASGOW_HASKELL__ < 808
import Control.Monad.Fail               (MonadFail)
#endif
import Control.Monad.Output (
  GenericOutputMonad (..),
  LangM,
  OutputMonad,
  Rated,
  ($=<<),
  english,
  german,
  multipleChoice,
  translate,
  translations,
  )
import Control.Monad.Random (
  MonadRandom (getRandom),
  evalRandT,
  mkStdGen,
  )
import Data.Bifunctor                   (Bifunctor (second))
import Data.Bitraversable               (bimapM)
import Data.Containers.ListUtils        (nubOrd)
import Data.GraphViz                    (DirType (Back))
import Data.Map                         (Map)
import Data.Maybe                       (fromJust, listToMaybe, mapMaybe)
import Data.String.Interpolate          (iii)
import GHC.Generics                     (Generic)
import Language.Alloy.Call              (AlloyInstance)
import System.Random.Shuffle            (shuffleM)

instance Exception MatchCdOdException

data MatchCdOdException = InvalidMatchCdOdInstance
  deriving Show

data MatchCdOdInstance = MatchCdOdInstance {
    diagrams       :: Map Int Cd,
    generatorValue :: Int,
    instances      :: Map Char ([Int], Od),
    showSolution   :: Bool
  } deriving (Generic, Read, Show)

data MatchCdOdConfig = MatchCdOdConfig {
    classConfig      :: ClassConfig,
    maxInstances     :: Maybe Integer,
    objectConfig     :: ObjectConfig,
    objectProperties :: ObjectProperties,
    printSolution    :: Bool,
    timeout          :: Maybe Int,
    withNonTrivialInheritance :: Maybe Bool
  } deriving (Generic, Read, Show)

defaultMatchCdOdConfig :: MatchCdOdConfig
defaultMatchCdOdConfig = MatchCdOdConfig {
    classConfig  = ClassConfig {
        classLimits        = (4, 4),
        aggregationLimits  = (0, Just 2),
        associationLimits  = (0, Just 2),
        compositionLimits  = (0, Just 1),
        inheritanceLimits  = (1, Just 2),
        relationshipLimits = (4, Just 6)
      },
    maxInstances     = Nothing,
    objectConfig = ObjectConfig {
      linkLimits           = (4, Just 10),
      linksPerObjectLimits = (0, Just 4),
      objectLimits         = (2, 4)
      },
    objectProperties = ObjectProperties {
      completelyInhabited = Nothing,
      hasLimitedIsolatedObjects = True,
      hasSelfLoops = Nothing,
      usesEveryRelationshipName = Nothing
      },
    printSolution    = False,
    timeout          = Nothing,
    withNonTrivialInheritance = Just True
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

checkMatchCdOdInstance :: MatchCdOdInstance -> Maybe String
checkMatchCdOdInstance MatchCdOdInstance {..} =
  foldr ((<>) . checkObjectDiagram . snd) Nothing $ M.elems instances

matchCdOdTask
  :: (MonadCache m, MonadDiagrams m, MonadGraphviz m, MonadThrow m, OutputMonad m)
  => FilePath
  -> MatchCdOdInstance
  -> LangM m
matchCdOdTask path task = do
  let anonymous o = length (objects o) `div` 3
  paragraph $ translate $ do
    english "Consider the following two class diagrams:"
    german "Betrachten Sie die folgenden zwei Klassendiagramme:"
  images show id
    $=<< (\_ c -> cacheCd True True mempty c path)
    `M.traverseWithKey` diagrams task
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
  images (:[]) snd
    $=<< flip evalRandT (mkStdGen $ generatorValue task)
    $ (\_ (is,o) -> (is,) <$> cacheOd o (anonymous o) Back True path)
    `M.traverseWithKey` instances task
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
    pure ()
  paragraph simplifiedInformation
  paragraph directionsAdvice
  paragraph hoveringInformation
  pure ()

newtype ShowLetters = ShowLetters { showLetters' :: Letters }

instance Show ShowLetters where
  show = showLetters . showLetters'

matchingShow :: [(Int, Letters)] -> [(Int, ShowLetters)]
matchingShow = fmap (second ShowLetters)

matchCdOdInitial :: [(Int, Letters)]
matchCdOdInitial = [(1, Letters "ab"), (2, Letters "")]

matchCdOdSyntax
  :: (OutputMonad m, Foldable t)
  => MatchCdOdInstance
  -> t (Int, Letters)
  -> LangM m
matchCdOdSyntax task sub = addPretext $ do
  assertion (all (availableCd . fst) sub) $ translate $ do
    english "Referenced class diagrams were provided within task?"
    german [iii|
      Referenzierte Klassendiagramme sind Bestandteil der Aufgabenstellung?
      |]
  assertion (all (all availableOd . lettersList . snd) sub) $ translate $ do
    english "Referenced object diagrams were provided within task?"
    german "Referenzierte Objektdiagramme sind Bestandteil der Aufgabenstellung?"
  pure ()
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

matchCdOd
  :: (MonadAlloy m, MonadFail m, MonadRandom m, MonadThrow m)
  => MatchCdOdConfig
  -> Int
  -> Int
  -> m MatchCdOdInstance
matchCdOd config segment seed = do
  let g = mkStdGen $ (segment +) $ 4 * seed
  inst <- evalRandT (getMatchCdOdTask getRandomTask config) g
  shuffleEverything inst

getMatchCdOdTask
  :: MonadThrow m
  => (MatchCdOdConfig
    -> m (Map Int Cd, Map Char ([Int], AlloyInstance)))
  -> MatchCdOdConfig
  -> m MatchCdOdInstance
getMatchCdOdTask f config = do
  (cds, ods) <- f config
  ods' <- mapM (mapM alloyInstanceToOd) ods
  return $ MatchCdOdInstance {
        diagrams       = cds,
        generatorValue = 0,
        instances      = ods',
        showSolution   = printSolution config
        }

defaultMatchCdOdInstance :: MatchCdOdInstance
defaultMatchCdOdInstance = MatchCdOdInstance {
  diagrams = M.fromList [
    (1, ClassDiagram {
      classNames = ["C","A","D","B"],
      relationships = [
        Inheritance {subClass = "A", superClass = "C"},
        Association {
          associationName = "z",
          associationFrom = LimitedLinking {
            linking = "B",
            limits = (2,Just 2)
            },
          associationTo = LimitedLinking {
            linking = "C",
            limits = (1,Just 2)
            }
           },
        Association {
          associationName = "y",
          associationFrom = LimitedLinking {
            linking = "B",
            limits = (0,Just 2)
            },
          associationTo = LimitedLinking {
            linking = "D",
            limits = (0,Just 2)
            }
           },
        Composition {
          compositionName = "x",
          compositionPart = LimitedLinking {
            linking = "D",
            limits = (0,Just 2)
            },
          compositionWhole = LimitedLinking {
            linking = "C",
            limits = (1,Just 1)
            }
           }
        ]
      }
    ),
    (2, ClassDiagram {
      classNames = ["C","A","D","B"],
      relationships = [
        Inheritance {subClass = "A", superClass = "C"},
        Composition {
          compositionName = "x",
          compositionPart = LimitedLinking {
            linking = "D",
            limits = (1,Just 2)
            },
          compositionWhole = LimitedLinking {
            linking = "C",
            limits = (1,Just 1)
            }
           },
        Association {
          associationName = "y",
          associationFrom = LimitedLinking {
            linking = "B",
            limits = (0,Just 2)
            },
          associationTo = LimitedLinking {
            linking = "D",
            limits = (0,Just 2)
            }
          }
        ]
      }
    )
    ],
  generatorValue = 7777369639206507645,
  instances = M.fromList [
    ('a',([1],ObjectDiagram {
      objects = [
        Object {objectName = "b", objectClass = "B"},
        Object {objectName = "b1", objectClass = "B"},
        Object {objectName = "c", objectClass = "C"},
        Object {objectName = "d", objectClass = "D"}
        ],
      links = [
        Link {linkName = "z", linkFrom = "c", linkTo = "b"},
        Link {linkName = "z", linkFrom = "c", linkTo = "b1"},
        Link {linkName = "y", linkFrom = "d", linkTo = "b1"},
        Link {linkName = "x", linkFrom = "c", linkTo = "d"}
        ]
      })),
    ('b',([1],ObjectDiagram {
      objects = [
        Object {objectName = "b", objectClass = "B"},
        Object {objectName = "b1", objectClass = "B"},
        Object {objectName = "c", objectClass = "C"},
        Object {objectName = "d", objectClass = "D"}
        ],
      links = [
        Link {linkName = "z", linkFrom = "c", linkTo = "b"},
        Link {linkName = "z", linkFrom = "c", linkTo = "b1"},
        Link {linkName = "x", linkFrom = "c", linkTo = "d"}
        ]
      })),
    ('c',([2],ObjectDiagram {
      objects = [
        Object {objectName = "a", objectClass = "A"},
        Object {objectName = "c", objectClass = "C"},
        Object {objectName = "d", objectClass = "D"},
        Object {objectName = "d1", objectClass = "D"}],
      links = [
        Link {linkName = "x", linkFrom = "a", linkTo = "d"},
        Link {linkName = "x", linkFrom = "c", linkTo = "d1"}
        ]
      })),
    ('d',([2],ObjectDiagram {
      objects = [
        Object {objectName = "a", objectClass = "A"},
        Object {objectName = "b", objectClass = "B"},
        Object {objectName = "d", objectClass = "D"}
        ],
      links = [
        Link {linkName = "x", linkFrom = "a", linkTo = "d"}
        ]
      })),
    ('e',([],ObjectDiagram {
      objects = [
        Object {objectName = "a", objectClass = "A"},
        Object {objectName = "b", objectClass = "B"},
        Object {objectName = "d", objectClass = "D"}
        ],
      links = [
        Link {linkName = "x", linkFrom = "a", linkTo = "d"},
        Link {linkName = "z", linkFrom = "a", linkTo = "b"},
        Link {linkName = "y", linkFrom = "d", linkTo = "b"}
        ]
      }))
    ],
  showSolution = False
  }

classAndNonInheritanceNames :: MatchCdOdInstance -> ([String], [String])
classAndNonInheritanceNames inst =
  let names = nubOrd $ concatMap classNames (diagrams inst)
      nonInheritances = nubOrd $ concatMap associationNames (diagrams inst)
        ++ concatMap (linkNames . snd) (instances inst)
  in (names, nonInheritances)

instance Randomise MatchCdOdInstance where
  randomise inst = do
    let (names, nonInheritances) = classAndNonInheritanceNames inst
    names'  <- shuffleM names
    nonInheritances' <- shuffleM nonInheritances
    renameInstance inst names' nonInheritances'
      >>= shuffleInstance
      >>= changeGeneratorValue
  isRandomisable MatchCdOdInstance {..} = listToMaybe
    $ mapMaybe (isObjectDiagramRandomisable . snd) $ M.elems instances

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
  :: (MonadThrow m, MonadRandom m)
  => MatchCdOdInstance
  -> m MatchCdOdInstance
shuffleInstance inst = do
  cds <- shuffleM $ M.toList $ diagrams inst
  ods <- shuffleM $ M.toList $ instances inst
  let changeId x (y, cd) = ((y, x), (x, cd))
      (idMap, cds') = unzip $ zipWith changeId [1..] cds
      replaceId x (_, od) = (x, od)
      rename = maybe (throwM InvalidMatchCdOdInstance) return
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
renameInstance inst names' nonInheritances' = do
  let cds = diagrams inst
      ods = instances inst
      (names, nonInheritances) = classAndNonInheritanceNames inst
      bmNames  = BM.fromList $ zip names names'
      bmNonInheritances = BM.fromList $ zip nonInheritances nonInheritances'
      renameCd = renameClassesAndRelationships bmNames bmNonInheritances
      renameOd = renameObjectsWithClassesAndLinksInOd bmNames bmNonInheritances
  cds' <- renameCd `mapM` cds
  ods' <- mapM renameOd `mapM` ods
  return $ MatchCdOdInstance {
    diagrams = cds',
    generatorValue = generatorValue inst,
    instances = ods',
    showSolution = showSolution inst
    }

getRandomTask
  :: (MonadAlloy m, MonadFail m, MonadRandom m, MonadThrow m)
  => MatchCdOdConfig
  -> m (Map Int Cd, Map Char ([Int], AlloyInstance))
getRandomTask config = do
  let alloyCode = Changes.transform
        (classConfig config)
        defaultProperties
        (withNonTrivialInheritance config)
  instas <- getInstances (maxInstances config) (timeout config) alloyCode
  randomInstances <- shuffleM instas
  ods <- getODsFor config { timeout = Nothing } randomInstances
  maybe (error "could not find instance") return ods

getODsFor
  :: (MonadAlloy m, MonadFail m, MonadRandom m, MonadThrow m)
  => MatchCdOdConfig
  -> [AlloyInstance]
  -> m (Maybe (Map Int Cd, Map Char ([Int], AlloyInstance)))
getODsFor _      []       = return Nothing
getODsFor config (cd:cds) = do
  [cd1, cd2, cd3] <- map changeClassDiagram . instanceChangesAndCds
    <$> getChangesAndCds cd
  instas <- getODInstances config cd1 cd2 cd3 $ length $ classNames cd1
  maybeRandomInstances <- takeRandomInstances instas
  case maybeRandomInstances of
    Nothing      -> getODsFor config cds
    Just randomInstances -> return $ Just (
      M.fromList [(1, cd1), (2, cd2)],
      M.fromList $ zip ['a' ..] randomInstances
      )

getChangesAndCds
  :: (MonadAlloy m, MonadThrow m)
  => AlloyInstance
  -> m ClassDiagramInstance
getChangesAndCds insta = do
  cdInstance <- fromInstance insta
  let cd  = instanceClassDiagram cdInstance
      cs  = classNames cd
      es  = instanceRelationshipNames cdInstance
      bimapEdges = BM.fromList $ zip es $ map (:[]) ['z', 'y' ..]
      bimapClasses = BM.fromList $ zip cs $ map (:[]) ['A' ..]
  cdInstance' <- renameClassesAndRelationshipsInCdInstance
    bimapClasses
    bimapEdges
    cdInstance
  return $ cdInstance' {
    instanceChangesAndCds = map deliberatelyNameReplacedEdgesSameInCdOnly
      $ instanceChangesAndCds cdInstance'
    }
  where
    deliberatelyNameReplacedEdgesSameInCdOnly change =
      case relationshipChange change of
        Change {add = Just rx, remove = Just ry}
          | Just x <- relationshipName rx
          , Just y <- relationshipName ry -> change {
            changeClassDiagram = second (\x' -> if x' == x then y else x')
              $ changeClassDiagram change
            }
        _ -> change

getODInstances
  :: MonadAlloy m
  => MatchCdOdConfig
  -> Cd
  -> Cd
  -> Cd
  -> Int
  -> m (Map [Int] [AlloyInstance])
getODInstances config cd1 cd2 cd3 numClasses = do
  let parts1 = alloyFor cd1 "1"
      parts2 = alloyFor cd2 "2"
      parts1and2 = mergeParts parts1 parts2
      combined1and2 = combineParts parts1and2
      parts3 = alloyFor cd3 "3"
      parts1to3 = mergeParts parts1and2 parts3
      relationships1and2 = relationships cd1 ++ relationships cd2
      relationships1to3 = relationships1and2 ++ relationships cd3
      cd1not2 = runCommand "cd1 and (not cd2)" relationships1and2 parts1and2
      cd2not1 = runCommand "cd2 and (not cd1)" relationships1and2 parts1and2
      cd1and2 = runCommand "cd1 and cd2" relationships1and2 parts1and2
      cdNot1not2 = runCommand
        "(not cd1) and (not cd2) and cd3"
        relationships1to3
        parts1to3
  instances1not2 <- getInstances maxIs to (combined1and2 ++ cd1not2)
  instances2not1 <- getInstances maxIs to (combined1and2 ++ cd2not1)
  instances1and2 <- getInstances maxIs to (combined1and2 ++ cd1and2)
  instancesNot1not2 <-
    getInstances maxIs to (combineParts parts1to3 ++ cdNot1not2)
  return $ M.fromList [([1]  , instances1not2),
                       ([2]  , instances2not1),
                       ([1,2], instances1and2),
                       ([]   , instancesNot1not2)]
  where
    alloyFor cd nr = transform
      (cd {relationships = map reverseAssociation $ relationships cd})
      []
      (objectConfig config)
      (objectProperties config)
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
      randomInstances <- mapM shuffleM instas
      ts:_    <- shuffleM takes
      shuffleM $ concatMap ($ randomInstances) ts
