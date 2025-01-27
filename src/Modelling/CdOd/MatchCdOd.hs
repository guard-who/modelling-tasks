{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Modelling.CdOd.MatchCdOd (
  MatchCdOdConfig (..),
  MatchCdOdInstance (..),
  MatchCdOdTaskTextElement (..),
  checkMatchCdOdConfig,
  checkMatchCdOdInstance,
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
  uniform,
  )
import Modelling.CdOd.CD2Alloy.Transform (
  LinguisticReuse (None),
  combineParts,
  createRunCommand,
  mergeParts,
  transform,
  )
import Modelling.CdOd.CdAndChanges.Instance (
  GenericClassDiagramInstance (..),
  fromInstanceWithNameOverlap,
  nameClassDiagramInstance,
  validChangeClassDiagram,
  )
import Modelling.CdOd.Auxiliary.Util (
  alloyInstanceToOd,
  )
import Modelling.CdOd.Output            (cacheCd, cacheOd)
import Modelling.CdOd.Types (
  Cd,
  CdDrawSettings (..),
  CdMutation,
  ClassConfig (..),
  ClassDiagram (..),
  LimitedLinking (..),
  Link (..),
  Object (..),
  ObjectConfig (..),
  ObjectDiagram (..),
  ObjectProperties (..),
  Od,
  OmittedDefaultMultiplicities (..),
  Relationship (..),
  allCdMutations,
  anonymiseObjects,
  associationNames,
  checkCdDrawSettings,
  checkCdMutations,
  checkClassConfigWithProperties,
  checkObjectDiagram,
  checkObjectProperties,
  checkOmittedDefaultMultiplicities,
  classNames,
  defaultOmittedDefaultMultiplicities,
  defaultProperties,
  fromClassDiagram,
  isObjectDiagramRandomisable,
  linkNames,
  relationshipName,
  renameClassesAndRelationships,
  renameObjectsWithClassesAndLinksInOd,
  shuffleCdNames,
  shuffleClassAndConnectionOrder,
  shuffleObjectAndLinkOrder,
  )
import Modelling.Types (
  Letters (Letters, lettersList),
  showLetters,
  )

import Control.Applicative              (Alternative ((<|>)))
import Control.Exception                (Exception)
import Control.Monad                    ((<=<))
import Control.Monad.Catch              (MonadThrow, throwM)
#if __GLASGOW_HASKELL__ < 808
import Control.Monad.Fail               (MonadFail)
#endif
import Control.OutputCapable.Blocks (
  ArticleToUse (DefiniteArticle),
  GenericOutputCapable (..),
  LangM,
  OutputCapable,
  Rated,
  ($=<<),
  english,
  german,
  multipleChoice,
  translate,
  translations,
  )
import Control.OutputCapable.Blocks.Generic.Type (
  GenericOutput (Code, Paragraph, Special, Translated),
  )
import Control.OutputCapable.Blocks.Type (
  SpecialOutput,
  specialToOutputCapable,
  )
import Control.Monad.Random (
  MonadRandom,
  evalRandT,
  mkStdGen,
  )
import Data.Bifunctor                   (Bifunctor (second))
import Data.Bitraversable               (bimapM)
import Data.Containers.ListUtils        (nubOrd)
import Data.GraphViz                    (DirType (Forward))
import Data.List                        (singleton)
import Data.Map                         (Map)
import Data.Maybe                       (fromJust, listToMaybe, mapMaybe)
import Data.Ratio                       ((%))
import Data.String.Interpolate          (iii)
import GHC.Generics                     (Generic)
import Language.Alloy.Call              (AlloyInstance)
import System.Random.Shuffle            (shuffleM)

instance Exception MatchCdOdException

data MatchCdOdException = InvalidMatchCdOdInstance
  deriving Show

data MatchCdOdInstance
  = MatchCdOdInstance {
    cdDrawSettings :: !CdDrawSettings,
    diagrams       :: Map Int Cd,
    instances      :: Map Char ([Int], Od),
    showSolution   :: !Bool,
    taskText       :: !MatchCdOdTaskText
  } deriving (Eq, Generic, Read, Show)

data MatchCdOdConfig
  = MatchCdOdConfig {
    allowedCdMutations :: ![CdMutation],
    classConfig      :: ClassConfig,
    maxInstances     :: Maybe Integer,
    objectConfig     :: ObjectConfig,
    objectProperties :: ObjectProperties,
    omittedDefaultMultiplicities :: OmittedDefaultMultiplicities,
    printSolution    :: Bool,
    timeout          :: Maybe Int,
    withNonTrivialInheritance :: Maybe Bool
  } deriving (Generic, Read, Show)

defaultMatchCdOdConfig :: MatchCdOdConfig
defaultMatchCdOdConfig
  = MatchCdOdConfig {
    allowedCdMutations = allCdMutations,
    classConfig  = ClassConfig {
        classLimits        = (4, 4),
        aggregationLimits  = (1, Just 2),
        associationLimits  = (0, Just 1),
        compositionLimits  = (1, Just 1),
        inheritanceLimits  = (1, Just 2),
        relationshipLimits = (4, Just 6)
      },
    maxInstances     = Just 200,
    objectConfig = ObjectConfig {
      linkLimits           = (4, Just 10),
      linksPerObjectLimits = (0, Just 4),
      objectLimits         = (2, 4)
      },
    objectProperties = ObjectProperties {
      anonymousObjectProportion = 1 % 3,
      completelyInhabited = Nothing,
      hasLimitedIsolatedObjects = True,
      hasSelfLoops = Nothing,
      usesEveryRelationshipName = Nothing
      },
    omittedDefaultMultiplicities = defaultOmittedDefaultMultiplicities,
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
checkMatchCdOdConfig MatchCdOdConfig {..}
  | Just True <- hasSelfLoops objectProperties
  = Just [iii|
    Enforcing self-loops in all object diagrams is not supported.
    You might want to change 'hasSelfLoops' to 'Nothing' in order
    to have self-loops (by chance) in some (or even all) of the object diagrams.
    |]
  | otherwise
  = checkClassConfigWithProperties classConfig defaultProperties
  <|> checkCdMutations allowedCdMutations
  <|> checkObjectProperties objectProperties
  <|> checkOmittedDefaultMultiplicities omittedDefaultMultiplicities

checkMatchCdOdInstance :: MatchCdOdInstance -> Maybe String
checkMatchCdOdInstance MatchCdOdInstance {..}
  | not $ printNames cdDrawSettings
  = Just [iii|printNames has to be set to True for this task type.|]
  | not $ printNavigations cdDrawSettings
  = Just [iii|printNavigations has to be set to True for this task type.|]
  | otherwise
  = foldr ((<>) . checkObjectDiagram . snd) Nothing (M.elems instances)
  <|> checkCdDrawSettings cdDrawSettings

type MatchCdOdTaskText = [SpecialOutput MatchCdOdTaskTextElement]

data MatchCdOdTaskTextElement
  = GivenCds
  | GivenOds
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

matchCdOdTask
  :: (
    MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    MonadThrow m,
    OutputCapable m
    )
  => FilePath
  -> MatchCdOdInstance
  -> LangM m
matchCdOdTask path task = do
  toTaskText path task
  paragraph simplifiedInformation
  paragraph directionsAdvice
  paragraph hoveringInformation
  pure ()

toTaskText
  :: (
    MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    MonadThrow m,
    OutputCapable m
    )
  => FilePath
  -> MatchCdOdInstance
  -> LangM m
toTaskText path task =
  specialToOutputCapable (toTaskSpecificText path task) (taskText task)

toTaskSpecificText
  :: (
    MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    MonadThrow m,
    OutputCapable m
    )
  => FilePath
  -> MatchCdOdInstance
  -> MatchCdOdTaskTextElement
  -> LangM m
toTaskSpecificText path MatchCdOdInstance {..} = \case
  GivenCds -> images show id
    $=<< (\_ cd -> cacheCd cdDrawSettings mempty (fromClassDiagram cd) path)
    `M.traverseWithKey` diagrams
  GivenOds -> images (:[]) snd
    $=<< (\_ (is,o) -> (is,) <$> cacheOd o Forward True path)
    `M.traverseWithKey` instances

defaultMatchCdOdTaskText :: MatchCdOdTaskText
defaultMatchCdOdTaskText = [
  Paragraph $ singleton $ Translated $ translations $ do
    english "Consider the following two (valid) class diagrams:"
    german "Betrachten Sie die folgenden zwei (validen) Klassendiagramme:",
  Special GivenCds,
  Paragraph $ singleton $ Translated $ translations $ do
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
      |],
  Special GivenOds,
  Paragraph [
    Translated $ translations $ do
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
        Zum Beispiel drückt#{" "}|],
    Code . uniform . show $ matchingShow matchCdOdInitial,
    Translated $ translations $ do
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
    ]
  ]

newtype ShowLetters = ShowLetters { showLetters' :: Letters }

instance Show ShowLetters where
  show = showLetters . showLetters'

matchingShow :: [(Int, Letters)] -> [(Int, ShowLetters)]
matchingShow = fmap (second ShowLetters)

matchCdOdInitial :: [(Int, Letters)]
matchCdOdInitial = [(1, Letters "ab"), (2, Letters "")]

matchCdOdSyntax
  :: (Foldable t, OutputCapable m)
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
  :: (Foldable t, OutputCapable m)
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
  multipleChoice DefiniteArticle what solution matching sub
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
  :: (MonadAlloy m, MonadFail m, MonadThrow m)
  => MatchCdOdConfig
  -> Int
  -> Int
  -> m MatchCdOdInstance
matchCdOd config segment seed = flip evalRandT g $ do
  inst <- getMatchCdOdTask getRandomTask config
  shuffleEverything inst
  where
    g = mkStdGen $ (segment +) $ 4 * seed

getMatchCdOdTask
  :: (MonadRandom m, MonadThrow m)
  => (MatchCdOdConfig
    -> m (Map Int Cd, Map Char ([Int], AlloyInstance)))
  -> MatchCdOdConfig
  -> m MatchCdOdInstance
getMatchCdOdTask f config@MatchCdOdConfig {..} = do
  (cds, ods) <- f config
  let possibleLinkNames = concatMap
        (mapMaybe relationshipName . relationships)
        cds
  ods' <- mapM (mapM $ toOd possibleLinkNames) ods
  return $ MatchCdOdInstance {
        cdDrawSettings = CdDrawSettings {
          omittedDefaults = omittedDefaultMultiplicities,
          printNames = True,
          printNavigations = True
          },
        diagrams       = cds,
        instances      = ods',
        showSolution = printSolution,
        taskText = defaultMatchCdOdTaskText
        }
  where
    toOd possibleLinkNames =
      anonymiseObjects (anonymousObjectProportion objectProperties)
      <=< alloyInstanceToOd possibleLinkNames

{-|
A 'defaultMatchCdOdInstance' as generated using 'defaultMatchCdOdConfig'.
-}
defaultMatchCdOdInstance :: MatchCdOdInstance
defaultMatchCdOdInstance = MatchCdOdInstance {
  cdDrawSettings = CdDrawSettings {
    omittedDefaults = OmittedDefaultMultiplicities {
      aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
      associationOmittedDefaultMultiplicity = Just (0, Nothing),
      compositionWholeOmittedDefaultMultiplicity = Just (1, Just 1)
      },
    printNames = True,
    printNavigations = True
    },
  diagrams = M.fromList [
    (1, ClassDiagram {
      classNames = ["A", "C", "B", "D"],
      relationships = [
        Composition {
          compositionName = "y",
          compositionPart = LimitedLinking {
            linking = "D",
            limits = (0, Nothing)
            },
          compositionWhole = LimitedLinking {
            linking = "B",
            limits = (0, Just 1)
            }
          },
        Aggregation {
          aggregationName = "w",
          aggregationPart = LimitedLinking {
            linking = "B",
            limits = (0, Nothing)
            },
          aggregationWhole = LimitedLinking {
            linking = "A",
            limits = (2, Just 2)
            }
          },
        Inheritance {subClass = "C", superClass = "B"}
        ]
      }),
    (2, ClassDiagram {
      classNames = ["A", "B", "D", "C"],
      relationships = [
        Inheritance {subClass = "C", superClass = "B"},
        Aggregation {
          aggregationName = "z",
          aggregationPart = LimitedLinking {
            linking = "A",
            limits = (1, Nothing)
            },
          aggregationWhole = LimitedLinking {
            linking = "D",
            limits = (2, Just 2)
            }
          },
        Composition {
          compositionName = "y",
          compositionPart = LimitedLinking {
            linking = "D",
            limits = (0, Nothing)
            },
          compositionWhole = LimitedLinking {
            linking = "B",
            limits = (0, Just 1)
            }
          }
        ]
      })
    ],
  instances = M.fromList [
    ('a', ([], ObjectDiagram {
      objects = [
        Object {isAnonymous = True, objectName = "d", objectClass = "D"},
        Object {isAnonymous = False, objectName = "a", objectClass = "A"},
        Object {isAnonymous = False, objectName = "c", objectClass = "C"},
        Object {isAnonymous = False, objectName = "a1", objectClass = "A"}
        ],
      links = [
        Link {linkName = "w", linkFrom = "c", linkTo = "a"},
        Link {linkName = "w", linkFrom = "c", linkTo = "a1"},
        Link {linkName = "z", linkFrom = "a", linkTo = "d"},
        Link {linkName = "z", linkFrom = "a1", linkTo = "d"}
        ]
      })),
    ('b', ([1], ObjectDiagram {
      objects = [
        Object {isAnonymous = False, objectName = "c", objectClass = "C"},
        Object {isAnonymous = False, objectName = "c1", objectClass = "C"},
        Object {isAnonymous = False, objectName = "a1", objectClass = "A"},
        Object {isAnonymous = True, objectName = "a", objectClass = "A"}
        ],
      links = [
        Link {linkName = "w", linkFrom = "c", linkTo = "a"},
        Link {linkName = "w", linkFrom = "c", linkTo = "a1"},
        Link {linkName = "w", linkFrom = "c1", linkTo = "a"},
        Link {linkName = "w", linkFrom = "c1", linkTo = "a1"}
        ]
      })),
    ('c', ([2], ObjectDiagram {
      objects = [
        Object {isAnonymous = False, objectName = "d1", objectClass = "D"},
        Object {isAnonymous = True, objectName = "a", objectClass = "A"},
        Object {isAnonymous = False, objectName = "b", objectClass = "B"},
        Object {isAnonymous = False, objectName = "d", objectClass = "D"}
        ],
      links = [
        Link {linkName = "z", linkFrom = "a", linkTo = "d"},
        Link {linkName = "y", linkFrom = "d", linkTo = "b"},
        Link {linkName = "z", linkFrom = "a", linkTo = "d1"},
        Link {linkName = "y", linkFrom = "d1", linkTo = "b"}
        ]
      })),
    ('d', ([1], ObjectDiagram {
      objects = [
        Object {isAnonymous = True, objectName = "a1", objectClass = "A"},
        Object {isAnonymous = False, objectName = "b1", objectClass = "B"},
        Object {isAnonymous = False, objectName = "b", objectClass = "B"},
        Object {isAnonymous = False, objectName = "a", objectClass = "A"}
        ],
      links = [
        Link {linkName = "w", linkFrom = "b", linkTo = "a"},
        Link {linkName = "w", linkFrom = "b1", linkTo = "a1"},
        Link {linkName = "w", linkFrom = "b", linkTo = "a1"},
        Link {linkName = "w", linkFrom = "b1", linkTo = "a"}
        ]
      })),
    ('e', ([], ObjectDiagram {
      objects = [
        Object {isAnonymous = False, objectName = "b", objectClass = "B"},
        Object {isAnonymous = False, objectName = "d", objectClass = "D"},
        Object {isAnonymous = True, objectName = "a", objectClass = "A"},
        Object {isAnonymous = False, objectName = "a1", objectClass = "A"}
        ],
      links = [
        Link {linkName = "w", linkFrom = "b", linkTo = "a"},
        Link {linkName = "z", linkFrom = "a1", linkTo = "d"},
        Link {linkName = "z", linkFrom = "a", linkTo = "d"},
        Link {linkName = "w", linkFrom = "b", linkTo = "a1"}
        ]
      }))
    ],
  showSolution = False,
  taskText = defaultMatchCdOdTaskText
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
    cdDrawSettings = cdDrawSettings,
    diagrams = cds,
    instances = ods,
    showSolution = showSolution,
    taskText = taskText
    }

shuffleInstance
  :: (MonadThrow m, MonadRandom m)
  => MatchCdOdInstance
  -> m MatchCdOdInstance
shuffleInstance MatchCdOdInstance {..} = do
  cds <- shuffleM $ M.toList diagrams
  ods <- shuffleM $ M.toList instances
  let changeId x (y, cd) = ((y, x), (x, cd))
      (idMap, cds') = unzip $ zipWith changeId [1..] cds
      replaceId x (_, od) = (x, od)
      rename = maybe (throwM InvalidMatchCdOdInstance) return
        . (`lookup` idMap)
  ods' <- mapM (mapM $ bimapM (mapM rename) return)
    $ zipWith replaceId ['a'..] ods
  return $ MatchCdOdInstance {
    cdDrawSettings = cdDrawSettings,
    diagrams = M.fromAscList cds',
    instances = M.fromAscList ods',
    showSolution = showSolution,
    taskText = taskText
    }

renameInstance
  :: MonadThrow m
  => MatchCdOdInstance
  -> [String]
  -> [String]
  -> m MatchCdOdInstance
renameInstance inst@MatchCdOdInstance {..} names' nonInheritances' = do
  let (names, nonInheritances) = classAndNonInheritanceNames inst
      bmNames  = BM.fromList $ zip names names'
      bmNonInheritances = BM.fromList $ zip nonInheritances nonInheritances'
      renameCd = renameClassesAndRelationships bmNames bmNonInheritances
      renameOd = renameObjectsWithClassesAndLinksInOd bmNames bmNonInheritances
  cds <- renameCd `mapM` diagrams
  ods <- mapM renameOd `mapM` instances
  return $ MatchCdOdInstance {
    cdDrawSettings = cdDrawSettings,
    diagrams = cds,
    instances = ods,
    showSolution = showSolution,
    taskText = taskText
    }

getRandomTask
  :: (MonadAlloy m, MonadFail m, MonadRandom m, MonadThrow m)
  => MatchCdOdConfig
  -> m (Map Int Cd, Map Char ([Int], AlloyInstance))
getRandomTask config = do
  let alloyCode = Changes.transform
        (classConfig config)
        (allowedCdMutations config)
        defaultProperties
        (withNonTrivialInheritance config)
  alloyInstances <- getInstances (maxInstances config) (timeout config) alloyCode
  randomInstances <- shuffleM alloyInstances
  ods <- getODsFor config { timeout = Nothing } randomInstances
  maybe (error "could not find instance") return ods

getODsFor
  :: (MonadAlloy m, MonadFail m, MonadRandom m, MonadThrow m)
  => MatchCdOdConfig
  -> [AlloyInstance]
  -> m (Maybe (Map Int Cd, Map Char ([Int], AlloyInstance)))
getODsFor _      []       = return Nothing
getODsFor config (cd:cds) = do
  cds' <- instanceChangesAndCds
    <$> (nameClassDiagramInstance <=< fromInstanceWithNameOverlap) cd
  cds'' <- mapM validChangeClassDiagram cds'
  [cd1', cd2', cd3] <- mapM shuffleClassAndConnectionOrder cds''
    >>= shuffleCdNames
  [cd1, cd2] <- shuffleM [cd1', cd2']
  alloyInstances <- getODInstances config cd1 cd2 cd3 $ length $ classNames cd1
  maybeRandomInstances <- takeRandomInstances alloyInstances
  case maybeRandomInstances of
    Nothing      -> getODsFor config cds
    Just randomInstances -> return $ Just (
      M.fromList [(1, cd1), (2, cd2)],
      M.fromList $ zip ['a' ..] randomInstances
      )

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
      allRelationshipNames = mapMaybe relationshipName relationships1to3
      alloyFor = alloyForAllRelationships allRelationshipNames
      cd1not2 = runCommand "cd1 and (not cd2)" relationships1and2
      cd2not1 = runCommand "cd2 and (not cd1)" relationships1and2
      cd1and2 = runCommand "cd1 and cd2" relationships1and2
      cdNot1not2 = runCommand
        "(not cd1) and (not cd2) and cd3"
        relationships1to3
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
    alloyForAllRelationships allRelationshipNames cd nr = transform
      None
      cd
      (Just allRelationshipNames)
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
takeRandomInstances alloyInstances =
  case takes of
    []  -> return Nothing
    _:_ -> Just <$> do
      randomInstances <- mapM shuffleM alloyInstances
      ts:_    <- shuffleM takes
      shuffleM $ concatMap ($ randomInstances) ts
  where
    takes =
      [ [takeL [1] x, takeL [2] y, takeL [1,2] z, takeL [] u]
      | x <- [0 .. min 2 (length $ fromJust $ M.lookup [1]   alloyInstances)]
      , y <- [0 .. min 2 (length $ fromJust $ M.lookup [2]   alloyInstances)]
      , z <- [0 .. min 2 (length $ fromJust $ M.lookup [1,2] alloyInstances)]
      , u <- [0 .. min 2 (length $ fromJust $ M.lookup []    alloyInstances)]
      , 5 == x + y + z + u
      ]
    takeL k n = take n . fmap (k,) . fromJust . M.lookup k
