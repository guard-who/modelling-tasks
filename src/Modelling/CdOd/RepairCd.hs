{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Modelling.CdOd.RepairCd (
  AllowedProperties (..),
  InValidOption (..),
  RelationshipChangeWithArticle,
  RepairCdConfig (..),
  RepairCdInstance (..),
  checkClassConfigAndChanges,
  checkRepairCdConfig,
  checkRepairCdInstance,
  classAndNonInheritanceNames,
  defaultRepairCdConfig,
  defaultRepairCdInstance,
  allowEverything,
  allowNothing,
  mapInValidOption,
  mapInValidOptionM,
  renameInstance,
  repairCd,
  repairCdEvaluation,
  repairCdSolution,
  repairCdSyntax,
  repairCdTask,
  repairIncorrect,
  PropertyChange (..),
  (.&.),
  illegalChanges,
  legalChanges,
  toProperty,
  ) where

import qualified Modelling.CdOd.CdAndChanges.Transform as Changes (
  transformChanges,
  transformImproveCd,
  )
import qualified Modelling.CdOd.Types             as T (
  CdDrawSettings (..),
  RelationshipProperties (selfInheritances, selfRelationships),
  )

import qualified Data.Bimap                       as BM (fromList)
import qualified Data.Map                         as M (
  delete,
  elems,
  filter,
  fromAscList,
  fromList,
  keys,
  toList,
  traverseWithKey,
  )

import Capabilities.Alloy               (MonadAlloy, getInstances)
import Capabilities.Cache               (MonadCache)
import Capabilities.Diagrams            (MonadDiagrams)
import Capabilities.Graphviz            (MonadGraphviz)
import Modelling.Auxiliary.Common (
  Randomise (randomise),
  RandomiseLayout (randomiseLayout),
  TaskGenerationException (NoInstanceAvailable),
  shuffleEverything,
  )
import Modelling.Auxiliary.Output (
  addPretext,
  hoveringInformation,
  reRefuse,
  simplifiedInformation,
  )
import Modelling.CdOd.Auxiliary.Util    (alloyInstanceToOd)
import Modelling.CdOd.CD2Alloy.Transform (
  combineParts,
  createRunCommand,
  transform,
  )
import Modelling.CdOd.CdAndChanges.Instance (
  AnnotatedChangeAndCd (..),
  ChangeAndCd (..),
  GenericClassDiagramInstance (..),
  fromInstance,
  fromInstanceWithPredefinedNames,
  nameClassDiagramInstance,
  uniformlyAnnotateChangeAndCd,
  )
import Modelling.CdOd.Output (
  cacheCd,
  )
import Modelling.CdOd.Phrasing (
  phraseChange,
  )
import Modelling.CdOd.Types (
  Annotation (..),
  ArticlePreference (..),
  ArticleToUse (DefiniteArticle),
  Cd,
  CdDrawSettings (CdDrawSettings),
  CdMutation,
  ClassConfig (..),
  ClassDiagram (..),
  LimitedLinking (..),
  ObjectProperties (..),
  Od,
  Relationship (..),
  RelationshipProperties (..),
  allCdMutations,
  associationNames,
  checkCdDrawSettings,
  checkCdMutations,
  checkClassConfig,
  checkClassConfigWithProperties,
  classNames,
  defaultOmittedDefaultMultiplicities,
  defaultProperties,
  maxObjects,
  relationshipName,
  renameClassesAndRelationships,
  reverseAssociation,
  shuffleClassAndConnectionOrder,
  toArticleToUse,
  )
import Modelling.Types                  (Change (..))

import Control.Applicative              (Alternative ((<|>)))
import Control.Monad                    ((>=>), forM, void, when, zipWithM)
import Control.Monad.Catch              (MonadThrow (throwM))
import Control.Monad.Output (
  GenericOutputMonad (..),
  LangM,
  Language (English, German),
  OutputMonad,
  Rated,
  ($=<<),
  english,
  enumerateM,
  german,
  multipleChoice,
  singleChoiceSyntax,
  translate,
  )
import Control.Monad.Random (
  MonadRandom,
  RandT,
  RandomGen,
  evalRandT,
  mkStdGen,
  )
import Control.Monad.Trans              (MonadTrans (lift))
import Data.Bifunctor                   (bimap, first, second)
import Data.Bitraversable               (bimapM)
import Data.Containers.ListUtils        (nubOrd, nubOrdOn)
import Data.Either                      (isRight)
import Data.Foldable                    (for_)
import Data.Map                         (Map)
import Data.Maybe                       (catMaybes, listToMaybe, mapMaybe)
import Data.String.Interpolate          (i, iii)
import GHC.Generics                     (Generic)
import System.Random.Shuffle            (shuffle', shuffleM)

data PropertyChange = PropertyChange {
    changeName     :: String,
    operation      :: RelationshipProperties -> RelationshipProperties,
    validityChange :: Bool -> Bool
  }

toProperty :: PropertyChange -> RelationshipProperties
toProperty p = operation p defaultProperties

isValid :: PropertyChange -> Bool
isValid p = validityChange p True

type RelationshipChangeWithArticle
  = Annotation ArticleToUse (Change (Relationship String String))

type CdChangeAndCd = InValidOption
  (AnnotatedChangeAndCd ArticleToUse String String)
  RelationshipChangeWithArticle
  Od

type RelationshipChange = InValidOption
  RelationshipChangeWithArticle
  Cd
  Cd

data InValidOption option forInvalidity forValidity = InValidOption {
  hint :: Either forInvalidity forValidity,
  option :: option
  } deriving (Eq, Generic, Read, Show)

mapInValidOption
  :: (a -> b)
  -> (c -> d)
  -> (e -> f)
  -> InValidOption a c e
  -> InValidOption b d f
mapInValidOption f g h InValidOption {..} = InValidOption {
  hint = bimap g h hint,
  option = f option
  }

mapInValidOptionM
  :: Applicative m
  => (a -> m b)
  -> (c -> m d)
  -> (e -> m f)
  -> InValidOption a c e
  -> m (InValidOption b d f)
mapInValidOptionM f g h InValidOption {..} = InValidOption
  <$> bimapM g h hint
  <*> f option

data RepairCdConfig = RepairCdConfig {
    allowedProperties :: AllowedProperties,
    -- | the article preference when referring to relationships
    articleToUse      :: ArticlePreference,
    classConfig      :: ClassConfig,
    maxInstances     :: Maybe Integer,
    objectProperties :: ObjectProperties,
    printExtendedFeedback :: Bool,
    printNames       :: Bool,
    printNavigations :: Bool,
    printSolution    :: Bool,
    timeout          :: Maybe Int,
    useNames         :: Bool
  } deriving (Generic, Read, Show)

defaultRepairCdConfig :: RepairCdConfig
defaultRepairCdConfig = RepairCdConfig {
    allowedProperties = allowNothing {
        compositionCycles = True,
        Modelling.CdOd.RepairCd.selfRelationships = True
        },
    articleToUse = UseDefiniteArticleWherePossible,
    classConfig = ClassConfig {
        classLimits        = (4, 4),
        aggregationLimits  = (0, Just 0),
        associationLimits  = (0, Just 1),
        compositionLimits  = (2, Just 3),
        inheritanceLimits  = (0, Just 0),
        relationshipLimits = (3, Just 4)
      },
    maxInstances     = Just 200,
    objectProperties = ObjectProperties {
      completelyInhabited = Just True,
      hasLimitedIsolatedObjects = False,
      hasSelfLoops = Nothing,
      usesEveryRelationshipName = Just True
      },
    printExtendedFeedback = False,
    printNames       = True,
    printNavigations = True,
    printSolution    = False,
    timeout          = Nothing,
    useNames         = False
  }

allowedCdMutations :: RepairCdConfig -> [CdMutation]
allowedCdMutations _ = allCdMutations

checkRepairCdConfig :: RepairCdConfig -> Maybe String
checkRepairCdConfig config@RepairCdConfig {..}
  | not printNames && useNames
  = Just "use names is only possible when printing names"
  | completelyInhabited objectProperties /= Just True
  = Just "completelyInhabited needs to be set to 'Just True' for this task type"
  | usesEveryRelationshipName objectProperties /= Just True
  = Just [iii|
      usesEveryRelationshipName needs to be set to 'Just True' for this task type
      |]
  | printExtendedFeedback && not printSolution
  = Just [iii|
      printExtendedFeedback leaks the correct solution
      and thus can only be enabled when printSolution is set to True
      |]
  | otherwise
  = checkClassConfigAndChanges classConfig allowedProperties
  <|> checkCdMutations (allowedCdMutations config)

checkClassConfigAndChanges
  :: ClassConfig
  -> AllowedProperties
  -> Maybe String
checkClassConfigAndChanges classConfig allowedProperties =
  checkClassConfig classConfig
  <|> onlyFirst (map checkChange $ legalChanges allowedProperties)
  where
    checkProp = checkClassConfigWithProperties classConfig
    onlyFirst = listToMaybe . catMaybes
    checkChange c =
      ([iii|
         You should amend your class configuration for
         or disable the property change "#{changeName c}":|] ++)
      <$> checkProp (toProperty c)

repairCdTask
  :: (MonadCache m, MonadDiagrams m, MonadGraphviz m, OutputMonad m)
  => FilePath
  -> RepairCdInstance
  -> LangM m
repairCdTask path task = do
  paragraph $ translate $ do
    english "Consider the following class diagram, which unfortunately is invalid:"
    german "Betrachten Sie folgendes Klassendiagramm, welches leider ungültig ist:"
  image $=<< cacheCd
    (cdDrawSettings task)
    mempty
    (classDiagram task)
    path
  paragraph $ translate $ do
    english [i|Which of the following changes would repair the class diagram?|]
    german [i|Welche der folgenden Änderungen würden das Klassendiagramm reparieren?|]
  let phrase x y Annotation {..} = translate $ do
        english $ phraseChange English annotation x y annotated
        german $ phraseChange German annotation x y annotated
  enumerateM (text . show)
    $ second (phrase (withNames task) (withDirections task) . option)
    <$> M.toList (changes task)
  paragraph $ translate $ do
    english [i|Please state your answer by giving a list of numbers, indicating all changes each resulting in a valid class diagram.|]
    german [i|Bitte geben Sie Ihre Antwort als Liste aller Zahlen an, deren Änderungen jeweils in einem gültigen Klassendiagramm resultieren.|]
  paragraph $ do
    translate $ do
      english [i|Answer by giving a comma separated list of all valid options, e.g., |]
      german [i|Antworten Sie durch Angabe einer durch Komma separierten Liste aller gültigen Optionen. Zum Beispiel |]
    code "[1, 2]"
    translate $ do
      english [i| would indicate that options 1 and 2 each repair the given class diagram.|]
      german [i| als Angabe würde bedeuten, dass die Optionen 1 und 2 jeweils das gegebene Klassendiagramm reparieren.|]
    pure ()
  paragraph simplifiedInformation
  paragraph hoveringInformation
  pure ()

repairCdSyntax :: OutputMonad m => RepairCdInstance -> [Int] -> LangM m
repairCdSyntax inst xs =
  for_ xs $ singleChoiceSyntax False (M.keys $ changes inst)

repairCdEvaluation
  :: (Alternative m, MonadCache m, MonadDiagrams m, MonadGraphviz m, OutputMonad m)
  => FilePath
  -> RepairCdInstance
  -> [Int]
  -> Rated m
repairCdEvaluation path inst xs = addPretext $ do
  let chs = M.fromAscList [
        (English, "changes"),
        (German, "Änderungen")
        ]
      solution = isRight . hint <$> changes inst
      correctAnswer
        | showSolution inst = Just $ show $ repairCdSolution inst
        | otherwise = Nothing
  reRefuse
    (multipleChoice chs correctAnswer solution xs)
    $ when (showExtendedFeedback inst)
    $ void $ M.traverseWithKey
      (repairCdFeedback path (cdDrawSettings inst) xs)
      (changes inst)

repairCdFeedback
  :: (MonadCache m, MonadDiagrams m, MonadGraphviz m, OutputMonad m)
  => FilePath
  -> CdDrawSettings
  -> [Int]
  -> Int
  -> RelationshipChange
  -> LangM m
repairCdFeedback path drawSettings xs x cdChange =
  case hint cdChange of
    Left cd
      | x `elem` xs -> notCorrect *> makesIncorrect *> showCd cd
      | otherwise   -> correct *> makesIncorrect *> showCd cd
    Right cd
      | x `elem` xs -> correct *> makesCorrect *> showCd cd
      | otherwise   -> notCorrect *> makesCorrect *> showCd cd
  where
    correct = paragraph $ translate $ do
      english [iii|Your answer about change #{x} is correct.|]
      german [iii|Ihre Antwort zu Änderung #{x} ist richtig.|]
    notCorrect = paragraph $ translate $ do
      english [iii|Your answer about change #{x} is not correct.|]
      german [iii|Ihre Antwort zu Änderung #{x} ist nicht richtig.|]
    makesCorrect = paragraph $ translate $ do
      english [iii|The change repairs the class diagram as it results in:|]
      german [iii|
        Die Änderung repariert das Klassendiagramm, da es dann so aussieht:
        |]
    makesIncorrect = paragraph $ translate $ do
      english [iii|The change does not repair the class diagram as it results in:|]
      german [iii|
        Die Änderung repariert das Klassendiagramm nicht, da es dann so aussieht:
        |]
    showCd cd = paragraph $
      image $=<< cacheCd drawSettings mempty cd path

repairCdSolution :: RepairCdInstance -> [Int]
repairCdSolution = M.keys . M.filter id . fmap (isRight . hint) . changes

data RepairCdInstance = RepairCdInstance {
    changes        :: Map Int RelationshipChange,
    classDiagram   :: Cd,
    showExtendedFeedback :: Bool,
    showSolution   :: Bool,
    withDirections :: Bool,
    withNames      :: Bool
  } deriving (Eq, Generic, Read, Show)

cdDrawSettings
  :: RepairCdInstance
  -> CdDrawSettings
cdDrawSettings RepairCdInstance {..} = CdDrawSettings {
  omittedDefaults = defaultOmittedDefaultMultiplicities,
  T.printNames = withNames,
  T.printNavigations = withDirections
  }

checkRepairCdInstance :: RepairCdInstance -> Maybe String
checkRepairCdInstance task@RepairCdInstance {..}
  | showExtendedFeedback && not showSolution
  = Just [iii|
      showExtendedFeedback leaks the correct solution
      and thus can only be enabled when showSolution is set to True
      |]
  | otherwise
  = checkCdDrawSettings (cdDrawSettings task)

classAndNonInheritanceNames :: RepairCdInstance -> ([String], [String])
classAndNonInheritanceNames inst =
  let cd = classDiagram inst
      allChs = M.elems $ changes inst
      cds = map (either id id . hint) allChs
      chs = map option allChs
      names = nubOrd $ classNames cd
        ++ concatMap classNames cds
      nonInheritances = nubOrd $ associationNames cd
        ++ mapMaybe (add . annotated >=> relationshipName) chs
        ++ mapMaybe (remove . annotated >=> relationshipName) chs
        ++ concatMap associationNames cds
  in (names, nonInheritances)

instance Randomise RepairCdInstance where
  randomise inst = do
    let (names, nonInheritances) = classAndNonInheritanceNames inst
    names' <- shuffleM names
    nonInheritances' <- shuffleM nonInheritances
    renameInstance inst names' nonInheritances'
      >>= shuffleInstance

instance RandomiseLayout RepairCdInstance where
  randomiseLayout RepairCdInstance {..} = do
    cd <- shuffleClassAndConnectionOrder classDiagram
    changes' <- mapInValidOptionM
      pure
      shuffleClassAndConnectionOrder
      shuffleClassAndConnectionOrder
      `mapM` changes
    return RepairCdInstance {
      changes = changes',
      classDiagram = cd,
      showExtendedFeedback = showExtendedFeedback,
      showSolution = showSolution,
      withDirections = withDirections,
      withNames = withNames
      }

shuffleInstance :: MonadRandom m => RepairCdInstance -> m RepairCdInstance
shuffleInstance inst = do
  chs <- M.fromAscList . zip [1..] <$> shuffleM (M.elems $ changes inst)
  return $ RepairCdInstance {
    changes = chs,
    classDiagram = classDiagram inst,
    showExtendedFeedback = showExtendedFeedback inst,
    showSolution = showSolution inst,
    withDirections = withDirections inst,
    withNames = withNames inst
    }

renameInstance
  :: MonadThrow m
  => RepairCdInstance
  -> [String]
  -> [String]
  -> m RepairCdInstance
renameInstance inst names' nonInheritances' = do
  let (names, nonInheritances) = classAndNonInheritanceNames inst
      bmNames  = BM.fromList $ zip names names'
      bmNonInheritances = BM.fromList $ zip nonInheritances nonInheritances'
      renameCd = renameClassesAndRelationships bmNames bmNonInheritances
      renameEdge = renameClassesAndRelationships bmNames bmNonInheritances
  cd <- renameCd $ classDiagram inst
  chs <- mapM (mapInValidOptionM (mapM $ mapM renameEdge) renameCd renameCd)
    $ changes inst
  return $ RepairCdInstance {
    changes        = chs,
    classDiagram   = cd,
    showExtendedFeedback = showExtendedFeedback inst,
    showSolution   = showSolution inst,
    withDirections = withDirections inst,
    withNames      = withNames inst
    }

repairCd
  :: (MonadAlloy m, MonadThrow m)
  => RepairCdConfig
  -> Int
  -> Int
  -> m RepairCdInstance
repairCd config segment seed = flip evalRandT g $ do
  (cd, chs) <- repairIncorrect
    (allowedProperties config)
    (classConfig config)
    (allowedCdMutations config)
    (objectProperties config)
    (articleToUse config)
    (maxInstances config)
    (timeout config)
  let chs' = map cdAsHint chs
  shuffleEverything $ RepairCdInstance
    (M.fromAscList $ zip [1..] chs')
    cd
    (printExtendedFeedback config)
    (printSolution config)
    (printNavigations config)
    (printNames config && useNames config)
  where
    g = mkStdGen $ (segment +) $ 4 * seed
    cdAsHint x =
      let cd _ = annotatedChangeClassDiagram $ option x
      in mapInValidOption annotatedRelationshipChange cd cd x

defaultRepairCdInstance :: RepairCdInstance
defaultRepairCdInstance = RepairCdInstance {
  changes = M.fromAscList [
    (1, InValidOption {
      hint = Left $ ClassDiagram {
        classNames = ["A", "D", "B", "C"],
        relationships = [
          Composition {
            compositionName = "x",
            compositionPart =
              LimitedLinking {linking = "B", limits = (0, Just 1)},
            compositionWhole =
              LimitedLinking {linking = "D", limits = (0, Just 1)}
            },
          Composition {
            compositionName = "v",
            compositionPart =
              LimitedLinking {linking = "A", limits = (1, Just 0)},
            compositionWhole =
              LimitedLinking {linking = "C", limits = (0, Just 1)}
            },
          Aggregation {
            aggregationName = "z",
            aggregationPart =
              LimitedLinking {linking = "A", limits = (0, Just 1)},
            aggregationWhole =
              LimitedLinking {linking = "B", limits = (0, Just 1)}
            }
          ]
        },
      option = Annotation {
        annotated = Change {
          add = Nothing,
          remove = Just $ Composition {
            compositionName = "y",
            compositionPart = LimitedLinking {linking = "D", limits = (0, Just 2)},
            compositionWhole = LimitedLinking {linking = "A", limits = (0, Just 1)}
            }
          },
        annotation = DefiniteArticle
        }
      }),
    (2, InValidOption {
      hint = Right $ ClassDiagram {
        classNames = ["D", "B", "A", "C"],
        relationships = [
          Composition {
            compositionName = "x",
            compositionPart =
              LimitedLinking {linking = "B", limits = (0, Just 1)},
            compositionWhole =
              LimitedLinking {linking = "D", limits = (0, Just 1)}
            },
          Aggregation {
            aggregationName = "z",
            aggregationPart =
              LimitedLinking {linking = "A", limits = (0, Just 1)},
            aggregationWhole =
              LimitedLinking {linking = "B", limits = (0, Just 1)}
            },
          Composition {
            compositionName = "v",
            compositionPart =
              LimitedLinking {linking = "A", limits = (1, Just 1)},
            compositionWhole =
              LimitedLinking {linking = "C", limits = (0, Just 1)}
            },
          Composition {
            compositionName = "y",
            compositionPart =
              LimitedLinking {linking = "D", limits = (0, Just 2)},
            compositionWhole =
              LimitedLinking {linking = "A", limits = (0, Just 1)}
            }
          ]
        },
      option = Annotation {
        annotated = Change {
          add = Just $ Composition {
            compositionName = "w",
            compositionPart = LimitedLinking {linking = "A", limits = (1, Just 1)},
            compositionWhole = LimitedLinking {linking = "C", limits = (0, Just 1)}
            },
          remove = Just $ Composition {
            compositionName = "v",
            compositionPart = LimitedLinking {linking = "A", limits = (1, Just 0)},
            compositionWhole = LimitedLinking {linking = "C", limits = (0, Just 1)}
            }
          },
        annotation = DefiniteArticle
        }
      }),
    (3, InValidOption {
      hint = Right $ ClassDiagram {
        classNames = ["C", "A", "B", "D"],
        relationships = [
          Composition {
            compositionName = "y",
            compositionPart =
              LimitedLinking {linking = "D", limits = (0, Just 2)},
            compositionWhole =
              LimitedLinking {linking = "A", limits = (0, Just 1)}
            },
          Aggregation {
            aggregationName = "z",
            aggregationPart =
              LimitedLinking {linking = "A", limits = (0, Just 1)},
            aggregationWhole =
              LimitedLinking {linking = "B", limits = (0, Just 1)}
            },
          Composition {
            compositionName = "x",
            compositionPart =
              LimitedLinking {linking = "B", limits = (0, Just 1)},
            compositionWhole =
              LimitedLinking {linking = "D", limits = (0, Just 1)}
            }
          ]
        },
      option = Annotation {
        annotated = Change {
          add = Nothing,
          remove = Just $ Composition {
            compositionName = "v",
            compositionPart = LimitedLinking {linking = "A", limits = (1, Just 0)},
            compositionWhole = LimitedLinking {linking = "C", limits = (0, Just 1)}
            }
          },
        annotation = DefiniteArticle
        }
      }),
    (4, InValidOption {
      hint = Left $ ClassDiagram {
        classNames = ["A", "D", "B", "C"],
        relationships = [
          Association {
            associationName = "u",
            associationFrom =
              LimitedLinking {linking = "C", limits = (2, Just 2)},
            associationTo =
              LimitedLinking {linking = "A", limits = (2, Just 2)}
            },
          Composition {
            compositionName = "y",
            compositionPart =
              LimitedLinking {linking = "D", limits = (0, Just 2)},
            compositionWhole =
              LimitedLinking {linking = "A", limits = (0, Just 1)}
            },
          Aggregation {
            aggregationName = "z",
            aggregationPart =
              LimitedLinking {linking = "A", limits = (0, Just 1)},
            aggregationWhole =
              LimitedLinking {linking = "B", limits = (0, Just 1)}
            },
          Composition {
            compositionName = "v",
            compositionPart =
              LimitedLinking {linking = "A", limits = (1, Just 0)},
            compositionWhole =
              LimitedLinking {linking = "C", limits = (0, Just 1)}
            },
          Composition {
            compositionName = "x",
            compositionPart =
              LimitedLinking {linking = "B", limits = (0, Just 1)},
            compositionWhole =
              LimitedLinking {linking = "D", limits = (0, Just 1)}
            }
          ]
        },
      option = Annotation {
        annotated = Change {
          add = Just $ Association {
            associationName = "u",
            associationFrom = LimitedLinking {linking = "C", limits = (2, Just 2)},
            associationTo = LimitedLinking {linking = "A", limits = (2, Just 2)}
            },
          remove = Nothing
          },
        annotation = DefiniteArticle
        }
      })
    ],
  classDiagram = ClassDiagram {
    classNames = ["D", "A", "C", "B"],
    relationships = [
      Composition {
        compositionName = "x",
        compositionPart = LimitedLinking {linking = "B", limits = (0, Just 1)},
        compositionWhole = LimitedLinking {linking = "D", limits = (0, Just 1)}
        },
      Aggregation {
        aggregationName = "z",
        aggregationPart = LimitedLinking {linking = "A", limits = (0, Just 1)},
        aggregationWhole = LimitedLinking {linking = "B", limits = (0, Just 1)}
        },
      Composition {
        compositionName = "v",
        compositionPart = LimitedLinking {linking = "A", limits = (1, Just 0)},
        compositionWhole = LimitedLinking {linking = "C", limits = (0, Just 1)}
        },
      Composition {
        compositionName = "y",
        compositionPart = LimitedLinking {linking = "D", limits = (0, Just 2)},
        compositionWhole = LimitedLinking {linking = "A", limits = (0, Just 1)}
        }
      ]
    },
  showExtendedFeedback = False,
  showSolution = False,
  withDirections = False,
  withNames = True
  }

type PropertyChangeSet = ChangeSet PropertyChange

data ChangeSet a = ChangeSet {
  illegalChange :: a,
  otherChanges :: (a, a, a, a)
  } deriving (Eq, Functor, Ord)

possibleChanges
  :: AllowedProperties
  -> [PropertyChangeSet]
possibleChanges allowed = nubOrdOn
  (fmap changeName)
  [ ChangeSet e0 cs
  | e0 <- illegalChanges allowed
  , l0 <- legalChanges allowed
  , let ls = delete l0 $ legalChanges allowed
  , c0 <- allChanges allowed
  , l1 <- if null ls then [[]] else map (\x -> [x .&. noChange, x]) ls
  , let changes = [c0, noChange, e0] ++ l1
  , c1 <- changes
  , c2 <- delete c1 changes
  , let cs = (l0 .&. e0, noChange, c1, c2)
  ]
  where
    delete x xs = M.elems . M.delete (changeName x) . M.fromList
      $ zip (map changeName xs) xs

{-|
Introduces deterministic permutations on a a list of 'PropertyChangeSet's.
The key point is to maintain reproducibility but achieving diversity nonetheless.
-}
diversify :: [PropertyChangeSet] -> [(PropertyChange, [PropertyChange])]
diversify = zipWith permutate [0..]
  where
    permutate g c =
      let (w, x, y, z) = otherChanges c
      in (illegalChange c, shuffle' [w, x, y, z] 4 $ mkStdGen g)

repairIncorrect
  :: (MonadAlloy m, MonadThrow m, RandomGen g)
  => AllowedProperties
  -> ClassConfig
  -> [CdMutation]
  -> ObjectProperties
  -> ArticlePreference
  -> Maybe Integer
  -> Maybe Int
  -> RandT g m (Cd, [CdChangeAndCd])
repairIncorrect cdProperties config cdMutations objectProperties preference maxInstances to = do
  changeSets <- shuffleM $ diversify $ possibleChanges cdProperties
  tryNextChangeSet changeSets
  where
    tryNextChangeSet [] = lift $ throwM NoInstanceAvailable
    tryNextChangeSet ((e0, propertyChanges) : changeSets) = do
      let alloyCode = Changes.transformChanges
            config
            cdMutations
            (toProperty e0)
            (Just config)
            $ map toProperty propertyChanges
      instances <- getInstances maxInstances to alloyCode
      randomInstances <- shuffleM instances
      getInstanceWithODs changeSets propertyChanges randomInstances
    article = toArticleToUse preference
    getInstanceWithODs changeSets _  [] =
      tryNextChangeSet changeSets
    getInstanceWithODs cs propertyChanges (alloyInstance : alloyInstances) = do
      cdInstance <- fromInstance alloyInstance >>= nameClassDiagramInstance
      (shuffledPropertyChanges, shuffledChangesAndCds) <-
        unzip <$> shuffleM (zip propertyChanges $ instanceChangesAndCds cdInstance)
      let shuffledCdInstance = cdInstance {
            instanceChangesAndCds = shuffledChangesAndCds
            }
      let cd = instanceClassDiagram shuffledCdInstance
          chs = instanceChangesAndCds shuffledCdInstance
      hints <- zipWithM getOdOrImprovedCd shuffledPropertyChanges chs
      case sequenceA hints of
        Nothing -> getInstanceWithODs cs propertyChanges alloyInstances
        Just odsAndCds -> do
          let odsAndCdWithArticle = map (first addArticle) odsAndCds
              chs' = map (uniformlyAnnotateChangeAndCd article) chs
          return (cd, zipWith InValidOption odsAndCdWithArticle chs')
    addArticle = (`Annotation` article)
    getOdOrImprovedCd propertyChange change
      | isValid propertyChange = fmap Right <$> getOD (changeClassDiagram change)
      | otherwise = fmap Left
        <$> getImprovedCd (changeClassDiagram change) (toProperty propertyChange)
    getImprovedCd cd properties = do
      let alloyCode = Changes.transformImproveCd
            cd
            config
            cdMutations
            properties
      changes <- listToMaybe <$> getInstances (Just 1) to alloyCode
      fmap (relationshipChange . head . instanceChangesAndCds)
        <$> traverse fromInstanceWithPredefinedNames changes
    getOD :: (MonadAlloy m, MonadThrow m) => Cd -> m (Maybe Od)
    getOD cd = do
      let reversedRelationships = map reverseAssociation $ relationships cd
          maxNumberOfObjects = maxObjects $ snd $ classLimits config
          parts = transform
            (cd {relationships = reversedRelationships})
            []
            maxNumberOfObjects
            objectProperties
            ""
            ""
          command = createRunCommand
            "cd"
            (length $ classNames cd)
            maxNumberOfObjects
            reversedRelationships
            parts
      od <- listToMaybe
        <$> getInstances (Just 1) to (combineParts parts ++ command)
      forM od alloyInstanceToOd

data AllowedProperties = AllowedProperties {
  compositionCycles      :: Bool,
  doubleRelationships    :: Bool,
  inheritanceCycles      :: Bool,
  reverseInheritances    :: Bool,
  reverseRelationships   :: Bool,
  selfInheritances       :: Bool,
  selfRelationships      :: Bool,
  wrongAssociationLimits :: Bool,
  wrongCompositionLimits :: Bool
  } deriving (Generic, Read, Show)

allowEverything :: AllowedProperties
allowEverything = AllowedProperties {
  compositionCycles = True,
  doubleRelationships    = True,
  inheritanceCycles      = True,
  reverseInheritances = True,
  reverseRelationships   = True,
  selfInheritances       = True,
  selfRelationships      = True,
  wrongAssociationLimits = True,
  wrongCompositionLimits = True
  }

allowNothing :: AllowedProperties
allowNothing = AllowedProperties {
  compositionCycles           = False,
  doubleRelationships         = False,
  inheritanceCycles           = False,
  reverseInheritances         = False,
  reverseRelationships        = False,
  selfInheritances            = False,
  selfRelationships           = False,
  wrongAssociationLimits      = False,
  wrongCompositionLimits      = False
  }

allChanges :: AllowedProperties -> [PropertyChange]
allChanges c = legalChanges c ++ illegalChanges c

noChange :: PropertyChange
noChange = PropertyChange "none" id id

infixl 9 .&.
(.&.) :: PropertyChange -> PropertyChange -> PropertyChange
PropertyChange n1 o1 v1 .&. PropertyChange n2 o2 v2 = PropertyChange
  (n1 ++ " + " ++ n2)
  (o1 . o2)
  (v1 . v2)

legalChanges :: AllowedProperties -> [PropertyChange]
legalChanges allowed = noChange : [
    PropertyChange "add one self relationship" addSelfRelationships id
  | Modelling.CdOd.RepairCd.selfRelationships allowed] ++ [
    PropertyChange "force double relationships" withDoubleRelationships id
  | doubleRelationships allowed] ++ [
    PropertyChange "force reverse relationships" withReverseRelationships id
  | reverseRelationships allowed]
--    PropertyChange "force multiple inheritances" withMultipleInheritances id
  where
    addSelfRelationships :: RelationshipProperties -> RelationshipProperties
    addSelfRelationships config@RelationshipProperties {..}
      = config { T.selfRelationships = selfRelationships + 1 }
    withDoubleRelationships :: RelationshipProperties -> RelationshipProperties
    withDoubleRelationships config
      = config { hasDoubleRelationships = Just True }
    withReverseRelationships :: RelationshipProperties -> RelationshipProperties
    withReverseRelationships config
      = config { hasReverseRelationships = Just True }
    -- withMultipleInheritances :: RelationshipProperties -> RelationshipProperties
    -- withMultipleInheritances config
    --   = config { hasMultipleInheritances = True }

illegalChanges :: AllowedProperties -> [PropertyChange]
illegalChanges allowed = map ($ const False) $ [
    PropertyChange "add wrong association" addWrongNonInheritances
  | wrongAssociationLimits allowed] ++ [
    PropertyChange "add wrong composition" addWrongCompositions
  | wrongCompositionLimits allowed] ++ [
    PropertyChange "force inheritance cycles" withNonTrivialInheritanceCycles
  | inheritanceCycles allowed] ++ [
    PropertyChange "force reverse inheritances" withReverseInheritances
  | reverseInheritances allowed] ++ [
    PropertyChange "add self inheritance" addSelfInheritance
  | Modelling.CdOd.RepairCd.selfInheritances allowed] ++ [
    PropertyChange "force composition cycles" withCompositionCycles
  | compositionCycles allowed]
  where
    addWrongNonInheritances :: RelationshipProperties -> RelationshipProperties
    addWrongNonInheritances config@RelationshipProperties {..}
      = config { wrongNonInheritances = wrongNonInheritances + 1 }
    addWrongCompositions :: RelationshipProperties -> RelationshipProperties
    addWrongCompositions config@RelationshipProperties {..}
      = config { wrongCompositions = wrongCompositions + 1 }
    addSelfInheritance :: RelationshipProperties -> RelationshipProperties
    addSelfInheritance config@RelationshipProperties {..}
      = config { T.selfInheritances = selfInheritances + 1 }
    withReverseInheritances :: RelationshipProperties -> RelationshipProperties
    withReverseInheritances config
      = config { hasReverseInheritances = True }
    withNonTrivialInheritanceCycles
      :: RelationshipProperties
      -> RelationshipProperties
    withNonTrivialInheritanceCycles config
      = config { hasNonTrivialInheritanceCycles = True }
    withCompositionCycles :: RelationshipProperties -> RelationshipProperties
    withCompositionCycles config
      = config { hasCompositionCycles = True }
