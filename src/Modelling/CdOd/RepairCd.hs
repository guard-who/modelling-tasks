{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Modelling.CdOd.RepairCd (
  InValidOption (..),
  RelationshipChangeWithArticle,
  RepairCdConfig (..),
  RepairCdInstance (..),
  RepairCdTaskTextElement (..),
  checkClassConfigAndChanges,
  checkRepairCdConfig,
  checkRepairCdInstance,
  classAndNonInheritanceNames,
  defaultRepairCdConfig,
  defaultRepairCdInstance,
  mapInValidOption,
  mapInValidOptionM,
  renameInstance,
  repairCd,
  repairCdEvaluation,
  repairCdSolution,
  repairCdSyntax,
  repairCdTask,
  repairIncorrect,
  StructuralWeakening (..),
  (.&.),
  illegalStructuralWeakenings,
  legalStructuralWeakenings,
  toProperty,
  ) where

import qualified Modelling.CdOd.CdAndChanges.Transform as Changes (
  transformChanges,
  transformImproveCd,
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
  checkTaskText,
  hoveringInformation,
  simplifiedInformation,
  uniform,
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
  AllowedProperties (..),
  Annotation (..),
  AnyCd,
  AnyClassDiagram (..),
  AnyRelationship,
  ArticlePreference (..),
  Cd,
  CdConstraints (..),
  CdDrawSettings (..),
  CdMutation (RemoveRelationship),
  ClassConfig (..),
  ClassDiagram (..),
  LimitedLinking (..),
  ObjectProperties (..),
  Od,
  OmittedDefaultMultiplicities (..),
  Relationship (..),
  RelationshipProperties (..),
  allCdMutations,
  allowNothing,
  anonymiseObjects,
  anyAssociationNames,
  anyRelationshipName,
  checkCdConstraints,
  checkCdDrawSettings,
  checkCdMutations,
  checkClassConfig,
  checkClassConfigWithProperties,
  checkObjectProperties,
  classNames,
  defaultCdConstraints,
  defaultCdDrawSettings,
  defaultProperties,
  fromClassDiagram,
  maxObjects,
  relationshipName,
  renameClassesAndRelationships,
  shuffleAnyClassAndConnectionOrder,
  shuffleClassAndConnectionOrder,
  toArticleToUse,
  toValidCd,
  )
import Modelling.Types                  (Change (..))

import Control.Applicative              (Alternative ((<|>)))
import Control.Monad                    ((>=>), forM, void, when, zipWithM)
import Control.Monad.Catch              (MonadThrow (throwM))
import Control.OutputCapable.Blocks (
  ArticleToUse (DefiniteArticle),
  GenericOutputCapable (..),
  LangM,
  Language (English, German),
  OutputCapable,
  Rated,
  ($=<<),
  english,
  enumerateM,
  german,
  multipleChoice,
  multipleChoiceSyntax,
  reRefuse,
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
import Data.List                        (singleton)
import Data.Map                         (Map)
import Data.Maybe                       (catMaybes, listToMaybe, mapMaybe)
import Data.Ratio                       ((%))
import Data.String.Interpolate          (i, iii)
import GHC.Generics                     (Generic)
import System.Random.Shuffle            (shuffle', shuffleM)

data StructuralWeakening = StructuralWeakening {
    weakeningName  :: !String,
    operation      :: RelationshipProperties -> RelationshipProperties,
    validityChange :: Bool -> Bool
  }

toProperty :: StructuralWeakening -> RelationshipProperties
toProperty p = operation p defaultProperties

isValidWeakening :: StructuralWeakening -> Bool
isValidWeakening p = validityChange p True

type RelationshipChangeWithArticle
  = Annotation ArticleToUse (Change (AnyRelationship String String))

type CdChangeAndCd = InValidOption
  (AnnotatedChangeAndCd ArticleToUse String String)
  RelationshipChangeWithArticle
  Od

type RelationshipChange = InValidOption
  RelationshipChangeWithArticle
  AnyCd
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

data RepairCdConfig
  = RepairCdConfig {
    allowedCdMutations :: ![CdMutation],
    allowedProperties :: AllowedProperties,
    -- | the article preference when referring to relationships
    articleToUse      :: ArticlePreference,
    cdConstraints :: !CdConstraints,
    classConfig      :: ClassConfig,
    drawSettings      :: !CdDrawSettings,
    maxInstances     :: Maybe Integer,
    objectProperties :: ObjectProperties,
    printExtendedFeedback :: Bool,
    printSolution    :: Bool,
    timeout          :: Maybe Int,
    useNames         :: Bool
  } deriving (Generic, Read, Show)

defaultRepairCdConfig :: RepairCdConfig
defaultRepairCdConfig
  = RepairCdConfig {
    allowedCdMutations = allCdMutations,
    allowedProperties = allowNothing {
        compositionCycles = True,
        selfRelationships = True
        },
    articleToUse = UseDefiniteArticleWherePossible,
    cdConstraints = defaultCdConstraints,
    classConfig = ClassConfig {
        classLimits        = (4, 4),
        aggregationLimits  = (0, Just 0),
        associationLimits  = (0, Just 1),
        compositionLimits  = (2, Just 3),
        inheritanceLimits  = (0, Just 0),
        relationshipLimits = (3, Just 4)
      },
    drawSettings = defaultCdDrawSettings,
    maxInstances     = Just 10,
    objectProperties = ObjectProperties {
      anonymousObjectProportion = 0 % 1,
      completelyInhabited = Just True,
      hasLimitedIsolatedObjects = False,
      hasSelfLoops = Nothing,
      usesEveryRelationshipName = Just True
      },
    printExtendedFeedback = True,
    printSolution    = True,
    timeout          = Nothing,
    useNames         = True
  }

checkRepairCdConfig :: RepairCdConfig -> Maybe String
checkRepairCdConfig RepairCdConfig {..}
  | not (printNames drawSettings) && useNames
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
  <|> checkCdConstraints allowedProperties cdConstraints
  <|> checkCdMutations allowedCdMutations
  <|> checkCdDrawSettings drawSettings
  <|> checkObjectProperties objectProperties

checkClassConfigAndChanges
  :: ClassConfig
  -> AllowedProperties
  -> Maybe String
checkClassConfigAndChanges classConfig allowedProperties =
  checkClassConfig classConfig
  <|> onlyFirst (map checkChange $ legalStructuralWeakenings allowedProperties)
  where
    checkProp = checkClassConfigWithProperties classConfig
    onlyFirst = listToMaybe . catMaybes
    checkChange c =
      ([iii|
         You should amend your class configuration for
         or disable the property change "#{weakeningName c}":|] ++)
      <$> checkProp (toProperty c)

defaultRepairCdTaskText :: RepairCdTaskText
defaultRepairCdTaskText = [
  Paragraph $ singleton $ Translated $ translations $ do
    english "Consider the following class diagram, which unfortunately is invalid:"
    german "Betrachten Sie folgendes Klassendiagramm, welches leider ungültig ist:",
  Paragraph $ singleton $ Special IncorrectCd,
  Paragraph $ singleton $ Translated $ translations $ do
    english [i|Which of the following changes would each repair the class diagram?|]
    german [i|Welche der folgenden Änderungen würden jeweils das Klassendiagramm reparieren?|],
  Special PotentialFixes,
  Paragraph $ singleton $ Translated $ translations $ do
    english [i|Please state your answer by giving a list of numbers, indicating all changes each resulting in a valid class diagram.|]
    german [i|Bitte geben Sie Ihre Antwort als Liste aller Zahlen an, deren Änderungen jeweils in einem gültigen Klassendiagramm resultieren.|],
  Paragraph [
    Translated $ translations $ do
      english [i|Answer by giving a comma separated list of all appropriate options, e.g., |]
      german [i|Antworten Sie durch Angabe einer durch Komma separierten Liste aller zutreffenden Optionen. Zum Beispiel |],
    Code $ uniform "[1, 2]",
    Translated $ translations $ do
      english [i| would indicate that options 1 and 2 each repair the given class diagram.|]
      german [i| als Angabe würde bedeuten, dass die Optionen 1 und 2 jeweils das gegebene Klassendiagramm reparieren.|]
    ]
  ]

repairCdTask
  :: (MonadCache m, MonadDiagrams m, MonadGraphviz m, OutputCapable m)
  => FilePath
  -> RepairCdInstance
  -> LangM m
repairCdTask path task = do
  toTaskText path task
  paragraph simplifiedInformation
  paragraph hoveringInformation
  pure ()

repairCdSyntax :: OutputCapable m => RepairCdInstance -> [Int] -> LangM m
repairCdSyntax inst =
  multipleChoiceSyntax False (M.keys $ changes inst)

repairCdEvaluation
  :: (Alternative m, MonadCache m, MonadDiagrams m, MonadGraphviz m, OutputCapable m)
  => FilePath
  -> RepairCdInstance
  -> [Int]
  -> Rated m
repairCdEvaluation path inst xs = addPretext $ do
  let chs = M.fromAscList [
        (English, "repairs"),
        (German, "Reparaturen")
        ]
      solution = isRight . hint <$> changes inst
      correctAnswer
        | showSolution inst = Just $ show $ repairCdSolution inst
        | otherwise = Nothing
  reRefuse
    (multipleChoice DefiniteArticle chs correctAnswer solution xs)
    $ when (showExtendedFeedback inst)
    $ void $ M.traverseWithKey
      (repairCdFeedback path (cdDrawSettings inst) xs)
      (changes inst)

repairCdFeedback
  :: (MonadCache m, MonadDiagrams m, MonadGraphviz m, OutputCapable m)
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
      | x `elem` xs -> correct *> makesCorrect *> showCd (fromClassDiagram cd)
      | otherwise -> notCorrect *> makesCorrect *> showCd (fromClassDiagram cd)
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

type RepairCdTaskText = [SpecialOutput RepairCdTaskTextElement]

data RepairCdTaskTextElement
  = IncorrectCd
  | PotentialFixes
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

toTaskText
  :: (MonadCache m, MonadDiagrams m, MonadGraphviz m, OutputCapable m)
  => FilePath
  -> RepairCdInstance
  -> LangM m
toTaskText path task =
  specialToOutputCapable (toTaskSpecificText path task) (taskText task)

toTaskSpecificText
  :: (MonadCache m, MonadDiagrams m, MonadGraphviz m, OutputCapable m)
  => FilePath
  -> RepairCdInstance
  -> RepairCdTaskTextElement
  -> LangM m
toTaskSpecificText path RepairCdInstance {..} = \case
  IncorrectCd -> image $=<< cacheCd
    cdDrawSettings
    mempty
    classDiagram
    path
  PotentialFixes ->
    enumerateM (text . show)
      $ second (phrase byName (printNavigations cdDrawSettings) . option)
      <$> M.toList changes
  where
    defaults = omittedDefaults cdDrawSettings
    phrase x y Annotation {..} = translate $ do
      english $ phraseChange English defaults annotation x y annotated
      german $ phraseChange German defaults annotation x y annotated

data RepairCdInstance
  = RepairCdInstance {
    byName         :: !Bool,
    cdDrawSettings :: !CdDrawSettings,
    changes        :: Map Int RelationshipChange,
    classDiagram   :: AnyCd,
    showExtendedFeedback :: Bool,
    showSolution   :: !Bool,
    taskText       :: !RepairCdTaskText
  } deriving (Eq, Generic, Read, Show)

checkRepairCdInstance :: RepairCdInstance -> Maybe String
checkRepairCdInstance RepairCdInstance {..}
  | not (printNames cdDrawSettings) && byName
  = Just "by name is only possible when printing names"
  | showExtendedFeedback && not showSolution
  = Just [iii|
      showExtendedFeedback leaks the correct solution
      and thus can only be enabled when showSolution is set to True
      |]
  | otherwise
  = checkTaskText taskText
  <|> checkCdDrawSettings cdDrawSettings

classAndNonInheritanceNames :: RepairCdInstance -> ([String], [String])
classAndNonInheritanceNames inst =
  let cd = classDiagram inst
      allChs = M.elems $ changes inst
      cds = map (either id fromClassDiagram . hint) allChs
      chs = map option allChs
      names = nubOrd $ anyClassNames cd
        ++ concatMap anyClassNames cds
      nonInheritances = nubOrd $ anyAssociationNames cd
        ++ mapMaybe (add . annotated >=> anyRelationshipName) chs
        ++ mapMaybe (remove . annotated >=> anyRelationshipName) chs
        ++ concatMap anyAssociationNames cds
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
    cd <- shuffleAnyClassAndConnectionOrder classDiagram
    changes' <- mapInValidOptionM
      pure
      shuffleAnyClassAndConnectionOrder
      shuffleClassAndConnectionOrder
      `mapM` changes
    return RepairCdInstance {
      byName = byName,
      cdDrawSettings = cdDrawSettings,
      changes = changes',
      classDiagram = cd,
      showExtendedFeedback = showExtendedFeedback,
      showSolution = showSolution,
      taskText = taskText
      }

shuffleInstance :: MonadRandom m => RepairCdInstance -> m RepairCdInstance
shuffleInstance RepairCdInstance {..} = do
  chs <- M.fromAscList . zip [1..] <$> shuffleM (M.elems changes)
  return $ RepairCdInstance {
    byName = byName,
    cdDrawSettings = cdDrawSettings,
    changes = chs,
    classDiagram = classDiagram,
    showExtendedFeedback = showExtendedFeedback,
    showSolution = showSolution,
    taskText = taskText
    }

renameInstance
  :: MonadThrow m
  => RepairCdInstance
  -> [String]
  -> [String]
  -> m RepairCdInstance
renameInstance inst@RepairCdInstance {..} names' nonInheritances' = do
  let (names, nonInheritances) = classAndNonInheritanceNames inst
      bmNames  = BM.fromList $ zip names names'
      bmNonInheritances = BM.fromList $ zip nonInheritances nonInheritances'
      renameCd = renameClassesAndRelationships bmNames bmNonInheritances
      renameAnyCd = renameClassesAndRelationships bmNames bmNonInheritances
      renameEdge = renameClassesAndRelationships bmNames bmNonInheritances
      renameAnyEdge = renameClassesAndRelationships bmNames bmNonInheritances
      renameEdges = mapM $ mapM $ bimapM renameAnyEdge renameEdge
  cd <- renameAnyCd classDiagram
  chs <- mapM
    (mapInValidOptionM renameEdges renameAnyCd renameCd)
    changes
  return $ RepairCdInstance {
    byName         = byName,
    cdDrawSettings = cdDrawSettings,
    changes        = chs,
    classDiagram   = cd,
    showExtendedFeedback = showExtendedFeedback,
    showSolution   = showSolution,
    taskText       = taskText
    }

repairCd
  :: (MonadAlloy m, MonadThrow m)
  => RepairCdConfig
  -> Int
  -> Int
  -> m RepairCdInstance
repairCd RepairCdConfig {..} segment seed = flip evalRandT g $ do
  (cd, chs) <- repairIncorrect
    allowedProperties
    classConfig
    cdConstraints
    allowedCdMutations
    objectProperties
    articleToUse
    maxInstances
    timeout
  chs' <- lift $ mapM cdAsHint chs
  shuffleEverything $ RepairCdInstance {
    byName = printNames drawSettings && useNames,
    cdDrawSettings = drawSettings,
    changes = M.fromAscList $ zip [1..] chs',
    classDiagram = cd,
    showExtendedFeedback = printExtendedFeedback,
    showSolution = printSolution,
    taskText = defaultRepairCdTaskText
    }
  where
    g = mkStdGen $ (segment +) $ 4 * seed
    cdAsHint x = do
      let cd _ = pure $ annotatedChangeClassDiagram $ option x
          validCd _ = toValidCd $ annotatedChangeClassDiagram $ option x
      mapInValidOptionM (pure . annotatedRelationshipChange) cd validCd x

defaultRepairCdInstance :: RepairCdInstance
defaultRepairCdInstance = RepairCdInstance {
  byName = True,
  cdDrawSettings = CdDrawSettings {
    omittedDefaults = OmittedDefaultMultiplicities {
      aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
      associationOmittedDefaultMultiplicity = Just (0, Nothing),
      compositionWholeOmittedDefaultMultiplicity = Just (1, Just 1)
      },
    printNames = True,
    printNavigations = True
    },
  changes = M.fromList [
    (1, InValidOption {
      hint = Right ClassDiagram {
        classNames = ["B", "A", "D", "C"],
        relationships = [
          Composition {
            compositionName = "z",
            compositionPart =
              LimitedLinking {linking = "D", limits = (1, Nothing)},
            compositionWhole =
              LimitedLinking {linking = "A", limits = (0, Just 1)}
            },
          Composition {
            compositionName = "w",
            compositionPart =
              LimitedLinking {linking = "C", limits = (1, Just 1)},
            compositionWhole =
              LimitedLinking {linking = "D", limits = (1, Just 1)}
            }
          ]
        },
      option = Annotation {
        annotated = Change {
          add = Nothing,
          remove = Just $ Right Composition {
            compositionName = "x",
            compositionPart =
              LimitedLinking {linking = "A", limits = (0, Just 1)},
            compositionWhole =
              LimitedLinking {linking = "C", limits = (1, Just 1)}
            }
          },
        annotation = DefiniteArticle
        }
      }),
    (2, InValidOption {
      hint = Right ClassDiagram {
        classNames = ["C", "D", "A", "B"],
        relationships = [
          Composition {
            compositionName = "z",
            compositionPart =
              LimitedLinking {linking = "D", limits = (1, Nothing)},
            compositionWhole =
              LimitedLinking {linking = "A", limits = (0, Just 1)}
            },
          Composition {
            compositionName = "x",
            compositionPart =
              LimitedLinking {linking = "A", limits = (0, Just 1)},
            compositionWhole =
              LimitedLinking {linking = "C", limits = (1, Just 1)}
            }
          ]
        },
      option = Annotation {
        annotated = Change {
          add = Nothing,
          remove = Just $ Right Composition {
            compositionName = "w",
            compositionPart =
              LimitedLinking {linking = "C", limits = (1, Just 1)},
            compositionWhole =
              LimitedLinking {linking = "D", limits = (1, Just 1)}
            }
          },
        annotation = DefiniteArticle
        }
      }),
    (3, InValidOption {
      hint = Left (AnyClassDiagram {
        anyClassNames = ["C", "D", "A", "B"],
        anyRelationships = [
          Right Composition {
            compositionName = "x",
            compositionPart =
              LimitedLinking {linking = "A", limits = (0, Just 1)},
            compositionWhole =
              LimitedLinking {linking = "C", limits = (1, Just 1)}
            },
          Right Association {
            associationName = "y",
            associationFrom =
              LimitedLinking {linking = "D", limits = (0, Just 1)},
            associationTo =
              LimitedLinking {linking = "D", limits = (0, Nothing)}
            },
          Right Composition {
            compositionName = "z",
            compositionPart =
              LimitedLinking {linking = "D", limits = (1, Nothing)},
            compositionWhole =
              LimitedLinking {linking = "A", limits = (0, Just 1)}
            },
          Right Composition {
            compositionName = "w",
            compositionPart =
              LimitedLinking {linking = "C", limits = (1, Just 1)},
            compositionWhole =
              LimitedLinking {linking = "D", limits = (1, Just 1)}
            }
          ]
        }),
      option = Annotation {
        annotated = Change {
          add = Just $ Right Association {
            associationName = "y",
            associationFrom =
              LimitedLinking {linking = "D", limits = (0, Just 1)},
            associationTo =
              LimitedLinking {linking = "D", limits = (0, Nothing)}
            },
          remove = Nothing
          },
        annotation = DefiniteArticle
        }
      }),
    (4, InValidOption {
      hint = Right ClassDiagram {
        classNames = ["B", "A", "C", "D"],
        relationships = [
          Composition {
            compositionName = "x",
            compositionPart =
              LimitedLinking {linking = "A", limits = (0, Just 1)},
            compositionWhole =
              LimitedLinking {linking = "C", limits = (1, Just 1)}
            },
          Composition {
            compositionName = "w",
            compositionPart =
              LimitedLinking {linking = "C", limits = (1, Just 1)},
            compositionWhole =
              LimitedLinking {linking = "D", limits = (1, Just 1)}
            }
          ]
        },
      option = Annotation {
        annotated = Change {
          add = Nothing,
          remove = Just $ Right Composition {
            compositionName = "z",
            compositionPart =
              LimitedLinking {linking = "D", limits = (1, Nothing)},
            compositionWhole =
              LimitedLinking {linking = "A", limits = (0, Just 1)}
            }
          },
        annotation = DefiniteArticle
        }
      })
    ],
  classDiagram = AnyClassDiagram {
    anyClassNames = ["D", "C", "B", "A"],
    anyRelationships = [
      Right Composition {
        compositionName = "z",
        compositionPart =
          LimitedLinking {linking = "D", limits = (1, Nothing)},
        compositionWhole =
          LimitedLinking {linking = "A", limits = (0, Just 1)}
        },
      Right Composition {
        compositionName = "w",
        compositionPart =
          LimitedLinking {linking = "C", limits = (1, Just 1)},
        compositionWhole =
          LimitedLinking {linking = "D", limits = (1, Just 1)}
        },
      Right Composition {
        compositionName = "x",
        compositionPart =
          LimitedLinking {linking = "A", limits = (0, Just 1)},
        compositionWhole =
          LimitedLinking {linking = "C", limits = (1, Just 1)}
        }
      ]
    },
  showExtendedFeedback = True,
  showSolution = True,
  taskText = defaultRepairCdTaskText
  }

type StructuralWeakeningSet = ChangeSet StructuralWeakening

data ChangeSet a = ChangeSet {
  illegalChange :: a,
  otherChanges :: (a, a, a, a)
  } deriving (Eq, Functor, Ord)

possibleWeakenings
  :: AllowedProperties
  -> [StructuralWeakeningSet]
possibleWeakenings allowed = nubOrdOn
  (fmap weakeningName)
  [ ChangeSet e0 cs
  | e0 <- illegalStructuralWeakenings allowed
  , l0 <- legalStructuralWeakenings allowed
  , let ls = delete l0 $ legalStructuralWeakenings allowed
  , c0 <- allStructuralWeakenings allowed
  , l1 <- if null ls then [[]] else map (\x -> [x .&. noStructuralWeakening, x]) ls
  , let weakenings = [c0, noStructuralWeakening, e0] ++ l1
  , c1 <- weakenings
  , c2 <- delete c1 weakenings
  , let cs = (l0 .&. e0, noStructuralWeakening, c1, c2)
  ]
  where
    delete x xs = M.elems . M.delete (weakeningName x) . M.fromList
      $ zip (map weakeningName xs) xs

{-|
Introduces deterministic permutations on a a list of 'StructuralWeakeningSet's.
The key point is to maintain reproducibility but achieving diversity nonetheless.
-}
diversify :: [StructuralWeakeningSet] -> [(StructuralWeakening, [StructuralWeakening])]
diversify = zipWith permutate [0..]
  where
    permutate g c =
      let (w, x, y, z) = otherChanges c
      in (illegalChange c, shuffle' [w, x, y, z] 4 $ mkStdGen g)

repairIncorrect
  :: (MonadAlloy m, MonadThrow m, RandomGen g)
  => AllowedProperties
  -> ClassConfig
  -> CdConstraints
  -> [CdMutation]
  -> ObjectProperties
  -> ArticlePreference
  -> Maybe Integer
  -> Maybe Int
  -> RandT g m (AnyCd, [CdChangeAndCd])
repairIncorrect
  cdProperties
  config
  cdConstraints
  cdMutations
  objectProperties
  preference
  maxInstances
  to
  = do
  weakeningSets <- shuffleM $ diversify $ possibleWeakenings cdProperties
  tryNextWeakeningSet weakeningSets
  where
    tryNextWeakeningSet [] = lift $ throwM NoInstanceAvailable
    tryNextWeakeningSet ((e0, structuralWeakenings) : weakeningSets) = do
      let alloyCode = Changes.transformChanges
            config
            cdConstraints
            cdMutations
            (toProperty e0)
            (Just config)
            $ map toProperty structuralWeakenings
      instances <- getInstances maxInstances to alloyCode
      randomInstances <- shuffleM instances
      getInstanceWithODs weakeningSets structuralWeakenings randomInstances
    article = toArticleToUse preference
    getInstanceWithODs weakeningSets _  [] =
      tryNextWeakeningSet weakeningSets
    getInstanceWithODs cs structuralWeakenings (alloyInstance : alloyInstances) = do
      cdInstance <- fromInstance alloyInstance >>= nameClassDiagramInstance
      (shuffledStructuralWeakenings, shuffledChangesAndCds) <-
        unzip <$> shuffleM (zip structuralWeakenings $ instanceChangesAndCds cdInstance)
      let shuffledCdInstance = cdInstance {
            instanceChangesAndCds = shuffledChangesAndCds
            }
      let cd = instanceClassDiagram shuffledCdInstance
          chs = instanceChangesAndCds shuffledCdInstance
      hints <- zipWithM getOdOrImprovedCd shuffledStructuralWeakenings chs
      case sequenceA hints of
        Nothing -> getInstanceWithODs cs structuralWeakenings alloyInstances
        Just odsAndCds -> do
          let odsAndCdWithArticle = map (first addArticle) odsAndCds
              chs' = map (uniformlyAnnotateChangeAndCd article) chs
          return (cd, zipWith InValidOption odsAndCdWithArticle chs')
    addArticle = (`Annotation` article)
    getOdOrImprovedCd structuralWeakenings change
      | isValidWeakening structuralWeakenings
      = do
        cd <- toValidCd $ changeClassDiagram change
        fmap Right <$> getOD cd
      | otherwise = fmap Left
        <$> getImprovedCd (changeClassDiagram change) (toProperty structuralWeakenings)
    getImprovedCd cd properties = do
      let alloyCode = Changes.transformImproveCd
            cd
            config
            [RemoveRelationship]
            properties
      changes <- listToMaybe <$> getInstances (Just 1) to alloyCode
      fmap (relationshipChange . head . instanceChangesAndCds)
        <$> traverse fromInstanceWithPredefinedNames changes
    getOD :: (MonadAlloy m, MonadRandom m, MonadThrow m) => Cd -> m (Maybe Od)
    getOD cd = do
      let maxNumberOfObjects = maxObjects $ snd $ classLimits config
          parts = transform
            cd
            Nothing
            []
            maxNumberOfObjects
            objectProperties
            ""
            ""
          command = createRunCommand
            "cd"
            (length $ classNames cd)
            maxNumberOfObjects
            (relationships cd)
      od <- listToMaybe
        <$> getInstances (Just 1) to (combineParts parts ++ command)
      od' <- forM od $ alloyInstanceToOd
        $ mapMaybe relationshipName $ relationships cd
      mapM (anonymiseObjects (anonymousObjectProportion objectProperties)) od'

allStructuralWeakenings :: AllowedProperties -> [StructuralWeakening]
allStructuralWeakenings c =
  legalStructuralWeakenings c ++ illegalStructuralWeakenings c

noStructuralWeakening :: StructuralWeakening
noStructuralWeakening = StructuralWeakening "none" id id

infixl 9 .&.
(.&.) :: StructuralWeakening -> StructuralWeakening -> StructuralWeakening
StructuralWeakening n1 o1 v1 .&. StructuralWeakening n2 o2 v2 =
  StructuralWeakening
  (n1 ++ " + " ++ n2)
  (o1 . o2)
  (v1 . v2)

legalStructuralWeakenings :: AllowedProperties -> [StructuralWeakening]
legalStructuralWeakenings allowed = noStructuralWeakening : [
    StructuralWeakening "add one self relationship" addSelfRelationships id
  | selfRelationships allowed] ++ [
    StructuralWeakening "force double relationships" withDoubleRelationships id
  | doubleRelationships allowed] ++ [
    StructuralWeakening "force reverse relationships" withReverseRelationships id
  | reverseRelationships allowed]
--    StructuralWeakening "force multiple inheritances" withMultipleInheritances id
  where
    addSelfRelationships :: RelationshipProperties -> RelationshipProperties
    addSelfRelationships config@RelationshipProperties {..}
      = config { selfRelationshipsAmount = selfRelationshipsAmount + 1 }
    withDoubleRelationships :: RelationshipProperties -> RelationshipProperties
    withDoubleRelationships config
      = config { hasDoubleRelationships = Just True }
    withReverseRelationships :: RelationshipProperties -> RelationshipProperties
    withReverseRelationships config
      = config { hasReverseRelationships = Just True }
    -- withMultipleInheritances :: RelationshipProperties -> RelationshipProperties
    -- withMultipleInheritances config
    --   = config { hasMultipleInheritances = True }

illegalStructuralWeakenings :: AllowedProperties -> [StructuralWeakening]
illegalStructuralWeakenings allowed = map ($ const False) $ [
    StructuralWeakening "add invalid inheritance" addInvalidInheritances
  | invalidInheritanceLimits allowed] ++ [
    StructuralWeakening "add wrong association" addWrongNonInheritances
  | wrongAssociationLimits allowed] ++ [
    StructuralWeakening "add wrong composition" addWrongCompositions
  | wrongCompositionLimits allowed] ++ [
    StructuralWeakening "force inheritance cycles" withNonTrivialInheritanceCycles
  | inheritanceCycles allowed] ++ [
    StructuralWeakening "force reverse inheritances" withReverseInheritances
  | reverseInheritances allowed] ++ [
    StructuralWeakening "add self inheritance" addSelfInheritance
  | selfInheritances allowed] ++ [
    StructuralWeakening "force composition cycles" withCompositionCycles
  | compositionCycles allowed]
  where
    addInvalidInheritances :: RelationshipProperties -> RelationshipProperties
    addInvalidInheritances config@RelationshipProperties {..}
      = config { invalidInheritances = invalidInheritances + 1 }
    addWrongNonInheritances :: RelationshipProperties -> RelationshipProperties
    addWrongNonInheritances config@RelationshipProperties {..}
      = config { wrongNonInheritances = wrongNonInheritances + 1 }
    addWrongCompositions :: RelationshipProperties -> RelationshipProperties
    addWrongCompositions config@RelationshipProperties {..}
      = config { wrongCompositions = wrongCompositions + 1 }
    addSelfInheritance :: RelationshipProperties -> RelationshipProperties
    addSelfInheritance config@RelationshipProperties {..}
      = config { selfInheritancesAmount = selfInheritancesAmount + 1 }
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
