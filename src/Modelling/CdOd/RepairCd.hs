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
  WeakeningKind (..),
  checkClassConfigAndChanges,
  checkRepairCdConfig,
  checkRepairCdInstance,
  classAndNonInheritanceNames,
  defaultRepairCdConfig,
  defaultRepairCdInstance,
  generateSetOfCds,
  mapInValidOption,
  mapInValidOptionM,
  renameInstance,
  repairCd,
  repairCdEvaluation,
  repairCdSolution,
  repairCdSyntax,
  repairCdTask,
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
  RandomiseNames (randomiseNames),
  TaskGenerationException (NoInstanceAvailable),
  )
import Modelling.Auxiliary.Output (
  addPretext,
  checkTaskText,
  hoveringInformation,
  simplifiedInformation,
  uniform, extra,
  )
import Modelling.Auxiliary.Shuffle.All  (shuffleEverything)
import Modelling.CdOd.Auxiliary.Util    (alloyInstanceToOd)
import Modelling.CdOd.CD2Alloy.Transform (
  ExtendsAnd (FieldPlacement),
  LinguisticReuse (ExtendsAnd),
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
  CdMutation (AddRelationship, RemoveRelationship),
  ClassConfig (..),
  ClassDiagram (..),
  LimitedLinking (..),
  ObjectProperties (..),
  Od,
  OmittedDefaultMultiplicities (..),
  Relationship (..),
  RelationshipProperties (..),
  allowNothing,
  anonymiseObjects,
  anyAssociationNames,
  anyRelationshipName,
  checkCdConstraints,
  checkCdDrawSettings,
  checkCdMutations,
  checkClassConfig,
  checkClassConfigAndObjectProperties,
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
import Control.Monad.Catch              (MonadCatch, MonadThrow (throwM))
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
import Data.Function                    (on)
import Data.List                        (deleteBy, singleton)
import Data.List.Extra                  (sortOn)
import Data.Map                         (Map)
import Data.Maybe                       (catMaybes, listToMaybe, mapMaybe)
import Data.Ratio                       ((%))
import Data.String.Interpolate          (i, iii)
import GHC.Generics                     (Generic)
import System.Random.Shuffle            (shuffle', shuffleM)

data StructuralWeakening = StructuralWeakening {
    weakeningName  :: ![Weakening],
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
    useNames         :: Bool,
    extraText        :: Maybe (Map Language String)
  } deriving (Generic, Read, Show)

defaultRepairCdConfig :: RepairCdConfig
defaultRepairCdConfig
  = RepairCdConfig {
    allowedCdMutations = [AddRelationship, RemoveRelationship],
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
    maxInstances     = Just 5,
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
    useNames         = True,
    extraText        = Nothing
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
  <|> checkClassConfigAndObjectProperties classConfig objectProperties

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
  extra $ addText task
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
      $ map (second (phrase byName (printNavigations cdDrawSettings) . option))
      $ M.toList changes
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
    taskText       :: !RepairCdTaskText,
    addText        :: Maybe (Map Language String)
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
  randomise = shuffleInstance

instance RandomiseNames RepairCdInstance where
  randomiseNames inst = do
    let (names, nonInheritances) = classAndNonInheritanceNames inst
    names' <- shuffleM names
    nonInheritances' <- shuffleM nonInheritances
    renameInstance inst names' nonInheritances'

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
      taskText = taskText,
      addText = addText
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
    taskText = taskText,
    addText = addText
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
    taskText       = taskText,
    addText        = addText
    }

repairCd
  :: (MonadAlloy m, MonadCatch m)
  => RepairCdConfig
  -> Int
  -> Int
  -> m RepairCdInstance
repairCd RepairCdConfig {..} segment seed = flip evalRandT g $ do
  (cd, chs) <- generateSetOfCds
    IllegalStructuralWeakening
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
    taskText = defaultRepairCdTaskText,
    addText = extraText
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
      hint = Left AnyClassDiagram {
        anyClassNames = ["D", "C", "A", "B"],
        anyRelationships = [
          Right Composition {
            compositionName = "z",
            compositionPart =
              LimitedLinking {linking = "A", limits = (0, Just 2)},
            compositionWhole =
              LimitedLinking {linking = "C", limits = (1, Just 1)}
            },
          Right Composition {
            compositionName = "v",
            compositionPart =
              LimitedLinking {linking = "D", limits = (1, Just 2)},
            compositionWhole =
              LimitedLinking {linking = "A", limits = (0, Just 1)}
            },
          Right Composition {
            compositionName = "w",
            compositionPart =
              LimitedLinking {linking = "C", limits = (0, Nothing)},
            compositionWhole =
              LimitedLinking {linking = "D", limits = (1, Just 1)}
            },
          Right Association {
            associationName = "x",
            associationFrom =
              LimitedLinking {linking = "B", limits = (2, Nothing)},
            associationTo =
              LimitedLinking {linking = "A", limits = (2, Nothing)}
            }
          ]
        },
      option = Annotation {
        annotated = Change {
          add = Just $ Right Association {
            associationName = "x",
            associationFrom =
              LimitedLinking {linking = "B", limits = (2, Nothing)},
            associationTo =
              LimitedLinking {linking = "A", limits = (2, Nothing)}
            },
          remove = Nothing
          },
        annotation = DefiniteArticle
        }
      }),
    (2, InValidOption {
      hint = Left AnyClassDiagram {
        anyClassNames = ["C", "D", "B", "A"],
        anyRelationships = [
          Right Association {
            associationName = "y",
            associationFrom =
              LimitedLinking {linking = "A", limits = (2, Nothing)},
            associationTo =
              LimitedLinking {linking = "B", limits = (2, Nothing)}
            },
          Right Composition {
            compositionName = "z",
            compositionPart =
              LimitedLinking {linking = "A", limits = (0, Just 2)},
            compositionWhole =
              LimitedLinking {linking = "C", limits = (1, Just 1)}
            },
          Right Composition {
            compositionName = "w",
            compositionPart =
              LimitedLinking {linking = "C", limits = (0, Nothing)},
            compositionWhole =
              LimitedLinking {linking = "D", limits = (1, Just 1)}
            },
          Right Composition {
            compositionName = "v",
            compositionPart =
              LimitedLinking {linking = "D", limits = (1, Just 2)},
            compositionWhole =
              LimitedLinking {linking = "A", limits = (0, Just 1)}
            }
          ]
        },
      option = Annotation {
        annotated = Change {
          add = Just $ Right Association {
            associationName = "y",
            associationFrom =
              LimitedLinking {linking = "A", limits = (2, Nothing)},
            associationTo =
              LimitedLinking {linking = "B", limits = (2, Nothing)}
            },
          remove = Nothing
          },
        annotation = DefiniteArticle
        }
      }),
    (3, InValidOption {
      hint = Right ClassDiagram {
        classNames = ["D", "A", "B", "C"],
        relationships = [
          Composition {
            compositionName = "z",
            compositionPart =
              LimitedLinking {linking = "A", limits = (0, Just 2)},
            compositionWhole =
              LimitedLinking {linking = "C", limits = (1, Just 1)}
            },
          Composition {
            compositionName = "w",
            compositionPart =
              LimitedLinking {linking = "C", limits = (0, Nothing)},
            compositionWhole =
              LimitedLinking {linking = "D", limits = (1, Just 1)}
            }
          ]
        },
      option = Annotation {
        annotated = Change {
          add = Nothing,
          remove = Just $ Right Composition {
            compositionName = "v",
            compositionPart =
              LimitedLinking {linking = "D", limits = (1, Just 2)},
            compositionWhole =
              LimitedLinking {linking = "A", limits = (0, Just 1)}
            }
          },
        annotation = DefiniteArticle
        }
      }),
    (4, InValidOption {
      hint = Right ClassDiagram {
        classNames = ["C", "A", "D", "B"],
        relationships = [
          Composition {
            compositionName = "v",
            compositionPart =
              LimitedLinking {linking = "D", limits = (1, Just 2)},
            compositionWhole =
              LimitedLinking {linking = "A", limits = (0, Just 1)}
            },
          Composition {
            compositionName = "z",
            compositionPart =
              LimitedLinking {linking = "A", limits = (0, Just 2)},
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
              LimitedLinking {linking = "C", limits = (0, Nothing)},
            compositionWhole =
              LimitedLinking {linking = "D", limits = (1, Just 1)}
            }
          },
        annotation = DefiniteArticle
        }
      })
    ],
  classDiagram = AnyClassDiagram {
    anyClassNames = ["C", "D", "B", "A"],
    anyRelationships = [
      Right Composition {
        compositionName = "w",
        compositionPart =
          LimitedLinking {linking = "C", limits = (0, Nothing)},
        compositionWhole =
          LimitedLinking {linking = "D", limits = (1, Just 1)}
        },
      Right Composition {
        compositionName = "v",
        compositionPart =
          LimitedLinking {linking = "D", limits = (1, Just 2)},
        compositionWhole =
          LimitedLinking {linking = "A", limits = (0, Just 1)}
        },
      Right Composition {
        compositionName = "z",
        compositionPart =
          LimitedLinking {linking = "A", limits = (0, Just 2)},
        compositionWhole =
          LimitedLinking {linking = "C", limits = (1, Just 1)}
        }
      ]
    },
  showExtendedFeedback = True,
  showSolution = True,
  taskText = defaultRepairCdTaskText,
  addText = Nothing
  }

type StructuralWeakeningSet = WeakeningSet StructuralWeakening

data WeakeningSet a = WeakeningSet {
  initialWeakening :: !a,
  otherWeakenings :: ![a]
  } deriving (Eq, Functor, Ord)

{-|
Creates all sets of structural weakenings (considering the parameters),
each in the following way:

1. create possible structural weakenings
    1. select one illegal structural weakening (\(iw$\))
    2. select two legal structural weakenings (\(l_1, l_2$\))
    3. select one illegal or legal structural weakening (\(sw$\))
    4. set no structural weakening (\(none$\))
2. choose randomly two of \(sw, none, iw, l_2, l_2$\) (as \(w1, w2$\))
3. define the weakening set as
    - 'initialWeakening' set to either \(l_1$\) or \(iw$\)
    - 'otherWeakenings' set to: \(l_1 . iw, none, w1, w2$\)
-}
possibleWeakenings
  :: WeakeningKind
  -- ^ kind to choose for 'initialWeakening'
  -> AllowedProperties
  -- ^ properties to be considered for weakenings
  -> [StructuralWeakeningSet]
possibleWeakenings basis allowed = nubOrdOn
  (fmap weakeningName)
  [ WeakeningSet {
      initialWeakening = initial,
      otherWeakenings =
        sortOn weakeningName [other .&. initial, noStructuralWeakening, w1, w2]
      }
  | iw <- illegalStructuralWeakenings allowed
  , l1 <- legalStructuralWeakenings allowed
  , (initial, other) <- case basis of
      AnyStructuralWeakening -> [(l1, iw), (iw, l1)]
      IllegalStructuralWeakening -> [(iw, l1) ]
      LegalStructuralWeakening -> [(l1, iw)]
  , l2 <- legalStructuralWeakenings allowed
  , w <- allStructuralWeakenings allowed
  , let weakenings = [w, noStructuralWeakening, iw, l2, l2]
  , w1 <- weakenings
  , w2 <- deleteBy ((==) `on` weakeningName) w1 weakenings
  ]

{-|
Introduces deterministic permutations on a a list of 'StructuralWeakeningSet's.
The key point is to maintain reproducibility but achieving diversity nonetheless.
-}
diversify :: [StructuralWeakeningSet] -> [StructuralWeakeningSet]
diversify = zipWith permutate [0..]
  where
    permutate g c = c {
      otherWeakenings = shuffle' (otherWeakenings c) 4 $ mkStdGen g
      }

{-|
This datatype specifies what kind of structural weakening to choose.
-}
data WeakeningKind
  = AnyStructuralWeakening
  -- ^ a weakening resulting in a valid or an invalid class diagram candidate
  | IllegalStructuralWeakening
  -- ^ a weakening resulting in an invalid class diagram candidate
  | LegalStructuralWeakening
  -- ^ a weakening resulting in a valid class diagram
  deriving (Generic, Read, Show)

{-|
Generate one base class diagram candidate and four (one step) changes,
resulting in four class diagram candidates based on the base candidate
together with changes to repair invalid class diagram candidates
and object diagrams witnessing correct class diagrams.
-}
generateSetOfCds
  :: (MonadAlloy m, MonadCatch m, RandomGen g)
  => WeakeningKind
  -- ^ to be used for the base class diagram
  -> AllowedProperties
  -- ^ potentially to be chosen for all class diagrams
  -> ClassConfig
  -- ^ to adhere to by all class diagrams
  -> CdConstraints
  -- ^ also for all class diagrams
  -> [CdMutation]
  -- ^ possible mutations to choose from in order to use for the changes
  -- (and thus use to derive the four different class diagram candidate)
  -> ObjectProperties
  -- ^ properties all object diagram witnesses need to satisfy
  -> ArticlePreference
  -- ^ how to refer to relationships
  -> Maybe Integer
  -- ^ number of instances to generate for both,
  -- class diagrams and object diagrams
  -> Maybe Int
  -- ^ when to abort any Alloy call early,
  -- destroys reproducibility when set (to 'Just')
  -> RandT g m (AnyCd, [CdChangeAndCd])
generateSetOfCds
  basisCd
  cdProperties
  config
  cdConstraints
  cdMutations
  objectProperties
  preference
  maxInstances
  to
  = do
  weakeningSets <- shuffleM $ diversify
    $ possibleWeakenings basisCd cdProperties
  tryNextWeakeningSet weakeningSets
  where
    tryNextWeakeningSet [] = lift $ throwM NoInstanceAvailable
    tryNextWeakeningSet (WeakeningSet {..} : weakeningSets) = do
      let alloyCode = Changes.transformChanges
            config
            cdConstraints
            cdMutations
            (toProperty initialWeakening)
            (Just config)
            $ map toProperty otherWeakenings
      instances <- getInstances maxInstances to alloyCode
      randomInstances <- shuffleM instances
      getInstanceWithODs weakeningSets otherWeakenings randomInstances
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
    getOD :: (MonadAlloy m, MonadCatch m, MonadRandom m) => Cd -> m (Maybe Od)
    getOD cd = do
      let maxNumberOfObjects = maxObjects $ snd $ classLimits config
          parts = transform
            (ExtendsAnd FieldPlacement)
            cd
            Nothing
            []
            maxNumberOfObjects
            objectProperties
            ""
            ""
          command = createRunCommand
            "cd"
            (Just $ classNames cd)
            (length $ classNames cd)
            maxNumberOfObjects
            (relationships cd)
      od <- listToMaybe
        <$> getInstances (Just 1) to (combineParts parts ++ command)
      od' <- forM od $ alloyInstanceToOd
        (Just $ classNames cd)
        $ mapMaybe relationshipName $ relationships cd
      mapM (anonymiseObjects (anonymousObjectProportion objectProperties)) od'

allStructuralWeakenings :: AllowedProperties -> [StructuralWeakening]
allStructuralWeakenings c =
  legalStructuralWeakenings c ++ illegalStructuralWeakenings c

data Weakening
  = None
  | Add !AdditiveWeakening
  | Force !ForcibleWeakening
  deriving (Eq, Ord, Show)

data AdditiveWeakening
  = InvalidInheritance
  | SelfInheritance
  | SelfRelationship
  | WrongAssociation
  | WrongComposition
  deriving (Eq, Ord, Show)

data ForcibleWeakening
  = CompositionCycles
  | DoubleRelationships
  | InheritanceCycles
  | ReverseInheritances
  | ReverseRelationships
  deriving (Eq, Ord, Show)

{-|
Assumes the given lists to be ordered already.

Removes duplicates, that do not enforce new property changes.
(i.e. 'None' and 'Force')
-}
mergeWeakenings :: [Weakening] -> [Weakening] -> [Weakening]
mergeWeakenings [] ys = ys
mergeWeakenings xs [] = xs
mergeWeakenings xs@(_:_) (None:ys) = mergeWeakenings xs ys
mergeWeakenings (None:xs) ys@(_:_) = mergeWeakenings xs ys
mergeWeakenings keeps@(x@Force {}:xs) others@(y@Force {}:ys)
  | x == y = mergeWeakenings keeps ys
  | x < y = x : mergeWeakenings xs others
  | otherwise = y : mergeWeakenings keeps ys
mergeWeakenings keeps@(x:xs) others@(y:ys)
  | x <= y = x : mergeWeakenings xs others
  | otherwise = y : mergeWeakenings keeps ys

noStructuralWeakening :: StructuralWeakening
noStructuralWeakening = StructuralWeakening [None] id id

infixl 9 .&.
(.&.) :: StructuralWeakening -> StructuralWeakening -> StructuralWeakening
StructuralWeakening n1 o1 v1 .&. StructuralWeakening n2 o2 v2 =
  StructuralWeakening
  (mergeWeakenings n1 n2)
  (o1 . o2)
  (v1 . v2)

legalStructuralWeakenings :: AllowedProperties -> [StructuralWeakening]
legalStructuralWeakenings allowed = noStructuralWeakening : [
    StructuralWeakening [Add SelfRelationship] addSelfRelationships id
  | selfRelationships allowed] ++ [
    StructuralWeakening [Force DoubleRelationships] withDoubleRelationships id
  | doubleRelationships allowed] ++ [
    StructuralWeakening [Force ReverseRelationships] withReverseRelationships id
  | reverseRelationships allowed]
--    StructuralWeakening [Force MultipleInheritances] withMultipleInheritances id
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
    StructuralWeakening [Add InvalidInheritance] addInvalidInheritances
  | invalidInheritanceLimits allowed] ++ [
    StructuralWeakening [Add WrongAssociation] addWrongNonInheritances
  | wrongAssociationLimits allowed] ++ [
    StructuralWeakening [Add WrongComposition] addWrongCompositions
  | wrongCompositionLimits allowed] ++ [
    StructuralWeakening [Force InheritanceCycles] withNonTrivialInheritanceCycles
  | inheritanceCycles allowed] ++ [
    StructuralWeakening [Force ReverseInheritances] withReverseInheritances
  | reverseInheritances allowed] ++ [
    StructuralWeakening [Add SelfInheritance] addSelfInheritance
  | selfInheritances allowed] ++ [
    StructuralWeakening [Force CompositionCycles] withCompositionCycles
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
