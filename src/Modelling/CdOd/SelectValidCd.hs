{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Modelling.CdOd.SelectValidCd (
  SelectValidCdConfig (..),
  SelectValidCdInstance (..),
  SelectValidCdTaskTextElement (..),
  checkSelectValidCdConfig,
  checkSelectValidCdInstance,
  defaultSelectValidCdConfig,
  defaultSelectValidCdInstance,
  selectValidCd,
  selectValidCdEvaluation,
  selectValidCdSolution,
  selectValidCdSyntax,
  selectValidCdTask,
  ) where

import qualified Data.Bimap                       as BM (fromList)
import qualified Data.Map                         as M (
  elems,
  filter,
  foldrWithKey,
  fromAscList,
  fromList,
  insert,
  keys,
  toList,
  traverseWithKey,
  )

import Capabilities.Alloy               (MonadAlloy)
import Capabilities.Cache               (MonadCache)
import Capabilities.Diagrams            (MonadDiagrams)
import Capabilities.Graphviz            (MonadGraphviz)
import Modelling.Auxiliary.Common (
  ModellingTasksException (NeverHappens),
  Randomise (randomise),
  RandomiseLayout (randomiseLayout),
  shuffleEverything,
  )
import Modelling.Auxiliary.Output (
  addPretext,
  checkTaskText,
  hoveringInformation,
  simplifiedInformation,
  uniform,
  extra,
  )
import Modelling.CdOd.CdAndChanges.Instance (
  AnnotatedChangeAndCd (..),
  )
import Modelling.CdOd.Phrasing (
  phraseRelationship,
  trailingCommaGerman,
  )
import Modelling.CdOd.RepairCd (
  InValidOption (..),
  RelationshipChangeWithArticle,
  checkClassConfigAndChanges,
  mapInValidOption,
  mapInValidOptionM,
  repairIncorrect,
  )
import Modelling.CdOd.Output            (cacheCd, cacheOd)
import Modelling.CdOd.Types (
  AllowedProperties (..),
  Annotation (..),
  AnyCd,
  AnyClassDiagram (..),
  ArticlePreference (..),
  CdConstraints (..),
  CdDrawSettings (..),
  CdMutation,
  ClassConfig (..),
  InvalidRelationship (..),
  Object (..),
  ObjectDiagram (..),
  ObjectProperties (..),
  Od,
  OmittedDefaultMultiplicities (..),
  PhrasingKind (Denoted),
  Relationship (..),
  allowNothing,
  allCdMutations,
  anyAssociationNames,
  anyRelationshipName,
  checkCdConstraints,
  checkCdDrawSettings,
  checkCdMutations,
  checkObjectProperties,
  defaultCdConstraints,
  defaultCdDrawSettings,
  linkNames,
  shuffleAnyClassAndConnectionOrder,
  renameClassesAndRelationships,
  renameObjectsWithClassesAndLinksInOd,
  shuffleObjectAndLinkOrder,
  )
import Modelling.Types                  (Change (..))

import Control.Applicative              (Alternative ((<|>)))
import Control.Functor.Trans            (FunctorTrans (lift))
import Control.Monad                    ((>=>), unless, void, when)
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
import Control.Monad.Random             (evalRandT, mkStdGen)
import Control.Monad.Random.Class       (MonadRandom)
import Data.Bitraversable               (bimapM)
import Data.Containers.ListUtils        (nubOrd)
import Data.Either                      (isRight, partitionEithers)
import Data.GraphViz                    (DirType (Forward, NoDir))
import Data.List                        (singleton)
import Data.Map                         (Map)
import Data.Maybe                       (mapMaybe)
import Data.Ratio                       ((%))
import Data.String.Interpolate          (i, iii)
import GHC.Generics                     (Generic)
import System.Random.Shuffle            (shuffleM)

data SelectValidCdConfig
  = SelectValidCdConfig {
    allowedCdMutations :: ![CdMutation],
    allowedProperties :: AllowedProperties,
    -- | the preferred article to use when referring to relationships
    articleToUse      :: ArticlePreference,
    cdConstraints :: CdConstraints,
    classConfig      :: ClassConfig,
    drawSettings     :: !CdDrawSettings,
    maxInstances     :: Maybe Integer,
    objectProperties :: ObjectProperties,
    -- | when enabled feedback for wrong answers will be shown
    -- this might include ODs
    printExtendedFeedback :: Bool,
    printSolution    :: Bool,
    shuffleEachCd    :: Bool,
    timeout          :: Maybe Int,
    extraText        :: Maybe (Map Language String)
  } deriving (Generic, Read, Show)

defaultSelectValidCdConfig :: SelectValidCdConfig
defaultSelectValidCdConfig
  = SelectValidCdConfig {
    allowedCdMutations = allCdMutations,
    allowedProperties = allowNothing {
      inheritanceCycles = True,
      reverseInheritances = True
      },
    articleToUse = UseDefiniteArticleWherePossible,
    cdConstraints = defaultCdConstraints,
    classConfig = ClassConfig {
        classLimits        = (4, 4),
        aggregationLimits  = (0, Just 0),
        associationLimits  = (0, Just 0),
        compositionLimits  = (0, Just 0),
        inheritanceLimits  = (2, Just 4),
        relationshipLimits = (2, Just 4)
      },
    drawSettings = defaultCdDrawSettings,
    maxInstances     = Just 200,
    objectProperties = ObjectProperties {
      anonymousObjectProportion = 0 % 1,
      completelyInhabited = Just True,
      hasLimitedIsolatedObjects = False,
      hasSelfLoops = Nothing,
      usesEveryRelationshipName = Just True
      },
    printExtendedFeedback = True,
    printSolution    = True,
    shuffleEachCd    = False,
    timeout          = Nothing,
    extraText        = Nothing
  }

checkSelectValidCdConfig :: SelectValidCdConfig -> Maybe String
checkSelectValidCdConfig SelectValidCdConfig {..}
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

type CdChange = InValidOption
  AnyCd
  RelationshipChangeWithArticle
  Od

data SelectValidCdInstance
  = SelectValidCdInstance {
    cdDrawSettings  :: !CdDrawSettings,
    classDiagrams   :: Map Int CdChange,
    -- | when enabled feedback for wrong answers will be shown
    -- this might include ODs
    showExtendedFeedback :: Bool,
    showSolution    :: !Bool,
    taskText        :: !SelectValidCdTaskText,
    addText         :: Maybe (Map Language String)
  } deriving (Eq, Generic, Read, Show)

checkSelectValidCdInstance :: SelectValidCdInstance -> Maybe String
checkSelectValidCdInstance SelectValidCdInstance {..}
  | showExtendedFeedback && not showSolution
  = Just [iii|
      showExtendedFeedback leaks the correct solution
      and thus can only be enabled when showSolution is set to True
      |]
  | otherwise
  = checkTaskText taskText
  <|> checkCdDrawSettings cdDrawSettings

selectValidCdSyntax
  :: OutputCapable m
  => SelectValidCdInstance
  -> [Int]
  -> LangM m
selectValidCdSyntax inst =
  multipleChoiceSyntax False (M.keys $ classDiagrams inst)

type SelectValidCdTaskText = [SpecialOutput SelectValidCdTaskTextElement]

data SelectValidCdTaskTextElement
  = CdCandidates
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

selectValidCdTask
  :: (MonadCache m, MonadDiagrams m, MonadGraphviz m, OutputCapable m)
  => FilePath
  -> SelectValidCdInstance
  -> LangM m
selectValidCdTask path task = do
  toTaskText path task
  paragraph simplifiedInformation
  paragraph hoveringInformation
  pure ()

toTaskText
  :: (MonadCache m, MonadDiagrams m, MonadGraphviz m, OutputCapable m)
  => FilePath
  -> SelectValidCdInstance
  -> LangM m
toTaskText path task = do
  specialToOutputCapable (toTaskSpecificText path task) (taskText task)
  extra $ addText task
  pure ()

toTaskSpecificText
  :: (MonadCache m, MonadDiagrams m, MonadGraphviz m, OutputCapable m)
  => FilePath
  -> SelectValidCdInstance
  -> SelectValidCdTaskTextElement
  -> LangM m
toTaskSpecificText path SelectValidCdInstance {..} = \case
  CdCandidates -> images show snd $=<< sequence
    $ M.foldrWithKey drawCd mempty classDiagrams
  where
    drawCd x theChange cds =
      let f = cacheCd
            cdDrawSettings
            mempty
            (option theChange)
            path
      in M.insert x ((isRight $ hint theChange,) <$> f) cds

defaultSelectValidCdTaskText :: SelectValidCdTaskText
defaultSelectValidCdTaskText = [
  Paragraph $ singleton $ Translated $ translations $ do
    english [i|Consider the following class diagram candidates:|]
    german [i|Betrachten Sie die folgenden Klassendiagrammkandidaten:|],
  Special CdCandidates,
  Paragraph $ singleton $ Translated $ translations $ do
    english [i|Which of these class diagram candidates are valid class diagrams?
Please state your answer by giving a list of numbers, indicating all valid class diagrams.|]
    german [i|Welche dieser Klassendiagrammkandidaten sind valide Klassendiagramme?
Bitte geben Sie Ihre Antwort in Form einer Liste von Zahlen an, die alle gültigen Klassendiagramme enthält.|],
  Paragraph [
    Translated $ translations $ do
      english [i|For example,|]
      german [i|Zum Beispiel würde|],
    Code $ uniform "[1, 2]",
    Translated $ translations $ do
      english [i|would indicate that only class diagram candidates 1 and 2 of the given ones are valid class diagrams.|]
      german [i|bedeuten, dass nur die Klassendiagrammkandidaten 1 und 2 der angegebenen Klassendiagrammkandidaten gültige Klassendiagramme sind.|]
    ]
  ]

selectValidCdEvaluation
  :: (
    Alternative m,
    MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    MonadThrow m,
    OutputCapable m
    )
  => FilePath
  -> SelectValidCdInstance
  -> [Int]
  -> Rated m
selectValidCdEvaluation path inst@SelectValidCdInstance{..} xs = addPretext $ do
  let cds = M.fromList [
        (English, "class diagrams"),
        (German, "Klassendiagramme")
        ]
      solution = isRight . hint <$> classDiagrams
      correctAnswer
        | showSolution = Just $ show $ selectValidCdSolution inst
        | otherwise = Nothing
  reRefuse (multipleChoice DefiniteArticle cds correctAnswer solution xs)
    $ when showExtendedFeedback
    $ void $ M.traverseWithKey
      (selectValidCdFeedback path cdDrawSettings xs)
      classDiagrams

selectValidCdFeedback
  :: (MonadCache m, MonadDiagrams m, MonadGraphviz m, MonadThrow m, OutputCapable m)
  => FilePath
  -> CdDrawSettings
  -> [Int]
  -> Int
  -> CdChange
  -> LangM m
selectValidCdFeedback path drawSettings xs x cdChange =
  case hint cdChange of
    Left articleAndChange | x `elem` xs -> do
      let change = annotated articleAndChange
          article = annotation articleAndChange
      notCorrect
      paragraph $ translate $ do
        english [iii|
          Class diagram #{x} is invalid.
          |]
        german [iii|
          Klassendiagramm #{x} ist ungültig.
          |]
      let sufficient = byName || maybe True isInheritance (remove change)
      unless sufficient showNamedCd
      paragraph $ case remove change of
        Nothing -> lift $ throwM NeverHappens
        Just relation -> translate $ do
          let phrase l = phraseRelationship
                l
                (omittedDefaults drawSettings)
                article
                Denoted
                True
                withDir
                relation
          english [iii|
            If for example #{phrase English} would not be there,
            it would be valid.
            |]
          german [iii|
            Wenn es zum Beispiel
            #{trailingCommaGerman $ phrase German}
            nicht gäbe, wäre es gültig.
            |]
      pure ()
    Right od | x `notElem` xs -> do
      notCorrect
      paragraph $ translate $ do
        english [iii|
          Class diagram #{x} is valid.
          |]
        german [iii|
          Klassendiagramm #{x} ist gültig.
          |]
      let sufficient = byName || onlyInheritances (option cdChange)
      unless sufficient showNamedCd
      paragraph $ translate $ do
        english [iii|
          #{if sufficient then "Consider" else "Now consider"} the following object diagram, which is an instance of this
          class diagram:
          |]
        german [iii|
          #{if sufficient then "Betrachten Sie" else "Betrachten Sie nun"} das folgende Objektdiagramm,
          welches eine Instanz dieses Klassendiagramms ist:
          |]
      paragraph $ image $=<< cacheOd od dir True path
      pure ()
    _ -> pure ()
  where
    byName = printNames drawSettings
    withDir = printNavigations drawSettings
    dir
      | withDir = Forward
      | otherwise = NoDir
    notCorrect = paragraph $ translate $ do
      english [iii|Your answer to class diagram #{x} is not correct.|]
      german [iii|Ihre Antwort zu Klassendiagramm #{x} ist nicht richtig.|]
    isInheritance = \case
      Right Inheritance {} -> True
      Right {} -> False
      Left InvalidInheritance {} -> True
    onlyInheritances = all isInheritance . anyRelationships
    showNamedCd = do
        paragraph $ translate $ do
          english [iii|
            The relationships in the class diagram could be named in the following way:
            |]
          german [iii|
            Die Beziehungen in dem Klassendiagramm könnten auf folgende Weise
            mit Namen versehen werden:
            |]
        let withNames = drawSettings {printNames = True}
        paragraph $ image $=<< cacheCd withNames mempty (option cdChange) path
        pure ()

selectValidCdSolution :: SelectValidCdInstance -> [Int]
selectValidCdSolution =
  M.keys . M.filter id . fmap (isRight . hint) . classDiagrams

selectValidCd
  :: (MonadAlloy m, MonadThrow m)
  => SelectValidCdConfig
  -> Int
  -> Int
  -> m SelectValidCdInstance
selectValidCd SelectValidCdConfig {..} segment seed = flip evalRandT g $ do
  (_, chs)  <- repairIncorrect
    allowedProperties
    classConfig
    cdConstraints
    allowedCdMutations
    objectProperties
    articleToUse
    maxInstances
    timeout
  let cds = map (mapInValidOption annotatedChangeClassDiagram id id) chs
  shuffleCds >=> shuffleEverything $ SelectValidCdInstance {
    cdDrawSettings  = drawSettings,
    classDiagrams   = M.fromAscList $ zip [1 ..] cds,
    showExtendedFeedback = printExtendedFeedback,
    showSolution    = printSolution,
    taskText        = defaultSelectValidCdTaskText,
    addText          = extraText
    }
  where
    g = mkStdGen $ (segment +) $ 4 * seed
    shuffleCds
      | shuffleEachCd        = shuffleEach
      | otherwise            = return

instance Randomise SelectValidCdInstance where
  randomise inst = do
    let (names, nonInheritances) = classAndNonInheritanceNames inst
    names' <- shuffleM names
    nonInheritances' <- shuffleM nonInheritances
    renameInstance inst names' nonInheritances'
      >>= shuffleInstance

instance RandomiseLayout SelectValidCdInstance where
  randomiseLayout SelectValidCdInstance {..} = do
    cds <- mapInValidOptionM
      shuffleAnyClassAndConnectionOrder
      pure
      shuffleObjectAndLinkOrder
      `mapM` classDiagrams
    return $ SelectValidCdInstance {
      cdDrawSettings          = cdDrawSettings,
      classDiagrams           = cds,
      showExtendedFeedback    = showExtendedFeedback,
      showSolution            = showSolution,
      taskText                = taskText,
      addText                 = addText
      }

shuffleEach
  :: (MonadRandom m, MonadThrow m)
  => SelectValidCdInstance
  -> m SelectValidCdInstance
shuffleEach inst@SelectValidCdInstance {..} = do
  cds <- shuffleCdChange inst `mapM` classDiagrams
  return $ SelectValidCdInstance {
    cdDrawSettings          = cdDrawSettings,
    classDiagrams           = cds,
    showExtendedFeedback    = showExtendedFeedback,
    showSolution            = showSolution,
    taskText                = taskText,
    addText                 = addText
    }

shuffleCdChange
  :: (MonadRandom m, MonadThrow m)
  => SelectValidCdInstance
  -> CdChange
  -> m CdChange
shuffleCdChange inst x = do
  names' <- shuffleM names
  nonInheritances' <- shuffleM nonInheritances
  let bmNames  = BM.fromList $ zip names names'
      bmNonInheritances = BM.fromList $ zip nonInheritances nonInheritances'
      renameCd = renameClassesAndRelationships bmNames bmNonInheritances
      renameOd = renameObjectsWithClassesAndLinksInOd bmNames bmNonInheritances
      renameEdge = renameClassesAndRelationships bmNames bmNonInheritances
      renameEdge' = renameClassesAndRelationships bmNames bmNonInheritances
  mapInValidOptionM
    renameCd
    (mapM $ mapM $ bimapM renameEdge renameEdge')
    renameOd
    x
  where
    (names, nonInheritances) = classAndNonInheritanceNames inst

shuffleInstance
  :: MonadRandom m
  => SelectValidCdInstance
  -> m SelectValidCdInstance
shuffleInstance SelectValidCdInstance {..} =
  (SelectValidCdInstance cdDrawSettings
    . M.fromAscList . zipWith replaceId [1..]
       <$> shuffleM (M.toList classDiagrams))
  <*> pure showExtendedFeedback
  <*> pure showSolution
  <*> pure taskText
  <*> pure addText
  where
    replaceId x (_, cd) = (x, cd)

classAndNonInheritanceNames :: SelectValidCdInstance -> ([String], [String])
classAndNonInheritanceNames inst =
  let cds = classDiagrams inst
      (improves, evidences) = partitionEithers $ map hint $ M.elems cds
      names = nubOrd $ concatMap (anyClassNames . option) cds
      nonInheritances = nubOrd $ concatMap (anyAssociationNames . option) cds
        ++ mapMaybe (add . annotated >=> anyRelationshipName) improves
        ++ mapMaybe (remove . annotated >=> anyRelationshipName) improves
        ++ concatMap linkNames evidences
  in (names, nonInheritances)

renameInstance
  :: MonadThrow m
  => SelectValidCdInstance
  -> [String]
  -> [String]
  -> m SelectValidCdInstance
renameInstance inst@SelectValidCdInstance {..} names' nonInheritances' = do
  let (names, nonInheritances) = classAndNonInheritanceNames inst
      bmNames  = BM.fromList $ zip names names'
      bmNonInheritances = BM.fromList $ zip nonInheritances nonInheritances'
      renameCd = renameClassesAndRelationships bmNames bmNonInheritances
      renameEdge = renameClassesAndRelationships bmNames bmNonInheritances
      renameEdge' = renameClassesAndRelationships bmNames bmNonInheritances
      renameOd = renameObjectsWithClassesAndLinksInOd bmNames bmNonInheritances
  cds <- mapM
    (mapInValidOptionM renameCd (mapM $ mapM $ bimapM renameEdge renameEdge') renameOd)
    classDiagrams
  return $ SelectValidCdInstance {
    cdDrawSettings  = cdDrawSettings,
    classDiagrams   = cds,
    showExtendedFeedback = showExtendedFeedback,
    showSolution    = showSolution,
    taskText        = taskText,
    addText         = addText
    }

defaultSelectValidCdInstance :: SelectValidCdInstance
defaultSelectValidCdInstance = SelectValidCdInstance {
  cdDrawSettings = CdDrawSettings {
    omittedDefaults = OmittedDefaultMultiplicities {
      aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
      associationOmittedDefaultMultiplicity = Just (0, Nothing),
      compositionWholeOmittedDefaultMultiplicity = Just (1, Just 1)
      },
    printNames = True,
    printNavigations = True
    },
  classDiagrams = M.fromList [
    (1, InValidOption {
      hint = Left (Annotation {
        annotated = Change {
          add = Nothing,
          remove = Just (Right Inheritance {
            subClass = "C",
            superClass = "A"
            })
          },
        annotation = DefiniteArticle
        }),
      option = AnyClassDiagram {
        anyClassNames = ["B", "C", "D", "A"],
        anyRelationships = [
          Right Inheritance {subClass = "D", superClass = "C"},
          Right Inheritance {subClass = "C", superClass = "A"},
          Right Inheritance {subClass = "B", superClass = "D"},
          Right Inheritance {subClass = "A", superClass = "C"}
          ]
        }
      }),
    (2, InValidOption {
      hint = Right ObjectDiagram {
        objects = [
          Object {isAnonymous = False, objectName = "b", objectClass = "B"},
          Object {isAnonymous = False, objectName = "c", objectClass = "C"},
          Object {isAnonymous = False, objectName = "d", objectClass = "D"},
          Object {isAnonymous = False, objectName = "a", objectClass = "A"}
          ],
        links = []
        },
      option = AnyClassDiagram {
        anyClassNames = ["B", "A", "D", "C"],
        anyRelationships = [
          Right Inheritance {subClass = "C", superClass = "A"},
          Right Inheritance {subClass = "D", superClass = "C"}
          ]
        }
      }),
    (3, InValidOption {
      hint = Left (Annotation {
        annotated = Change {
          add = Nothing,
          remove = Just (Right Inheritance {
            subClass = "C",
            superClass = "A"
            })
          },
        annotation = DefiniteArticle
        }),
      option = AnyClassDiagram {
        anyClassNames = ["B", "D", "C", "A"],
        anyRelationships = [
          Right Inheritance {subClass = "B", superClass = "A"},
          Right Inheritance {subClass = "A", superClass = "C"},
          Right Inheritance {subClass = "D", superClass = "C"},
          Right Inheritance {subClass = "C", superClass = "A"}
          ]
        }
      }),
    (4, InValidOption {
      hint = Right ObjectDiagram {
        objects = [
          Object {isAnonymous = False, objectName = "d", objectClass = "D"},
          Object {isAnonymous = False, objectName = "a", objectClass = "A"},
          Object {isAnonymous = False, objectName = "b", objectClass = "B"},
          Object {isAnonymous = False, objectName = "c", objectClass = "C"}
          ],
        links = []
        },
      option = AnyClassDiagram {
        anyClassNames = ["D", "C", "B", "A"],
        anyRelationships = [
          Right Inheritance {subClass = "A", superClass = "C"},
          Right Inheritance {subClass = "D", superClass = "C"}
          ]
        }
      })
    ],
  showExtendedFeedback = True,
  showSolution = True,
  taskText = defaultSelectValidCdTaskText,
  addText = Nothing
  }
