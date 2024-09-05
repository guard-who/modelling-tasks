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
  hoveringInformation,
  reRefuse,
  simplifiedInformation,
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
  CdDrawSettings (..),
  CdMutation,
  ClassConfig (..),
  InvalidRelationship (..),
  Object (..),
  ObjectDiagram (..),
  ObjectProperties (..),
  Od,
  Relationship (..),
  allowNothing,
  allCdMutations,
  anyAssociationNames,
  anyRelationshipName,
  checkCdDrawSettings,
  checkCdMutations,
  checkObjectProperties,
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
  singleChoiceSyntax,
  translate,
  )
import Control.Monad.Random             (evalRandT, mkStdGen)
import Control.Monad.Random.Class       (MonadRandom)
import Data.Bitraversable               (bimapM)
import Data.Containers.ListUtils        (nubOrd)
import Data.Either                      (isRight, partitionEithers)
import Data.Foldable                    (for_)
import Data.GraphViz                    (DirType (Back, NoDir))
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
    classConfig      :: ClassConfig,
    drawSettings     :: !CdDrawSettings,
    maxInstances     :: Maybe Integer,
    objectProperties :: ObjectProperties,
    -- | when enabled feedback for wrong answers will be shown
    -- this might include ODs
    printExtendedFeedback :: Bool,
    printSolution    :: Bool,
    shuffleEachCd    :: Bool,
    timeout          :: Maybe Int
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
    printExtendedFeedback = False,
    printSolution    = False,
    shuffleEachCd    = False,
    timeout          = Nothing
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
    showSolution    :: Bool
  } deriving (Generic, Read, Show)

checkSelectValidCdInstance :: SelectValidCdInstance -> Maybe String
checkSelectValidCdInstance SelectValidCdInstance {..}
  | showExtendedFeedback && not showSolution
  = Just [iii|
      showExtendedFeedback leaks the correct solution
      and thus can only be enabled when showSolution is set to True
      |]
  | otherwise
  = checkCdDrawSettings cdDrawSettings

selectValidCdSyntax
  :: OutputCapable m
  => SelectValidCdInstance
  -> [Int]
  -> LangM m
selectValidCdSyntax inst xs =
  for_ xs $ singleChoiceSyntax False (M.keys $ classDiagrams inst)

selectValidCdTask
  :: (MonadCache m, MonadDiagrams m, MonadGraphviz m, OutputCapable m)
  => FilePath
  -> SelectValidCdInstance
  -> LangM m
selectValidCdTask path SelectValidCdInstance {..} = do
  paragraph $ translate $ do
    english [i|Consider the following class diagram candidates:|]
    german [i|Betrachten Sie die folgenden Klassendiagrammkandidaten:|]
  images show snd $=<< sequence $
    M.foldrWithKey drawCd mempty classDiagrams
  paragraph $ translate $ do
    english [i|Which of these class diagram candidates are valid class diagrams?
Please state your answer by giving a list of numbers, indicating all valid class diagrams.|]
    german [i|Welche dieser Klassendiagrammkandidaten sind valide Klassendiagramme?
Bitte geben Sie Ihre Antwort in Form einer Liste von Zahlen an, die alle gültigen Klassendiagramme enthält.|]
  paragraph $ do
    translate $ do
      english [i|For example,|]
      german [i|Zum Beispiel würde|]
    code "[1, 2]"
    translate $ do
      english [i|would indicate that only class diagram candidates 1 and 2 of the given ones are valid class diagrams.|]
      german [i|bedeuten, dass nur die Klassendiagrammkandidaten 1 und 2 der angegebenen Klassendiagrammkandidaten gültige Klassendiagramme sind.|]
    pure ()
  paragraph simplifiedInformation
  paragraph hoveringInformation
  pure ()
  where
    drawCd x theChange cds =
      let f = cacheCd
            cdDrawSettings
            mempty
            (option theChange)
            path
      in M.insert x ((isRight $ hint theChange,) <$> f) cds

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
          Class diagram #{x} is in fact invalid.
          |]
        german [iii|
          Klassendiagramm #{x} ist ungültig.
          |]
      showNamedCd (byName || maybe True isInheritance (remove change))
      paragraph $ case remove change of
        Nothing -> lift $ throwM NeverHappens
        Just relation -> translate $ do
          let phrase l =
                phraseRelationship l article True withDir relation
          english [iii|
            If #{phrase English} would not be there,
            it would be valid.
            |]
          german [iii|
            Wenn es
            #{trailingCommaGerman $ phrase German}
            nicht gäbe, wäre es gültig.
            |]
      pure ()
    Right od | x `notElem` xs -> do
      notCorrect
      paragraph $ translate $ do
        english [iii|
          Class diagram #{x} is in fact valid.
          |]
        german [iii|
          Klassendiagramm #{x} ist gültig.
          |]
      showNamedCd (byName || onlyInheritances (option cdChange))
      paragraph $ translate $ do
        english [iii|
          Consider the following object diagram, which is an instance of this
          class diagram:
          |]
        german [iii|
          Betrachten Sie das folgende Objektdiagramm,
          das Instanz dieses Klassendiagramms ist:
          |]
      paragraph $ image $=<< cacheOd od dir True path
      pure ()
    _ -> pure ()
  where
    byName = printNames drawSettings
    withDir = printNavigations drawSettings
    dir
      | withDir = Back
      | otherwise = NoDir
    notCorrect = paragraph $ translate $ do
      english [iii|Your answer to class diagram #{x} is not correct.|]
      german [iii|Ihre Antwort zu Klassendiagramm #{x} ist nicht richtig.|]
    isInheritance = \case
      Right Inheritance {} -> True
      Right {} -> False
      Left InvalidInheritance {} -> True
    onlyInheritances = all isInheritance . anyRelationships
    showNamedCd sufficient =
      unless sufficient $ do
        paragraph $ translate $ do
          english [iii|
            The relationships in the class diagram could be named in the following way:
            |]
          german [iii|
            Die Beziehungen in dem Klassendiagramm könnten auf folgende Weise
            mit Namen versehen werden:
            |]
        paragraph $ image $=<< cacheCd drawSettings mempty (option cdChange) path
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
    showSolution    = printSolution
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
      showSolution            = showSolution
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
    showSolution            = showSolution
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
shuffleInstance inst =
  (SelectValidCdInstance (cdDrawSettings inst)
    . M.fromAscList . zipWith replaceId [1..]
       <$> shuffleM (M.toList $ classDiagrams inst))
  <*> pure (showExtendedFeedback inst)
  <*> pure (showSolution inst)
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
    showSolution    = showSolution
    }

defaultSelectValidCdInstance :: SelectValidCdInstance
defaultSelectValidCdInstance = SelectValidCdInstance {
  cdDrawSettings = defaultCdDrawSettings,
  classDiagrams = M.fromAscList [
    (1, InValidOption {
      hint = Right $ ObjectDiagram {
        objects = [
          Object {isAnonymous = False, objectName = "b", objectClass = "B"},
          Object {isAnonymous = False, objectName = "b1", objectClass = "B"},
          Object {isAnonymous = False, objectName = "b2", objectClass = "B"},
          Object {isAnonymous = False, objectName = "d", objectClass = "D"}
          ],
        links = []
        },
      option = AnyClassDiagram {
        anyClassNames = ["A", "D", "B", "C"],
        anyRelationships = [
          Right Inheritance {subClass = "B", superClass = "A"},
          Right Inheritance {subClass = "C", superClass = "A"},
          Right Inheritance {subClass = "D", superClass = "C"}
          ]
        }
      }),
    (2, InValidOption {
      hint = Right $ ObjectDiagram {
        objects = [
          Object {isAnonymous = False, objectName = "b2", objectClass = "B"},
          Object {isAnonymous = False, objectName = "b1", objectClass = "B"},
          Object {isAnonymous = False, objectName = "d", objectClass = "D"},
          Object {isAnonymous = False, objectName = "b", objectClass = "B"}
          ],
        links = []
        },
      option = AnyClassDiagram {
        anyClassNames = ["C", "A", "D", "B"],
        anyRelationships = [
          Right Inheritance {subClass = "A", superClass = "B"},
          Right Inheritance {subClass = "D", superClass = "C"},
          Right Inheritance {subClass = "C", superClass = "A"}
          ]
        }
      }),
    (3, InValidOption {
      hint = Left $ Annotation {
          annotated = Change {
            add = Nothing,
            remove = Just $ Right Inheritance {subClass = "A", superClass = "B"}
            },
          annotation = DefiniteArticle
        },
      option = AnyClassDiagram {
        anyClassNames = ["A", "C", "B", "D"],
        anyRelationships = [
          Right Inheritance {subClass = "C", superClass = "A"},
          Right Inheritance {subClass = "B", superClass = "A"},
          Right Inheritance {subClass = "A", superClass = "B"}
          ]
        }
      }),
    (4, InValidOption {
      hint = Left $ Annotation {
          annotated = Change {
            add = Nothing,
            remove = Just $ Right Inheritance {subClass = "A", superClass = "B"}
            },
          annotation = DefiniteArticle
        },
      option = AnyClassDiagram {
        anyClassNames = ["A", "D", "C", "B"],
        anyRelationships = [
          Right Inheritance {subClass = "D", superClass = "C"},
          Right Inheritance {subClass = "A", superClass = "B"},
          Right Inheritance {subClass = "B", superClass = "A"}
          ]
        }
      })
    ],
  showExtendedFeedback = False,
  showSolution = False
  }
