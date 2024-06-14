{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
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

import qualified Modelling.CdOd.Types             as T (CdDrawSettings (..))

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
  phraseChange,
  phraseRelationship,
  trailingCommaGerman,
  )
import Modelling.CdOd.RepairCd (
  AllowedProperties (..),
  InValidOption (..),
  RelationshipChangeWithArticle,
  allowNothing,
  checkClassConfigAndChanges,
  mapInValidOption,
  mapInValidOptionM,
  repairIncorrect,
  )
import Modelling.CdOd.Output            (cacheCd, cacheOd)
import Modelling.CdOd.Types (
  Annotation (..),
  ArticlePreference (..),
  ArticleToUse (DefiniteArticle),
  Cd,
  CdDrawSettings (CdDrawSettings),
  ClassConfig (..),
  ClassDiagram (..),
  Object (..),
  ObjectDiagram (..),
  ObjectProperties (..),
  Od,
  Relationship (..),
  associationNames,
  checkCdDrawSettings,
  classNames,
  defaultOmittedDefaultMultiplicites,
  linkNames,
  shuffleClassAndConnectionOrder,
  relationshipName,
  renameClassesAndRelationships,
  renameObjectsWithClassesAndLinksInOd,
  shuffleObjectAndLinkOrder,
  )
import Modelling.Types                  (Change (..))

import Control.Applicative              (Alternative)
import Control.Monad                    ((>=>), unless, void, when)
import Control.Monad.Catch              (MonadThrow)
import Control.Monad.Output (
  GenericOutputMonad (..),
  LangM,
  Language (English, German),
  OutputMonad,
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
import Data.Containers.ListUtils        (nubOrd)
import Data.Either                      (isRight, partitionEithers)
import Data.Foldable                    (for_)
import Data.GraphViz                    (DirType (Back, NoDir))
import Data.Map                         (Map)
import Data.Maybe                       (mapMaybe)
import Data.String.Interpolate          (i, iii)
import GHC.Generics                     (Generic)
import System.Random.Shuffle            (shuffleM)

data SelectValidCdConfig = SelectValidCdConfig {
    allowedProperties :: AllowedProperties,
    -- | the preferred article to use when referring to relationships
    articleToUse      :: ArticlePreference,
    classConfig      :: ClassConfig,
    maxInstances     :: Maybe Integer,
    objectProperties :: ObjectProperties,
    -- | when enabled feedback for wrong answers will be shown
    -- this might include ODs
    printExtendedFeedback :: Bool,
    printNames       :: Bool,
    printNavigations :: Bool,
    printSolution    :: Bool,
    shuffleEachCd    :: Bool,
    timeout          :: Maybe Int
  } deriving (Generic, Read, Show)

defaultSelectValidCdConfig :: SelectValidCdConfig
defaultSelectValidCdConfig = SelectValidCdConfig {
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

type CdChange = InValidOption
  Cd
  RelationshipChangeWithArticle
  Od

data SelectValidCdInstance = SelectValidCdInstance {
    classDiagrams   :: Map Int CdChange,
    -- | when enabled feedback for wrong answers will be shown
    -- this might include ODs
    showExtendedFeedback :: Bool,
    showSolution    :: Bool,
    withNames       :: Bool,
    withNavigations :: Bool
  } deriving (Generic, Read, Show)

cdDrawSettings
  :: SelectValidCdInstance
  -> CdDrawSettings
cdDrawSettings SelectValidCdInstance {..} = CdDrawSettings {
  omittedDefaults = defaultOmittedDefaultMultiplicites,
  T.printNames = withNames,
  T.printNavigations = withNavigations
  }

checkSelectValidCdInstance :: SelectValidCdInstance -> Maybe String
checkSelectValidCdInstance task@SelectValidCdInstance {..}
  | showExtendedFeedback && not showSolution
  = Just [iii|
      showExtendedFeedback leaks the correct solution
      and thus can only be enabled when showSolution is set to True
      |]
  | otherwise
  = checkCdDrawSettings (cdDrawSettings task)

selectValidCdSyntax :: OutputMonad m => SelectValidCdInstance -> [Int] -> LangM m
selectValidCdSyntax inst xs =
  for_ xs $ singleChoiceSyntax False (M.keys $ classDiagrams inst)

selectValidCdTask
  :: (MonadCache m, MonadDiagrams m, MonadGraphviz m, OutputMonad m)
  => FilePath
  -> SelectValidCdInstance
  -> LangM m
selectValidCdTask path inst@SelectValidCdInstance {..} = do
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
            (cdDrawSettings inst)
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
    OutputMonad m
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
  reRefuse (multipleChoice cds correctAnswer solution xs)
    $ when showExtendedFeedback
    $ void $ M.traverseWithKey
      (selectValidCdFeedback path (cdDrawSettings inst) xs)
      classDiagrams

selectValidCdFeedback
  :: (MonadCache m, MonadDiagrams m, MonadGraphviz m, MonadThrow m, OutputMonad m)
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
      paragraph $ translate $ case remove change of
        Nothing -> do
          english [iii|
            Consider the following change, which aims at fixing a
            problematic situation within the given class diagram:
            #{phraseChange English article True withDir change}.
            |]
          german [iii|
            Sehen Sie sich die folgende Änderung an, die darauf abzielt, eine
            problematische Stelle im Klassendiagramm zu beheben:
            #{phraseChange German article True withDir change}.
            |]
        Just relation -> do
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
      paragraph $ image $=<< flip evalRandT (mkStdGen 0)
        $ cacheOd od 0 dir True path
      pure ()
    _ -> pure ()
  where
    byName = T.printNames drawSettings
    withDir = T.printNavigations drawSettings
    dir
      | withDir = Back
      | otherwise = NoDir
    notCorrect = paragraph $ translate $ do
      english [iii|Your answer to class diagram #{x} is not correct.|]
      german [iii|Ihre Antwort zu Klassendiagramm #{x} ist nicht richtig.|]
    isInheritance Inheritance {} = True
    isInheritance _ = False
    onlyInheritances = all isInheritance . relationships
    useDirections = drawSettings {
      T.printNavigations = True
      }
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
        paragraph $ image $=<< cacheCd useDirections mempty (option cdChange) path
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
selectValidCd config segment seed = flip evalRandT g $ do
  (_, chs)  <- repairIncorrect
    (allowedProperties config)
    (classConfig config)
    (objectProperties config)
    (articleToUse config)
    (maxInstances config)
    (timeout config)
  let cds = map (mapInValidOption annotatedChangeClassDiagram id id) chs
  shuffleCds >=> shuffleEverything $ SelectValidCdInstance {
    classDiagrams   = M.fromAscList $ zip [1 ..] cds,
    showExtendedFeedback = printExtendedFeedback config,
    showSolution    = printSolution config,
    withNames       = printNames config,
    withNavigations = printNavigations config
    }
  where
    g = mkStdGen $ (segment +) $ 4 * seed
    shuffleCds
      | shuffleEachCd config = shuffleEach
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
      shuffleClassAndConnectionOrder
      pure
      shuffleObjectAndLinkOrder
      `mapM` classDiagrams
    return $ SelectValidCdInstance {
      classDiagrams           = cds,
      showExtendedFeedback    = showExtendedFeedback,
      showSolution            = showSolution,
      withNames               = withNames,
      withNavigations         = withNavigations
      }

shuffleEach
  :: (MonadRandom m, MonadThrow m)
  => SelectValidCdInstance
  -> m SelectValidCdInstance
shuffleEach inst@SelectValidCdInstance {..} = do
  cds <- shuffleCdChange inst `mapM` classDiagrams
  return $ SelectValidCdInstance {
    classDiagrams           = cds,
    showExtendedFeedback    = showExtendedFeedback,
    showSolution            = showSolution,
    withNames               = withNames,
    withNavigations         = withNavigations
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
  mapInValidOptionM renameCd (mapM $ mapM renameEdge) renameOd x
  where
    (names, nonInheritances) = classAndNonInheritanceNames inst

shuffleInstance
  :: MonadRandom m
  => SelectValidCdInstance
  -> m SelectValidCdInstance
shuffleInstance inst =
  (SelectValidCdInstance . M.fromAscList . zipWith replaceId [1..]
       <$> shuffleM (M.toList $ classDiagrams inst))
  <*> pure (showExtendedFeedback inst)
  <*> pure (showSolution inst)
  <*> pure (withNames inst)
  <*> pure (withNavigations inst)
  where
    replaceId x (_, cd) = (x, cd)

classAndNonInheritanceNames :: SelectValidCdInstance -> ([String], [String])
classAndNonInheritanceNames inst =
  let cds = classDiagrams inst
      (improves, evidences) = partitionEithers $ map hint $ M.elems cds
      names = nubOrd $ concatMap (classNames . option) cds
      nonInheritances = nubOrd $ concatMap (associationNames . option) cds
        ++ mapMaybe (add . annotated >=> relationshipName) improves
        ++ mapMaybe (remove . annotated >=> relationshipName) improves
        ++ concatMap linkNames evidences
  in (names, nonInheritances)

renameInstance
  :: MonadThrow m
  => SelectValidCdInstance
  -> [String]
  -> [String]
  -> m SelectValidCdInstance
renameInstance inst names' nonInheritances' = do
  let (names, nonInheritances) = classAndNonInheritanceNames inst
      bmNames  = BM.fromList $ zip names names'
      bmNonInheritances = BM.fromList $ zip nonInheritances nonInheritances'
      renameCd = renameClassesAndRelationships bmNames bmNonInheritances
      renameEdge = renameClassesAndRelationships bmNames bmNonInheritances
      renameOd = renameObjectsWithClassesAndLinksInOd bmNames bmNonInheritances
  cds <- mapM
    (mapInValidOptionM renameCd (mapM $ mapM renameEdge) renameOd)
    $ classDiagrams inst
  return $ SelectValidCdInstance {
    classDiagrams   = cds,
    showExtendedFeedback = showExtendedFeedback inst,
    showSolution    = showSolution inst,
    withNames       = withNames inst,
    withNavigations = withNavigations inst
    }

defaultSelectValidCdInstance :: SelectValidCdInstance
defaultSelectValidCdInstance = SelectValidCdInstance {
  classDiagrams = M.fromAscList [
    (1, InValidOption {
      hint = Right $ ObjectDiagram {
        objects = [
          Object {objectName = "b", objectClass = "B"},
          Object {objectName = "b1", objectClass = "B"},
          Object {objectName = "b2", objectClass = "B"},
          Object {objectName = "d", objectClass = "D"}
          ],
        links = []
        },
      option = ClassDiagram {
        classNames = ["A", "D", "B", "C"],
        relationships = [
          Inheritance {subClass = "B", superClass = "A"},
          Inheritance {subClass = "C", superClass = "A"},
          Inheritance {subClass = "D", superClass = "C"}
          ]
        }
      }),
    (2, InValidOption {
      hint = Right $ ObjectDiagram {
        objects = [
          Object {objectName = "b2", objectClass = "B"},
          Object {objectName = "b1", objectClass = "B"},
          Object {objectName = "d", objectClass = "D"},
          Object {objectName = "b", objectClass = "B"}
          ],
        links = []
        },
      option = ClassDiagram {
        classNames = ["C", "A", "D", "B"],
        relationships = [
          Inheritance {subClass = "A", superClass = "B"},
          Inheritance {subClass = "D", superClass = "C"},
          Inheritance {subClass = "C", superClass = "A"}
          ]
        }
      }),
    (3, InValidOption {
      hint = Left $ Annotation {
          annotated = Change {
            add = Nothing,
            remove = Just $ Inheritance {subClass = "A", superClass = "B"}
            },
          annotation = DefiniteArticle
        },
      option = ClassDiagram {
        classNames = ["A", "C", "B", "D"],
        relationships = [
          Inheritance {subClass = "C", superClass = "A"},
          Inheritance {subClass = "B", superClass = "A"},
          Inheritance {subClass = "A", superClass = "B"}
          ]
        }
      }),
    (4, InValidOption {
      hint = Left $ Annotation {
          annotated = Change {
            add = Nothing,
            remove = Just $ Inheritance {subClass = "A", superClass = "B"}
            },
          annotation = DefiniteArticle
        },
      option = ClassDiagram {
        classNames = ["A", "D", "C", "B"],
        relationships = [
          Inheritance {subClass = "D", superClass = "C"},
          Inheritance {subClass = "A", superClass = "B"},
          Inheritance {subClass = "B", superClass = "A"}
          ]
        }
      })
    ],
  showExtendedFeedback = False,
  showSolution = False,
  withNames = True,
  withNavigations = True
  }
