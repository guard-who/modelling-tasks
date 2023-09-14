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
  )

import Modelling.Auxiliary.Common (
  Randomise (randomise),
  RandomiseLayout (randomiseLayout),
  shuffleEverything,
  )
import Modelling.Auxiliary.Output (
  addPretext,
  hoveringInformation,
  simplifiedInformation,
  )
import Modelling.CdOd.CdAndChanges.Instance (
  ChangeAndCd (..),
  )
import Modelling.CdOd.RepairCd (
  AllowedProperties (..),
  InValidOption (..),
  allowEverything,
  checkClassConfigAndChanges,
  mapInValidOption,
  mapInValidOptionM,
  repairIncorrect,
  )
import Modelling.CdOd.Output            (cacheCd)
import Modelling.CdOd.Types (
  Cd,
  Change (..),
  ClassConfig (..),
  ClassDiagram (..),
  Object (..),
  ObjectDiagram (..),
  ObjectProperties (..),
  Od,
  Relationship (..),
  associationNames,
  classNames,
  linkNames,
  shuffleClassAndConnectionOrder,
  relationshipName,
  renameClassesAndRelationshipsInCd,
  renameClassesAndRelationshipsInRelationship,
  renameObjectsWithClassesAndLinksInOd,
  shuffleObjectAndLinkOrder,
  )

import Control.Monad                    ((>=>))
import Control.Monad.Catch              (MonadThrow)
import Control.Monad.IO.Class           (MonadIO (liftIO))
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
import Data.Map                         (Map)
import Data.Maybe                       (mapMaybe)
import Data.String.Interpolate          (i)
import GHC.Generics                     (Generic)
import System.Random.Shuffle            (shuffleM)

data SelectValidCdConfig = SelectValidCdConfig {
    allowedProperties :: AllowedProperties,
    classConfig      :: ClassConfig,
    maxInstances     :: Maybe Integer,
    objectProperties :: ObjectProperties,
    printNames       :: Bool,
    printNavigations :: Bool,
    printSolution    :: Bool,
    shuffleEachCd    :: Bool,
    timeout          :: Maybe Int
  } deriving (Generic, Read, Show)

defaultSelectValidCdConfig :: SelectValidCdConfig
defaultSelectValidCdConfig = SelectValidCdConfig {
    allowedProperties = allowEverything {
        reverseInheritances    = False,
        selfInheritances       = False
        },
    classConfig = ClassConfig {
        classLimits        = (4, 4),
        aggregationLimits  = (0, Just 2),
        associationLimits  = (0, Just 2),
        compositionLimits  = (0, Just 3),
        inheritanceLimits  = (1, Just 3),
        relationshipLimits = (4, Just 6)
      },
    maxInstances     = Just 200,
    objectProperties = ObjectProperties {
      completelyInhabited = Nothing,
      hasLimitedIsolatedObjects = True,
      hasSelfLoops = Nothing
      },
    printNames       = True,
    printNavigations = True,
    printSolution    = False,
    shuffleEachCd    = False,
    timeout          = Nothing
  }

checkSelectValidCdConfig :: SelectValidCdConfig -> Maybe String
checkSelectValidCdConfig SelectValidCdConfig {..} =
  checkClassConfigAndChanges classConfig allowedProperties

type CdChange = InValidOption
  Cd
  (Change (Relationship String String))
  Od

data SelectValidCdInstance = SelectValidCdInstance {
    classDiagrams   :: Map Int CdChange,
    showSolution    :: Bool,
    withNames       :: Bool,
    withNavigations :: Bool
  } deriving (Generic, Read, Show)

selectValidCdSyntax :: OutputMonad m => SelectValidCdInstance -> [Int] -> LangM m
selectValidCdSyntax inst xs =
  for_ xs $ singleChoiceSyntax False (M.keys $ classDiagrams inst)

selectValidCdTask
  :: (OutputMonad m, MonadIO m)
  => FilePath
  -> SelectValidCdInstance
  -> LangM m
selectValidCdTask path task = do
  paragraph $ translate $ do
    english [i|Consider the following class diagram candidates.|]
    german [i|Betrachten Sie die folgenden Klassendiagrammkandidaten.|]
  images show snd $=<< liftIO $ sequence $
    M.foldrWithKey drawCd mempty $ classDiagrams task
  paragraph $ translate $ do
    english [i|Which of these class diagram candidates are valid class diagrams?
Please state your answer by giving a list of numbers, indicating all valid class diagrams.|]
    german [i|Welche dieser Klassendiagrammkandidaten sind valide Klassendiagramme?
Bitte geben Sie Ihre Antwort in Form einer Liste von Zahlen an, die alle gültigen Klassendiagramme enthält.|]
  paragraph $ do
    translate $ do
      english [i|For example,|]
      german [i|Zum Beispiel|]
    code "[1, 2]"
    translate $ do
      english [i|would indicate that only class diagram candidates 1 and 2 of the given ones are valid class diagrams.|]
      german [i|würde bedeuten, dass nur die Klassendiagrammkandidaten 1 und 2 der angegebenen Klassendiagrammkandidaten gültige Klassendiagramme sind.|]
    pure ()
  paragraph simplifiedInformation
  paragraph hoveringInformation
  pure ()
  where
    drawCd x theChange cds =
      let f = cacheCd
            (withNavigations task)
            (withNames task)
            mempty
            (option theChange)
            path
      in M.insert x ((isRight $ hint theChange,) <$> f) cds

selectValidCdEvaluation
  :: OutputMonad m
  => SelectValidCdInstance
  -> [Int]
  -> Rated m
selectValidCdEvaluation inst xs = addPretext $ do
  let cds = M.fromList [
        (English, "class diagrams"),
        (German, "Klassendiagramme")
        ]
      solution = isRight . hint <$> classDiagrams inst
      correctAnswer
        | showSolution inst = Just $ show $ selectValidCdSolution inst
        | otherwise = Nothing
  multipleChoice cds correctAnswer solution xs

selectValidCdSolution :: SelectValidCdInstance -> [Int]
selectValidCdSolution =
  M.keys . M.filter id . fmap (isRight . hint) . classDiagrams

selectValidCd
  :: SelectValidCdConfig
  -> Int
  -> Int
  -> IO SelectValidCdInstance
selectValidCd config segment seed = do
  let g = mkStdGen $ (segment +) $ 4 * seed
  (_, chs)  <- flip evalRandT g $ repairIncorrect
    (allowedProperties config)
    (classConfig config)
    (objectProperties config)
    (maxInstances config)
    (timeout config)
  let cds = map (mapInValidOption changeClassDiagram id id) chs
  shuffleCds >=> shuffleEverything $ SelectValidCdInstance {
    classDiagrams   = M.fromAscList $ zip [1 ..] cds,
    showSolution    = printSolution config,
    withNames       = printNames config,
    withNavigations = printNavigations config
    }
  where
    shuffleCds
      | shuffleEachCd config = shuffleEach
      | otherwise            = return

instance Randomise SelectValidCdInstance where
  randomise inst = do
    let (names, assocs) = classAndAssocNames inst
    names' <- shuffleM names
    assocs' <- shuffleM assocs
    renameInstance inst names' assocs'
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
      showSolution            = showSolution,
      withNames               = withNames,
      withNavigations         = withNavigations
      }

shuffleEach
  :: (MonadRandom m, MonadThrow m)
  => SelectValidCdInstance
  -> m SelectValidCdInstance
shuffleEach inst@SelectValidCdInstance {..} = do
  names' <- shuffleM names
  assocs' <- shuffleM assocs
  let bmNames  = BM.fromList $ zip names names'
      bmAssocs = BM.fromList $ zip assocs assocs'
      renameCd = renameClassesAndRelationshipsInCd bmNames bmAssocs
      renameOd = renameObjectsWithClassesAndLinksInOd bmNames bmAssocs
      renameEdge = renameClassesAndRelationshipsInRelationship bmNames bmAssocs
  cds <- mapInValidOptionM renameCd (mapM renameEdge) renameOd
    `mapM` classDiagrams
  return $ SelectValidCdInstance {
    classDiagrams           = cds,
    showSolution            = showSolution,
    withNames               = withNames,
    withNavigations         = withNavigations
    }
  where
    (names, assocs) = classAndAssocNames inst

shuffleInstance
  :: MonadRandom m
  => SelectValidCdInstance
  -> m SelectValidCdInstance
shuffleInstance inst = SelectValidCdInstance
  <$> (M.fromAscList . zipWith replaceId [1..]
       <$> shuffleM (M.toList $ classDiagrams inst))
  <*> pure (showSolution inst)
  <*> pure (withNames inst)
  <*> pure (withNavigations inst)
  where
    replaceId x (_, cd) = (x, cd)

classAndAssocNames :: SelectValidCdInstance -> ([String], [String])
classAndAssocNames inst =
  let cds = classDiagrams inst
      (improves, evidences) = partitionEithers $ map hint $ M.elems cds
      names = nubOrd $ concatMap (classNames . option) cds
      assocs = nubOrd $ concatMap (associationNames . option) cds
        ++ mapMaybe (add >=> relationshipName) improves
        ++ mapMaybe (remove >=> relationshipName) improves
        ++ concatMap linkNames evidences
  in (names, assocs)

renameInstance
  :: MonadThrow m
  => SelectValidCdInstance
  -> [String]
  -> [String]
  -> m SelectValidCdInstance
renameInstance inst names' assocs' = do
  let (names, assocs) = classAndAssocNames inst
      bmNames  = BM.fromList $ zip names names'
      bmAssocs = BM.fromList $ zip assocs assocs'
      renameCd = renameClassesAndRelationshipsInCd bmNames bmAssocs
      renameEdge = renameClassesAndRelationshipsInRelationship bmNames bmAssocs
      renameOd = renameObjectsWithClassesAndLinksInOd bmNames bmAssocs
  cds <- mapM
    (mapInValidOptionM renameCd (mapM renameEdge) renameOd)
    $ classDiagrams inst
  return $ SelectValidCdInstance {
    classDiagrams   = cds,
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
      hint = Left $ Change {
        add = Nothing,
        remove = Just $ Inheritance {subClass = "A", superClass = "B"}
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
      hint = Left $ Change {
        add = Nothing,
        remove = Just $ Inheritance {subClass = "A", superClass = "B"}
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
  showSolution = False,
  withNames = True,
  withNavigations = True
  }
