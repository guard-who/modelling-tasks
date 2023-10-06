{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Modelling.CdOd.NameCdError (
  NameCdErrorConfig (..),
  NameCdErrorInstance (..),
  checkNameCdErrorConfig,
  checkNameCdErrorInstance,
  classAndAssocNames,
  defaultNameCdErrorConfig,
  defaultNameCdErrorInstance,
  nameCdErrorEvaluation,
  nameCdErrorGenerate,
  nameCdErrorSolution,
  nameCdErrorSyntax,
  nameCdErrorTask,
  renameInstance,
  ) where

import qualified Modelling.CdOd.CdAndChanges.Transform as Changes (
  transformGetNextFix,
  )

import qualified Data.Bimap                       as BM (fromList)
import qualified Data.Map                         as M (
  elems,
  filter,
  fromAscList,
  keys,
  toList,
  )

import Modelling.Auxiliary.Common (
  Randomise (randomise),
  RandomiseLayout (randomiseLayout), shuffleEverything,
  )
import Modelling.Auxiliary.Output (
  addPretext,
  hoveringInformation,
  simplifiedInformation,
  )
import Modelling.CdOd.Auxiliary.Util    (alloyInstanceToOd, getInstances)
import Modelling.CdOd.CD2Alloy.Transform (
  combineParts,
  createRunCommand,
  transform,
  )
import Modelling.CdOd.CdAndChanges.Instance (
  ChangeAndCd (..),
  GenericClassDiagramInstance (..),
  )
import Modelling.CdOd.MatchCdOd         (getChangesAndCds)
import Modelling.CdOd.Output (
  cacheCd,
  )
import Modelling.CdOd.RepairCd (
  AllowedProperties (..),
  PropertyChange (..),
  (.&.),
  allowEverything,
  checkClassConfigAndChanges,
  illegalChanges,
  legalChanges,
  phraseRelation,
  phraseRelationDE,
  toProperty,
  )
import Modelling.CdOd.Types (
  Cd,
  Change (..),
  ClassConfig (..),
  ClassDiagram (..),
  LimitedLinking (..),
  ObjectProperties (..),
  Relationship (..),
  RelationshipProperties (..),
  associationNames,
  classNames,
  maxObjects,
  relationshipName,
  renameClassesAndRelationshipsInCd,
  renameClassesAndRelationshipsInRelationship,
  reverseAssociation,
  shuffleClassAndConnectionOrder,
  )

import Control.Monad                    ((>=>), forM, join)
import Control.Monad.Catch              (MonadThrow)
import Control.Monad.Except             (runExceptT)
import Control.Monad.IO.Class           (MonadIO (liftIO))
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
  printSolutionAndAssert,
  singleChoice,
  singleChoiceSyntax,
  translate,
  )
import Control.Monad.Output.Generic     (($>>=))
import Control.Monad.Random
  (MonadRandom, RandT, RandomGen, evalRandT, mkStdGen)
import Control.Monad.Trans.State        (put)
import Data.Aeson.TH                    (defaultOptions, deriveJSON)
import Data.Bifunctor                   (second)
import Data.ByteString.UTF8             (toString)
import Data.Containers.ListUtils        (nubOrd)
import Data.Either.Extra                (eitherToMaybe)
import Data.Foldable                    (for_)
import Data.Map                         (Map)
import Data.Maybe                       (listToMaybe, mapMaybe)
import Data.String.Interpolate          (i, iii)
import Data.Yaml                        (encode)
import GHC.Generics                     (Generic)
import System.Random.Shuffle            (shuffleM)

data NameCdErrorConfig = NameCdErrorConfig {
  allowedProperties           :: AllowedProperties,
  classConfig                 :: ClassConfig,
  maxInstances                :: Maybe Integer,
  objectProperties            :: ObjectProperties,
  printNames                  :: Bool,
  printNavigations            :: Bool,
  printSolution               :: Bool,
  timeout                     :: Maybe Int,
  useNames                    :: Bool
  } deriving (Generic, Read, Show)

defaultNameCdErrorConfig :: NameCdErrorConfig
defaultNameCdErrorConfig = NameCdErrorConfig {
  allowedProperties = allowEverything {
    reverseInheritances = False,
    Modelling.CdOd.RepairCd.selfInheritances = False
    },
  classConfig = ClassConfig {
    classLimits = (4, 4),
    aggregationLimits = (1, Just 1),
    associationLimits = (0, Just 1),
    compositionLimits = (2, Just 3),
    inheritanceLimits = (0, Just 0),
    relationshipLimits = (3, Just 5)
    },
  maxInstances = Just 200,
  objectProperties = ObjectProperties {
    completelyInhabited = Just True,
    hasLimitedIsolatedObjects = True,
    hasSelfLoops = Nothing,
    usesEveryRelationshipName = Just True
    },
  printNames = True,
  printNavigations = True,
  printSolution = False,
  timeout = Nothing,
  useNames = False
  }

checkNameCdErrorConfig :: NameCdErrorConfig -> Maybe String
checkNameCdErrorConfig NameCdErrorConfig {..}
  | not printNames && useNames
  = Just "use names is only possible when printing names"
  | completelyInhabited objectProperties /= Just True
  = Just "completelyInhabited needs to be set to 'Just True' for this task type"
  | usesEveryRelationshipName objectProperties /= Just True
  = Just [iii|
      usesEveryRelationshipName needs to be set to 'Just True' for this task type
      |]
  | otherwise
  = checkClassConfigAndChanges classConfig allowedProperties

data NameCdErrorInstance = NameCdErrorInstance {
  allRelationships            :: Map Int (Bool, Relationship String String),
  classDiagram                :: Cd,
  errorReasons                :: Map Int (Bool, Map Language String),
  showSolution                :: Bool,
  withDirections              :: Bool,
  withNames                   :: Bool
  } deriving (Eq, Generic, Read, Show)

checkNameCdErrorInstance :: NameCdErrorInstance -> Maybe String
checkNameCdErrorInstance NameCdErrorInstance {..}
  | any (`notElem` relationshipsOnly) $ relationships classDiagram
  = Just [iii|
      'allRelationships' must contain all relationships that are
      relationships of the 'classDiagram'.
      |]
  | any (`notElem` relationships classDiagram) relationshipsOnly
  = Just [iii|
      'allRelationships' must contain only relationships that are in fact
      relationships of the 'classDiagram'.
      |]
  | otherwise
  = Nothing
  where
    relationshipsOnly = map snd (M.elems allRelationships)

data NameCdErrorAnswer = NameCdErrorAnswer {
  reason                      :: Int,
  contributing                :: [Int]
  } deriving (Read, Show)

$(deriveJSON defaultOptions ''NameCdErrorAnswer)

nameCdErrorTask
  :: (OutputMonad m, MonadIO m)
  => FilePath
  -> NameCdErrorInstance
  -> LangM m
nameCdErrorTask path task = do
  paragraph $ translate $ do
    english "Consider the following class diagram, which unfortunately is invalid."
    german "Betrachten Sie das folgende Klassendiagramm, welches leider ungültig ist."
  image $=<< liftIO $ cacheCd
    (withDirections task)
    (withNames task)
    mempty
    (classDiagram task)
    path
  paragraph $ translate $ do
    english "It consists of the following relationships:"
    german "Es besteht aus den folgenden Beziehungen:"
  let phrase x y z = translate $ do
        english $ phraseRelation x y z
        german $ phraseRelationDE x y z
  enumerateM (text . show)
    $ second (phrase (withNames task) (withDirections task) . snd)
    <$> M.toList (allRelationships task)
  paragraph $ translate $ do
    english [iii|
      Choose the reason why you think that this class diagram is incorrect
      and state all relationships that contribute to the problem,
      i.e. removing any of them would fix the problem.
      |]
    german [iii|
      Wählen Sie den Grund aus, warum Sie denken,
      dass dieses Klassendiagramm ungültig ist
      und nennen Sie alle Beziehungen, die zum Problem beitragen,
      d.h. deren Entfernung das Problem jeweils beheben würde.
      |]
  paragraph $ translate $ do
    english [i|Possible reasons are:|]
    german [i|Mögliche Gründe sind:|]
  enumerateM (text . show)
    $ second (translate . put . snd)
    <$> M.toList (errorReasons task)
  paragraph $ do
    paragraph $ translate $ do
      english [iii|
        Please state your answer by providing one number for reason,
        indicating the conceptual reason why the class diagram is invalid
        and a list of numbers for relationships,
        indicating the relationships that form the issue.
        E.g.
        |]
      german [iii|
        Bitte geben Sie Ihre Antwort an, indem Sie folgendes angeben:
        eine Zahl für den Grund,
        der Ihrer Meinung nach der konzeptuelle Grund dafür ist,
        dass das Klassendiagramm ungültig ist,
        und eine Liste von Zahlen für die Beziehungen,
        die Ihrer Meinung nach das Problem bilden.
        Zum Beispiel
        |]
    paragraph $ code $ toString $ encode $ NameCdErrorAnswer {
      reason = 2,
      contributing = [3, 4]
      }
    paragraph $ translate $ do
      english [iii|
        would indicate that the class diagram is invalid because of reason 2
        and that relationship 3 and 4 contribute to the problem.
        |]
      german [iii|
        würde bedeuten, dass das Klassendiagramm wegen Grund 2 ungültig ist
        und dass die Beziehungen 3 und 4 zum Problem beitragen.
        |]
    pure ()
  paragraph simplifiedInformation
  paragraph hoveringInformation
  pure ()

nameCdErrorSyntax
  :: OutputMonad m
  => NameCdErrorInstance
  -> NameCdErrorAnswer
  -> LangM m
nameCdErrorSyntax inst x = do
  singleChoiceSyntax False (M.keys $ errorReasons inst) $ reason x
  for_ (contributing x) $ singleChoiceSyntax False (M.keys $ allRelationships inst)
  pure ()

{-| Grading is done the following way:

 * 0 points if the reason is wrong
 * otherwise, multiple choice grading for answer on contributing relationships
-}
nameCdErrorEvaluation
  :: (Monad m, OutputMonad m)
  => NameCdErrorInstance
  -> NameCdErrorAnswer
  -> Rated m
nameCdErrorEvaluation inst x = addPretext $ do
  let chs = M.fromAscList [
        (English, "changes"),
        (German, "Änderungen")
        ]
      solutionReason = head . M.keys . M.filter fst $ errorReasons inst
      solutionContributing = fst <$> allRelationships inst
      correctAnswer
        | showSolution inst = Just $ toString $ encode $ nameCdErrorSolution inst
        | otherwise = Nothing
  singleChoice chs Nothing solutionReason (reason x)
    $>>= \p ->
    if p < 1
    then pure 0
    else multipleChoice chs Nothing solutionContributing (contributing x)
    $>>= printSolutionAndAssert correctAnswer

nameCdErrorSolution :: NameCdErrorInstance -> NameCdErrorAnswer
nameCdErrorSolution x = NameCdErrorAnswer {
  reason = head . M.keys . M.filter fst $ errorReasons x,
  contributing = M.keys . M.filter fst $ allRelationships x
  }

classAndAssocNames :: NameCdErrorInstance -> ([String], [String])
classAndAssocNames inst =
  let cd = classDiagram inst
      relationships = map snd $ M.elems $ allRelationships inst
      names = nubOrd $ classNames cd
      assocs = nubOrd $ associationNames cd
        ++ mapMaybe relationshipName relationships
  in (names, assocs)

instance Randomise NameCdErrorInstance where
  randomise inst = do
    let (names, assocs) = classAndAssocNames inst
    names' <- shuffleM names
    assocs' <- shuffleM assocs
    renameInstance inst names' assocs'
      >>= shuffleInstance

instance RandomiseLayout NameCdErrorInstance where
  randomiseLayout NameCdErrorInstance {..} = do
    cd <- shuffleClassAndConnectionOrder classDiagram
    return NameCdErrorInstance {
      allRelationships = allRelationships,
      classDiagram = cd,
      errorReasons = errorReasons,
      showSolution = showSolution,
      withDirections = withDirections,
      withNames = withNames
      }

shuffleInstance :: MonadRandom m => NameCdErrorInstance -> m NameCdErrorInstance
shuffleInstance inst = do
  chs <- M.fromAscList . zip [1..] <$> shuffleM (M.elems $ allRelationships inst)
  rs <- M.fromAscList . zip [1..] <$> shuffleM (M.elems $ errorReasons inst)
  return $ NameCdErrorInstance {
    allRelationships = chs,
    classDiagram = classDiagram inst,
    errorReasons = rs,
    showSolution = showSolution inst,
    withDirections = withDirections inst,
    withNames = withNames inst
    }

renameInstance
  :: MonadThrow m
  => NameCdErrorInstance
  -> [String]
  -> [String]
  -> m NameCdErrorInstance
renameInstance inst names' assocs' = do
  let (names, assocs) = classAndAssocNames inst
      bmNames  = BM.fromList $ zip names names'
      bmAssocs = BM.fromList $ zip assocs assocs'
      renameCd = renameClassesAndRelationshipsInCd bmNames bmAssocs
      renameEdge = renameClassesAndRelationshipsInRelationship bmNames bmAssocs
  cd <- renameCd $ classDiagram inst
  chs <- mapM (mapM renameEdge) $ allRelationships inst
  return $ NameCdErrorInstance {
    allRelationships = chs,
    classDiagram = cd,
    errorReasons = errorReasons inst,
    showSolution = showSolution inst,
    withDirections = withDirections inst,
    withNames = withNames inst
    }

nameCdErrorGenerate
  :: NameCdErrorConfig
  -> Int
  -> Int
  -> IO NameCdErrorInstance
nameCdErrorGenerate config segment seed = do
  let g = mkStdGen $ (segment +) $ 4 * seed
  (cd, reason, rs) <- flip evalRandT g $ nameCdError
    (allowedProperties config)
    (classConfig config)
    (objectProperties config)
    (maxInstances config)
    (timeout config)
  shuffleEverything $ NameCdErrorInstance {
    allRelationships = M.fromAscList
      $ zip [1..] $ map (\x -> (x `elem` rs, x)) $ relationships cd,
    classDiagram = cd,
    errorReasons = M.fromAscList
      [(1, (True, M.fromAscList [(English, reason), (German, reason)]))],
    showSolution = printSolution config,
    withDirections = printNavigations config,
    withNames = printNames config
    }

nameCdError
  :: RandomGen g
  => AllowedProperties
  -> ClassConfig
  -> ObjectProperties
  -> Maybe Integer
  -> Maybe Int
  -> RandT g IO (Cd, String, [Relationship String String])
nameCdError allowed config objectProperties maxInsts to = do
  e0:_    <- shuffleM $ illegalChanges allowed
  l0:_   <- shuffleM $ legalChanges allowed
  let p = e0 .&. l0
      alloyCode = Changes.transformGetNextFix Nothing config (toProperty p)
  instas  <- liftIO $ getInstances maxInsts to alloyCode
  rinstas <- shuffleM instas
  getInstanceWithODs p rinstas
  where
    getInstanceWithODs _ [] =
      nameCdError allowed config objectProperties maxInsts to
    getInstanceWithODs change (rinsta:rinstas) = do
      cdInstance <- liftIO $ getChangesAndCds rinsta
      let cd = instanceClassDiagram cdInstance
          p = (toProperty change) {
            hasDoubleRelationships = Nothing,
            hasReverseRelationships = Nothing,
            hasMultipleInheritances = Nothing
            }
          alloyCode = Changes.transformGetNextFix (Just cd) config p
      instas <- liftIO $ getInstances Nothing to alloyCode
      correctInstance <- liftIO $ mapM getChangesAndCds instas
      let allChs = concatMap instanceChangesAndCds correctInstance
          cd2 = instanceClassDiagram $ head correctInstance
      mremoves <- do
        liftIO $ mapM (getOD . changeClassDiagram) allChs
        return $ traverse (remove . relationshipChange) allChs
      case mremoves of
        Nothing -> getInstanceWithODs change rinstas
        Just removes -> return (cd2, changeName change, removes)
    getOD cd = do
      let reversedRelationships = map reverseAssociation $ relationships cd
          maxNObjects = maxObjects $ snd $ classLimits config
          parts = transform
            (cd {relationships = reversedRelationships})
            []
            maxNObjects
            objectProperties
            ""
            ""
          command = createRunCommand
            "cd"
            (length $ classNames cd)
            maxNObjects
            reversedRelationships
            parts
      od <- listToMaybe
        <$> getInstances (Just 1) to (combineParts parts ++ command)
      fmap join $ forM od
        $ runExceptT . alloyInstanceToOd >=> return . eitherToMaybe

defaultNameCdErrorInstance :: NameCdErrorInstance
defaultNameCdErrorInstance = NameCdErrorInstance {
  allRelationships = M.fromAscList [
    (1, (True, Composition {
      compositionName = "z",
      compositionPart = LimitedLinking {linking = "D", limits = (1, Nothing)},
      compositionWhole = LimitedLinking {linking = "A", limits = (1, Just 1)}
      })),
    (2, (True, Composition {
      compositionName = "w",
      compositionPart = LimitedLinking {linking = "B", limits = (1, Nothing)},
      compositionWhole = LimitedLinking {linking = "D", limits = (1, Just 1)}
      })),
    (3, (False, Aggregation {
      aggregationName = "y",
      aggregationPart = LimitedLinking {linking = "B", limits = (2, Nothing)},
      aggregationWhole = LimitedLinking {linking = "D", limits = (2, Just 2)}
      })),
    (4, (True, Composition {
      compositionName = "x", compositionPart = LimitedLinking {
      linking = "A", limits = (0, Just 1)},
      compositionWhole = LimitedLinking {linking = "B", limits = (0, Just 1)}
      }))
    ],
  classDiagram = ClassDiagram {
    classNames = ["D","C","B","A"],
    relationships = [
      Aggregation {
        aggregationName = "y",
        aggregationPart = LimitedLinking {linking = "B", limits = (2, Nothing)},
        aggregationWhole = LimitedLinking {linking = "D", limits = (2, Just 2)}
        },
      Composition {
        compositionName = "x",
        compositionPart = LimitedLinking {linking = "A", limits = (0, Just 1)},
        compositionWhole = LimitedLinking {linking = "B", limits = (0, Just 1)}
        },
      Composition {
        compositionName = "w",
        compositionPart = LimitedLinking {linking = "B", limits = (1, Nothing)},
        compositionWhole = LimitedLinking {linking = "D", limits = (1, Just 1)}
        },
      Composition {
        compositionName = "z",
        compositionPart = LimitedLinking {linking = "D", limits = (1, Nothing)},
        compositionWhole = LimitedLinking {linking = "A", limits = (1, Just 1)}
        }
      ]
    },
  errorReasons = M.fromAscList [
    (1, (True, M.fromAscList [
      (English, "force composition cycles + force double relationships"),
      (German, "force composition cycles + force double relationships")]))
    ],
  showSolution = False,
  withDirections = True,
  withNames = True
  }
