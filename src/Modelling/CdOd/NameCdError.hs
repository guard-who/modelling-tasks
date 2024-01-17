{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Modelling.CdOd.NameCdError (
  NameCdErrorAnswer (..),
  NameCdErrorConfig (..),
  NameCdErrorInstance (..),
  NameCdErrorTaskText,
  NameCdErrorTaskTextElement (..),
  NumberOfReasons (..),
  Reason (..),
  TaskTextPart (..),
  checkNameCdErrorConfig,
  checkNameCdErrorInstance,
  classAndAssocNames,
  defaultNameCdErrorAnswer,
  defaultNameCdErrorConfig,
  defaultNameCdErrorInstance,
  nameCdErrorEvaluation,
  nameCdErrorGenerate,
  nameCdErrorSolution,
  nameCdErrorSyntax,
  nameCdErrorTask,
  parseNameCdErrorAnswer,
  renameInstance,
  showNameCdErrorAnswer,
  ) where

import qualified Modelling.CdOd.CdAndChanges.Transform as Changes (
  transformGetNextFix,
  )

import qualified Data.Bimap                       as BM (fromList, lookup)
import qualified Data.Map                         as M (
  elems,
  filter,
  fromAscList,
  keys,
  toList,
  )
import qualified Data.Set                         as S (
  (\\),
  filter,
  fromList,
  null,
  toList,
  )

import Modelling.Auxiliary.Common (
  Randomise (randomise),
  RandomiseLayout (randomiseLayout),
  mapIndicesTo,
  shuffleEverything,
  upperToDash,
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
  ClassConfig (..),
  ClassDiagram (..),
  LimitedLinking (..),
  ObjectProperties (..),
  Property (..),
  Relationship (..),
  RelationshipProperties (..),
  associationNames,
  classNames,
  isIllegal,
  maxObjects,
  renameClassesAndRelationshipsInCd,
  reverseAssociation,
  shuffleClassAndConnectionOrder,
  toPropertySet,
  towardsValidProperties,
  )
import Modelling.Types                  (Change (..))

import Control.Applicative              (Alternative)
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
  recoverWith,
  singleChoice,
  singleChoiceSyntax,
  translate,
  translations,
  )
import Control.Monad.Output.Generic     (($>>=))
import Control.Monad.Random
  (MonadRandom, RandT, RandomGen, evalRandT, mkStdGen)
import Control.Monad.Trans.State        (put)
import Data.Aeson.TH                    (Options (..), defaultOptions, deriveJSON)
import Data.Bifunctor                   (second)
import Data.ByteString.UTF8             (fromString, toString)
import Data.Containers.ListUtils        (nubOrd)
import Data.Either.Extra                (eitherToMaybe, fromEither)
import Data.Foldable                    (for_)
import Data.List                        ((\\), delete, partition, singleton)
import Data.Map                         (Map)
import Data.Maybe                       (catMaybes, listToMaybe, mapMaybe)
import Data.Set                         (Set)
import Data.String.Interpolate          (i, iii)
import Data.Yaml                        (decodeEither', encode)
import GHC.Generics                     (Generic)
import System.Random.Shuffle            (shuffleM)
import Text.Parsec                      (parserFail, parserReturn)
import Text.ParserCombinators.Parsec    (Parser, anyToken, many)

data NameCdErrorAnswer = NameCdErrorAnswer {
  reason                      :: Char,
  dueTo                       :: [Int]
  } deriving (Generic, Read, Show)

$(deriveJSON defaultOptions {fieldLabelModifier = upperToDash} ''NameCdErrorAnswer)

data Reason
  = Custom (Map Language String)
  | PreDefined Property
  deriving (Eq, Generic, Read, Show)

isCustom :: Reason -> Bool
isCustom = \case
  Custom {} -> True
  PreDefined {} -> False

data NumberOfReasons = NumberOfReasons {
  customReasons :: Int,
  preDefinedInvalid :: Int,
  preDefinedValid :: Int
  } deriving (Generic, Read, Show)

data NameCdErrorConfig = NameCdErrorConfig {
  allowedProperties           :: AllowedProperties,
  classConfig                 :: ClassConfig,
  maxInstances                :: Maybe Integer,
  objectProperties            :: ObjectProperties,
  possibleReasons             :: [Reason],
  printNames                  :: Bool,
  printNavigations            :: Bool,
  printSolution               :: Bool,
  reasonsPerInstance          :: NumberOfReasons,
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
  possibleReasons = map PreDefined [minBound ..],
  printNames = True,
  printNavigations = True,
  printSolution = False,
  reasonsPerInstance = NumberOfReasons {
    customReasons = 0,
    preDefinedInvalid = length $ filter isIllegal [minBound ..],
    preDefinedValid = length $ filter (not . isIllegal) [minBound ..]
    },
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
  | S.null illegalProperties
  = Just [iii|
      There has to be set at least one illegal property for this task type,
      amend your allowedProperties
      |]
  | any (`notElem` possibleReasons) illegalReasons
  = Just [iii|
      possibleReasons must contain at least #{illegalReasons}
      considering your current configuration.
      Suggested additional reasons are: #{additionalReasons}
      |]
  | customReasons reasonsPerInstance < 0
  = Just "customReasons must not be negative"
  | preDefinedValid reasonsPerInstance < 0
  = Just "preDefinedVvalid must not be negative"
  | preDefinedInvalid reasonsPerInstance < 1
  = Just "preDefinedInvalid must be set at least to one"
  | customReasons reasonsPerInstance > length (filter isCustom possibleReasons)
  = Just [iii|
      customReasons is set higher than the number of Custom reasons
      defined within possibleReasons
      |]
  | preDefinedInvalid reasonsPerInstance > length (filter isIllegal predefined)
  = Just [iii|
      preDefinedInvalid is set higher than the number of PreDefined reasons
      of illegal properties defined within possibleReasons
      |]
  | preDefinedValid reasonsPerInstance
    > length (filter (not . isIllegal) predefined)
  = Just [iii|
      preDefinedValid is set higher than the number of PreDefined reasons
      of legal properties defined within possibleReasons
      |]
  | otherwise
  = checkClassConfigAndChanges classConfig allowedProperties
  where
    predefined = concatMap
      (\case Custom {} -> []; PreDefined x -> [x])
      possibleReasons
    properties = allowedPropertiesToPropertySet allowedProperties
    illegalProperties = S.filter isIllegal properties
    illegalReasons = map PreDefined $ S.toList illegalProperties
    additionalReasons = map PreDefined
      $ S.toList (properties S.\\ illegalProperties)

allowedPropertiesToPropertySet :: AllowedProperties -> Set Property
allowedPropertiesToPropertySet AllowedProperties {..} =
  S.fromList $ catMaybes [
    ifTrue compositionCycles CompositionCycles,
    ifTrue doubleRelationships DoubleRelationships,
    ifTrue inheritanceCycles InheritanceCycles,
    ifTrue reverseInheritances ReverseInheritances,
    ifTrue reverseRelationships ReverseRelationships,
    ifTrue selfInheritances SelfInheritances,
    ifTrue selfRelationships SelfRelationships,
    ifTrue wrongAssociationLimits WrongAssociationLimits,
    ifTrue wrongCompositionLimits WrongCompositionLimits
    ]
  where
    ifTrue x p = if x then Just p else Nothing

type NameCdErrorTaskText = [TaskTextPart NameCdErrorTaskTextElement]

data TaskTextPart element =
  Code String |
  Paragraph [TaskTextPart element] |
  TaskSpecific element |
  Translated (Map Language String)
  deriving (Eq, Foldable, Generic, Read, Show)

data NameCdErrorTaskTextElement =
  IncorrectCd |
  ReasonsList |
  RelationshipsList
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

toTaskText
  :: (OutputMonad m, MonadIO m)
  => NameCdErrorTaskText
  -> FilePath
  -> NameCdErrorInstance
  -> LangM m
toTaskText xs path task =
  for_ xs $ \x -> toTaskTextPart x path task

toTaskTextPart
  :: (OutputMonad m, MonadIO m)
  => TaskTextPart NameCdErrorTaskTextElement
  -> FilePath
  -> NameCdErrorInstance
  -> LangM m
toTaskTextPart output path task = case output of
  Code c -> code c
  Paragraph xs -> paragraph $ for_ xs $ \x -> toTaskTextPart x path task
  TaskSpecific element -> case element of
    IncorrectCd -> image $=<< liftIO $ cacheCd
      (withDirections task)
      (withNames task)
      mempty
      (classDiagram task)
      path
    ReasonsList -> enumerateM (text . singleton)
      $ second (translate . put . snd)
      <$> M.toList (errorReasons task)
    RelationshipsList -> do
      let phrase x y z = translate $ do
            english $ phraseRelation x y z
            german $ phraseRelationDE x y z
          phraseRelationship =
            phrase (withNames task) (withDirections task)
            . (relationships (classDiagram task) !!)
      enumerateM (text . show)
        $ second (phraseRelationship . snd)
        <$> M.toList (relevantRelationships task)
  Translated xs -> translate $ put xs

data NameCdErrorInstance = NameCdErrorInstance {
  -- | maps the value to the index of 'relationships' of the specified
  --   'classDiagram' (starting at @0@).
  relevantRelationships       :: Map Int (Bool, Int),
  classDiagram                :: Cd,
  errorReasons                :: Map Char (Bool, Map Language String),
  showSolution                :: Bool,
  taskText                    :: NameCdErrorTaskText,
  withDirections              :: Bool,
  withNames                   :: Bool
  } deriving (Eq, Generic, Read, Show)

checkNameCdErrorInstance :: NameCdErrorInstance -> Maybe String
checkNameCdErrorInstance NameCdErrorInstance {..}
  | 1 /= length (filter fst $ M.elems errorReasons)
  = Just [iii|
      There needs to be exactly one error defined within errorReasons
      (i.e., set to 'True')
      |]
  | x:_ <- filter (\x -> x < 0 || x >= numberOfRelationships) relationshipsOnly
  = Just [iii|
      The index '#{x}' in 'relevantRelationships' is out of the
      possible range 0..#{numberOfRelationships - 1}.
      |]
  | x:_ <- relationshipsOnly \\ nubOrd relationshipsOnly
  = Just [iii|
      'relevantRelationships' references '#{x}' at least twice
      which is not allowed.
      |]
  | x:_ <- filter (`notElem` letters) $ M.keys errorReasons
  = Just [iii|
      'errorReasons' defines the invalid key character '#{x}' as one option,
      please choose one of #{letters} instead.
      |]
  | x:_ <- reasons \\ nubOrd reasons
  = Just [iii|
      'errorReasons' contains duplicate '#{x}' which is not allowed.
      |]
  | x:_ <- mapMaybe checkTranslation reasons
  = Just $ [i|Problem within 'errorReasons': |] ++ x
  | otherwise
  = checkNameCdErrorTaskText taskText
  where
    letters = ['a' .. 'z'] ++ ['A' .. 'Z']
    numberOfRelationships = length $ relationships classDiagram
    reasons = map snd $ M.elems errorReasons
    relationshipsOnly = map snd (M.elems relevantRelationships)

checkTranslation :: Map Language String -> Maybe String
checkTranslation xs
  | x:_ <- [minBound ..] \\ M.keys xs
  = Just [i|Missing #{x} translation for #{xs}.|]
  | otherwise
  = Nothing

checkTranslations :: TaskTextPart element -> [Maybe String]
checkTranslations output = case output of
  Code {} -> []
  Paragraph xs -> concatMap checkTranslations xs
  TaskSpecific {} -> []
  Translated xs -> [checkTranslation xs]

checkNameCdErrorTaskText :: NameCdErrorTaskText -> Maybe String
checkNameCdErrorTaskText xs
  | x:_ <- allElements \\ usedElements
  = Just [iii|Your task text is incomplete as it is missing '#{x}'.|]
  | x:_ <- usedElements \\ allElements
  = Just [iii|
      Your task text is using '#{x}' at least twice,
      but it should appear exactly once.
      |]
  | x:_ <- concatMap checkTranslations xs
  = ([i|Problem within your task text: |] ++) <$> x
  | otherwise
  = Nothing
  where
    usedElements = concatMap (concatMap singleton) xs
    allElements = [minBound ..]

defaultNameCdErrorTaskText :: NameCdErrorTaskText
defaultNameCdErrorTaskText = [
  Paragraph $ singleton $ Translated $ translations $ do
    english "Consider the following class diagram, which unfortunately is invalid:"
    german "Betrachten Sie folgendes Klassendiagramm, welches leider ungültig ist:",
  Paragraph $ singleton $ TaskSpecific IncorrectCd,
  Paragraph $ singleton $ Translated $ translations $ do
    english "It contains the following relationships between classes:"
    german "Es enthält die folgenden Beziehungen zwischen Klassen:",
  Paragraph $ singleton $ TaskSpecific RelationshipsList,
  Paragraph $ singleton $ Translated $ translations $ do
    english [iii|
      Choose what you think is the single reason that this class diagram is incorrect,
      and mention all relationships that definitely contribute to the problem,
      i.e., removing any of them would fix the problem.
      |]
    german [iii|
      Wählen Sie aus, was Sie für den einen Grund dafür halten,
      dass dieses Klassendiagramm ungültig ist,
      und nennen Sie alle Beziehungen, die definitiv zum Problem beitragen,
      d.h., deren Entfernung das Problem jeweils beheben würde.
      |],
  Paragraph $ singleton $ Translated $ translations $ do
    english [i|Possible reasons are:|]
    german [i|Mögliche Gründe sind:|],
  Paragraph $ singleton $ Translated $ translations $ do
    english [i|The class diagram ...|]
    german [i|Das Klassendiagramm ...|],
  Paragraph $ singleton $ TaskSpecific ReasonsList,
  Paragraph [
    Paragraph $ singleton $ Translated $ translations $ do
      english [iii|
        Please state your answer by providing a letter for the reason,
        indicating the most specifically expressed reason
        for which you think this class diagram is invalid,
        and a list of numbers for those relationships
        on whose individual presence the problem depends.
        E.g.
        |]
      german [iii|
        Bitte geben Sie Ihre Antwort an, indem Sie Folgendes angeben:
        einen Buchstaben für den Grund,
        der Ihrer Meinung nach der am spezifischsten
        ausgedrückte Grund dafür ist,
        dass dieses Klassendiagramm ungültig ist,
        und eine Liste von Zahlen für diejenigen Beziehungen,
        von deren individueller Präsenz das Problem abhängt.
        Zum Beispiel
        |],
    Paragraph $ singleton $ Code $ showNameCdErrorAnswer answer,
    Paragraph $ singleton $ Translated $ translations $ do
      english [iii|
        would indicate that the class diagram is invalid
        because of reason #{singleton $ reason answer}
        and that the relationships #{dueTo1} and #{dueTo2}
        make the problem appear.
        |]
      german [iii|
        würde bedeuten, dass das Klassendiagramm wegen Grund #{singleton $ reason answer} ungültig ist
        und dass die Beziehungen #{dueTo1} und #{dueTo2}
        das Problem erschaffen.
        |]
    ]
  ]
  where
    answer = defaultNameCdErrorAnswer

nameCdErrorTask
  :: (OutputMonad m, MonadIO m)
  => FilePath
  -> NameCdErrorInstance
  -> LangM m
nameCdErrorTask path task = do
  toTaskText (taskText task) path task
  paragraph simplifiedInformation
  paragraph hoveringInformation
  pure ()

dueTo1 :: Int
dueTo1 = 3

dueTo2 :: Int
dueTo2 = 4

defaultNameCdErrorAnswer :: NameCdErrorAnswer
defaultNameCdErrorAnswer = NameCdErrorAnswer {
  reason = 'b',
  dueTo = [dueTo1, dueTo2]
  }

showNameCdErrorAnswer :: NameCdErrorAnswer -> String
showNameCdErrorAnswer = toString . encode

parseNameCdErrorAnswer :: Parser NameCdErrorAnswer
parseNameCdErrorAnswer = do
  xs <- many anyToken
  case decodeEither' $ fromString xs of
    Left e -> parserFail $ show e
    Right r -> parserReturn r

nameCdErrorSyntax
  :: OutputMonad m
  => NameCdErrorInstance
  -> NameCdErrorAnswer
  -> LangM m
nameCdErrorSyntax inst x = do
  paragraph $ translate $ do
    english "Feedback on chosen reason:"
    german "Hinweis zum gewählten Grund:"
  singleChoiceSyntax False (M.keys $ errorReasons inst) $ reason x
  paragraph $ translate $ do
    english "Feedback on chosen relationships:"
    german "Hinweis zu gewählten Beziehungen:"
  for_
    (dueTo x)
    $ singleChoiceSyntax False (M.keys $ relevantRelationships inst)
  pure ()

{-| Grading is done the following way:

 * 0 points if the reason is wrong
 * otherwise, multiple choice grading for answer on dueTo relationships
-}
nameCdErrorEvaluation
  :: (Alternative m, Monad m, OutputMonad m)
  => NameCdErrorInstance
  -> NameCdErrorAnswer
  -> Rated m
nameCdErrorEvaluation inst x = addPretext $ do
  let reasonTranslation = M.fromAscList [
        (English, "reason"),
        (German, "Grund")
        ]
      dueToTranslation = M.fromAscList [
        (English, "relationships constituting the problem"),
        (German, "das Problem ausmachende Beziehungen")
        ]
      solutionReason = head . M.keys . M.filter fst $ errorReasons inst
      solutionDueTo = fst <$> relevantRelationships inst
      correctAnswer
        | showSolution inst = Just $ toString $ encode $ nameCdErrorSolution inst
        | otherwise = Nothing
  recoverWith 0 (
    singleChoice reasonTranslation Nothing solutionReason (reason x)
    $>>= \p ->
      if p < 1
      then pure 0
      else multipleChoice
        dueToTranslation
        Nothing
        solutionDueTo
        (dueTo x)
    )
    $>>= printSolutionAndAssert correctAnswer . fromEither

nameCdErrorSolution :: NameCdErrorInstance -> NameCdErrorAnswer
nameCdErrorSolution x = NameCdErrorAnswer {
  reason = head . M.keys . M.filter fst $ errorReasons x,
  dueTo = M.keys . M.filter fst $ relevantRelationships x
  }

classAndAssocNames :: NameCdErrorInstance -> ([String], [String])
classAndAssocNames inst =
  let cd = classDiagram inst
      names = nubOrd $ classNames cd
      assocs = nubOrd $ associationNames cd
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
    mapping <- BM.fromList
      <$> mapIndicesTo (relationships classDiagram) (relationships cd)
    relevant <- traverse (traverse (`BM.lookup` mapping)) relevantRelationships
    return NameCdErrorInstance {
      relevantRelationships = relevant,
      classDiagram = cd,
      errorReasons = errorReasons,
      showSolution = showSolution,
      taskText = taskText,
      withDirections = withDirections,
      withNames = withNames
      }

shuffleInstance :: MonadRandom m => NameCdErrorInstance -> m NameCdErrorInstance
shuffleInstance NameCdErrorInstance {..} = do
  chs <- M.fromAscList . zip [1..] <$> shuffleM (M.elems relevantRelationships)
  rs <- M.fromAscList . zip ['a' ..] <$> shuffleM (M.elems errorReasons)
  return $ NameCdErrorInstance {
    relevantRelationships = chs,
    classDiagram = classDiagram,
    errorReasons = rs,
    showSolution = showSolution,
    taskText = taskText,
    withDirections = withDirections,
    withNames = withNames
    }

renameInstance
  :: MonadThrow m
  => NameCdErrorInstance
  -> [String]
  -> [String]
  -> m NameCdErrorInstance
renameInstance inst@NameCdErrorInstance {..} names' assocs' = do
  let (names, assocs) = classAndAssocNames inst
      bmNames  = BM.fromList $ zip names names'
      bmAssocs = BM.fromList $ zip assocs assocs'
      renameCd = renameClassesAndRelationshipsInCd bmNames bmAssocs
  cd <- renameCd classDiagram
  return $ NameCdErrorInstance {
    relevantRelationships = relevantRelationships,
    classDiagram = cd,
    errorReasons = errorReasons,
    showSolution = showSolution,
    taskText = taskText,
    withDirections = withDirections,
    withNames = withNames
    }

nameCdErrorGenerate
  :: NameCdErrorConfig
  -> Int
  -> Int
  -> IO NameCdErrorInstance
nameCdErrorGenerate NameCdErrorConfig {..} segment seed = do
  let g = mkStdGen $ (segment +) $ 4 * seed
  (cd, reason, rs) <- flip evalRandT g $ nameCdError
    allowedProperties
    classConfig
    objectProperties
    maxInstances
    timeout
  reasons <- shuffleM possibleReasons
  let (custom, predefined) = partition isCustom $ delete (PreDefined reason) reasons
      (invalid, valid) = partition
        (\case (PreDefined x) -> isIllegal x; Custom {} -> False)
        predefined
      chosenReasons = take (customReasons reasonsPerInstance) custom
        ++ take (preDefinedInvalid reasonsPerInstance - 1) invalid
        ++ take (preDefinedValid reasonsPerInstance) valid
  shuffleEverything $ NameCdErrorInstance {
    relevantRelationships = M.fromAscList $ zipWith
      (\x r -> (x, (r, x - 1)))
      [1..]
      $ map (`elem` rs) $ relationships cd,
    classDiagram = cd,
    errorReasons = M.fromAscList $ zip ['a' ..]
      $ map (second toTranslations)
      $ (True, PreDefined reason)
      : map (False,) chosenReasons,
    showSolution = printSolution,
    taskText = defaultNameCdErrorTaskText,
    withDirections = printNavigations,
    withNames = printNames
    }
  where
    toTranslations x = case x of
      Custom y -> y
      PreDefined y -> translateProperty printNavigations y

nameCdError
  :: RandomGen g
  => AllowedProperties
  -> ClassConfig
  -> ObjectProperties
  -> Maybe Integer
  -> Maybe Int
  -> RandT g IO (Cd, Property, [Relationship String String])
nameCdError allowed config objectProperties maxInsts to = do
  changes <- shuffleM $ (,)
    <$> illegalChanges allowed
    <*> legalChanges allowed
  getInstanceWithChanges changes
  where
    getInstanceWithChanges [] =
      error "there seems to be no instance for the provided configuration"
    getInstanceWithChanges ((e0, l0) : chs) = do
      let p = toProperty $ e0 .&. l0
          alloyCode = Changes.transformGetNextFix Nothing config p
      instas  <- liftIO $ getInstances maxInsts to alloyCode
      rinstas <- shuffleM instas
      getInstanceWithODs chs p rinstas
    getInstanceWithODs chs _ [] = getInstanceWithChanges chs
    getInstanceWithODs chs p (rinsta:rinstas) = do
      cdInstance <- liftIO $ getChangesAndCds rinsta
      let cd = instanceClassDiagram cdInstance
          p' = p {
            hasCompositionsPreventingParts = Nothing,
            hasDoubleRelationships = Nothing,
            hasReverseRelationships = Nothing,
            hasMultipleInheritances = Nothing
            }
          alloyCode = Changes.transformGetNextFix (Just cd) config p'
      instas <- liftIO $ getInstances Nothing to alloyCode
      correctInstance <- liftIO $ mapM getChangesAndCds instas
      let allChs = concatMap instanceChangesAndCds correctInstance
          cd2 = instanceClassDiagram $ head correctInstance
      mremoves <- do
        liftIO $ mapM (getOD . changeClassDiagram) allChs
        return $ traverse (remove . relationshipChange) allChs
      case mremoves of
        Nothing -> getInstanceWithODs chs p rinstas
        Just removes ->
          let fixes = toPropertySet p
                S.\\ toPropertySet (towardsValidProperties p)
          in case S.toList fixes of
            [problem] -> return (cd2, problem, removes)
            _ -> error "error in task type: property fix is not unique"
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

translateProperty :: Bool -> Property -> Map Language String
translateProperty True x = translatePropertyWithDirections x
translateProperty False x = case x of
  DoubleRelationships -> doubleOrReverseRelationships
  _ -> translatePropertyWithDirections x
  where
    doubleOrReverseRelationships = translations $ do
      english [iii|
        contains at least two non-inheritance relationships
        between the same two classes.
        |]
      german [iii|
        enthält mindestens zwei Nicht-Vererbungs-Beziehungen
        zwischen denselben beiden Klassen.
        |]

translatePropertyWithDirections :: Property -> Map Language String
translatePropertyWithDirections x = translations $ case x of
  CompositionCycles -> do
    english "contains at least one composition cycle."
    german "enthält mindestens einen Kompositionszyklus."
  DoubleRelationships -> do
    english [iii|
      contains at least two non-inheritance relationships
      between the same two classes each pointing in the same direction.
      |]
    german [iii|
      enthält mindestens zwei Nicht-Vererbungs-Beziehungen
      zwischen denselben beiden Klassen, die in dieselbe Richtung zeigen.
      |]
  InheritanceCycles -> do
    english "contains at least one inheritance cycle."
    german "enthält mindestens einen Vererbungszyklus."
  MultipleInheritances -> do
    english "contains at least one multiple inheritance."
    german "enthält mindestens eine Mehrfachvererbung."
  ReverseInheritances -> do
    english "contains at least one pair of classes each inheriting from the other."
    german [iii|
      enthält mindestens ein Paar von Klassen, die sich gegenseiting beerben.
      |]
  ReverseRelationships -> do
    english [iii|
      contains at least two non-inheritance relationships
      between the same two classes pointing in opposite directions.
      |]
    german [iii|
      enthält mindestens zwei Nicht-Vererbungs-Beziehungen
      zwischen denselben beiden Klassen,
      die in entgegengesetzte Richtungen zeigen.
      |]
  SelfInheritances -> do
    english "contains at least one self-inheritance."
    german "enthält mindestens eine Selbstvererbung."
  SelfRelationships -> do
    english "contains at least one self-relationship that is no inheritance."
    german "enthält mindestens eine Selbstbeziehung, die keine Vererbung ist."
  WrongAssociationLimits -> do
    english "contains at least one invalid multiplicity at some relationship."
    german "enthält mindestens eine ungültige Multiplizität an einer Beziehung."
  WrongCompositionLimits -> do
    english [iii|
      contains at least one invalid multiplicity near the whole of a composition.
      |]
    german [iii|
      enthält mindestens eine ungültige Multiplizität am Ganzen einer Komposition.
      |]

defaultNameCdErrorInstance :: NameCdErrorInstance
defaultNameCdErrorInstance = NameCdErrorInstance {
  relevantRelationships = M.fromAscList [
    (1, (True, 3)),
    (2, (True, 2)),
    (3, (False, 0)),
    (4, (True, 1))
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
    ('a', (False, M.fromAscList [
      (English, "contains at least one self-inheritance."),
      (German, "enthält mindestens eine Selbstvererbung.")
      ])),
    ('b', (False, M.fromAscList [
      (English, "contains at least one multiple inheritance."),
      (German, "enthält mindestens eine Mehrfachvererbung.")
      ])),
    ('c', (False, M.fromAscList [
      (English, "contains at least one self-relationship that is no inheritance."),
      (German, "enthält mindestens eine Selbstbeziehung, die keine Vererbung ist.")
      ])),
    ('d', (False, M.fromAscList [
      (English, "contains at least two non-inheritance relationships "
        ++ "between the same two classes pointing in opposite directions."),
      (German, "enthält mindestens zwei Nicht-Vererbungs-Beziehungen "
        ++ "zwischen denselben beiden Klassen, die in entgegengesetzte Richtungen zeigen.")
      ])),
    ('e', (False, M.fromAscList [
      (English, "contains at least two non-inheritance relationships "
        ++ "between the same two classes each pointing in the same direction."),
      (German, "enthält mindestens zwei Nicht-Vererbungs-Beziehungen "
        ++ "zwischen denselben beiden Klassen, die in dieselbe Richtung zeigen.")
      ])),
    ('f', (True, M.fromAscList [
      (English, "contains at least one composition cycle."),
      (German, "enthält mindestens einen Kompositionszyklus.")
      ])),
    ('g', (False, M.fromAscList [
      (English, "contains at least one inheritance cycle."),
      (German, "enthält mindestens einen Vererbungszyklus.")
      ])),
    ('h', (False, M.fromAscList [
      (English, "contains at least one invalid multiplicity "
        ++ "near the whole of a composition."),
      (German, "enthält mindestens eine ungültige Multiplizität "
        ++ "am Ganzen einer Komposition.")
      ])),
    ('i', (False, M.fromAscList [
      (English, "contains at least one pair of classes each inheriting from the other."),
      (German, "enthält mindestens ein Paar von Klassen, die sich gegenseiting beerben.")
      ])),
    ('j', (False, M.fromAscList [
      (English, "contains at least one invalid multiplicity at some relationship."),
      (German, "enthält mindestens eine ungültige Multiplizität an einer Beziehung.")
      ]))
    ],
  showSolution = False,
  taskText = defaultNameCdErrorTaskText,
  withDirections = True,
  withNames = True
  }
