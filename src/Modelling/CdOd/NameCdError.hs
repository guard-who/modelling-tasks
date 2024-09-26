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
  Relevance (..),
  TaskTextPart (..),
  checkNameCdErrorConfig,
  checkNameCdErrorInstance,
  classAndNonInheritanceNames,
  defaultNameCdErrorAnswer,
  defaultNameCdErrorConfig,
  defaultNameCdErrorInstance,
  isRelevant,
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

import qualified Data.Bimap                       as BM (fromList)
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

import Capabilities.Alloy               (MonadAlloy, getInstances)
import Capabilities.Cache               (MonadCache)
import Capabilities.Diagrams            (MonadDiagrams)
import Capabilities.Graphviz            (MonadGraphviz)
import Modelling.Auxiliary.Common (
  Randomise (randomise),
  RandomiseLayout (randomiseLayout),
  shuffleEverything,
  upperToDash,
  )
import Modelling.Auxiliary.Output (
  addPretext,
  hoveringInformation,
  simplifiedInformation,
  )
import Modelling.CdOd.Auxiliary.Util    (alloyInstanceToOd)
import Modelling.CdOd.CD2Alloy.Transform (
  combineParts,
  createRunCommand,
  transform,
  )
import Modelling.CdOd.CdAndChanges.Instance (
  ChangeAndCd (..),
  GenericClassDiagramInstance (..),
  fromInstance,
  fromInstanceWithPredefinedNames,
  nameClassDiagramInstance,
  )
import Modelling.CdOd.Output (
  cacheCd,
  )
import Modelling.CdOd.Phrasing (
  phraseRelationship,
  )
import Modelling.CdOd.RepairCd (
  (.&.),
  checkClassConfigAndChanges,
  illegalChanges,
  legalChanges,
  toProperty,
  )
import Modelling.CdOd.Types (
  AllowedProperties (..),
  Annotation (..),
  AnnotatedCd,
  AnnotatedClassDiagram (..),
  AnyCd,
  AnyClassDiagram (..),
  AnyRelationship,
  ArticlePreference (..),
  CdDrawSettings (..),
  ClassConfig (..),
  ClassDiagram (..),
  LimitedLinking (..),
  ObjectProperties (..),
  Property (..),
  Relationship (..),
  RelationshipProperties (..),
  allowEverything,
  anonymiseObjects,
  anyAssociationNames,
  checkCdDrawSettings,
  checkObjectProperties,
  classNames,
  defaultCdDrawSettings,
  isIllegal,
  maxObjects,
  renameClassesAndRelationships,
  reverseAssociation,
  shuffleAnnotatedClassAndConnectionOrder,
  toArticleToUse,
  toPropertySet,
  toValidCd,
  towardsValidProperties,
  unannotateCd,
  )
import Modelling.Types                  (Change (..))

import Control.Applicative              (Alternative ((<|>)))
import Control.Monad                    ((>=>), forM, join)
import Control.Monad.Catch              (MonadThrow)
import Control.Monad.Except             (runExceptT)
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
  printSolutionAndAssert,
  recoverWith,
  singleChoice,
  singleChoiceSyntax,
  translate,
  translations,
  )
import Control.OutputCapable.Blocks.Generic (($>>=))
import Control.Monad.Random
  (MonadRandom, RandT, RandomGen, evalRandT, mkStdGen)
import Control.Monad.Trans.Class        (MonadTrans (lift))
import Control.Monad.Trans.State        (put)
import Data.Aeson.TH                    (Options (..), defaultOptions, deriveJSON)
import Data.Bifunctor                   (second)
import Data.ByteString.UTF8             (fromString, toString)
import Data.Containers.ListUtils        (nubOrd)
import Data.Either.Extra                (eitherToMaybe, fromEither)
import Data.Foldable                    (for_)
import Data.List (
  (\\),
  delete,
  partition,
  singleton,
  sortOn,
  )
import Data.Map                         (Map)
import Data.Maybe                       (catMaybes, listToMaybe, mapMaybe)
import Data.Ratio                       ((%))
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
  -- | the article preference when referring to relationships
  articleToUse                :: ArticlePreference,
  classConfig                 :: ClassConfig,
  drawSettings                :: !CdDrawSettings,
  maxInstances                :: Maybe Integer,
  objectProperties            :: ObjectProperties,
  possibleReasons             :: [Reason],
  printSolution               :: Bool,
  reasonsPerInstance          :: NumberOfReasons,
  timeout                     :: Maybe Int,
  useNames                    :: Bool
  } deriving (Generic, Read, Show)

defaultNameCdErrorConfig :: NameCdErrorConfig
defaultNameCdErrorConfig = NameCdErrorConfig {
  allowedProperties = allowEverything {
    reverseInheritances = False,
    selfInheritances = False
    },
  articleToUse = UseDefiniteArticleWherePossible,
  classConfig = ClassConfig {
    classLimits = (4, 4),
    aggregationLimits = (1, Just 1),
    associationLimits = (0, Just 1),
    compositionLimits = (2, Just 3),
    inheritanceLimits = (0, Just 0),
    relationshipLimits = (3, Just 5)
    },
  drawSettings = defaultCdDrawSettings,
  maxInstances = Just 200,
  objectProperties = ObjectProperties {
    anonymousObjectProportion = 0 % 1,
    completelyInhabited = Just True,
    hasLimitedIsolatedObjects = True,
    hasSelfLoops = Nothing,
    usesEveryRelationshipName = Just True
    },
  possibleReasons = map PreDefined [minBound ..],
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
  | not (printNames drawSettings) && useNames
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
  = Just "preDefinedValid must not be negative"
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
  <|> checkObjectProperties objectProperties
  <|> checkCdDrawSettings drawSettings
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
    ifTrue invalidInheritanceLimits InvalidInheritanceLimits,
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
  :: (MonadCache m, MonadDiagrams m, MonadGraphviz m, OutputCapable m)
  => NameCdErrorTaskText
  -> FilePath
  -> NameCdErrorInstance
  -> LangM m
toTaskText xs path task =
  for_ xs $ \x -> toTaskTextPart x path task

toTaskTextPart
  :: (MonadCache m, MonadDiagrams m, MonadGraphviz m, OutputCapable m)
  => TaskTextPart NameCdErrorTaskTextElement
  -> FilePath
  -> NameCdErrorInstance
  -> LangM m
toTaskTextPart output path task@NameCdErrorInstance {..} = case output of
  Code c -> code c
  Paragraph xs -> paragraph $ for_ xs $ \x -> toTaskTextPart x path task
  TaskSpecific element -> case element of
    IncorrectCd -> image $=<< cacheCd
      cdDrawSettings
      mempty
      (unannotateCd classDiagram)
      path
    ReasonsList -> enumerateM (text . singleton)
      $ second (translate . put . snd)
      <$> M.toList errorReasons
    RelationshipsList -> do
      let phrase article x y z = translate $ do
            english $ phraseRelationship English article x y z
            german $ phraseRelationship German article x y z
          phraseRelationship' Annotation {..} = phrase
            (referenceUsing annotation)
            byName
            (printNavigations cdDrawSettings)
            annotated
      enumerateM (text . show)
        $ map (second phraseRelationship')
        $ relevantRelationships task
  Translated xs -> translate $ put xs

data NameCdErrorInstance = NameCdErrorInstance {
  byName                      :: !Bool,
  classDiagram                :: AnnotatedCd Relevance,
  cdDrawSettings              :: !CdDrawSettings,
  errorReasons                :: Map Char (Bool, Map Language String),
  showSolution                :: Bool,
  taskText                    :: !NameCdErrorTaskText
  } deriving (Eq, Generic, Read, Show)

relevantRelationships
  :: NameCdErrorInstance
  -> [(Int, Annotation Relevance (AnyRelationship String String))]
relevantRelationships NameCdErrorInstance {..} = zip [1..]
  . sortOn (listingPriority . annotation)
  . filter isRelevant
  $ annotatedRelationships classDiagram

data Relevance
  = NotRelevant
  | Relevant {
    contributingToProblem     :: Bool,
    -- | for 'randomiseLayout' to work without influencing
    --   the order of the relationships in the list of relevant relationships
    --   each priority has to be unique for each class diagram
    --
    --   * the order applies to the list of relevant relationships
    --   * a lower number means higher priority
    listingPriority           :: Int,
    referenceUsing            :: ArticleToUse
    }
  deriving (Eq, Generic, Read, Show)

isRelevant :: Annotation Relevance annotated -> Bool
isRelevant =
  (\case NotRelevant -> False; Relevant {} -> True)
  . annotation

checkNameCdErrorInstance :: NameCdErrorInstance -> Maybe String
checkNameCdErrorInstance NameCdErrorInstance {..}
  | not (printNames cdDrawSettings) && byName
  = Just "by name is only possible when printing names"
  | 1 /= length (filter fst $ M.elems errorReasons)
  = Just [iii|
      There needs to be exactly one error defined within errorReasons
      (i.e., set to 'True')
      |]
  | x:_ <- listingPriorities \\ nubOrd listingPriorities
  = Just [iii|
      'listingPriority' has to be unique for the class diagram,
      but '#{x}' appears twice which is not allowed.
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
  <|> checkCdDrawSettings cdDrawSettings
  where
    letters = ['a' .. 'z'] ++ ['A' .. 'Z']
    reasons = map snd $ M.elems errorReasons
    listingPriorities = map (listingPriority . annotation)
      . filter isRelevant
      $ annotatedRelationships classDiagram

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
        For example,
        |]
      german [iii|
        Bitte geben Sie Ihre Antwort an, indem Sie Folgendes angeben:
        einen Buchstaben für den Grund,
        der Ihrer Meinung nach der am spezifischsten
        ausgedrückte Grund dafür ist,
        dass dieses Klassendiagramm ungültig ist,
        und eine Liste von Zahlen für diejenigen Beziehungen,
        von deren individueller Präsenz das Problem abhängt.
        Zum Beispiel würde
        |],
    Paragraph $ singleton $ Code $ showNameCdErrorAnswer answer,
    Paragraph $ singleton $ Translated $ translations $ do
      english [iii|
        would indicate that the class diagram is invalid
        because of reason #{singleton $ reason answer}
        and that the #{dueTo1}. and #{dueTo2}. relationship
        make the problem appear.
        |]
      german [iii|
        bedeuten, dass das Klassendiagramm wegen Grund #{singleton $ reason answer} ungültig ist
        und dass die #{dueTo1}. und #{dueTo2}. Beziehung
        das Problem erschaffen.
        |]
    ]
  ]
  where
    answer = defaultNameCdErrorAnswer

nameCdErrorTask
  :: (MonadCache m, MonadDiagrams m, MonadGraphviz m, OutputCapable m)
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
  :: OutputCapable m
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
  multipleChoiceSyntax False (map fst $ relevantRelationships inst) (dueTo x)
  pure ()

{-| Grading is done the following way:

 * 0 points if the reason is wrong
 * otherwise, multiple choice grading for answer on dueTo relationships
-}
nameCdErrorEvaluation
  :: (Alternative m, Monad m, OutputCapable m)
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
        (German, "das Problem ausmachenden Beziehungen")
        ]
      solutionReason = head . M.keys . M.filter fst $ errorReasons inst
      solutionDueTo = M.fromAscList
        $ map (second (contributingToProblem . annotation))
        $ relevantRelationships inst
      correctAnswer
        | showSolution inst = Just $ toString $ encode $ nameCdErrorSolution inst
        | otherwise = Nothing
  recoverWith 0 (
    singleChoice DefiniteArticle reasonTranslation Nothing solutionReason (reason x)
    $>>= \p ->
      if p < 1
      then pure 0
      else multipleChoice DefiniteArticle
        dueToTranslation
        Nothing
        solutionDueTo
        (dueTo x)
    )
    $>>= printSolutionAndAssert DefiniteArticle correctAnswer . fromEither

nameCdErrorSolution :: NameCdErrorInstance -> NameCdErrorAnswer
nameCdErrorSolution x = NameCdErrorAnswer {
  reason = head . M.keys . M.filter fst $ errorReasons x,
  dueTo = map fst
    . filter (contributingToProblem . annotation . snd)
    $ relevantRelationships x
  }

classAndNonInheritanceNames :: NameCdErrorInstance -> ([String], [String])
classAndNonInheritanceNames inst =
  let cd = unannotateCd $ classDiagram inst
      names = nubOrd $ anyClassNames cd
      nonInheritances = nubOrd $ anyAssociationNames cd
  in (names, nonInheritances)

instance Randomise NameCdErrorInstance where
  randomise inst = do
    let (names, nonInheritances) = classAndNonInheritanceNames inst
    names' <- shuffleM names
    nonInheritances' <- shuffleM nonInheritances
    renameInstance inst names' nonInheritances'
      >>= shuffleInstance

instance RandomiseLayout NameCdErrorInstance where
  randomiseLayout NameCdErrorInstance {..} = do
    cd <- shuffleAnnotatedClassAndConnectionOrder classDiagram
    return NameCdErrorInstance {
      byName = byName,
      cdDrawSettings = cdDrawSettings,
      classDiagram = cd,
      errorReasons = errorReasons,
      showSolution = showSolution,
      taskText = taskText
      }

shuffleInstance :: MonadRandom m => NameCdErrorInstance -> m NameCdErrorInstance
shuffleInstance NameCdErrorInstance {..} = do
  priorities <- shuffleM
    $ map (listingPriority . annotation)
    $ filter isRelevant
    $ annotatedRelationships classDiagram
  rs <- M.fromAscList . zip ['a' ..] <$> shuffleM (M.elems errorReasons)
  return $ NameCdErrorInstance {
    byName = byName,
    cdDrawSettings = cdDrawSettings,
    classDiagram = classDiagram {
      annotatedRelationships = snd $ foldr
        updatePriority
        (priorities, [])
        (annotatedRelationships classDiagram)
      },
    errorReasons = rs,
    showSolution = showSolution,
    taskText = taskText
    }
  where
    updatePriority x (priorities, ys) = case x of
      Annotation {annotation = NotRelevant} -> (priorities, x : ys)
      Annotation {..} -> (tail priorities,) $
        Annotation {
          annotation = annotation {listingPriority = head priorities},
          annotated = annotated
          }
        : ys

renameInstance
  :: MonadThrow m
  => NameCdErrorInstance
  -> [String]
  -> [String]
  -> m NameCdErrorInstance
renameInstance inst@NameCdErrorInstance {..} names' nonInheritances' = do
  let (names, nonInheritances) = classAndNonInheritanceNames inst
      bmNames  = BM.fromList $ zip names names'
      bmNonInheritances = BM.fromList $ zip nonInheritances nonInheritances'
      renameCd = renameClassesAndRelationships bmNames bmNonInheritances
  cd <- renameCd classDiagram
  return $ NameCdErrorInstance {
    byName = byName,
    cdDrawSettings = cdDrawSettings,
    classDiagram = cd,
    errorReasons = errorReasons,
    showSolution = showSolution,
    taskText = taskText
    }

nameCdErrorGenerate
  :: (MonadAlloy m, MonadThrow m)
  => NameCdErrorConfig
  -> Int
  -> Int
  -> m NameCdErrorInstance
nameCdErrorGenerate config segment seed = do
  let g = mkStdGen $ (segment +) $ 4 * seed
  flip evalRandT g $ generateAndRandomise config

generateAndRandomise
  :: (MonadAlloy m, MonadThrow m, RandomGen g)
  => NameCdErrorConfig
  -> RandT g m NameCdErrorInstance
generateAndRandomise NameCdErrorConfig {..} = do
  (cd, reason, rs) <- nameCdError
    allowedProperties
    classConfig
    objectProperties
    useNames
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
    byName = printNames drawSettings && useNames,
    cdDrawSettings = drawSettings,
    classDiagram = AnnotatedClassDiagram {
      annotatedClasses = anyClassNames cd,
      annotatedRelationships = zipWith
        (relevanceFor rs)
        [1..]
        (anyRelationships cd)
      },
    errorReasons = M.fromAscList $ zip ['a' ..]
      $ map (second toTranslations)
      $ (True, PreDefined reason)
      : map (False,) chosenReasons,
    showSolution = printSolution,
    taskText = defaultNameCdErrorTaskText
    }
  where
    relevanceFor xs n x = Annotation {
      annotated = x,
      annotation = Relevant {
        contributingToProblem = x `elem` xs,
        listingPriority = n,
        referenceUsing = toArticleToUse articleToUse
        }
      }
    toTranslations x = case x of
      Custom y -> y
      PreDefined y -> translateProperty (printNavigations drawSettings) y

nameCdError
  :: (MonadAlloy m, MonadThrow m, RandomGen g)
  => AllowedProperties
  -> ClassConfig
  -> ObjectProperties
  -> Bool
  -> Maybe Integer
  -> Maybe Int
  -> RandT g m (AnyCd, Property, [AnyRelationship String String])
nameCdError allowed config objectProperties byName maxInstances to = do
  changes <- shuffleM $ (,)
    <$> illegalChanges allowed
    <*> legalChanges allowed
  getInstanceWithChanges changes
  where
    getInstanceWithChanges [] =
      error "there seems to be no instance for the provided configuration"
    getInstanceWithChanges ((e0, l0) : chs) = do
      let p = toProperty $ e0 .&. l0
          alloyCode = Changes.transformGetNextFix Nothing config p allowed byName
      instances <- lift $ getInstances maxInstances to alloyCode
      randomInstances <- shuffleM instances
      getInstanceWithODs chs p randomInstances
    getInstanceWithODs chs _ [] = getInstanceWithChanges chs
    getInstanceWithODs chs p (randomInstance:randomInstances) = do
      cdInstance <- lift
        $ fromInstance randomInstance
        >>= nameClassDiagramInstance
      let cd = instanceClassDiagram cdInstance
          p' = p {
            hasCompositionsPreventingParts = Nothing,
            hasDoubleRelationships = Nothing,
            hasReverseRelationships = Nothing,
            hasMultipleInheritances = Nothing
            }
          alloyCode =
            Changes.transformGetNextFix (Just cd) config p' allowed byName
      instances <- lift $ getInstances Nothing to alloyCode
      correctInstance <- lift $ mapM fromInstanceWithPredefinedNames instances
      let allChs = concatMap instanceChangesAndCds correctInstance
          cd2 = instanceClassDiagram $ head correctInstance
      possibleRemoves <- do
        validCds <- lift $ mapM (toValidCd . changeClassDiagram) allChs
        mapM_ getOD validCds
        return $ traverse (remove . relationshipChange) allChs
      case possibleRemoves of
        Nothing -> getInstanceWithODs chs p randomInstances
        Just removes ->
          let fixes = toPropertySet p
                S.\\ toPropertySet (towardsValidProperties p)
          in case S.toList fixes of
            [problem] -> return (cd2, problem, removes)
            _ -> error "error in task type: property fix is not unique"
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
      od' <- fmap join $ forM od
        $ runExceptT . alloyInstanceToOd >=> return . eitherToMaybe
      mapM (anonymiseObjects (anonymousObjectProportion objectProperties)) od'

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
        enthält mindestens zwei Nicht-Vererbungsbeziehungen
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
      enthält mindestens zwei Nicht-Vererbungsbeziehungen
      zwischen denselben beiden Klassen, die in dieselbe Richtung zeigen.
      |]
  InheritanceCycles -> do
    english "contains at least one inheritance cycle."
    german "enthält mindestens einen Vererbungszyklus."
  InvalidInheritanceLimits -> do
    english "contains at least one invalid multiplicity at some inheritance."
    german [iii|
      enthält mindestens eine nicht erlaubte Multiplizität an einer Vererbung.
      |]
  MultipleInheritances -> do
    english "contains at least one multiple inheritance."
    german "enthält mindestens eine Mehrfachvererbung."
  ReverseInheritances -> do
    english "contains at least one pair of classes each inheriting from the other."
    german [iii|
      enthält mindestens ein Paar von Klassen, die sich gegenseitig beerben.
      |]
  ReverseRelationships -> do
    english [iii|
      contains at least two non-inheritance relationships
      between the same two classes pointing in opposite directions.
      |]
    german [iii|
      enthält mindestens zwei Nicht-Vererbungsbeziehungen
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
    english [iii|
      contains at least one invalid multiplicity at some relationship
      that is no inheritance.
      |]
    german [iii|
      enthält mindestens eine nicht erlaubte Multiplizität an einer Beziehung,
      die keine Vererbung ist.
      |]
  WrongCompositionLimits -> do
    english [iii|
      contains at least one invalid multiplicity near the whole of a composition.
      |]
    german [iii|
      enthält mindestens eine nicht erlaubte Multiplizität am Ganzen einer Komposition.
      |]

defaultNameCdErrorInstance :: NameCdErrorInstance
defaultNameCdErrorInstance = NameCdErrorInstance {
  byName = True,
  cdDrawSettings = defaultCdDrawSettings,
  classDiagram = AnnotatedClassDiagram {
    annotatedClasses = ["D","C","B","A"],
    annotatedRelationships = [
      Annotation {
        annotated = Right Aggregation {
          aggregationName = "y",
          aggregationPart = LimitedLinking {linking = "B", limits = (2, Nothing)},
          aggregationWhole = LimitedLinking {linking = "D", limits = (2, Just 2)}
          },
        annotation = Relevant {
          contributingToProblem = False,
          listingPriority = 3,
          referenceUsing = DefiniteArticle
          }
        },
      Annotation {
        annotated = Right Composition {
          compositionName = "x",
          compositionPart = LimitedLinking {linking = "A", limits = (0, Just 1)},
          compositionWhole = LimitedLinking {linking = "B", limits = (0, Just 1)}
          },
        annotation = Relevant {
          contributingToProblem = True,
          listingPriority = 4,
          referenceUsing = DefiniteArticle
          }
        },
      Annotation {
        annotated = Right Composition {
          compositionName = "w",
          compositionPart = LimitedLinking {linking = "B", limits = (1, Nothing)},
          compositionWhole = LimitedLinking {linking = "D", limits = (1, Just 1)}
          },
        annotation = Relevant {
          contributingToProblem = True,
          listingPriority = 2,
          referenceUsing = DefiniteArticle
          }
        },
      Annotation {
        annotated = Right Composition {
          compositionName = "z",
          compositionPart = LimitedLinking {linking = "D", limits = (1, Nothing)},
          compositionWhole = LimitedLinking {linking = "A", limits = (1, Just 1)}
          },
        annotation = Relevant {
          contributingToProblem = True,
          listingPriority = 1,
          referenceUsing = DefiniteArticle
          }
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
      (German, "enthält mindestens zwei Nicht-Vererbungsbeziehungen "
        ++ "zwischen denselben beiden Klassen, die in entgegengesetzte Richtungen zeigen.")
      ])),
    ('e', (False, M.fromAscList [
      (English, "contains at least two non-inheritance relationships "
        ++ "between the same two classes each pointing in the same direction."),
      (German, "enthält mindestens zwei Nicht-Vererbungsbeziehungen "
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
      (German, "enthält mindestens eine nicht erlaubte Multiplizität "
        ++ "am Ganzen einer Komposition.")
      ])),
    ('i', (False, M.fromAscList [
      (English, "contains at least one pair of classes each inheriting from the other."),
      (German, "enthält mindestens ein Paar von Klassen, die sich gegenseitig beerben.")
      ])),
    ('j', (False, M.fromAscList [
      (English, "contains at least one invalid multiplicity at some relationship."),
      (German, "enthält mindestens eine nicht erlaubte Multiplizität an einer Beziehung.")
      ]))
    ],
  showSolution = False,
  taskText = defaultNameCdErrorTaskText
  }
