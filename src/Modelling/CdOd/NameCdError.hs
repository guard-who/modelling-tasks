{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
import Data.Data                        (Data)
import Modelling.Auxiliary.Common (
  Randomise (randomise),
  RandomiseLayout (randomiseLayout),
  RandomiseNames (randomiseNames),
  upperToDash,
  )
import Modelling.Auxiliary.Output (
  addPretext,
  checkTaskText,
  hoveringInformation,
  simplifiedInformation,
  uniform,
  extra,
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
  illegalStructuralWeakenings,
  legalStructuralWeakenings,
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
  CdConstraints (..),
  CdDrawSettings (..),
  ClassConfig (..),
  ClassDiagram (..),
  LimitedLinking (..),
  ObjectProperties (..),
  PhrasingKind (Denoted),
  Property (..),
  Relationship (..),
  RelationshipProperties (..),
  allowEverything,
  anonymiseObjects,
  anyAssociationNames,
  checkCdConstraints,
  checkCdDrawProperties,
  checkCdDrawSettings,
  checkClassConfigAndObjectProperties,
  checkObjectProperties,
  classNames,
  defaultCdConstraints,
  defaultCdDrawSettings,
  isIllegal,
  maxObjects,
  relationshipName,
  renameClassesAndRelationships,
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
import Control.Monad.Catch              (MonadCatch, MonadThrow)
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
import Control.OutputCapable.Blocks.Generic (($>>), ($>>=))
import Control.OutputCapable.Blocks.Generic.Type (
  GenericOutput (Code, Paragraph, Special, Translated),
  )
import Control.OutputCapable.Blocks.Type (
  SpecialOutput,
  checkTranslation,
  specialToOutputCapable,
  )
import Control.Monad.Random
  (MonadRandom, RandT, RandomGen, evalRandT, mkStdGen)
import Control.Monad.Trans.Class        (MonadTrans (lift))
import Control.Monad.Trans.State        (put)
import Data.Aeson.TH                    (Options (..), defaultOptions, deriveJSON)
import Data.Bifunctor                   (second)
import Data.ByteString.UTF8             (fromString, toString)
import Data.Containers.ListUtils        (nubOrd)
import Data.Either.Extra                (eitherToMaybe, fromEither)
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
  deriving (Data, Eq, Generic, Ord, Read, Show)

isCustom :: Reason -> Bool
isCustom = \case
  Custom {} -> True
  PreDefined {} -> False

renderReason :: OutputCapable m => Bool -> Reason -> LangM m
renderReason withDirections = translate . put . toTranslations withDirections

toTranslations :: Bool -> Reason -> Map Language String
toTranslations withDirections = \case
  Custom x -> x
  PreDefined x -> translateProperty withDirections x

data NumberOfReasons = NumberOfReasons {
  customReasons :: Int,
  preDefinedInvalid :: Int,
  preDefinedValid :: Int
  } deriving (Generic, Read, Show)

data NameCdErrorConfig = NameCdErrorConfig {
  allowedProperties           :: AllowedProperties,
  -- | the article preference when referring to relationships
  articleToUse                :: ArticlePreference,
  cdConstraints               :: !CdConstraints,
  classConfig                 :: ClassConfig,
  drawSettings                :: !CdDrawSettings,
  maxInstances                :: Maybe Integer,
  objectProperties            :: ObjectProperties,
  possibleReasons             :: [Reason],
  printSolution               :: Bool,
  reasonsPerInstance          :: NumberOfReasons,
  timeout                     :: Maybe Int,
  useNames                    :: Bool,
  extraText                   :: Maybe (Map Language String)
  } deriving (Generic, Read, Show)

defaultNameCdErrorConfig :: NameCdErrorConfig
defaultNameCdErrorConfig = NameCdErrorConfig {
  allowedProperties = allowEverything {
    reverseInheritances = False,
    selfInheritances = False
    },
  articleToUse = UseDefiniteArticleWherePossible,
  cdConstraints = defaultCdConstraints,
  classConfig = ClassConfig {
    classLimits = (4, 4),
    aggregationLimits = (1, Just 1),
    associationLimits = (0, Just 1),
    compositionLimits = (2, Just 3),
    inheritanceLimits = (0, Just 0),
    relationshipLimits = (3, Just 5)
    },
  drawSettings = defaultCdDrawSettings {printNames = True},
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
  useNames = True,
  extraText = Nothing
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
  <|> checkCdConstraints allowedProperties cdConstraints
  <|> checkObjectProperties objectProperties
  <|> checkClassConfigAndObjectProperties classConfig objectProperties
  <|> checkCdDrawSettings drawSettings
  <|> checkCdDrawProperties drawSettings allowedProperties
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

type NameCdErrorTaskText = [SpecialOutput NameCdErrorTaskTextElement]

data NameCdErrorTaskTextElement =
  IncorrectCd |
  ReasonsList |
  RelationshipsList
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

toTaskText
  :: (MonadCache m, MonadDiagrams m, MonadGraphviz m, OutputCapable m)
  => FilePath
  -> NameCdErrorInstance
  -> LangM m
toTaskText path task =
  specialToOutputCapable (toTaskSpecificText path task) (taskText task)

toTaskSpecificText
  :: (MonadCache m, MonadDiagrams m, MonadGraphviz m, OutputCapable m)
  => FilePath
  -> NameCdErrorInstance
  -> NameCdErrorTaskTextElement
  -> LangM m
toTaskSpecificText path task@NameCdErrorInstance {..} = \case
    IncorrectCd -> image $=<< cacheCd
      cdDrawSettings
      mempty
      (unannotateCd classDiagram)
      path
    ReasonsList -> enumerateM (text . singleton)
      $ map (second (renderReason (printNavigations cdDrawSettings) . snd))
      $ M.toList errorReasons
    RelationshipsList -> do
      let defaults = omittedDefaults cdDrawSettings
          phrase article x y z = translate $ do
            english $ phraseRelationship English defaults article Denoted x y z
            german $ phraseRelationship German defaults article Denoted x y z
          phraseRelationship' Annotation {..} = phrase
            (referenceUsing annotation)
            byName
            (printNavigations cdDrawSettings)
            annotated
      enumerateM (text . show)
        $ map (second phraseRelationship')
        $ relevantRelationships task

data NameCdErrorInstance = NameCdErrorInstance {
  byName                      :: !Bool,
  classDiagram                :: AnnotatedCd Relevance,
  cdDrawSettings              :: !CdDrawSettings,
  errorReasons                :: !(Map Char (Bool, Reason)),
  showSolution                :: Bool,
  taskText                    :: !NameCdErrorTaskText,
  addText                     :: Maybe (Map Language String)
  } deriving (Data, Eq, Generic, Read, Show)

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
  deriving (Data, Eq, Generic, Read, Show)

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
  | x:_ <- concatMap (checkTranslation . toTranslations True) reasons
  = Just $ [i|Problem within 'errorReasons': |] ++ x
  | otherwise
  = checkTaskText taskText
  <|> checkCdDrawSettings cdDrawSettings
  where
    letters = ['a' .. 'z'] ++ ['A' .. 'Z']
    reasons = map snd $ M.elems errorReasons
    listingPriorities = map (listingPriority . annotation)
      . filter isRelevant
      $ annotatedRelationships classDiagram

defaultNameCdErrorTaskText :: NameCdErrorTaskText
defaultNameCdErrorTaskText = [
  Paragraph $ singleton $ Translated $ translations $ do
    english "Consider the following class diagram, which unfortunately is invalid:"
    german "Betrachten Sie folgendes Klassendiagramm, welches leider ungültig ist:",
  Paragraph $ singleton $ Special IncorrectCd,
  Paragraph $ singleton $ Translated $ translations $ do
    english "It contains the following relationships between classes:"
    german "Es enthält die folgenden Beziehungen zwischen Klassen:",
  Paragraph $ singleton $ Special RelationshipsList,
  Paragraph $ singleton $ Translated $ translations $ do
    english [iii|
      Choose what you think is the single reason that this class diagram is invalid,
      and mention all relationships that definitely contribute to the problem,
      i.e., removing any of them would fix the invalidity.
      |]
    german [iii|
      Wählen Sie aus, was Sie für den einen Grund dafür halten,
      dass dieses Klassendiagramm ungültig ist,
      und nennen Sie alle Beziehungen, die definitiv zum Problem beitragen,
      d.h., deren Entfernung die Ungültigkeit jeweils beheben würde.
      |],
  Paragraph $ singleton $ Translated $ translations $ do
    english [i|Reasons available to choose from are:|]
    german [i|Gründe, die hierfür zur Auswahl stehen, sind:|],
  Paragraph $ singleton $ Translated $ translations $ do
    english [i|The class diagram ...|]
    german [i|Das Klassendiagramm ...|],
  Paragraph $ singleton $ Special ReasonsList,
  Paragraph [
    Paragraph $ singleton $ Translated $ translations $ do
      english [iii|
        Please state your answer by providing a letter for the reason,
        indicating the most specifically expressed reason
        for which you think this class diagram is invalid,
        and a listing of numbers for those relationships
        on whose individual presence the invalidity depends.
        For example,
        |]
      german [iii|
        Bitte geben Sie Ihre Antwort an, indem Sie Folgendes angeben:
        einen Buchstaben für den Grund,
        der Ihrer Meinung nach der am spezifischsten
        ausgedrückte Grund dafür ist,
        dass dieses Klassendiagramm ungültig ist,
        und eine Auflistung von Zahlen für diejenigen Beziehungen,
        von deren individueller Präsenz die Ungültigkeit abhängt.
        Zum Beispiel würde
        |],
    Paragraph $ singleton $ Code $ uniform $ showNameCdErrorAnswer answer,
    Paragraph $ singleton $ Translated $ translations $ do
      english [iii|
        would indicate that the class diagram is invalid
        because of reason #{singleton $ reason answer}
        and that the #{dueTo1}. and #{dueTo2}. relationship (appearing together)
        create the invalidity.
        |]
      german [iii|
        bedeuten, dass das Klassendiagramm wegen Grund #{singleton $ reason answer} ungültig ist
        und dass die #{dueTo1}. und #{dueTo2}. Beziehung (zusammen auftretend)
        die Ungültigkeit erzeugen.
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
  toTaskText path task
  paragraph simplifiedInformation
  paragraph hoveringInformation
  extra $ addText task
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
  :: (
    Alternative m,
    MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    OutputCapable m
    )
  => FilePath
  -> NameCdErrorInstance
  -> NameCdErrorAnswer
  -> Rated m
nameCdErrorEvaluation path inst@NameCdErrorInstance {..} x = addPretext $ do
  let reasonTranslation = M.fromAscList [
        (English, "reason"),
        (German, "Grund")
        ]
      dueToTranslation = M.fromAscList [
        (English, "relationships constituting the problem"),
        (German, "das Problem ausmachenden Beziehungen")
        ]
      solutionReason = head . M.keys . M.filter fst $ errorReasons
      solutionDueTo = M.fromAscList
        $ map (second (contributingToProblem . annotation))
        relevant
      correctAnswer
        | showSolution = Just $ toString $ encode $ nameCdErrorSolution inst
        | otherwise = Nothing
  recoverWith 0 (
    singleChoice DefiniteArticle reasonTranslation Nothing solutionReason (reason x)
      $>> multipleChoice DefiniteArticle
        dueToTranslation
        Nothing
        solutionDueTo
        (dueTo x)
    )
    $>>= \points -> do
      paragraph $ translate $ classDiagramDescription points
      paragraph $ image $=<< cacheCd cdDrawSettings mempty changedCd path
      pure ()
    $>> printSolutionAndAssert DefiniteArticle correctAnswer $ fromEither points
  where
    relevant = relevantRelationships inst
    changedCd = unannotateCd $ classDiagram {
      annotatedRelationships = annotatedRelationships classDiagram
        \\ map snd chosenRelevant
      }
    chosenRelevant = filter ((`elem` nubOrd (dueTo x)) . fst) relevant
    classDiagramDescription points
      | points == Right 1 = do
        english [iii|
          If all relationships you correctly gave as constituting the problem
          would be removed, the following class diagram would result:
          |]
        german [iii|
          Wenn alle von Ihnen korrekterweise als das Problem ausmachend angegebenen
          Beziehungen entfernt würden,
          würde das folgende Klassendiagramm entstehen:
          |]
      | any (contributingToProblem . annotation . snd) chosenRelevant = do
        english [iii|
          Nevertheless, the removal of all relationships you gave as
          contributing to the problem results in resolving the invalidity
          as the class diagram then would look like this:
          |]
        german [iii|
          Dennoch behebt das Entfernen aller von Ihnen als zum Problem beitragend
          angegebenen Beziehungen die Ungültigkeit,
          da das Klassendiagramm dann so aussehen würde:
          |]
      | otherwise = do
        english [iii|
          The removal of all relationships you gave as contributing to the problem
          still does not resolve the underlying issue
          as the class diagram then would look like this:
          |]
        german [iii|
          Das Entfernen aller von Ihnen als zum Problem beitragend angegebenen
          Beziehungen behebt das vorliegende Problem nicht,
          da das Klassendiagramm dann so aussehen würde:
          |]

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
  randomise = shuffleInstance

instance RandomiseNames NameCdErrorInstance where
  randomiseNames inst = do
    let (names, nonInheritances) = classAndNonInheritanceNames inst
    names' <- shuffleM names
    nonInheritances' <- shuffleM nonInheritances
    renameInstance inst names' nonInheritances'

instance RandomiseLayout NameCdErrorInstance where
  randomiseLayout NameCdErrorInstance {..} = do
    cd <- shuffleAnnotatedClassAndConnectionOrder classDiagram
    return NameCdErrorInstance {
      byName = byName,
      cdDrawSettings = cdDrawSettings,
      classDiagram = cd,
      errorReasons = errorReasons,
      showSolution = showSolution,
      taskText = taskText,
      addText = addText
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
    taskText = taskText,
    addText = addText
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
    taskText = taskText,
    addText = addText
    }

nameCdErrorGenerate
  :: (MonadAlloy m, MonadCatch m)
  => NameCdErrorConfig
  -> Int
  -> Int
  -> m NameCdErrorInstance
nameCdErrorGenerate config segment seed = do
  let g = mkStdGen $ (segment +) $ 4 * seed
  flip evalRandT g $ generateAndRandomise config

generateAndRandomise
  :: (MonadAlloy m, MonadCatch m, RandomGen g)
  => NameCdErrorConfig
  -> RandT g m NameCdErrorInstance
generateAndRandomise config@NameCdErrorConfig {..} = do
  (cd, reason, rs) <- nameCdError config
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
      $ (True, PreDefined reason)
      : map (False,) chosenReasons,
    showSolution = printSolution,
    taskText = defaultNameCdErrorTaskText,
    addText = extraText
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

nameCdError
  :: (MonadAlloy m, MonadCatch m, RandomGen g)
  => NameCdErrorConfig
  -> RandT g m (AnyCd, Property, [AnyRelationship String String])
nameCdError NameCdErrorConfig {..}  = do
  structuralWeakenings <- shuffleM [ (x,y) |
    x <- illegalStructuralWeakenings allowedProperties,
    y <- legalStructuralWeakenings allowedProperties
    ]
  getInstanceWithStructuralWeakenings structuralWeakenings
  where
    getFixWith cd properties = Changes.transformGetNextFix
      cd
      classConfig
      cdConstraints
      properties
      allowedProperties
      useNames
    getInstanceWithStructuralWeakenings [] =
      error "there seems to be no instance for the provided configuration"
    getInstanceWithStructuralWeakenings ((e0, l0) : chs) = do
      let p = toProperty $ e0 .&. l0
          alloyCode = getFixWith Nothing p
      instances <- lift $ getInstances maxInstances timeout alloyCode
      randomInstances <- shuffleM instances
      getInstanceWithODs chs p randomInstances
    getInstanceWithODs chs _ [] = getInstanceWithStructuralWeakenings chs
    getInstanceWithODs chs p (randomInstance:randomInstances) = do
      cdInstance <- lift
        $ fromInstance randomInstance
        >>= nameClassDiagramInstance
      let cd = instanceClassDiagram cdInstance
          p' = p {
            hasDoubleRelationships = Nothing,
            hasReverseRelationships = Nothing,
            hasMultipleInheritances = Nothing
            }
          alloyCode = getFixWith (Just cd) p'
      instances <- lift $ getInstances Nothing timeout alloyCode
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
      let maxNumberOfObjects = maxObjects $ snd $ classLimits classConfig
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
          possibleLinkNames = mapMaybe relationshipName $ relationships cd
      od <- listToMaybe
        <$> getInstances (Just 1) timeout (combineParts parts ++ command)
      od' <- fmap join $ forM od
        $ runExceptT . alloyInstanceToOd (Just $ classNames cd) possibleLinkNames
        >=> return . eitherToMaybe
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
      enthält mindestens eine ungültige Multiplizität an einer Vererbung.
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
      enthält mindestens eine ungültige Multiplizität an einer Beziehung,
      die keine Vererbung ist.
      |]
  WrongCompositionLimits -> do
    english [iii|
      contains at least one invalid multiplicity near the whole of a composition.
      |]
    german [iii|
      enthält mindestens eine ungültige Multiplizität am Ganzen einer Komposition.
      |]

defaultNameCdErrorInstance :: NameCdErrorInstance
defaultNameCdErrorInstance = NameCdErrorInstance {
  byName = True,
  cdDrawSettings = defaultCdDrawSettings {printNames = True},
  classDiagram = AnnotatedClassDiagram {
    annotatedClasses = ["C", "B", "D", "A"],
    annotatedRelationships = [
      Annotation {
        annotated = Right Association {
          associationName = "w",
          associationFrom =
            LimitedLinking {linking = "A", limits = (1, Just 2)},
          associationTo =
            LimitedLinking {linking = "B", limits = (2, Nothing)}
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
          compositionPart =
            LimitedLinking {linking = "B", limits = (1, Just 1)},
          compositionWhole =
            LimitedLinking {linking = "D", limits = (1, Just 2)}
          },
        annotation = Relevant {
          contributingToProblem = True,
          listingPriority = 2,
          referenceUsing = DefiniteArticle
          }
        },
      Annotation {
        annotated = Right Aggregation {
          aggregationName = "y",
          aggregationPart =
            LimitedLinking {linking = "A", limits = (2, Just 2)},
          aggregationWhole =
            LimitedLinking {linking = "C", limits = (1, Nothing)}
          },
        annotation = Relevant {
          contributingToProblem = False,
          listingPriority = 1,
          referenceUsing = DefiniteArticle
          }
        },
      Annotation {
        annotated = Right Composition {
          compositionName = "z",
          compositionPart =
            LimitedLinking {linking = "C", limits = (2, Nothing)},
          compositionWhole =
            LimitedLinking {linking = "B", limits = (1, Just 1)}
          },
        annotation = Relevant {
          contributingToProblem = False,
          listingPriority = 4,
          referenceUsing = DefiniteArticle
          }
        }
      ]
    },
  errorReasons = M.fromAscList [
    ('a', (False, PreDefined DoubleRelationships)),
    ('b', (False, PreDefined SelfRelationships)),
    ('c', (True, PreDefined WrongCompositionLimits)),
    ('d', (False, PreDefined WrongAssociationLimits)),
    ('e', (False, PreDefined SelfInheritances)),
    ('f', (False, PreDefined InvalidInheritanceLimits)),
    ('g', (False, PreDefined CompositionCycles)),
    ('h', (False, PreDefined ReverseInheritances)),
    ('i', (False, PreDefined InheritanceCycles)),
    ('j', (False, PreDefined MultipleInheritances)),
    ('k', (False, PreDefined ReverseRelationships))
    ],
  showSolution = False,
  taskText = defaultNameCdErrorTaskText,
  addText = Nothing
  }
