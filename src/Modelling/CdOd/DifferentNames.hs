{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Modelling.CdOd.DifferentNames (
  DifferentNamesConfig (..),
  DifferentNamesInstance (..),
  DifferentNamesTaskTextElement (..),
  ShufflingOption (..),
  checkDifferentNamesConfig,
  checkDifferentNamesInstance,
  defaultDifferentNamesConfig,
  defaultDifferentNamesInstance,
  defaultDifferentNamesTaskText,
  differentNames,
  differentNamesEvaluation,
  differentNamesInitial,
  differentNamesSolution,
  differentNamesSyntax,
  differentNamesTask,
  getDifferentNamesTask,
  mappingShow,
  renameInstance,
  ) where

import qualified Data.Bimap                       as BM (
  filter,
  fromList,
  keysR,
  lookup,
  lookupR,
  toAscList,
  twist,
  )
import qualified Data.Map                         as M (
  fromAscList,
  )

import Capabilities.Alloy               (MonadAlloy, getInstances)
import Capabilities.Cache               (MonadCache)
import Capabilities.Diagrams            (MonadDiagrams)
import Capabilities.Graphviz            (MonadGraphviz)
import Modelling.Auxiliary.Common (
  Randomise (isRandomisable, randomise),
  RandomiseLayout (randomiseLayout),
  TaskGenerationException (NoInstanceAvailable),
  shuffleEverything,
  )
import Modelling.Auxiliary.Output (
  addPretext,
  directionsAdvice,
  hoveringInformation,
  simplifiedInformation,
  uniform,
  )
import Modelling.CdOd.Auxiliary.Util
import Modelling.CdOd.CD2Alloy.Transform (
  combineParts,
  createRunCommand,
  mergeParts,
  transform,
  )
import Modelling.CdOd.Generate          (generateCds, instanceToCd)
import Modelling.CdOd.Output            (cacheCd, cacheOd)
import Modelling.CdOd.Types (
  Cd,
  CdDrawSettings (..),
  ClassConfig (..),
  ClassDiagram (..),
  LimitedLinking (..),
  Link (..),
  Object (..),
  ObjectConfig (..),
  ObjectDiagram (..),
  ObjectProperties (..),
  Od,
  OmittedDefaultMultiplicities,
  Relationship (..),
  anonymiseObjects,
  associationNames,
  checkCdDrawSettings,
  checkClassConfigWithProperties,
  checkObjectDiagram,
  checkObjectProperties,
  checkOmittedDefaultMultiplicities,
  classNames,
  defaultCdDrawSettings,
  defaultOmittedDefaultMultiplicities,
  defaultProperties,
  fromClassDiagram,
  isObjectDiagramRandomisable,
  linkNames,
  relationshipName,
  renameObjectsWithClassesAndLinksInOd,
  renameClassesAndRelationships,
  shuffleCdNames,
  shuffleClassAndConnectionOrder,
  shuffleObjectAndLinkOrder,
  )
import Modelling.Types (
  Name (Name),
  NameMapping (nameMapping),
  fromNameMapping,
  showName,
  toNameMapping,
  )

import Control.Applicative              (Alternative ((<|>)))
import Control.Monad.Catch              (MonadThrow, throwM)
import Control.Monad.Extra              (whenJust)
import Control.OutputCapable.Blocks (
  ArticleToUse (DefiniteArticle),
  GenericOutputCapable (..),
  LangM,
  OutputCapable,
  Rated,
  ($=<<),
  english,
  german,
  multipleChoice,
  translations,
  translate,
  yesNo,
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
  evalRandT,
  mkStdGen,
  )
import Control.Monad.Trans.Except       (runExceptT)
import Data.Bifunctor                   (Bifunctor (bimap))
import Data.Bimap                       (Bimap)
import Data.Bitraversable               (bitraverse)
import Data.Bool                        (bool)
import Data.Containers.ListUtils        (nubOrd, nubOrdOn)
import Data.Functor.Identity            (Identity (Identity, runIdentity))
import Data.GraphViz                    (DirType (Forward))
import Data.List (
  group,
  intersect,
  permutations,
  singleton,
  sort,
  )
import Data.Maybe (
  catMaybes,
  isJust,
  isNothing,
  listToMaybe,
  mapMaybe,
  )
import Data.Ratio                       ((%))
import Data.String.Interpolate          (i, iii)
import Data.Tuple.Extra                 (swap)
import GHC.Generics                     (Generic)
import Language.Alloy.Call (
  AlloyInstance,
  getDoubleAs,
  lookupSig,
  scoped,
  )
import System.Random.Shuffle            (shuffleM)

data ShufflingOption a =
    ConsecutiveLetters
  | WithAdditionalNames [a]
  deriving (Eq, Generic, Foldable, Functor, Read, Show, Traversable)

data DifferentNamesInstance = DifferentNamesInstance {
    cDiagram :: Cd,
    cdDrawSettings :: !CdDrawSettings,
    oDiagram :: Od,
    showSolution :: Bool,
    mapping  :: NameMapping,
    linkShuffling :: ShufflingOption String,
    taskText :: !DifferentNamesTaskText,
    -- | whether every relationship has an associated link (in the mapping)
    usesAllRelationships :: Bool
  } deriving (Eq, Generic, Read, Show)

checkDifferentNamesInstance :: DifferentNamesInstance -> Maybe String
checkDifferentNamesInstance DifferentNamesInstance {..}
  | not $ printNames cdDrawSettings
  = Just [iii|printNames has to be set to True for this task type.|]
  | not $ printNavigations cdDrawSettings
  = Just [iii|printNavigations has to be set to True for this task type.|]
  | WithAdditionalNames xs <- linkShuffling
  , length associations > length links + length xs
  = Just [iii|
      WithAdditionalNames must provide at least a name for
      each missing link in the Object diagram,
      i.e., for which an association in the Class diagram exists
      but not a link in the Object diagram.
      |]
  | (x:_) <- nubOrd links `intersect` nubOrd associations
  = Just [iii|
      Link names and association names must be disjoint
      but currently "#{x}" is among both.
      |]
  | otherwise
  = checkObjectDiagram oDiagram
  <|> checkCdDrawSettings cdDrawSettings
  where
    associations = associationNames cDiagram
    links = linkNames oDiagram

data DifferentNamesConfig
  = DifferentNamesConfig {
    classConfig      :: ClassConfig,
    withNonTrivialInheritance :: Maybe Bool,
    maxInstances     :: Maybe Integer,
    objectConfig     :: ObjectConfig,
    objectProperties :: ObjectProperties,
    omittedDefaultMultiplicities :: OmittedDefaultMultiplicities,
    printSolution    :: Bool,
    timeout          :: !(Maybe Int),
    -- | Obvious means here that each individual relationship to link mapping
    -- can be made without considering other relationships.
    withObviousMapping :: !(Maybe Bool)
  } deriving (Generic, Read, Show)

checkDifferentNamesConfig :: DifferentNamesConfig -> Maybe String
checkDifferentNamesConfig DifferentNamesConfig {..}
  | isJust withObviousMapping
  = Just [iii|
    'withObviousMapping' is not yet supported and has to be set to Nothing
    |]
  | (x, Just y) <- relationshipLimits classConfig, x /= y
  = Just [iii|
      The minimum number of relationships has to equal its maximum number
      for this task type.
      Otherwise task instances would vary too much in complexity.
      |]
  | (x, Just y) <- inheritanceLimits classConfig, x /= y
  = Just [iii|
      The minimum number of inheritances has to equal its maximum number
      for this task type.
      Otherwise task instances could vary too much in difficulty.
      |]
  | isNothing . snd $ relationshipLimits classConfig
  , any
      (different . ($ classConfig))
      [aggregationLimits, associationLimits, compositionLimits]
  = Just [iii|
      The minimum number and maximum number
      of aggregations, associations or compositions may not vary
      if the maximum number of relationships is not fixed.
      Otherwise task instances would vary too much in complexity.
      |]
  | otherwise = checkClassConfigWithProperties classConfig defaultProperties
    <|> checkObjectProperties objectProperties
    <|> checkOmittedDefaultMultiplicities omittedDefaultMultiplicities
  where
    different (_, Nothing) = True
    different (x, Just y)  = x /= y

defaultDifferentNamesConfig :: DifferentNamesConfig
defaultDifferentNamesConfig = DifferentNamesConfig {
    classConfig  = ClassConfig {
        classLimits        = (4, 4),
        aggregationLimits  = (1, Just 1),
        associationLimits  = (1, Just 1),
        compositionLimits  = (1, Just 1),
        inheritanceLimits  = (1, Just 1),
        relationshipLimits = (4, Just 4)
      },
    objectConfig = ObjectConfig {
      linkLimits           = (5, Just 10),
      linksPerObjectLimits = (0, Just 3),
      objectLimits         = (4, 6)
      },
    objectProperties = ObjectProperties {
      anonymousObjectProportion = 1 % 1,
      completelyInhabited = Nothing,
      hasLimitedIsolatedObjects = True,
      hasSelfLoops = Nothing,
      usesEveryRelationshipName = Just True
      },
    omittedDefaultMultiplicities = defaultOmittedDefaultMultiplicities,
    printSolution    = False,
    withNonTrivialInheritance = Just True,
    withObviousMapping = Nothing,
    maxInstances     = Just 200,
    timeout          = Nothing
  }

newtype ShowName = ShowName { showName' :: Name }

instance Show ShowName where
  show = showName . showName'

mappingShow :: [(Name, Name)] -> [(ShowName, ShowName)]
mappingShow = fmap (bimap ShowName ShowName)

type DifferentNamesTaskText = [SpecialOutput DifferentNamesTaskTextElement]

data DifferentNamesTaskTextElement
  = GivenCd
  | GivenOd
  | MappingAdvice
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

differentNamesTask
  :: (MonadCache m, MonadDiagrams m, MonadGraphviz m, MonadThrow m, OutputCapable m)
  => FilePath
  -> DifferentNamesInstance
  -> LangM m
differentNamesTask path task = do
  toTaskText path task
  paragraph simplifiedInformation
  paragraph directionsAdvice
  paragraph hoveringInformation
  pure ()

toTaskText
  :: (
    MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    MonadThrow m,
    OutputCapable m
    )
  => FilePath
  -> DifferentNamesInstance
  -> LangM m
toTaskText path task =
  specialToOutputCapable (toTaskSpecificText path task) (taskText task)

toTaskSpecificText
  :: (
    MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    MonadThrow m,
    OutputCapable m
    )
  => FilePath
  -> DifferentNamesInstance
  -> DifferentNamesTaskTextElement
  -> LangM m
toTaskSpecificText path DifferentNamesInstance {..} = \case
  GivenCd ->
    paragraph $ image $=<< cacheCd cdDrawSettings mempty cd path
  GivenOd -> paragraph $ image $=<<
    cacheOd oDiagram Forward True path
  MappingAdvice -> do
    paragraph $ translate $ do
      english [iii|
        Please note: Links are already grouped correctly and fully,
        i.e., all links with the same name (and only links with the same name!)
        in the OD correspond to exactly the same relationship name in the CD.
        |]
      german [iii|
        Bitte beachten Sie: Links sind bereits vollständig und korrekt gruppiert,
        d.h., alle Links mit dem selben Namen
        (and auch nur Links mit dem selben Namen!)
        im OD entsprechen genau dem selben Beziehungsnamen im CD.
        |]
    paragraph $ translate $ do
      english [iii|
        Thus, no link or relationship name should occur
        more than once in your mapping.
        |]
      german [iii|
        Deshalb sollte kein Link- oder Beziehungsname
        mehr als einmal in Ihrer Zuordnung auftreten.
        |]
    pure ()
  where
    cd = fromClassDiagram cDiagram

defaultDifferentNamesTaskText :: DifferentNamesTaskText
defaultDifferentNamesTaskText = [
  Paragraph $ singleton $ Translated $ translations $ do
    english "Consider the following (valid) class diagram:"
    german "Betrachten Sie folgendes (valide) Klassendiagramm:",
  Special GivenCd,
  Paragraph $ singleton $ Translated $ translations $ do
    english "and the following object diagram (which conforms to it):"
    german "und das folgende (dazu passende) Objektdiagramm:",
  Special GivenOd,
  Paragraph [
    Translated $ translations $ do
      english [iii|
        Which relationship in the class diagram (CD) corresponds
        to which of the links in the object diagram (OD)?
        \n
        State your answer by giving a mapping of
        relationships in the CD to links in the OD.
        \n
        To state that a in the CD corresponds to x in the OD and
        b in the CD corresponds to y in the OD, write the mapping as:
        |]
      german [iii|
        Welche Beziehung im Klassendiagramm (CD)
        entspricht welchen Links im Objektdiagramm (OD)?
        \n
        Geben Sie Ihre Antwort als eine Zuordnung von
        Beziehungen im CD zu Links im OD an.
        \n
        Um anzugeben, dass a im CD zu x im OD und b im CD
        zu y im OD korrespondieren, schreiben Sie die Zuordnung als:
        |],
    Code . uniform . show $ mappingShow differentNamesInitial
    ],
  Special MappingAdvice
  ]

differentNamesInitial :: [(Name, Name)]
differentNamesInitial = bimap Name Name <$> [("a", "x"), ("b", "y")]

differentNamesSyntax
  :: OutputCapable m
  => DifferentNamesInstance
  -> [(Name, Name)]
  -> LangM m
differentNamesSyntax DifferentNamesInstance {..} cs = addPretext $ do
  yesNo (null invalidMappings) $ translate $ do
    english [iii|
      All provided pairs are matching an existing relationship
      and an existing link?
      |]
    german [iii|
      Alle angegebenen Paare ordnen einen vorhandenen Link
      einer vorhandenen Beziehung zu?
      |]
  whenJust (listToMaybe invalidMappings) $ \x ->
    refuse $ paragraph $ translate $ do
      let y = bimap ShowName ShowName x
      english [i|The mapping '#{y}' uses a non-existing identifier.|]
      german [iii|
        Die Zuordnung '#{y}' benutzt einen nicht vorhandenen Bezeichner.
        |]
  yesNo (null allMappingValues) $ translate $ do
    english "All provided pairs are non-overlapping?"
    german "Alle angegebenen Paare sind nicht überlappend?"
  case allMappingValues of
    (x:_):_ -> refuse $ paragraph $ translate $ do
      let y = ShowName x
      english [i|The identifier '#{y}' appears twice within the given mappings.|]
      german [i|
        Der Bezeichner '#{y}' existiert doppelt in den angegebenen Zuordnungen.
        |]
    _ -> pure ()
  pure ()
  where
    links = linkNames oDiagram
    sortPair (x, y) = if x <= y then (x, y) else (y, x)
    choices = nubOrdOn sortPair cs
    associations = associationNames cDiagram
    isAssociationMappingForward (Name x, Name y) =
      x `elem` associations && y `elem` links
    isAssociationMapping x = isAssociationMappingForward x
      || isAssociationMappingForward (swap x)
    invalidMappings = filter (not . isAssociationMapping) choices
    allMappingValues = filter
      (not . null . tail)
      $ group $ sort (map fst choices ++ map snd choices)

readMapping :: Ord a => Bimap a a -> (a, a) -> Maybe (a, a)
readMapping m (x, y)
  | isJust $ BM.lookup x m, isJust $ BM.lookupR y m
  = Just (x, y)
  | isJust $ BM.lookup y m, isJust $ BM.lookupR x m
  = Just (y, x)
  | otherwise
  = Nothing

differentNamesEvaluation
  :: OutputCapable m
  => DifferentNamesInstance
  -> [(Name, Name)]
  -> Rated m
differentNamesEvaluation task cs = do
  let what = translations $ do
        german "Zuordnungen"
        english "mappings"
      m = nameMapping $ mapping task
      ms = M.fromAscList $ map (,True) $ BM.toAscList m
      solution =
        if showSolution task
        then Just . show . mappingShow $ differentNamesSolution task
        else Nothing
  multipleChoice DefiniteArticle what solution ms (mapMaybe (readMapping m) cs)

differentNamesSolution :: DifferentNamesInstance -> [(Name, Name)]
differentNamesSolution = BM.toAscList . nameMapping . mapping

differentNames
  :: (MonadAlloy m, MonadThrow m)
  => DifferentNamesConfig
  -> Int
  -> Int
  -> m DifferentNamesInstance
differentNames config segment seed = do
  let g = mkStdGen (segment + 4 * seed)
  flip evalRandT g $ do
    is <- generateCds
      (withNonTrivialInheritance config)
      (classConfig config)
      defaultProperties
      (maxInstances config)
      (timeout config)
    tryGettingValidInstanceFor is
  where
    tryGettingValidInstanceFor []             = throwM NoInstanceAvailable
    tryGettingValidInstanceFor (inst:instances) = do
      cd <- instanceToCd inst >>= shuffleClassAndConnectionOrder
        >>= fmap runIdentity . shuffleCdNames . Identity
      taskInstance <- getDifferentNamesTask
        (tryGettingValidInstanceFor instances)
        config
        cd
      shuffleEverything taskInstance

{-|
A 'defaultDifferentNamesInstance' as generated
using 'defaultDifferentNamesConfig'.
-}
defaultDifferentNamesInstance :: DifferentNamesInstance
defaultDifferentNamesInstance = DifferentNamesInstance {
  cDiagram = ClassDiagram {
    classNames = ["C", "A", "D", "B"],
    relationships = [
      Inheritance {subClass = "D", superClass = "A"},
      Composition {
        compositionName = "a",
        compositionPart = LimitedLinking {
          linking = "C",
          limits = (2, Nothing)
          },
        compositionWhole = LimitedLinking {
          linking = "B",
          limits = (0, Just 1)
          }
        },
      Association {
        associationName = "b",
        associationFrom = LimitedLinking {
          linking = "A",
          limits = (0, Nothing)
          },
        associationTo = LimitedLinking {
          linking = "C",
          limits = (0, Just 1)
          }
        },
      Aggregation {
        aggregationName = "c",
        aggregationPart = LimitedLinking {
          linking = "B",
          limits = (0, Just 2)
          },
        aggregationWhole = LimitedLinking {
          linking = "D",
          limits = (0, Just 2)
          }
        }
      ]
    },
  cdDrawSettings = defaultCdDrawSettings,
  oDiagram = ObjectDiagram {
    objects = [
      Object {isAnonymous = True, objectName = "d", objectClass = "D"},
      Object {isAnonymous = True, objectName = "c1", objectClass = "C"},
      Object {isAnonymous = True, objectName = "c2", objectClass = "C"},
      Object {isAnonymous = True, objectName = "b", objectClass = "B"},
      Object {isAnonymous = True, objectName = "d1", objectClass = "D"},
      Object {isAnonymous = True, objectName = "c", objectClass = "C"}
      ],
    links = [
      Link {linkName = "y", linkFrom = "c1", linkTo = "b"},
      Link {linkName = "x", linkFrom = "d1", linkTo = "c1"},
      Link {linkName = "z", linkFrom = "b", linkTo = "d1"},
      Link {linkName = "x", linkFrom = "d", linkTo = "c2"},
      Link {linkName = "y", linkFrom = "c2", linkTo = "b"}
      ]
    },
  showSolution = False,
  mapping = toNameMapping $ BM.fromList [("a", "y"), ("b", "x"), ("c", "z")],
  linkShuffling = ConsecutiveLetters,
  taskText = defaultDifferentNamesTaskText,
  usesAllRelationships = True
  }

getDifferentNamesTask
  :: (MonadAlloy m, MonadRandom m, MonadThrow m)
  => m DifferentNamesInstance
  -> DifferentNamesConfig
  -> Cd
  -> m DifferentNamesInstance
getDifferentNamesTask tryNext DifferentNamesConfig {..} cd = do
    let cd0    = (0 :: Integer, cd)
        parts0 = uncurry alloyFor cd0
        labels = mapMaybe relationshipName . relationships $ snd cd0
        cds    = map
          (flip renameEdges cd . BM.fromList . zip labels)
          $ drop 1 (permutations labels)
        cds'   = zip [1 :: Integer ..] cds
        partsList = map (uncurry alloyFor) cds'
        runCmd = foldr (\(n, _) -> (++ " and (not cd" ++ show n ++ ")")) "cd0" cds'
        onlyCd0 = createRunCommand
          runCmd
          (length $ classNames cd)
          objectConfig
          (concatMap relationships cds)
        partsList' = foldr mergeParts parts0 partsList
    instances  <- getInstances
      maxInstances
      timeout
      (combineParts partsList' ++ onlyCd0)
    instances' <- shuffleM (instances :: [AlloyInstance])
    continueWithHead instances' $ \od1 -> do
      labels' <- shuffleM labels
      used <- usedLabels labels od1
      let bm  = BM.fromList $ zip (map (:[]) ['a', 'b' ..]) labels'
          cd1 = renameEdges (BM.twist bm) cd
          bm' = BM.filter (const (`elem` used)) bm
          isCompleteMapping = BM.keysR bm == sort used
      if maybe
        (const True)
        (bool not id)
        (usesEveryRelationshipName objectProperties)
        isCompleteMapping
        then do
        od1' <- either error id <$> runExceptT (alloyInstanceToOd labels od1)
        od1'' <- anonymiseObjects (anonymousObjectProportion objectProperties) od1'
        return $ DifferentNamesInstance {
              cDiagram  = cd1,
              cdDrawSettings = CdDrawSettings {
                omittedDefaults = omittedDefaultMultiplicities,
                printNames = True,
                printNavigations = True
                },
              oDiagram  = od1'',
              showSolution = printSolution,
              mapping   = toNameMapping bm',
              linkShuffling = ConsecutiveLetters,
              taskText = defaultDifferentNamesTaskText,
              usesAllRelationships = isCompleteMapping
              }
        else tryNext
  where
    renameEdges bm = either (error . show) id . bitraverse pure (`BM.lookup` bm)
    alloyFor n cd' = transform
      cd'
      Nothing
      []
      objectConfig
      objectProperties
      (show n)
      ""
    continueWithHead []    _ = tryNext
    continueWithHead (x:_) f = f x
    usedLabels :: MonadThrow m => [String] -> AlloyInstance -> m [String]
    usedLabels labels inst = do
      let ignore = const $ const $ return ()
          usedLabel label xs = if null xs then Nothing else Just label
      os    <- lookupSig (scoped "this" "Object") inst
      catMaybes . zipWith usedLabel labels
        <$> mapM (\label -> getDoubleAs label ignore ignore os) labels

{-|
All names within a 'DifferentNamesInstance'
including names reserved for shuffling.
-}
classNonInheritanceAndLinkNames
  :: DifferentNamesInstance
  -> ([String], [String], [String])
classNonInheritanceAndLinkNames DifferentNamesInstance {..} =
  let names = classNames cDiagram
      nonInheritances = associationNames cDiagram
      additional = case linkShuffling of
        ConsecutiveLetters -> []
        WithAdditionalNames xs -> xs
      links = linkNames oDiagram ++ additional
  in (names, nonInheritances, links)

instance Randomise DifferentNamesInstance where
  randomise inst@DifferentNamesInstance {..} = do
    let (names, nonInheritances, lNames) = classNonInheritanceAndLinkNames inst
        links = case linkShuffling of
          ConsecutiveLetters -> take (length lNames) (map (:[]) ['z', 'y' ..])
          WithAdditionalNames _ -> lNames
    names'  <- shuffleM names
    nonInheritances' <- shuffleM nonInheritances
    links' <- shuffleM links
    renameInstance inst names' nonInheritances' links'
  isRandomisable DifferentNamesInstance {..} =
    isObjectDiagramRandomisable oDiagram

instance RandomiseLayout DifferentNamesInstance where
  randomiseLayout DifferentNamesInstance {..} = do
    cd <- shuffleClassAndConnectionOrder cDiagram
    od <- shuffleObjectAndLinkOrder oDiagram
    return $ DifferentNamesInstance {
      cDiagram = cd,
      cdDrawSettings = cdDrawSettings,
      oDiagram = od,
      showSolution = showSolution,
      mapping = mapping,
      linkShuffling = linkShuffling,
      taskText = taskText,
      usesAllRelationships = usesAllRelationships
      }

renameInstance
  :: MonadThrow m
  => DifferentNamesInstance
  -> [String]
  -> [String]
  -> [String]
  -> m DifferentNamesInstance
renameInstance inst@DifferentNamesInstance {..} names' nonInheritances' linkNs' = do
  let cd = cDiagram
      od = oDiagram
      (names, nonInheritances, linkNs) = classNonInheritanceAndLinkNames inst
      bm = BM.toAscList $ fromNameMapping mapping
      bmNames  = BM.fromList $ zip names names'
      bmNonInheritances = BM.fromList $ zip nonInheritances nonInheritances'
      bmLinks  = BM.fromList $ zip linkNs linkNs'
      bm'      = BM.fromList
        [ (a', l')
        | (a, l) <- bm
        , a' <- BM.lookup a bmNonInheritances
        , l' <- BM.lookup l bmLinks
        ]
  cd' <- renameClassesAndRelationships bmNames bmNonInheritances cd
  od' <- renameObjectsWithClassesAndLinksInOd bmNames bmLinks od
  shuffling <- mapM (`BM.lookup` bmLinks) linkShuffling
  return $ DifferentNamesInstance {
    cDiagram  = cd',
    cdDrawSettings = cdDrawSettings,
    oDiagram  = od',
    showSolution = showSolution,
    mapping   = toNameMapping bm',
    linkShuffling = shuffling,
    taskText = taskText,
    usesAllRelationships = usesAllRelationships
    }
