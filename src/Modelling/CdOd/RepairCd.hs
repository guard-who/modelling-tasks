{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Modelling.CdOd.RepairCd (
  RepairCdConfig (..),
  RepairCdInstance (..),
  checkRepairCdConfig,
  constrainConfig,
  defaultRepairCdConfig,
  phraseChange,
  repairCd,
  repairCdEvaluation,
  repairCdTask,
  repairIncorrect,
  ) where

import qualified Modelling.CdOd.CdAndChanges.Transform as Changes (transformChanges)

import qualified Data.Map                         as M (empty, insert, fromList)

import Modelling.Auxiliary.Output (
  OutputMonad (..),
  hoveringInformation,
  multipleChoice,
  simplifiedInformation,
  LangM,
  )
import Modelling.CdOd.Auxiliary.Util    (getInstances)
import Modelling.CdOd.CD2Alloy.Transform (transform)
import Modelling.CdOd.Edges             (toEdges)
import Modelling.CdOd.MatchCdOd         (applyChanges)
import Modelling.CdOd.Output            (drawCdFromSyntax, drawOdFromInstance)
import Modelling.CdOd.Types (
  AssociationType (..),
  Change (..),
  ClassConfig (..),
  Connection (..),
  DiagramEdge,
  RelationshipProperties (..),
  Syntax,
  defaultProperties,
  toOldSyntax,
  )

import Control.Monad                    (void, when)
import Control.Monad.IO.Class           (liftIO)
import Control.Monad.Random
  (RandomGen, RandT, evalRandT, getRandomR, mkStdGen)
import Data.Bifunctor                   (second)
import Data.GraphViz                    (DirType (..), GraphvizOutput (Pdf, Svg))
import Data.Map                         (Map)
import Data.Maybe                       (fromMaybe)
import Data.String.Interpolate          (i)
import GHC.Generics                     (Generic)
import Language.Alloy.Call              (AlloyInstance)
import System.Random.Shuffle            (shuffleM)

debug :: Bool
debug = False

phraseChange :: Bool -> Bool -> Change DiagramEdge -> String
phraseChange byName withDir c = case (add c, remove c) of
  (Nothing, Nothing) -> "change nothing"
  (Just e,  Nothing) -> "add " ++ phraseRelation False withDir e
  (Nothing, Just e ) -> "remove " ++ phraseRelation byName withDir e
  (Just e1, Just e2) ->
    "replace " ++ phraseRelation byName withDir e2
    ++ " by " ++ phraseRelation False withDir e1

phraseRelation :: Bool -> Bool -> DiagramEdge -> String
phraseRelation _ _ (from, to, Inheritance) =
  "an inheritance where " ++ from ++ " inherits from " ++ to
phraseRelation True _ (_, _, Assoc t n _ _ _) = (++ n) $ case t of
  Association -> "association "
  Aggregation -> "aggregation "
  Composition -> "composition "
phraseRelation _ False (from, to, Assoc Association _ l h _)
  | from == to = [i|a self-association #{selfParticipates}|]
  | otherwise  = "an association" ++ participations l from h to
  where
    selfParticipates :: String
    selfParticipates =
      [i|for #{from} where #{participates l "it"} at one end and #{phraseLimit h} at the other end|]
phraseRelation _ _ (from, to, Assoc t _ l h _)
  | from == to = case t of
      Association -> [i|a self-association #{selfParticipates}|]
      Aggregation -> [i|a self-aggregation #{selfParticipatesPartWhole}|]
      Composition -> [i|a self-composition #{selfParticipatesPartWhole}|]
  | otherwise = (++ participations l from h to) $ case t of
    Association -> "an association from " ++ from ++ " to " ++ to
    Aggregation -> "a relationship that makes " ++ from
      ++ " an aggregation of " ++ to ++ "s"
    Composition -> "a relationship that makes " ++ from
      ++ " a composition of " ++ to ++ "s"
  where
    selfParticipates :: String
    selfParticipates =
      [i|for #{from} where #{participates l "it"} at its beginning and #{phraseLimit h} at its arrow end|]
    selfParticipatesPartWhole :: String
    selfParticipatesPartWhole =
      [i|for #{from} where #{participates l "it"} as part and #{phraseLimit h} as whole|]

participations
  :: (Int, Maybe Int)
  -> String
  -> (Int, Maybe Int)
  -> String
  -> String
participations l from h to =
  " where " ++ participates l from ++ " and " ++ participates h to

participates :: (Int, Maybe Int) -> String -> String
participates r c = c ++ " participates " ++ phraseLimit r

phraseLimit :: (Int, Maybe Int) -> String
phraseLimit (0, Just 0)  = "not at all"
phraseLimit (1, Just 1)  = "exactly once"
phraseLimit (2, Just 2)  = "exactly twice"
phraseLimit (-1, Just n) = "*.." ++ show n ++ " times"
phraseLimit (m, Nothing) = show m ++ "..* times"
phraseLimit (m, Just n)  = show m ++ ".." ++ show n ++ " times"

data PropertyChange = PropertyChange {
    changeName     :: String,
    operation      :: RelationshipProperties -> RelationshipProperties,
    validityChange :: Bool -> Bool
  }

toProperty :: PropertyChange -> RelationshipProperties
toProperty p = operation p defaultProperties

isValid :: PropertyChange -> Bool
isValid p = validityChange p True

data RepairCdConfig = RepairCdConfig {
    classConfig      :: ClassConfig,
    maxInstances     :: Maybe Integer,
    printNames       :: Bool,
    printNavigations :: Bool,
    timeout          :: Maybe Int,
    useNames         :: Bool
  } deriving Generic

defaultRepairCdConfig :: RepairCdConfig
defaultRepairCdConfig = RepairCdConfig {
    classConfig = ClassConfig {
        classes      = (4, 4),
        aggregations = (0, Just 2),
        associations = (0, Just 2),
        compositions = (0, Just 3),
        inheritances = (1, Just 3)
      },
    maxInstances     = Just 200,
    printNames       = True,
    printNavigations = True,
    timeout          = Nothing,
    useNames         = False
  }

checkRepairCdConfig :: RepairCdConfig -> Maybe String
checkRepairCdConfig config
  | not (printNames config) && useNames config
  = Just "use navigations is only possible when printing navigations"
  | otherwise
  = Nothing

repairCdTask :: OutputMonad m => RepairCdInstance -> LangM m
repairCdTask task = do
  paragraph $ text
    "Consider the following class diagram, which unfortunately is invalid."
  image $ classDiagram task
  paragraph $ text
    [i|Which of the following changes would repair the class diagram?|]
  enumerate show (phraseChange (withNames task) (withDirections task) . snd) $ changes task
  paragraph $ text
    [i|Please state your answer by giving a list of numbers, indicating all changes each resulting in a valid class diagram.|]
  paragraph $ text
    [i|Answer by giving a comma separated list of all valid options, e.g. [0, 9] would indicate that option 0 and 9 repair the given class diagram.|]
  paragraph simplifiedInformation
  paragraph hoveringInformation

repairCdEvaluation :: OutputMonad m => RepairCdInstance -> [Int] -> LangM m
repairCdEvaluation = multipleChoice "changes" . changes

data RepairCdInstance = RepairCdInstance {
    changes        :: Map Int (Bool, Change DiagramEdge),
    classDiagram   :: FilePath,
    withDirections :: Bool,
    withNames      :: Bool
  } deriving (Generic, Show)

repairCd
  :: RepairCdConfig
  -> FilePath
  -> Int
  -> Int
  -> IO RepairCdInstance
repairCd config path segment seed = do
  let g = mkStdGen $ (segment +) $ 4 * seed
  (cd, chs) <- evalRandT (repairIncorrect (classConfig config) (maxInstances config) (timeout config)) g
  let chs' = map (second fst) chs
  cd'       <- drawCdFromSyntax (printNavigations config) (printNames config) Nothing cd path Svg
  return $ RepairCdInstance
    (M.fromList $ zip [1..] chs')
    cd'
    (printNavigations config)
    (printNames config && useNames config)

constrainConfig :: RandomGen g => Int -> ClassConfig -> RandT g IO ClassConfig
constrainConfig n config = do
  clas <- getRandomR $ classes config
  let maxAg  = ((clas * (clas - 1)) `div` 2)
        + n - sum (map (fst . ($ config)) edges)
  (maxAs, aggs) <- randOf aggregations maxAg
  (maxCo, asss) <- randOf associations maxAs
  (maxIn, coms) <- randOf compositions maxCo
  (_    , inhs) <- randOf inheritances maxIn
  return $ ClassConfig (clas, clas) aggs asss coms inhs
  where
    edges = [aggregations, associations, compositions, inheritances]
    randOf f maxF = do
      x <- getRandomR (second (fromMaybe maxF) (f config))
      return (maxF - x, (x, Just x))

repairIncorrect
  :: RandomGen g
  => ClassConfig
  -> Maybe Integer
  -> Maybe Int
  -> RandT g IO (Syntax, [(Bool, (Change DiagramEdge, Syntax))])
repairIncorrect config maxInsts to = do
  e0:_    <- shuffleM illegalChanges
  l0:l1:_ <- shuffleM legalChanges
  c0:_    <- shuffleM allChanges
  csm     <- shuffleM $ c0 : noChange : l1 .&. noChange : l1 : [e0]
  cs      <- shuffleM $ l0 .&. e0 : noChange : take 2 csm
--  config' <- constrainConfig 5 config
  let code = Changes.transformChanges config (toProperty e0) (Just config)
        $ map toProperty cs
  when debug $ liftIO $ do
    putStrLn $ changeName e0
    print $ map changeName cs
    writeFile "repair.als" code
  instas  <- liftIO $ getInstances maxInsts to code
  rinstas <- shuffleM instas
  getInstanceWithODs (map isValid cs) rinstas
  where
    drawCd :: Syntax -> Integer -> IO FilePath
    drawCd cd' n = drawCdFromSyntax True True Nothing cd' ("cd-" ++ show n) Pdf
    drawOd :: Syntax -> AlloyInstance -> Integer -> IO FilePath
    drawOd cd od x =
      let backwards   = [n | (_, _, Assoc t n _ _ _) <- toEdges cd
                           , t /= Association]
          forwards    = [n | (_, _, Assoc t n _ _ _) <- toEdges cd
                           , t == Association]
          navigations = foldr (`M.insert` Back)
                              (foldr (`M.insert` Forward) M.empty forwards)
                              backwards
      in drawOdFromInstance od Nothing navigations True ("od-" ++ show x) Pdf
    getInstanceWithODs _  [] = repairIncorrect config maxInsts to
    getInstanceWithODs vs (rinsta:rinstas) = do
      (cd, chs, _) <- applyChanges rinsta
      let cds  = zip vs (map snd chs)
          chs' = zip vs chs
      ods <- (liftIO . getOD . snd) `mapM` filter fst cds
      if not (any null ods)
        then do
        when debug $ liftIO $ do
          void $ drawCd cd 0
          uncurry drawCd `mapM_` zip (map snd chs) [1 ..]
          uncurry (drawOd cd . head) `mapM_` zip ods [1 ..]
        return (cd, chs')
        else getInstanceWithODs vs rinstas
    getOD cd = do
      let (p1, p2, p3, p4, p5) = transform (toOldSyntax cd) "" ""
      getInstances (Just 1) to (p1 ++ p2 ++ p3 ++ p4 ++ p5)

allChanges :: [PropertyChange]
allChanges = legalChanges ++ illegalChanges

noChange :: PropertyChange
noChange = PropertyChange "none" id id

infixl 9 .&.
(.&.) :: PropertyChange -> PropertyChange -> PropertyChange
PropertyChange n1 o1 v1 .&. PropertyChange n2 o2 v2 = PropertyChange
  (n1 ++ " + " ++ n2)
  (o1 . o2)
  (v1 . v2)

legalChanges :: [PropertyChange]
legalChanges = [
    noChange,
    PropertyChange "add one self relationship" addSelfRelationships id,
    PropertyChange "force double relationships" withDoubleRelationships id,
    PropertyChange "force reverse relationships" withReverseRelationships id
--    PropertyChange "force multiple inheritances" withMultipleInheritances id
  ]
  where
    addSelfRelationships :: RelationshipProperties -> RelationshipProperties
    addSelfRelationships config@RelationshipProperties {..}
      = config { selfRelationships = selfRelationships + 1 }
    withDoubleRelationships :: RelationshipProperties -> RelationshipProperties
    withDoubleRelationships config
      = config { hasDoubleRelationships = True }
    withReverseRelationships :: RelationshipProperties -> RelationshipProperties
    withReverseRelationships config
      = config { hasReverseRelationships = True }
    -- withMultipleInheritances :: RelationshipProperties -> RelationshipProperties
    -- withMultipleInheritances config
    --   = config { hasMultipleInheritances = True }

illegalChanges :: [PropertyChange]
illegalChanges = map ($ const False) [
    PropertyChange "add wrong association" addWrongAssocs,
    PropertyChange "add wrong composition" addWrongCompositions,
    PropertyChange "force inheritance cycles" withInheritanceCycles,
    PropertyChange "force composition cycles" withCompositionCycles
  ]
  where
    addWrongAssocs :: RelationshipProperties -> RelationshipProperties
    addWrongAssocs config@RelationshipProperties {..}
      = config { wrongAssocs = wrongAssocs + 1 }
    addWrongCompositions :: RelationshipProperties -> RelationshipProperties
    addWrongCompositions config@RelationshipProperties {..}
      = config { wrongCompositions = wrongCompositions + 1 }
    withInheritanceCycles :: RelationshipProperties -> RelationshipProperties
    withInheritanceCycles config
      = config { hasInheritanceCycles = True }
    withCompositionCycles :: RelationshipProperties -> RelationshipProperties
    withCompositionCycles config
      = config { hasCompositionCycles = True }
