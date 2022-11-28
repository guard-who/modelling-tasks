{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Modelling.CdOd.RepairCd (
  AllowedProperties (..),
  RepairCdConfig (..),
  RepairCdInstance (..),
  checkRepairCdConfig,
  constrainConfig,
  defaultRepairCdConfig,
  defaultRepairCdInstance,
  allowEverything,
  phraseChange,
  repairCd,
  repairCdEvaluation,
  repairCdSolution,
  repairCdSyntax,
  repairCdTask,
  repairIncorrect,
  ) where

import qualified Modelling.CdOd.CdAndChanges.Transform as Changes (transformChanges)
import qualified Modelling.CdOd.Types             as T (
  RelationshipProperties (selfInheritances, selfRelationships),
  )

import qualified Data.Bimap                       as BM (fromList)
import qualified Data.Map                         as M (
  empty,
  filter,
  fromList,
  insert,
  keys,
  toList,
  )

import Modelling.Auxiliary.Output (
  addPretext,
  hoveringInformation,
  simplifiedInformation,
  )
import Modelling.CdOd.Auxiliary.Util    (getInstances)
import Modelling.CdOd.CD2Alloy.Transform (
  combineParts,
  createRunCommand,
  transform,
  )
import Modelling.CdOd.Edges             (toEdges)
import Modelling.CdOd.MatchCdOd         (applyChanges)
import Modelling.CdOd.Output (
  cacheCd,
  drawCdFromSyntax,
  drawOdFromInstance,
  )
import Modelling.CdOd.Types (
  AssociationType (..),
  Change (..),
  ClassConfig (..),
  Connection (..),
  DiagramEdge,
  RelationshipProperties (..),
  Syntax,
  addedAssociation,
  associationNames,
  classNames,
  defaultProperties,
  maxFiveObjects,
  renameAssocsInCd,
  renameAssocsInEdge,
  renameClassesInCd,
  renameClassesInEdge,
  toOldSyntax,
  )

import Control.Monad                    (forM_, void, when)
import Control.Monad.IO.Class           (MonadIO (liftIO))
import Control.Monad.Output (
  LangM,
  Language (English, German),
  OutputMonad (..),
  Rated,
  english,
  enumerateM,
  german,
  multipleChoice,
  singleChoiceSyntax,
  translate,
  )
import Control.Monad.Random
  (RandT, RandomGen, StdGen, evalRandT, getRandomR, getStdGen, mkStdGen)
import Control.Monad.Trans              (MonadTrans (lift))
import Data.Bifunctor                   (second)
import Data.GraphViz                    (DirType (..))
import Data.List                        (nub)
import Data.Map                         (Map)
import Data.Maybe                       (mapMaybe, fromMaybe)
import Data.String.Interpolate          (i)
import GHC.Generics                     (Generic)
import Language.Alloy.Call              (AlloyInstance)
import System.Random.Shuffle            (shuffleM)

debug :: Bool
debug = False

phraseChangeDE :: Bool -> Bool -> Change DiagramEdge -> String
phraseChangeDE byName withDir c = case (add c, remove c) of
  (Nothing, Nothing) -> "verändere nichts"
  (Just a,  Nothing) -> "ergänze " ++ trailingComma (phraseRelationDE False withDir a)
  (Nothing, Just e)  -> "entferne " ++ phraseRelationDE byName withDir e
  (Just a,  Just e)  ->
    "ersetze " ++ trailingComma (phraseRelationDE byName withDir e)
    ++ " durch " ++ phraseRelationDE False withDir a
  where
    trailingComma xs
      | ',' `elem` xs = xs ++ ","
      | otherwise     = xs

phraseRelationDE :: Bool -> Bool -> DiagramEdge -> String
phraseRelationDE _ _ (from, to, Inheritance) =
  "eine Vererbung, bei der " ++ from ++ " von " ++ to ++ " erbt"
phraseRelationDE True _ (_, _, Assoc t n _ _ _) = (++ n) $ case t of
  Association -> "Assoziation "
  Aggregation -> "Aggregation "
  Composition -> "Komposition "
phraseRelationDE _ False (from, to, Assoc Association _ l h _)
  | from == to = [i|eine Selbst-Assoziation #{selfParticipates}|]
  | otherwise  = "eine Assoziation" ++ participationsDE l from h to
  where
    selfParticipates :: String
    selfParticipates =
      [i|für #{from}, wobei es an einem Ende #{phraseLimitDE l} genau einmal und am anderen Ende #{phraseLimitDE h} beteiligt ist|]
phraseRelationDE _ _ (from, to, Assoc t _ l h _)
  | from == to = case t of
      Association -> [i|eine Selbst-Assoziation #{selfParticipates}|]
      Aggregation -> [i|eine Selbst-Aggregation #{selfParticipatesPartWhole}|]
      Composition -> [i|eine Selbst-Komposition #{selfParticipatesPartWhole}|]
  | otherwise = (++ participationsDE l from h to) $ case t of
    Association -> "eine Assoziation von " ++ from ++ " nach " ++ to
    Aggregation -> "eine Beziehung, die " ++ from
      ++ " eine Aggregation aus " ++ to ++ "s macht"
    Composition -> "eine Beziehung, die " ++ from
      ++ " eine Komposition aus " ++ to ++ "s macht"
  where
    selfParticipates :: String
    selfParticipates =
      [i|für #{from}, wobei es am Anfang #{phraseLimitDE l} und am Ende #{phraseLimitDE h} beteiligt ist|]
    selfParticipatesPartWhole :: String
    selfParticipatesPartWhole =
      [i|für #{from}, wobei es #{phraseLimitDE l} als Teil and #{phraseLimitDE h} als Ganzes beteiligt ist|]

participationsDE
  :: (Int, Maybe Int)
  -> String
  -> (Int, Maybe Int)
  -> String
  -> String
participationsDE l from h to =
  [i|, wobei #{from} #{phraseLimitDE l} und #{to} #{phraseLimitDE h} beteiligt ist|]

phraseLimitDE :: (Int, Maybe Int) -> String
phraseLimitDE (0, Just 0)  = "gar nicht"
phraseLimitDE (1, Just 1)  = "genau einmal"
phraseLimitDE (2, Just 2)  = "genau zweimal"
phraseLimitDE (-1, Just n) = "*.." ++ show n ++ "-mal"
phraseLimitDE (m, Nothing) = show m ++ "..*-mal"
phraseLimitDE (m, Just n)  = show m ++ ".." ++ show n ++ "-mal"

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
    allowedProperties :: AllowedProperties,
    classConfig      :: ClassConfig,
    maxInstances     :: Maybe Integer,
    noIsolationLimit :: Bool,
    printNames       :: Bool,
    printNavigations :: Bool,
    timeout          :: Maybe Int,
    useNames         :: Bool
  } deriving (Generic, Read, Show)

defaultRepairCdConfig :: RepairCdConfig
defaultRepairCdConfig = RepairCdConfig {
    allowedProperties = allowEverything {
        reverseInheritances    = False,
        Modelling.CdOd.RepairCd.selfInheritances = False
        },
    classConfig = ClassConfig {
        classes      = (4, 4),
        aggregations = (0, Just 2),
        associations = (0, Just 2),
        compositions = (0, Just 3),
        inheritances = (1, Just 3)
      },
    maxInstances     = Just 200,
    noIsolationLimit = False,
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

repairCdTask
  :: (OutputMonad m, MonadIO m)
  => FilePath
  -> RepairCdInstance
  -> LangM m
repairCdTask path task = do
  cd <- lift . liftIO $ cacheCd
    (withDirections task)
    (withNames task)
    mempty
    (classDiagram task)
    path
  paragraph $ translate $ do
    english "Consider the following class diagram, which unfortunately is invalid."
    german "Betrachten Sie das folgende Klassendiagramm, welches leider ungültig ist."
  image cd
  paragraph $ translate $ do
    english [i|Which of the following changes would repair the class diagram?|]
    german [i|Welche der folgenden Änderungen würden das Klassendiagramm reparieren?|]
  let phrase x y z = translate $ do
        english $ phraseChange x y z
        german $ phraseChangeDE x y z
  enumerateM (text . show) $ second (phrase (withNames task) (withDirections task) . snd) <$> M.toList (changes task)
  paragraph $ translate $ do
    english [i|Please state your answer by giving a list of numbers, indicating all changes each resulting in a valid class diagram.|]
    german [i|Bitte geben Sie Ihre Antwort als Liste aller Zahlen an, deren Änderungen jeweils in einem gültigen Klassendiagramm resultieren. |]
  paragraph $ do
    translate $ do
      english [i|Answer by giving a comma separated list of all valid options, e.g. |]
      german [i|Antworten Sie durch Angabe einer durch Komma separierten Liste aller gültigen Optionen. Zum Beispiel |]
    code "[1, 2]"
    translate $ do
      english [i| would indicate that options 1 and 2 each repair the given class diagram.|]
      german [i| als Angabe würde bedeuten, dass die Optionen 1 und 2 jeweils das gegebene Klassendiagramm reparieren würden.|]
  paragraph simplifiedInformation
  paragraph hoveringInformation

repairCdSyntax :: OutputMonad m => RepairCdInstance -> [Int] -> LangM m
repairCdSyntax inst xs =
  forM_ xs $ singleChoiceSyntax True (M.keys $ changes inst)

repairCdEvaluation :: OutputMonad m => RepairCdInstance -> [Int] -> Rated m
repairCdEvaluation inst xs = addPretext $ do
  let chs = M.fromList [
        (English, "changes"),
        (German, "Änderungen")
        ]
      solution = fst <$> changes inst
  multipleChoice chs (Just $ show $ repairCdSolution inst) solution xs

repairCdSolution :: RepairCdInstance -> [Int]
repairCdSolution = M.keys . M.filter id . fmap fst . changes

data RepairCdInstance = RepairCdInstance {
    changes        :: Map Int (Bool, Change DiagramEdge),
    classDiagram   :: Syntax,
    withDirections :: Bool,
    withNames      :: Bool
  } deriving (Generic, Read, Show)

repairCd
  :: RepairCdConfig
  -> Int
  -> Int
  -> IO RepairCdInstance
repairCd config segment seed = do
  let g = mkStdGen $ (segment +) $ 4 * seed
  (cd, chs) <- flip evalRandT g $ repairIncorrect
    (allowedProperties config)
    (classConfig config)
    (noIsolationLimit config)
    (maxInstances config)
    (timeout config)
  let chs' = map (second fst) chs
  return $ RepairCdInstance
    (M.fromList $ zip [1..] chs')
    cd
    (printNavigations config)
    (printNames config && useNames config)

defaultRepairCdInstance :: RepairCdInstance
defaultRepairCdInstance = RepairCdInstance {
  changes = M.fromList [
    (1,(False,Change {
          add = Just ("A","D",Assoc Composition "s" (1,Just 1) (2,Nothing) False),
          remove = Just ("A","D",Assoc Composition "v" (1,Just 1) (0,Nothing) False)
          })),
    (2,(False,Change {
          add = Just ("C","A",Assoc Aggregation "t" (2,Just 2) (1,Nothing) False),
          remove = Nothing
          })),
    (3,(True,Change {
          add = Just ("B","D",Assoc Composition "z" (1,Just 1) (0,Nothing) False),
          remove = Just ("D","B",Assoc Composition "x" (1,Just 1) (0,Nothing) False)
          })),
    (4,(True,Change {
          add = Just ("A","B",Assoc Composition "u" (1,Just 1) (0,Just 2) False),
          remove = Just ("B","A",Assoc Composition "w" (1,Just 1) (0,Just 2) False)
          }))
    ],
  classDiagram = (
    [("A",[]),("D",[]),("B",[]),("C",[])],
    [(Composition,"v",(1,Just 1),"A","D",(0,Nothing)),
     (Composition,"x",(1,Just 1),"D","B",(0,Nothing)),
     (Composition,"w",(1,Just 1),"B","A",(0,Just 2)),
     (Association,"y",(0,Just 2),"C","A",(2,Nothing))]),
  withDirections = False,
  withNames = True
  }

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
  => AllowedProperties
  -> ClassConfig
  -> Bool
  -> Maybe Integer
  -> Maybe Int
  -> RandT g IO (Syntax, [(Bool, (Change DiagramEdge, Syntax))])
repairIncorrect allowed config noIsolationLimitation maxInsts to = do
  e0:_    <- shuffleM $ illegalChanges allowed
  l0:ls   <- shuffleM $ legalChanges allowed
  let addLegals
        | l1:_ <- ls = (l1 .&. noChange :) . (l1 :)
        | otherwise  = id
  c0:_    <- shuffleM $ allChanges allowed
  csm     <- shuffleM $ c0 : noChange : addLegals [e0]
  cs      <- shuffleM $ l0 .&. e0 : noChange : take 2 csm
--  config' <- constrainConfig 5 config
  let alloyCode = Changes.transformChanges config (toProperty e0) (Just config)
        $ map toProperty cs
  when debug $ liftIO $ do
    putStrLn $ changeName e0
    print $ map changeName cs
    writeFile "repair.als" alloyCode
  instas  <- liftIO $ getInstances maxInsts to alloyCode
  rinstas <- shuffleM instas
  inst <- getInstanceWithODs (map isValid cs) rinstas
  let names = nub $ classNames (fst inst)
      assocs = nub $ associationNames (fst inst)
        ++ mapMaybe (addedAssociation . fst . snd) (snd inst)
  names'  <- shuffleM names
  assocs' <- shuffleM assocs
  let bmNames  = BM.fromList $ zip names names'
      bmAssocs = BM.fromList $ zip assocs assocs'
      renameCd cd = renameClassesInCd bmNames cd
        >>= renameAssocsInCd bmAssocs
      renameEdge e = renameAssocsInEdge bmAssocs e
        >>= renameClassesInEdge bmNames
  liftIO $ (,)
    <$> renameCd (fst inst)
    <*> mapM (\(b, (c, cd)) -> fmap (b,) . (,) <$> mapM renameEdge c <*> renameCd cd) (snd inst)
  where
    drawCd :: Syntax -> Integer -> IO FilePath
    drawCd cd' n =
      drawCdFromSyntax True True mempty cd' ("cd-" ++ show n ++ ".svg")
    drawOd :: Syntax -> AlloyInstance -> Integer -> RandT StdGen IO FilePath
    drawOd cd od x =
      let backwards   = [n | (_, _, Assoc t n _ _ _) <- toEdges cd
                           , t /= Association]
          forwards    = [n | (_, _, Assoc t n _ _ _) <- toEdges cd
                           , t == Association]
          navigations = foldr (`M.insert` Back)
                              (foldr (`M.insert` Forward) M.empty forwards)
                              backwards
      in drawOdFromInstance od Nothing navigations True ("od-" ++ show x)
    getInstanceWithODs _  [] =
      repairIncorrect allowed config noIsolationLimitation maxInsts to
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
          g <- getStdGen
          flip evalRandT g $ uncurry (drawOd cd . head) `mapM_` zip ods [1 ..]
        return (cd, chs')
        else getInstanceWithODs vs rinstas
    getOD cd = do
      let parts = combineParts $ transform
            (toOldSyntax cd)
            maxFiveObjects
            Nothing
            noIsolationLimitation
            ""
            ""
          command = createRunCommand
            "cd"
            (length $ fst cd)
            maxFiveObjects
      getInstances (Just 1) to (parts ++ command)

data AllowedProperties = AllowedProperties {
  compositionCycles      :: Bool,
  doubleRelationships    :: Bool,
  inheritanceCycles      :: Bool,
  reverseInheritances    :: Bool,
  reverseRelationships   :: Bool,
  selfInheritances       :: Bool,
  selfRelationships      :: Bool,
  wrongAssociationLimits :: Bool,
  wrongCompositionLimits :: Bool
  } deriving (Generic, Read, Show)

allowEverything :: AllowedProperties
allowEverything = AllowedProperties {
  compositionCycles = True,
  doubleRelationships    = True,
  inheritanceCycles      = True,
  reverseInheritances = True,
  reverseRelationships   = True,
  selfInheritances       = True,
  selfRelationships      = True,
  wrongAssociationLimits = True,
  wrongCompositionLimits = True
  }

allChanges :: AllowedProperties -> [PropertyChange]
allChanges c = legalChanges c ++ illegalChanges c

noChange :: PropertyChange
noChange = PropertyChange "none" id id

infixl 9 .&.
(.&.) :: PropertyChange -> PropertyChange -> PropertyChange
PropertyChange n1 o1 v1 .&. PropertyChange n2 o2 v2 = PropertyChange
  (n1 ++ " + " ++ n2)
  (o1 . o2)
  (v1 . v2)

legalChanges :: AllowedProperties -> [PropertyChange]
legalChanges allowed = noChange : [
    PropertyChange "add one self relationship" addSelfRelationships id
  | Modelling.CdOd.RepairCd.selfRelationships allowed] ++ [
    PropertyChange "force double relationships" withDoubleRelationships id
  | doubleRelationships allowed] ++ [
    PropertyChange "force reverse relationships" withReverseRelationships id
  | reverseRelationships allowed]
--    PropertyChange "force multiple inheritances" withMultipleInheritances id
  where
    addSelfRelationships :: RelationshipProperties -> RelationshipProperties
    addSelfRelationships config@RelationshipProperties {..}
      = config { T.selfRelationships = selfRelationships + 1 }
    withDoubleRelationships :: RelationshipProperties -> RelationshipProperties
    withDoubleRelationships config
      = config { hasDoubleRelationships = True }
    withReverseRelationships :: RelationshipProperties -> RelationshipProperties
    withReverseRelationships config
      = config { hasReverseRelationships = True }
    -- withMultipleInheritances :: RelationshipProperties -> RelationshipProperties
    -- withMultipleInheritances config
    --   = config { hasMultipleInheritances = True }

illegalChanges :: AllowedProperties -> [PropertyChange]
illegalChanges allowed = map ($ const False) $ [
    PropertyChange "add wrong association" addWrongAssocs
  | wrongAssociationLimits allowed] ++ [
    PropertyChange "add wrong composition" addWrongCompositions
  | wrongCompositionLimits allowed] ++ [
    PropertyChange "force inheritance cycles" withNonTrivialInheritanceCycles
  | inheritanceCycles allowed] ++ [
    PropertyChange "force reverse inheritances" withReverseInheritances
  | reverseInheritances allowed] ++ [
    PropertyChange "add self inheritance" addSelfInheritance
  | Modelling.CdOd.RepairCd.selfInheritances allowed] ++ [
    PropertyChange "force composition cycles" withCompositionCycles
  | compositionCycles allowed]
  where
    addWrongAssocs :: RelationshipProperties -> RelationshipProperties
    addWrongAssocs config@RelationshipProperties {..}
      = config { wrongAssocs = wrongAssocs + 1 }
    addWrongCompositions :: RelationshipProperties -> RelationshipProperties
    addWrongCompositions config@RelationshipProperties {..}
      = config { wrongCompositions = wrongCompositions + 1 }
    addSelfInheritance :: RelationshipProperties -> RelationshipProperties
    addSelfInheritance config@RelationshipProperties {..}
      = config { T.selfInheritances = selfInheritances + 1 }
    withReverseInheritances :: RelationshipProperties -> RelationshipProperties
    withReverseInheritances config
      = config { hasReverseInheritances = True }
    withNonTrivialInheritanceCycles
      :: RelationshipProperties
      -> RelationshipProperties
    withNonTrivialInheritanceCycles config
      = config { hasNonTrivialInheritanceCycles = True }
    withCompositionCycles :: RelationshipProperties -> RelationshipProperties
    withCompositionCycles config
      = config { hasCompositionCycles = True }
