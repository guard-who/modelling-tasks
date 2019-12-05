{-# LANGUAGE RecordWildCards #-}
module NaiveTasks where

import qualified CdAndChanges.Transform           as Changes (transformChanges)
import qualified Alloy                            (getInstances)

import qualified Data.Bimap                       as BM (fromList)

import Edges                            (fromEdges, renameEdges)
import Generate                         (generate)
import Output                           (drawCdFromSyntax)
import CD2Alloy.Transform               (createRunCommand, mergeParts, transform)
import Types (
  AssociationType (..),
  ClassConfig (..),
  Change (..),
  Connection (..),
  DiagramEdge,
  RelationshipProperties (..),
  Syntax,
  defaultProperties,
  )
import Auxiliary.Util

import Control.Monad                    (when)
import Control.Monad.IO.Class           (liftIO)
import Control.Monad.Random             (MonadRandom, RandomGen, RandT)
import Data.Bifunctor                   (first, second)
import Data.Bimap                       (Bimap)
import Data.GraphViz                    (GraphvizOutput (Pdf))
import Data.List                        (permutations)
import Data.Maybe                       (listToMaybe)
import MatchCdOd                        (applyChanges)
import System.Random.Shuffle            (shuffleM)

debug :: Bool
debug = False

phraseChange :: Change DiagramEdge -> String
phraseChange c = case (add c, remove c) of
  (Nothing, Nothing) -> "change nothing"
  (Just e,  Nothing) -> "add " ++ phraseRelation e
  (Nothing, Just e ) -> "remove " ++ phraseRelation e
  (Just e1, Just e2) -> "replace " ++ phraseRelation e2 ++ " by " ++ phraseRelation e1

phraseRelation :: DiagramEdge -> String
phraseRelation (from, to, Inheritance) =
  "an inheritance where " ++ from ++ " inherits from " ++ to
phraseRelation (from, to, Assoc t _ l h _) = (++ participations) $ case t of
  Association -> "an association from " ++ from ++ " to " ++ to
  Aggregation -> "an aggregation for " ++ from ++ " of " ++ to
  Composition -> "an composition for " ++ from ++ " of " ++ to
  where
    participations = " where " ++ participates l from ++ " and " ++ participates h to
    participates r c = c ++ " participates " ++ phraseLimit r ++ " times"

phraseLimit :: (Int, Maybe Int) -> String
phraseLimit (-1, Just n) = "*.." ++ show n
phraseLimit (m, Nothing) = show m ++ "..*"
phraseLimit (m, Just n)  = show m ++ ".." ++ show n

data PropertyChange = PropertyChange {
    changeName     :: String,
    operation      :: RelationshipProperties -> RelationshipProperties,
    validityChange :: Bool -> Bool
  }

toProperty :: PropertyChange -> RelationshipProperties
toProperty p = operation p defaultProperties

isValid :: PropertyChange -> Bool
isValid p = validityChange p True

repairIncorrect
  :: RandomGen g
  => ClassConfig
  -> RandT g IO (Syntax, [(Bool, Change DiagramEdge)])
repairIncorrect config = do
  e0:_    <- shuffleM illegalChanges
  l0:l1:_ <- shuffleM legalChanges
  c0:_    <- shuffleM changes
  csm     <- shuffleM $ c0 : noChange : l1 .&. noChange : l1 : [e0]
  cs      <- shuffleM $ l0 .&. e0 : noChange : take 2 csm
  let code = Changes.transformChanges config (toProperty e0) (Just config)
        $ toProperty <$> cs
  when debug $ liftIO $ do
    putStrLn $ changeName e0
    print $ changeName <$> cs
    writeFile "repair.als" code
  instas <- liftIO $ Alloy.getInstances 200 code
  if null instas then liftIO (putStr ".") >> repairIncorrect config
    else do
    rinsta       <- head <$> shuffleM instas
    (cd, chs, _) <- applyChanges rinsta
    let chs' = zip (isValid <$> cs) (fst <$> chs)
    when debug $ liftIO $ do
      drawCd cd 0
      uncurry drawCd `mapM_` zip (snd <$> chs) [1 ..]
    return (cd, chs')
  where
    drawCd :: Syntax -> Integer -> IO ()
    drawCd cd' n = drawCdFromSyntax True True Nothing cd' ("cd-" ++ show n) Pdf

changes :: [PropertyChange]
changes = legalChanges ++ illegalChanges

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
    withMultipleInheritances :: RelationshipProperties -> RelationshipProperties
    withMultipleInheritances config
      = config { hasMultipleInheritances = True }

illegalChanges :: [PropertyChange]
illegalChanges = ($ const False) <$> [
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

getDifferentNamesTask
  :: RandomGen g
  => ClassConfig
  -> Int
  -> Int
  -> Int
  -> RandT g IO (Syntax, String, Bimap String String)
getDifferentNamesTask config maxObjects searchSpace maxInstances = do
  configs <- withMinimalLabels 3 config
  continueWithHead configs $ \config' -> do
    (names, edges) <- generate config' searchSpace
    let cd0    = (0 :: Integer, fromEdges names edges)
        parts0 = extractFourParts cd0
        labels = [l | (_, l, _, _, _, _) <- snd $ snd cd0]
        cds    = fromEdges names
          . flip renameEdges edges . BM.fromList . zip labels
          <$> drop 1 (permutations labels)
        cds'   = zip [1 :: Integer ..] cds
        partss = extractFourParts <$> cds'
        runCmd = foldr (\(n, _) -> (++ " and (not cd" ++ show n ++ ")")) "cd0" cds'
        onlyCd0 = createRunCommand runCmd (length names) maxObjects
        partss' = foldr mergeParts parts0 partss
    when debug . liftIO $ drawCd cd0
    when debug . liftIO $ drawCd `mapM_` cds'
    instances  <- liftIO
      $ Alloy.getInstances maxInstances (combineParts partss' ++ onlyCd0)
    instances' <- shuffleM instances
    continueWithHead instances' $ \od1 -> do
      labels' <- shuffleM labels
      let bm  = BM.fromList $ zip labels' $ (:[]) <$> ['a', 'b' ..]
          cd1 = fromEdges names $ renameEdges bm edges
      return (cd1, od1, bm)
  where
    toOldSyntax = first (second listToMaybe <$>)
    extractFourParts (n, cd) = case transform (toOldSyntax cd) (show n) "" of
      (p1, p2, p3, p4, _) -> (p1, p2, p3, p4)
    combineParts (p1, p2, p3, p4) = p1 ++ p2 ++ p3 ++ p4
    drawCd (n, cd) =
      drawCdFromSyntax True True (Just redColor) cd ("debug-" ++ show n) Pdf
    continueWithHead []    _ =
      getDifferentNamesTask config maxObjects searchSpace maxInstances
    continueWithHead (x:_) f = f x

withMinimalLabels :: MonadRandom m => Int -> ClassConfig -> m [ClassConfig]
withMinimalLabels n config
  | n <= lowerLimit = return [config]
  | otherwise       = shuffleM
    [ config {
        aggregations = (aggrs, snd (aggregations config)),
        associations = (assos, snd (associations config)),
        compositions = (comps, snd (compositions config))
      }
    | aggrs <- range aggregations  0                           n
    , assos <- range associations  0                          (n - aggrs)
    , comps <- range compositions (max 0 $ n - aggrs - assos) (n - aggrs - assos)]
  where
    lowerLimit = 0
      + fst (aggregations config)
      + fst (associations config)
      + fst (compositions config)
    min' l1 Nothing   = l1
    min' l1 (Just l2) = min l1 l2
    range f low high  = [low + fst (f config) .. min' high (snd $ f config)]
  
