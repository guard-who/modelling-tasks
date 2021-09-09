{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
module Modelling.CdOd.DifferentNames (
  DifferentNamesConfig (..),
  DifferentNamesInstance (..),
  debug,
  defaultDifferentNamesConfig,
  differentNames,
  differentNamesEvaluation,
  differentNamesTask,
  ) where

import qualified Data.Bimap                       as BM
  (fromList, keysR, lookup, lookupR, toList, twist)
import qualified Data.Map                         as M (empty, insert)
import qualified Data.Set                         as S (toList)

import Modelling.Auxiliary.Output (
  OutputMonad (..),
  directionsAdvice,
  hoveringInformation,
  simplifiedInformation,
  LangM,
  )
import Modelling.CdOd.Auxiliary.Util
import Modelling.CdOd.CD2Alloy.Transform (createRunCommand, mergeParts, transform)
import Modelling.CdOd.Edges             (fromEdges, renameEdges, toEdges)
import Modelling.CdOd.Generate          (generate)
import Modelling.CdOd.Output            (drawCdFromSyntax, drawOdFromNodesAndEdges)
import Modelling.CdOd.Types (
  AssociationType (..),
  ClassConfig (..),
  Connection (..),
  DiagramEdge,
  NameMapping (NameMapping, nameMapping),
  Od,
  Syntax,
  renameAssocsInCd,
  renameClassesInCd,
  renameClassesInOd,
  renameLinksInOd,
  toOldSyntax,
  )

import Control.Monad                    (void, when)
import Control.Monad.IO.Class           (MonadIO (liftIO))
import Control.Monad.Random
  (MonadRandom, RandT, RandomGen, evalRandT, mkStdGen)
import Control.Monad.Trans              (MonadTrans (lift))
import Control.Monad.Trans.Except       (ExceptT, runExceptT)
import Data.Bimap                       (Bimap)
import Data.GraphViz                    (DirType (..), GraphvizOutput (Pdf, Svg))
import Data.List                        (nub, permutations, sort)
import Data.Maybe                       (catMaybes, fromMaybe, isJust)
import Data.String.Interpolate          (i)
import GHC.Generics                     (Generic)
import Language.Alloy.Call
  (AlloyInstance, getTriple, lookupSig, objectName, scoped)
import System.Random.Shuffle            (shuffleM)

debug :: Bool
debug = False

data DifferentNamesInstance = DifferentNamesInstance {
    cDiagram :: Syntax,
    generatorValue :: Int,
    oDiagram :: Od,
    mapping  :: NameMapping
  } deriving (Generic, Read, Show)

data DifferentNamesConfig = DifferentNamesConfig {
    allowSelfLoops   :: Maybe Bool,
    classConfig      :: ClassConfig,
    maxObjects       :: Int,
    withNonTrivialInheritance :: Maybe Bool,
    maxInstances     :: Maybe Integer,
    searchSpace      :: Int,
    timeout          :: Maybe Int
  } deriving (Generic, Read, Show)

defaultDifferentNamesConfig :: DifferentNamesConfig
defaultDifferentNamesConfig = DifferentNamesConfig {
    allowSelfLoops   = Nothing,
    classConfig  = ClassConfig {
        classes      = (4, 4),
        aggregations = (0, Just 2),
        associations = (0, Just 2),
        compositions = (0, Just 1),
        inheritances = (1, Just 2)
      },
    maxObjects       = 4,
    withNonTrivialInheritance = Just True,
    maxInstances     = Nothing,
    searchSpace      = 10,
    timeout          = Nothing
  }

differentNamesTask
  :: (OutputMonad m, MonadIO m)
  => FilePath
  -> DifferentNamesInstance
  -> LangM m
differentNamesTask path task = do
  let cd = cDiagram task
      od = oDiagram task
      bm = nameMapping $ mapping task
      backwards   = [n | (_, _, Assoc t n' _ _ _) <- toEdges cd
                       , t /= Association
                       , n <- BM.lookup n' bm]
      forwards    = [n | (_, _, Assoc t n' _ _ _) <- toEdges cd
                       , t == Association
                       , n <- BM.lookup n' bm]
      navigations = foldr (`M.insert` Back)
                          (foldr (`M.insert` Forward) M.empty forwards)
                          backwards
      anonymous = fromMaybe (length (fst od) `div` 3) (Just 1000)
  cd' <- lift $ liftIO $ drawCdFromSyntax True True Nothing cd (path ++ "-cd") Svg
  od' <- lift $ liftIO $ flip evalRandT (mkStdGen $ generatorValue task) $
    uncurry drawOdFromNodesAndEdges od anonymous navigations True (path ++ "-od") Svg
  paragraph $ text "Consider the following class diagram:"
  image cd'
  paragraph $ text "and the following object diagram (which conforms to it):"
  image od'
  paragraph $ do
    text [i|Which relationship in the class diagram (CD) corresponds to which of the links in the object diagram (OD)?
State your answer by giving a mapping of relationships in the CD to links in the OD.
To state that "a" in the CD corresponds to "x" in the OD and "b" in the CD corresponds to "y" in the OD write it as:|]
    code $ show [("a", "x"), ("b", "y")]
  paragraph $ text
    [i|Please note: Links are already grouped correctly and fully, i.e. all links with the same name (and only links with the same name!) in the OD correspond to exactly the same relationship name in the CD.
Thus, every link name and every relationship name should occur exactly once in your mapping.|]
  paragraph simplifiedInformation
  paragraph directionsAdvice
  paragraph hoveringInformation

differentNamesEvaluation
  :: OutputMonad m
  => DifferentNamesInstance
  -> [(String, String)]
  -> LangM m
differentNamesEvaluation task cs = do
  paragraph $ text "Remarks on your solution:"
  let ss = catMaybes $ readMapping <$> cs
  assertion (length ss == length cs)
    "All provided pairs are linking a valid link and a valid relationship"
  let cs' = catMaybes $ readValidMapping <$> cs
  let ms = BM.toList $ nameMapping $ mapping task
  assertion (length cs' == length ss)
    "Given mappings are correct?"
  assertion (nub (sort cs') == ms) "Given mappings are exhaustive?"
  where
    m = nameMapping $ mapping task
    readMapping (x, y)
      | isJust $ BM.lookup x m, isJust $ BM.lookupR y m
      = Just (x, y)
      | isJust $ BM.lookup y m, isJust $ BM.lookupR x m
      = Just (y, x)
      | otherwise
      = Nothing
    readValidMapping (x, y)
      | Just y' <- BM.lookup x m, isJust $ BM.lookupR y m, y' == y
      = Just (x, y)
      | Just x' <- BM.lookup y m, isJust $ BM.lookupR x m, x' == x
      = Just (y, x)
      | otherwise
      = Nothing

differentNames
  :: MonadIO m
  => DifferentNamesConfig
  -> Int
  -> Int
  -> ExceptT String m DifferentNamesInstance
differentNames config segment seed = do
  let gv = (segment +) $ 4 * seed
      g = mkStdGen gv
  (cd, od, bm) <- liftIO $ evalRandT (getDifferentNamesTask config) g
  return $ DifferentNamesInstance {
    cDiagram  = cd,
    generatorValue = gv,
    oDiagram  = od,
    mapping   = NameMapping bm
    }

reverseAssociation :: DiagramEdge -> DiagramEdge
reverseAssociation (from, to, e@(Assoc Association _ _ _ _)) =
  (to, from, e)
reverseAssociation x = x

getDifferentNamesTask
  :: RandomGen g
  => DifferentNamesConfig
  -> RandT g IO (Syntax, Od, Bimap String String)
getDifferentNamesTask config = do
  configs <- withMinimalLabels 3 $ classConfig config
  continueWithHead configs $ \config' -> do
    (names, edges') <- generate
      (withNonTrivialInheritance config)
      config'
      $ searchSpace config
    let edges  = reverseAssociation <$> edges'
        cd0    = (0 :: Integer, fromEdges names edges)
        parts0 = extractFourParts cd0
        labels = [l | (_, l, _, _, _, _) <- snd $ snd cd0]
        cds    = map
          (fromEdges names . flip renameEdges edges . BM.fromList . zip labels)
          $ drop 1 (permutations labels)
        cds'   = zip [1 :: Integer ..] cds
        partss = map extractFourParts cds'
        runCmd = foldr (\(n, _) -> (++ " and (not cd" ++ show n ++ ")")) "cd0" cds'
        onlyCd0 = createRunCommand
          (allowSelfLoops config)
          runCmd
          (length names)
          $ maxObjects config
        partss' = foldr mergeParts parts0 partss
    when debug . liftIO . void $ drawCd cd0
    when debug . liftIO . void $ drawCd `mapM_` cds'
    instances  <- liftIO $ getInstances
      (maxInstances config)
      (timeout config)
      (combineParts partss' ++ onlyCd0)
    instances' <- shuffleM (instances :: [AlloyInstance])
    continueWithHead instances' $ \od1 -> do
      labels' <- shuffleM labels
      let bm  = BM.fromList $ zip (map (:[]) ['a', 'b' ..]) labels'
          cd1 = fromEdges names $ renameEdges (BM.twist bm) edges
          --bm' = BM.filter (const (`elem` usedLabels od1)) bm
      if BM.keysR bm == sort (usedLabels od1)
        then do
        let (assocs, links) = unzip $ BM.toList bm
        names'  <- shuffleM names
        assocs' <- shuffleM assocs
        links'  <- shuffleM links
        od1' <- either error id <$> runExceptT (alloyInstanceToOd od1)
        let bmNames  = BM.fromList $ zip names names'
            bmAssocs = BM.fromList $ zip assocs assocs'
            bmLinks  = BM.fromList $ zip links links'
            bm'      = BM.fromList $ zip assocs' links'
        cd2 <- liftIO $ renameClassesInCd bmNames =<< renameAssocsInCd bmAssocs cd1
        od2 <- liftIO $ renameClassesInOd bmNames =<< renameLinksInOd bmLinks od1'
        return (cd2, od2, bm')
        else getDifferentNamesTask config
  where
    extractFourParts (n, cd) = case transform (toOldSyntax cd) (allowSelfLoops config) (show n) "" of
      (p1, p2, p3, p4, _) -> (p1, p2, p3, p4)
    combineParts (p1, p2, p3, p4) = p1 ++ p2 ++ p3 ++ p4
    drawCd (n, cd) =
      drawCdFromSyntax True True (Just redColor) cd ("debug-" ++ show n) Pdf
    continueWithHead
      :: RandomGen g
      => [a]
      -> (a -> RandT g IO (Syntax, Od, Bimap String String))
      -> RandT g IO (Syntax, Od, Bimap String String)
    continueWithHead []    _ =
      getDifferentNamesTask config
    continueWithHead (x:_) f = f x
    usedLabels :: AlloyInstance -> [String]
    usedLabels inst = either error id $ do
      os    <- lookupSig (scoped "this" "Obj") inst
      links <- map nameOf . S.toList <$> getTriple "get" os
      return $ nub links
    nameOf (_,l,_) = takeWhile (/= '$') $ objectName l

withMinimalLabels :: MonadRandom m => Int -> ClassConfig -> m [ClassConfig]
withMinimalLabels n config
  | n <= lowerLimit = return [config]
  | Just u <- upperLimit, n > u = return [config]
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
    upperLimit = (\x y z -> x + y + z)
      <$> snd (aggregations config)
      <*> snd (associations config)
      <*> snd (compositions config)
    lowerLimit = 0
      + fst (aggregations config)
      + fst (associations config)
      + fst (compositions config)
    min' l1 Nothing   = l1
    min' l1 (Just l2) = min l1 l2
    range f low high  = [low + fst (f config) .. min' high (snd $ f config)]
