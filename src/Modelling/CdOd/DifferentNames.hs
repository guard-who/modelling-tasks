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
  (filter, fromList, lookup, toList, twist)
import qualified Data.Map                         as M (empty, insert)
import qualified Data.Set                         as S (toList)

import Modelling.Auxiliary.Output (
  OutputMonad (..),
  directionsAdvice,
  hoveringInformation,
  simplifiedInformation,
  )
import Modelling.CdOd.Auxiliary.Util
import Modelling.CdOd.CD2Alloy.Transform (createRunCommand, mergeParts, transform)
import Modelling.CdOd.Edges             (fromEdges, renameEdges, toEdges)
import Modelling.CdOd.Generate          (generate)
import Modelling.CdOd.Output            (drawCdFromSyntax, drawOdFromInstance)
import Modelling.CdOd.Types (
  AssociationType (..),
  ClassConfig (..),
  Connection (..),
  Syntax,
  toOldSyntax,
  )

import Control.Monad                    (void, when)
import Control.Monad.IO.Class           (liftIO)
import Control.Monad.Random
  (MonadRandom, RandomGen, RandT, evalRandT, mkStdGen)
import Data.Bimap                       (Bimap)
import Data.GraphViz                    (DirType (..), GraphvizOutput (Pdf, Svg))
import Data.List                        (nub, permutations, sort)
import Data.String.Interpolate          (i)
import GHC.Generics                     (Generic)
import Language.Alloy.Call
  (AlloyInstance, getTriple, lookupSig, objectName, scoped)
import System.Random.Shuffle            (shuffleM)

debug :: Bool
debug = False

data DifferentNamesInstance = DifferentNamesInstance {
    cDiagram :: FilePath,
    oDiagram :: FilePath,
    mapping  :: Bimap String String
  } deriving (Generic, Show)

data DifferentNamesConfig = DifferentNamesConfig {
    classConfig      :: ClassConfig,
    maxObjects       :: Int,
    maxInstances     :: Maybe Integer,
    searchSpace      :: Int,
    timeout          :: Maybe Int
  } deriving Generic

defaultDifferentNamesConfig :: DifferentNamesConfig
defaultDifferentNamesConfig = DifferentNamesConfig {
    classConfig  = ClassConfig {
        classes      = (4, 4),
        aggregations = (0, Just 2),
        associations = (0, Just 2),
        compositions = (0, Just 1),
        inheritances = (1, Just 2)
      },
    maxObjects       = 4,
    maxInstances     = Nothing,
    searchSpace      = 10,
    timeout          = Nothing
  }

differentNamesTask :: OutputMonad m => DifferentNamesInstance -> m ()
differentNamesTask task = do
  paragraph "Consider the following class diagram"
  image $ cDiagram task
  paragraph "and the following object diagram."
  image $ oDiagram task
  paragraph [i|Which relationship in the class diagram (CD) corresponds to which of the links in the object diagram (OD)?
State your answer by giving a mapping of relationships in the CD to links in the OD.
To state that "foo" in the CD corresponds to "bar" in the OD and "foofoo" in the CD corresponds to "baz" in the OD write it as
[("foo", "bar"), ("foofoo", "baz")].|]
  paragraph simplifiedInformation
  paragraph directionsAdvice
  paragraph hoveringInformation

differentNamesEvaluation
  :: OutputMonad m
  => DifferentNamesInstance
  -> [(String, String)]
  -> m ()
differentNamesEvaluation task cs = do
  paragraph "Remarks on your solution:"
  let cs' = nub $ sort cs
      links = fst <$> cs'
      rels  = snd <$> cs'
  assertion (links == nub links) "No relationship is mapped twice?"
  assertion (rels == nub rels) "No link is mapped twice?"
  let ms = BM.toList $ mapping task
  assertion (null $ [c | c <- cs', c `notElem` ms])
    "Given mappings are correct?"
  assertion (cs' == ms) "Given mappings are exhaustive?"

differentNames
  :: DifferentNamesConfig
  -> String
  -> Int
  -> Int
  -> IO DifferentNamesInstance
differentNames config path segment seed = do
  let g = mkStdGen $ (segment +) $ 4 * seed
  (cd, od, bm) <-
    evalRandT (getDifferentNamesTask (classConfig config) (maxObjects config) (searchSpace config) (maxInstances config) (timeout config)) g
  let backwards   = [n | (_, _, Assoc t n' _ _ _) <- toEdges cd
                       , t /= Association
                       , n <- BM.lookup n' bm]
      forwards    = [n | (_, _, Assoc t n' _ _ _) <- toEdges cd
                       , t == Association
                       , n <- BM.lookup n' bm]
      navigations = foldr (`M.insert` Back)
                          (foldr (`M.insert` Forward) M.empty forwards)
                          backwards
  cd' <- drawCdFromSyntax True True Nothing cd (path ++ "-cd") Svg
  od' <- drawOdFromInstance od (Just 1000) navigations True (path ++ "-od") Svg
  return $ DifferentNamesInstance cd' od' bm

getDifferentNamesTask
  :: RandomGen g
  => ClassConfig
  -> Int
  -> Int
  -> Maybe Integer
  -> Maybe Int
  -> RandT g IO (Syntax, AlloyInstance, Bimap String String)
getDifferentNamesTask config maxObs sSpace maxIs to = do
  configs <- withMinimalLabels 3 config
  continueWithHead configs $ \config' -> do
    (names, edges) <- generate config' sSpace
    let cd0    = (0 :: Integer, fromEdges names edges)
        parts0 = extractFourParts cd0
        labels = [l | (_, l, _, _, _, _) <- snd $ snd cd0]
        cds    = map
          (fromEdges names . flip renameEdges edges . BM.fromList . zip labels)
          $ drop 1 (permutations labels)
        cds'   = zip [1 :: Integer ..] cds
        partss = map extractFourParts cds'
        runCmd = foldr (\(n, _) -> (++ " and (not cd" ++ show n ++ ")")) "cd0" cds'
        onlyCd0 = createRunCommand runCmd (length names) maxObs
        partss' = foldr mergeParts parts0 partss
    when debug . liftIO . void $ drawCd cd0
    when debug . liftIO . void $ drawCd `mapM_` cds'
    instances  <- liftIO
      $ getInstances maxIs to (combineParts partss' ++ onlyCd0)
    instances' <- shuffleM (instances :: [AlloyInstance])
    continueWithHead instances' $ \od1 -> do
      labels' <- shuffleM labels
      let bm  = BM.fromList $ zip (map (:[]) ['a', 'b' ..]) labels'
          cd1 = fromEdges names $ renameEdges (BM.twist bm) edges
          bm' = BM.filter (const (`elem` usedLabels od1)) bm
      return (cd1, od1, bm')
  where
    extractFourParts (n, cd) = case transform (toOldSyntax cd) (show n) "" of
      (p1, p2, p3, p4, _) -> (p1, p2, p3, p4)
    combineParts (p1, p2, p3, p4) = p1 ++ p2 ++ p3 ++ p4
    drawCd (n, cd) =
      drawCdFromSyntax True True (Just redColor) cd ("debug-" ++ show n) Pdf
    continueWithHead
      :: RandomGen g
      => [a]
      -> (a -> RandT g IO (Syntax, AlloyInstance, Bimap String String))
      -> RandT g IO (Syntax, AlloyInstance, Bimap String String)
    continueWithHead []    _ =
      getDifferentNamesTask config maxObs sSpace maxIs to
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
