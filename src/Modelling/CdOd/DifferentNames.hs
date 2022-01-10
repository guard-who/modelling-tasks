{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
module Modelling.CdOd.DifferentNames (
  DifferentNamesConfig (..),
  DifferentNamesInstance (..),
  debug,
  defaultDifferentNamesConfig,
  defaultDifferentNamesInstance,
  differentNames,
  differentNamesEvaluation,
  differentNamesInitial,
  differentNamesSyntax,
  differentNamesTask,
  newDifferentNamesInstances,
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
  empty,
  fromAscList,
  insert,
  )
import qualified Data.Set                         as S (toList)

import Modelling.Auxiliary.Output (
  LangM,
  OutputMonad (..),
  Rated,
  addPretext,
  directionsAdvice,
  english,
  german,
  hoveringInformation,
  multipleChoice,
  simplifiedInformation,
  translations,
  translate,
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
  Name (Name),
  NameMapping (nameMapping),
  Od,
  Syntax,
  associationNames,
  classNames,
  fromNameMapping,
  linkNames,
  renameAssocsInCd,
  renameClassesInCd,
  renameClassesInOd,
  renameLinksInOd,
  showName,
  toNameMapping,
  toOldSyntax,
  )

import Control.Monad                    (void, when)
import Control.Monad.Catch              (MonadThrow)
import Control.Monad.IO.Class           (MonadIO (liftIO))
import Control.Monad.Random
  (MonadRandom (getRandom), RandT, RandomGen, evalRandT, mkStdGen)
import Control.Monad.Trans              (MonadTrans (lift))
import Control.Monad.Trans.Except       (ExceptT, runExceptT)
import Data.Bifunctor                   (Bifunctor (bimap))
import Data.Bimap                       (Bimap)
import Data.Containers.ListUtils        (nubOrd)
import Data.GraphViz                    (DirType (..), GraphvizOutput (Pdf, Svg))
import Data.List                        (permutations, sort)
import Data.Maybe                       (catMaybes, fromMaybe, isJust)
import Data.String.Interpolate          (i)
import GHC.Generics                     (Generic)
import Language.Alloy.Call
  (AlloyInstance, getTriple, lookupSig, objectName, scoped)
import System.Random.Shuffle            (shuffleM)

debug :: Bool
debug = False

data DifferentNamesInstance = DifferentNamesInstance {
    anonymousObjects :: Bool,
    cDiagram :: Syntax,
    generatorValue :: Int,
    oDiagram :: Od,
    showSolution :: Bool,
    mapping  :: NameMapping,
    usesAllRelationships :: Maybe Bool
  } deriving (Eq, Generic, Read, Show)

data DifferentNamesConfig = DifferentNamesConfig {
    classConfig      :: ClassConfig,
    maxObjects       :: Int,
    withNonTrivialInheritance :: Maybe Bool,
    maxInstances     :: Maybe Integer,
    onlyAnonymousObjects :: Bool,
    presenceOfLinkSelfLoops :: Maybe Bool,
    printSolution    :: Bool,
    searchSpace      :: Int,
    timeout          :: Maybe Int,
    useAllRelationships :: Maybe Bool
  } deriving (Generic, Read, Show)

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
    onlyAnonymousObjects = True,
    presenceOfLinkSelfLoops = Nothing,
    printSolution    = False,
    withNonTrivialInheritance = Just True,
    maxInstances     = Nothing,
    searchSpace      = 10,
    timeout          = Nothing,
    useAllRelationships = Just True
  }

newtype ShowName = ShowName { showName' :: Name }

instance Show ShowName where
  show = showName . showName'

showMapping :: [(Name, Name)] -> String
showMapping = show . fmap (bimap ShowName ShowName)

differentNamesTask
  :: (OutputMonad m, MonadIO m)
  => FilePath
  -> DifferentNamesInstance
  -> LangM m
differentNamesTask path task = do
  let cd = cDiagram task
      od = oDiagram task
      bm = fromNameMapping $ mapping task
      backwards   = [n | (_, _, Assoc _ n' _ _ _) <- toEdges cd
                       , n <- BM.lookup n' bm]
      navigations = foldr (`M.insert` Back) M.empty backwards
      anonymous = fromMaybe (length (fst od) `div` 3)
        (if anonymousObjects task then Just 1000 else Nothing)
  cd' <- lift $ liftIO $ drawCdFromSyntax True True Nothing cd (path ++ "-cd") Svg
  od' <- lift $ liftIO $ flip evalRandT (mkStdGen $ generatorValue task) $
    uncurry drawOdFromNodesAndEdges od anonymous navigations True (path ++ "-od") Svg
  paragraph $ translate $ do
    english "Consider the following class diagram:"
    german "Betrachten Sie das folgende Klassendiagramm:"
  image cd'
  paragraph $ translate $ do
    english "and the following object diagram (which conforms to it):"
    german "und das folgende (dazu passende) Objektdiagramm:"
  image od'
  paragraph $ do
    translate $ do
      english [i|Which relationship in the class diagram (CD) corresponds to which of the links in the object diagram (OD)?
State your answer by giving a mapping of relationships in the CD to links in the OD.
To state that a in the CD corresponds to x in the OD and b in the CD corresponds to y in the OD write it as:|]
      german [i|Welche Beziehung im Klassendiagramm (CD) entspricht welchen Links im Objektdiagramm (OD)?
Geben Sie Ihre Antwort als eine Zuordnung von Beziehungen im CD zu Links im OD an.
Um anzugeben, dass a im CD zu x im OD und b im CD zu y im OD korrespondieren, schreiben Sie es als:|]
    code $ showMapping differentNamesInitial
  paragraph $ translate $ do
    english [i|Please note: Links are already grouped correctly and fully, i.e. all links with the same name (and only links with the same name!) in the OD correspond to exactly the same relationship name in the CD.|]
    german [i|Bitte beachten Sie: Links sind bereits vollständig und korrekt gruppiert, d.h. alle Links mit dem selben Namen (and auch nur Links mit dem selben Namen!) im OD entsprechen genau dem selben Beziehungsnamen im CD.|]
  paragraph $ translate $ if fromMaybe False $ usesAllRelationships task
    then do
    english [i|Thus, every link name and every relationship name should occur exactly once in your mapping.|]
    german [i|Deshalb sollte jeder Linkname and jeder Beziehungsname genau einmal in Ihrer Zuordnung auftauchen.|]
    else do
    english [i|Thus, every link name should occur exactly once in your mapping.|]
    german [i|Deshalb sollte jeder Linkname genau einmal in Ihrer Zuordnung auftauchen.|]
  paragraph simplifiedInformation
  paragraph directionsAdvice
  paragraph hoveringInformation

differentNamesInitial :: [(Name, Name)]
differentNamesInitial = bimap Name Name <$> [("a", "x"), ("b", "y")]

differentNamesSyntax
  :: OutputMonad m
  => DifferentNamesInstance
  -> [(Name, Name)]
  -> LangM m
differentNamesSyntax task cs = addPretext $ do
  let l = length $ catMaybes $ readMapping m <$> cs
  assertion (l == length cs) $ translate $ do
    english "All provided pairs are linking a valid link and a valid relationship"
    german "Alle angegebenen Paare ordnen einen gültigen Link einer gültigen Beziehung zu"
  assertion (l == nubLengthOn fst && l == nubLengthOn snd) $ translate $ do
    english "All provided pairs are non-overlapping"
    german "Alle angegebenen Paare sind nicht überlappend"
  where
    nubLengthOn f = length (nubOrd (map f cs))
    m = nameMapping $ mapping task

readMapping :: Ord a => Bimap a a -> (a, a) -> Maybe (a, a)
readMapping m (x, y)
  | isJust $ BM.lookup x m, isJust $ BM.lookupR y m
  = Just (x, y)
  | isJust $ BM.lookup y m, isJust $ BM.lookupR x m
  = Just (y, x)
  | otherwise
  = Nothing

differentNamesEvaluation
  :: OutputMonad m
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
        then Just $ showMapping $ BM.toAscList m
        else Nothing
  multipleChoice what solution ms (catMaybes $ readMapping m <$> cs)

differentNames
  :: MonadIO m
  => DifferentNamesConfig
  -> Int
  -> Int
  -> ExceptT String m DifferentNamesInstance
differentNames config segment seed = do
  let g = mkStdGen (segment + 4 * seed)
  liftIO $ evalRandT (getDifferentNamesTask config) g

reverseAssociation :: DiagramEdge -> DiagramEdge
reverseAssociation (from, to, Assoc Association n lf lt im) =
  (to, from, Assoc Association n lt lf im)
reverseAssociation x = x

defaultDifferentNamesInstance :: DifferentNamesInstance
defaultDifferentNamesInstance = DifferentNamesInstance {
  anonymousObjects = True,
  cDiagram = (
    [("A",[]),("D",["A"]),("B",[]),("C",["D"])],
    [(Aggregation,"b",(0,Nothing),"D","B",(1,Just 1)),
     (Association,"c",(0,Just 2),"C","A",(0,Just 2)),
     (Composition,"a",(1,Just 1),"A","B",(2,Nothing))
    ]
    ),
  generatorValue = -3894126834283525023,
  oDiagram = (
    ["C$0","B$0","B$1","B$2"],
    [(0,1,"y"),(0,2,"y"),(0,3,"y"),(0,0,"x"),(0,3,"z")]
    ),
  showSolution = False,
  mapping = toNameMapping $ BM.fromList [("a","y"),("b","z"),("c","x")],
  usesAllRelationships = Just True
  }

getDifferentNamesTask
  :: RandomGen g
  => DifferentNamesConfig
  -> RandT g IO DifferentNamesInstance
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
          (presenceOfLinkSelfLoops config)
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
          cd1 = fromEdges names $ renameEdges (BM.twist bm) edges'
          bm' = BM.filter (const (`elem` usedLabels od1)) bm
      if maybe (const True) (\x -> if x then id else not) (useAllRelationships config)
         $ BM.keysR bm == sort (usedLabels od1)
        then do
        let (assocs, links) = unzip $ BM.toAscList bm
        names'  <- shuffleM names
        assocs' <- shuffleM assocs
        links'  <- shuffleM links
        od1' <- either error id <$> runExceptT (alloyInstanceToOd od1)
        gv <- getRandom
        let inst =  DifferentNamesInstance {
              anonymousObjects = onlyAnonymousObjects config,
              cDiagram  = cd1,
              generatorValue = gv,
              oDiagram  = od1',
              showSolution = printSolution config,
              mapping   = toNameMapping bm',
              usesAllRelationships = useAllRelationships config
              }
        lift $ renameInstance inst names' assocs' links'
        else getDifferentNamesTask config
  where
    extractFourParts (n, cd) =
      case transform (toOldSyntax cd) (presenceOfLinkSelfLoops config) False (show n) "" of
      (p1, p2, p3, p4, _) -> (p1, p2, p3, p4)
    combineParts (p1, p2, p3, p4) = p1 ++ p2 ++ p3 ++ p4
    drawCd (n, cd) =
      drawCdFromSyntax True True (Just redColor) cd ("debug-" ++ show n) Pdf
    continueWithHead
      :: RandomGen g
      => [a]
      -> (a -> RandT g IO DifferentNamesInstance)
      -> RandT g IO DifferentNamesInstance
    continueWithHead []    _ =
      getDifferentNamesTask config
    continueWithHead (x:_) f = f x
    usedLabels :: AlloyInstance -> [String]
    usedLabels inst = either error id $ do
      os    <- lookupSig (scoped "this" "Obj") inst
      links <- map nameOf . S.toList <$> getTriple "get" os
      return $ nubOrd links
    nameOf (_,l,_) = takeWhile (/= '$') $ objectName l

renameInstance
  :: MonadThrow m
  => DifferentNamesInstance
  -> [String]
  -> [String]
  -> [String]
  -> m DifferentNamesInstance
renameInstance inst names' assocs' links' = do
  let cd = cDiagram inst
      od = oDiagram inst
      names = classNames cd
      assocs = associationNames cd
      links  = linkNames od
      bm = BM.toAscList $ fromNameMapping $ mapping inst
      bmNames  = BM.fromList $ zip names names'
      bmAssocs = BM.fromList $ zip assocs assocs'
      bmLinks  = BM.fromList $ zip links links'
      bm'      = BM.fromList
        [ (a', l')
        | (a, l) <- bm
        , a' <- BM.lookup a bmAssocs
        , l' <- BM.lookup l bmLinks
        ]
  cd' <- renameClassesInCd bmNames =<< renameAssocsInCd bmAssocs cd
  od' <- renameClassesInOd bmNames =<< renameLinksInOd bmLinks od
  return $ DifferentNamesInstance {
    anonymousObjects = anonymousObjects inst,
    cDiagram  = cd',
    generatorValue = generatorValue inst,
    oDiagram  = od',
    showSolution = showSolution inst,
    mapping   = toNameMapping bm',
    usesAllRelationships = usesAllRelationships inst
    }

newDifferentNamesInstances
  :: (MonadRandom m, MonadThrow m)
  => DifferentNamesInstance
  -> m [DifferentNamesInstance]
newDifferentNamesInstances inst = do
  let names = classNames $ cDiagram inst
      assocs = associationNames $ cDiagram inst
      links  = linkNames $ oDiagram inst
  names'  <- shuffleM $ tail $ permutations names
  assocs' <- shuffleM $ tail $ permutations assocs
  links'  <- shuffleM $ tail $ permutations links
  sequence
    [ renameInstance inst ns as ls
    | (ns, as, ls) <- zip3 names' assocs' links'
    ]

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
