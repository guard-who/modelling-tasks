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
  differentNamesSolution,
  differentNamesSyntax,
  differentNamesTask,
  getDifferentNamesTask,
  mappingShow,
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
  addPretext,
  directionsAdvice,
  hoveringInformation,
  simplifiedInformation,
  )
import Modelling.CdOd.Auxiliary.Util
import Modelling.CdOd.CD2Alloy.Transform (
  combineParts,
  createRunCommand,
  mergeParts,
  transform,
  )
import Modelling.CdOd.Edges             (fromEdges, renameEdges, toEdges)
import Modelling.CdOd.Generate          (generateCds, instanceToEdges)
import Modelling.CdOd.Output            (cacheCd, cacheOd, drawCdFromSyntax)
import Modelling.CdOd.Types (
  AssociationType (..),
  ClassConfig (..),
  Connection (..),
  DiagramEdge,
  Name (Name),
  NameMapping (nameMapping),
  ObjectConfig (..),
  Od,
  Syntax,
  associationNames,
  classNames,
  defaultProperties,
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
import Control.Monad.Output (
  LangM,
  OutputMonad (..),
  Rated,
  english,
  german,
  multipleChoice,
  translations,
  translate,
  )
import Control.Monad.Random
  (MonadRandom (getRandom), RandT, RandomGen, evalRandT, mkStdGen)
import Control.Monad.Trans              (MonadTrans (lift))
import Control.Monad.Trans.Except       (ExceptT, runExceptT)
import Data.Bifunctor                   (Bifunctor (bimap))
import Data.Bimap                       (Bimap)
import Data.Bool                        (bool)
import Data.Containers.ListUtils        (nubOrd)
import Data.GraphViz                    (DirType (..))
import Data.List                        (permutations, sort)
import Data.Maybe                       (fromMaybe, isJust, mapMaybe)
import Data.String.Interpolate          (i, iii)
import Data.Tuple.Extra                 (snd3)
import Diagrams.Prelude                 ((#), red)
import Diagrams.TwoD.Attributes         (lc)
import GHC.Generics                     (Generic)
import Language.Alloy.Call (
  AlloyInstance,
  getTripleAs,
  lookupSig,
  scoped,
  )
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
    -- | whether every relationship has an associated link (in the mapping)
    usesAllRelationships :: Bool
  } deriving (Eq, Generic, Read, Show)

data DifferentNamesConfig = DifferentNamesConfig {
    classConfig      :: ClassConfig,
    withNonTrivialInheritance :: Maybe Bool,
    maxInstances     :: Maybe Integer,
    objectConfig     :: ObjectConfig,
    onlyAnonymousObjects :: Bool,
    presenceOfLinkSelfLoops :: Maybe Bool,
    -- | whether to enforce one relationship not matching to
    -- any link in the object diagram
    ignoreOneRelationship :: Maybe Bool,
    printSolution    :: Bool,
    searchSpace      :: Int,
    timeout          :: Maybe Int
  } deriving (Generic, Read, Show)

{-# DEPRECATED searchSpace "because Modelling.Cd.generate' is not used anymore and will be removed soon" #-}

defaultDifferentNamesConfig :: DifferentNamesConfig
defaultDifferentNamesConfig = DifferentNamesConfig {
    classConfig  = ClassConfig {
        classes      = (4, 4),
        aggregations = (1, Just 1),
        associations = (1, Just 1),
        compositions = (1, Just 1),
        inheritances = (1, Just 1)
      },
    objectConfig = ObjectConfig {
      links          = (5, Just 10),
      linksPerObject = (0, Just 3),
      objects        = (4, 6)
      },
    onlyAnonymousObjects = True,
    presenceOfLinkSelfLoops = Nothing,
    ignoreOneRelationship = Just False,
    printSolution    = False,
    withNonTrivialInheritance = Just True,
    maxInstances     = Nothing,
    searchSpace      = 10,
    timeout          = Nothing
  }

newtype ShowName = ShowName { showName' :: Name }

instance Show ShowName where
  show = showName . showName'

mappingShow :: [(Name, Name)] -> [(ShowName, ShowName)]
mappingShow = fmap (bimap ShowName ShowName)

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
  cd' <- lift $ liftIO $ cacheCd True True mempty cd (path ++ "-cd")
  od' <- lift $ liftIO $ flip evalRandT (mkStdGen $ generatorValue task) $
    uncurry cacheOd od anonymous navigations True (path ++ "-od")
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
      english [iii|
        Which relationship in the class diagram (CD) corresponds
        to which of the links in the object diagram (OD)?
        \n
        State your answer by giving a mapping of
        relationships in the CD to links in the OD.
        \n
        To state that a in the CD corresponds to x in the OD and
        b in the CD corresponds to y in the OD write it as:
        |]
      german [iii|
        Welche Beziehung im Klassendiagramm (CD)
        entspricht welchen Links im Objektdiagramm (OD)?
        \n
        Geben Sie Ihre Antwort als eine Zuordnung von
        Beziehungen im CD zu Links im OD an.
        \n
        Um anzugeben, dass a im CD zu x im OD und b im CD
        zu y im OD korrespondieren, schreiben Sie es als:
        |]
    code . show $ mappingShow differentNamesInitial
  paragraph $ translate $ do
    english [iii|
      Please note: Links are already grouped correctly and fully,
      i.e. all links with the same name (and only links with the same name!)
      in the OD correspond to exactly the same relationship name in the CD.
      |]
    german [iii|
      Bitte beachten Sie: Links sind bereits vollständig und korrekt gruppiert,
      d.h. alle Links mit dem selben Namen
      (and auch nur Links mit dem selben Namen!)
      im OD entsprechen genau dem selben Beziehungsnamen im CD.
      |]
  paragraph $ translate $ if usesAllRelationships task
    then do
    english [iii|
      Thus, every link name and every relationship name should occur
      exactly once in your mapping.
      |]
    german [iii|
      Deshalb sollte jeder Linkname and jeder Beziehungsname
      genau einmal in Ihrer Zuordnung auftauchen.
      |]
    else do
    english [i|Thus, every link name should occur exactly once in your mapping.|]
    german [iii|
      Deshalb sollte jeder Linkname
      genau einmal in Ihrer Zuordnung auftauchen.
      |]
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
  let l = length $ mapMaybe (readMapping m) cs
  assertion (l == length cs) $ translate $ do
    english [iii|
      All provided pairs are matching a valid link and a valid relationship?
      |]
    german [iii|
      Alle angegebenen Paare ordnen einen gültigen Link
      einer gültigen Beziehung zu?
      |]
  assertion (l == nubLengthOn fst && l == nubLengthOn snd) $ translate $ do
    english "All provided pairs are non-overlapping?"
    german "Alle angegebenen Paare sind nicht überlappend?"
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
        then Just . show . mappingShow $ differentNamesSolution task
        else Nothing
  multipleChoice what solution ms (mapMaybe (readMapping m) cs)

differentNamesSolution :: DifferentNamesInstance -> [(Name, Name)]
differentNamesSolution = BM.toAscList . nameMapping . mapping

differentNames
  :: MonadIO m
  => DifferentNamesConfig
  -> Int
  -> Int
  -> ExceptT String m DifferentNamesInstance
differentNames config segment seed = do
  let g = mkStdGen (segment + 4 * seed)
  liftIO $ flip evalRandT g $ do
    is <- generateCds
      (withNonTrivialInheritance config)
      (classConfig config)
      defaultProperties
      (maxInstances config)
      (timeout config)
    fgen is
  where
    fgen []             = error $
      "it seems to be impossible to generate such a model"
      ++ "; check your configuration"
    fgen (insta:instas) =
      let (names, edges) = either error id $ instanceToEdges insta
      in getDifferentNamesTask (fgen instas) config names edges

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
  usesAllRelationships = True
  }

getDifferentNamesTask
  :: (RandomGen g, MonadIO m, MonadThrow m)
  => RandT g m DifferentNamesInstance
  -> DifferentNamesConfig
  -> [String]
  -> [DiagramEdge]
  -> RandT g m DifferentNamesInstance
getDifferentNamesTask fhead config names edges' = do
    let edges  = reverseAssociation <$> edges'
        cd0    = (0 :: Integer, fromEdges names edges)
        parts0 = uncurry alloyFor cd0
        labels = [l | (_, l, _, _, _, _) <- snd $ snd cd0]
        cds    = map
          (fromEdges names . flip renameEdges edges . BM.fromList . zip labels)
          $ drop 1 (permutations labels)
        cds'   = zip [1 :: Integer ..] cds
        partss = map (uncurry alloyFor) cds'
        runCmd = foldr (\(n, _) -> (++ " and (not cd" ++ show n ++ ")")) "cd0" cds'
        onlyCd0 = createRunCommand
          runCmd
          (length names)
          $ objectConfig config
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
          isCompleteMapping = BM.keysR bm == sort (usedLabels od1)
      if maybe (const True) (bool id not) (ignoreOneRelationship config)
         isCompleteMapping
        then do
        let (assocs, linkNs) = unzip $ BM.toAscList bm
        names'  <- shuffleM names
        assocs' <- shuffleM assocs
        linkNs' <- shuffleM linkNs
        od1' <- either error id <$> runExceptT (alloyInstanceToOd od1)
        gv <- getRandom
        let inst =  DifferentNamesInstance {
              anonymousObjects = onlyAnonymousObjects config,
              cDiagram  = cd1,
              generatorValue = gv,
              oDiagram  = od1',
              showSolution = printSolution config,
              mapping   = toNameMapping bm',
              usesAllRelationships = isCompleteMapping
              }
        lift $ renameInstance inst names' assocs' linkNs'
        else fhead
  where
    alloyFor n cd = transform
      (toOldSyntax cd)
      (objectConfig config)
      (presenceOfLinkSelfLoops config)
      False
      (show n)
      ""
    drawCd (n, cd) = drawCdFromSyntax
      True
      True
      (mempty # lc red)
      cd
      ("debug-" ++ show n ++ ".svg")
    continueWithHead []    _ = fhead
    continueWithHead (x:_) f = f x
    usedLabels :: AlloyInstance -> [String]
    usedLabels inst = either error id $ do
      let ignore = const $ const $ return ()
          name = const . return
      os    <- lookupSig (scoped "this" "Obj") inst
      map snd3 . S.toList <$> getTripleAs "get" ignore name ignore os

renameInstance
  :: MonadThrow m
  => DifferentNamesInstance
  -> [String]
  -> [String]
  -> [String]
  -> m DifferentNamesInstance
renameInstance inst names' assocs' linkNs' = do
  let cd = cDiagram inst
      od = oDiagram inst
      names = classNames cd
      assocs = associationNames cd
      linkNs = linkNames od
      bm = BM.toAscList $ fromNameMapping $ mapping inst
      bmNames  = BM.fromList $ zip names names'
      bmAssocs = BM.fromList $ zip assocs assocs'
      bmLinks  = BM.fromList $ zip linkNs linkNs'
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
      linkNs = linkNames $ oDiagram inst
  names'  <- shuffleM $ tail $ permutations names
  assocs' <- shuffleM $ tail $ permutations assocs
  links'  <- shuffleM $ tail $ permutations linkNs
  sequence
    [ renameInstance inst ns as ls
    | (ns, as, ls) <- zip3 names' assocs' links'
    ]
