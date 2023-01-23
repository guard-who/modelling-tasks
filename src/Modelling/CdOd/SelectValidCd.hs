{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Modelling.CdOd.SelectValidCd (
  SelectValidCdConfig (..),
  SelectValidCdInstance (..),
  checkSelectValidCdConfig,
  defaultSelectValidCdConfig,
  defaultSelectValidCdInstance,
  selectValidCd,
  selectValidCdEvaluation,
  selectValidCdSolution,
  selectValidCdSyntax,
  selectValidCdTask,
  newSelectValidCdInstances,
  ) where

import qualified Data.Bimap                       as BM (fromList)
import qualified Data.Map                         as M (
  filter,
  foldrWithKey,
  fromAscList,
  fromList,
  insert,
  keys,
  toList,
  )

import Modelling.Auxiliary.Common (
  Randomise (randomise),
  RandomiseLayout (randomiseLayout),
  shuffleEverything,
  )
import Modelling.Auxiliary.Output (
  addPretext,
  hoveringInformation,
  simplifiedInformation,
  )
import Modelling.CdOd.RepairCd (
  AllowedProperties (..),
  allowEverything,
  checkClassConfigAndChanges,
  repairIncorrect,
  )
import Modelling.CdOd.Output            (cacheCd)
import Modelling.CdOd.Types (
  ClassConfig (..),
  Syntax,
  associationNames,
  classNames,
  shuffleClassAndConnectionOrder,
  renameAssocsInCd,
  renameClassesInCd,
  )

import Control.Monad                    ((>=>))
import Control.Monad.Catch              (MonadThrow)
import Control.Monad.IO.Class           (MonadIO (liftIO))
import Control.Monad.Output (
  LangM,
  Language (English, German),
  OutputMonad (..),
  Rated,
  english,
  german,
  multipleChoice,
  singleChoiceSyntax,
  translate,
  )
import Control.Monad.Random             (evalRandT, forM_, mkStdGen)
import Control.Monad.Random.Class       (MonadRandom)
import Control.Monad.Trans              (MonadTrans (lift))
import Data.Containers.ListUtils        (nubOrd)
import Data.Bifunctor                   (second)
import Data.List                        (nub, permutations)
import Data.Map                         (Map)
import Data.String.Interpolate          (i)
import GHC.Generics                     (Generic)
import System.Random.Shuffle            (shuffleM)

data SelectValidCdConfig = SelectValidCdConfig {
    allowedProperties :: AllowedProperties,
    classConfig      :: ClassConfig,
    maxInstances     :: Maybe Integer,
    noIsolationLimit :: Bool,
    printNames       :: Bool,
    printNavigations :: Bool,
    shuffleEachCd    :: Bool,
    timeout          :: Maybe Int
  } deriving (Generic, Read, Show)

defaultSelectValidCdConfig :: SelectValidCdConfig
defaultSelectValidCdConfig = SelectValidCdConfig {
    allowedProperties = allowEverything {
        reverseInheritances    = False,
        selfInheritances       = False
        },
    classConfig = ClassConfig {
        classes      = (4, 4),
        aggregations = (0, Just 2),
        associations = (0, Just 2),
        compositions = (0, Just 3),
        inheritances = (1, Just 3),
        relationships = (4, Just 6)
      },
    maxInstances     = Just 200,
    noIsolationLimit = False,
    printNames       = True,
    printNavigations = True,
    shuffleEachCd    = False,
    timeout          = Nothing
  }

checkSelectValidCdConfig :: SelectValidCdConfig -> Maybe String
checkSelectValidCdConfig SelectValidCdConfig {..} =
  checkClassConfigAndChanges classConfig allowedProperties

data SelectValidCdInstance = SelectValidCdInstance {
    classDiagrams   :: Map Int (Bool, Syntax),
    withNames       :: Bool,
    withNavigations :: Bool
  } deriving (Generic, Read, Show)

selectValidCdSyntax :: OutputMonad m => SelectValidCdInstance -> [Int] -> LangM m
selectValidCdSyntax inst xs =
  forM_ xs $ singleChoiceSyntax True (M.keys $ classDiagrams inst)

selectValidCdTask
  :: (OutputMonad m, MonadIO m)
  => FilePath
  -> SelectValidCdInstance
  -> LangM m
selectValidCdTask path task = do
  paragraph $ translate $ do
    english [i|Consider the following class diagram candidates.|]
    german [i|Betrachten Sie die folgenden Klassendiagrammkandidaten.|]
  cds      <- lift $ liftIO $ sequence $
    M.foldrWithKey drawCd mempty $ classDiagrams task
  images show snd cds
  paragraph $ translate $ do
    english [i|Which of these class diagram candidates are valid class diagrams?
Please state your answer by giving a list of numbers, indicating all valid class diagrams.|]
    german [i|Welche dieser Klassendiagrammkandidaten sind valide Klassendiagramme?
Bitte geben Sie Ihre Antwort in Form einer Liste von Zahlen an, die alle gültigen Klassendiagramme enthält.|]
  paragraph $ do
    translate $ do
      english [i|For example,|]
      german [i|Zum Beispiel|]
    code "[1, 2]"
    translate $ do
      english [i|would indicate that only class diagram candidates 1 and 2 of the given ones are valid class diagrams.|]
      german [i|würde bedeuten, dass nur die Klassendiagrammkandidaten 1 und 2 der angegebenen Klassendiagrammkandidaten gültige Klassendiagramme sind.|]
  paragraph simplifiedInformation
  paragraph hoveringInformation
  where
    drawCd x (b, cd) cds =
      let f = cacheCd
            (withNavigations task)
            (withNames task)
            mempty
            cd
            path
      in M.insert x ((b,) <$> f) cds

selectValidCdEvaluation
  :: OutputMonad m
  => SelectValidCdInstance
  -> [Int]
  -> Rated m
selectValidCdEvaluation inst xs = addPretext $ do
  let cds = M.fromList [
        (English, "class diagrams"),
        (German, "Klassendiagramme")
        ]
      solution = fst <$> classDiagrams inst
  multipleChoice cds (Just $ show $ selectValidCdSolution inst) solution xs

selectValidCdSolution :: SelectValidCdInstance -> [Int]
selectValidCdSolution = M.keys . M.filter id . fmap fst . classDiagrams

selectValidCd
  :: SelectValidCdConfig
  -> Int
  -> Int
  -> IO SelectValidCdInstance
selectValidCd config segment seed = do
  let g = mkStdGen $ (segment +) $ 4 * seed
  (_, chs)  <- flip evalRandT g $ repairIncorrect
    (allowedProperties config)
    (classConfig config)
    (noIsolationLimit config)
    (maxInstances config)
    (timeout config)
  let cds = map (second snd) chs
  shuffleCds >=> shuffleEverything $ SelectValidCdInstance {
    classDiagrams   = M.fromAscList $ zip [1 ..] cds,
    withNames       = printNames config,
    withNavigations = printNavigations config
    }
  where
    shuffleCds
      | shuffleEachCd config = shuffleEach
      | otherwise            = return

instance Randomise SelectValidCdInstance where
  randomise inst = do
    let (names, assocs) = classAndAssocNames inst
    names' <- shuffleM names
    assocs' <- shuffleM assocs
    renameInstance inst names' assocs'
      >>= shuffleInstance

instance RandomiseLayout SelectValidCdInstance where
  randomiseLayout SelectValidCdInstance {..} = do
    cds <- mapM shuffleClassAndConnectionOrder `mapM` classDiagrams
    return $ SelectValidCdInstance {
      classDiagrams           = cds,
      withNames               = withNames,
      withNavigations         = withNavigations
      }

shuffleEach
  :: (MonadRandom m, MonadThrow m)
  => SelectValidCdInstance
  -> m SelectValidCdInstance
shuffleEach inst@SelectValidCdInstance {..} = do
  cds <- mapM shuffleCd `mapM` classDiagrams
  return $ SelectValidCdInstance {
    classDiagrams           = cds,
    withNames               = withNames,
    withNavigations         = withNavigations
    }
  where
    (names, assocs) = classAndAssocNames inst
    shuffleCd cd = do
      names' <- shuffleM names
      assocs' <- shuffleM assocs
      let bmNames  = BM.fromList $ zip names names'
          bmAssocs = BM.fromList $ zip assocs assocs'
      renameClassesInCd bmNames cd
        >>= renameAssocsInCd bmAssocs

shuffleInstance
  :: MonadRandom m
  => SelectValidCdInstance
  -> m SelectValidCdInstance
shuffleInstance inst = SelectValidCdInstance
  <$> (M.fromAscList . zipWith replaceId [1..]
       <$> shuffleM (M.toList $ classDiagrams inst))
  <*> pure (withNames inst)
  <*> pure (withNavigations inst)
  where
    replaceId x (_, cd) = (x, cd)

classAndAssocNames :: SelectValidCdInstance -> ([String], [String])
classAndAssocNames inst =
  let cds = classDiagrams inst
      names = nubOrd $ concatMap (classNames . snd) cds
      assocs = nubOrd $ concatMap (associationNames . snd) cds
  in (names, assocs)

renameInstance
  :: MonadThrow m
  => SelectValidCdInstance
  -> [String]
  -> [String]
  -> m SelectValidCdInstance
renameInstance inst names' assocs' = do
  let (names, assocs) = classAndAssocNames inst
      bmNames  = BM.fromList $ zip names names'
      bmAssocs = BM.fromList $ zip assocs assocs'
      renameCd cd = renameClassesInCd bmNames cd >>= renameAssocsInCd bmAssocs
  cds <- mapM (mapM renameCd) $ classDiagrams inst
  return $ SelectValidCdInstance {
    classDiagrams   = cds,
    withNames       = withNames inst,
    withNavigations = withNavigations inst
    }

newSelectValidCdInstances
  :: (MonadRandom m, MonadThrow m)
  => SelectValidCdInstance
  -> m [SelectValidCdInstance]
newSelectValidCdInstances inst = do
  let names = nub $ concatMap (classNames . snd) (classDiagrams inst)
      assocs = nub $ concatMap (associationNames . snd) (classDiagrams inst)
  names'  <- shuffleM $ tail $ permutations names
  assocs' <- shuffleM $ tail $ permutations assocs
  sequence
    [ renameInstance inst ns as >>= shuffleInstance
    | (ns, as) <- zip names' (concat $ replicate 5 assocs')
    ]

defaultSelectValidCdInstance :: SelectValidCdInstance
defaultSelectValidCdInstance = SelectValidCdInstance {
  classDiagrams = M.fromAscList [
    (1,(False,([("D",["B"]),("B",["D"]),("A",["C"]),("C",["B"])],[]))),
    (2,(True,([("D",["B"]),("B",[]),("A",[]),("C",["B"])],[]))),
    (3,(False,([("D",["B"]),("B",["D"]),("A",["B"]),("C",["B"])],[]))),
    (4,(True,([("D",[]),("B",["D"]),("A",[]),("C",["B"])],[])))
    ],
  withNames = True,
  withNavigations = True
  }
