{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
module Modelling.CdOd.SelectValidCd (
  SelectValidCdConfig (..),
  SelectValidCdInstance (..),
  defaultSelectValidCdConfig,
  defaultSelectValidCdInstance,
  selectValidCd,
  selectValidCdEvaluation,
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

import Modelling.Auxiliary.Output (
  LangM,
  Language (English, German),
  OutputMonad (..),
  Rated,
  addPretext,
  english,
  german,
  hoveringInformation,
  multipleChoice,
  simplifiedInformation,
  singleChoiceSyntax,
  translate,
  )
import Modelling.CdOd.RepairCd (
  AllowedProperties (..),
  allowEverything,
  repairIncorrect,
  )
import Modelling.CdOd.Output            (drawCdFromSyntax)
import Modelling.CdOd.Types (
  ClassConfig (..),
  Syntax,
  associationNames,
  classNames,
  renameAssocsInCd,
  renameClassesInCd,
  )

import Control.Monad.Catch              (MonadThrow)
import Control.Monad.IO.Class           (MonadIO (liftIO))
import Control.Monad.Random             (evalRandT, forM_, mkStdGen)
import Control.Monad.Random.Class       (MonadRandom)
import Control.Monad.Trans              (MonadTrans (lift))
import Data.Bifunctor                   (second)
import Data.GraphViz                    (GraphvizOutput (Svg))
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
        inheritances = (1, Just 3)
      },
    maxInstances     = Just 200,
    noIsolationLimit = False,
    printNames       = True,
    printNavigations = True,
    timeout          = Nothing
  }

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
      let f = drawCdFromSyntax
            (withNavigations task)
            (withNames task)
            Nothing
            cd
            [i|#{path}-#{show x}|]
            Svg
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
  multipleChoice cds (Just $ show $ M.keys $ M.filter id solution) solution xs

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
  return $ SelectValidCdInstance {
    classDiagrams   = M.fromAscList $ zip [1 ..] cds,
    withNames       = printNames config,
    withNavigations = printNavigations config
    }

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

renameInstance
  :: MonadThrow m
  => SelectValidCdInstance
  -> [String]
  -> [String]
  -> m SelectValidCdInstance
renameInstance inst names' assocs' = do
  let cds = classDiagrams inst
      names = nub $ concat $ classNames . snd <$> cds
      assocs = nub $ concat $ associationNames . snd <$> cds
      bmNames  = BM.fromList $ zip names names'
      bmAssocs = BM.fromList $ zip assocs assocs'
      renameCd cd = renameClassesInCd bmNames cd >>= renameAssocsInCd bmAssocs
  cds' <- mapM (mapM renameCd) cds
  return $ SelectValidCdInstance {
    classDiagrams   = cds',
    withNames       = withNames inst,
    withNavigations = withNavigations inst
    }

newSelectValidCdInstances
  :: (MonadRandom m, MonadThrow m)
  => SelectValidCdInstance
  -> m [SelectValidCdInstance]
newSelectValidCdInstances inst = do
  let names = nub $ concat $ classNames . snd <$> classDiagrams inst
      assocs = nub $ concat $ associationNames . snd <$> classDiagrams inst
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
