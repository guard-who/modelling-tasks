{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
module Modelling.CdOd.SelectValidCd (
  SelectValidCdConfig (..),
  SelectValidCdInstance (..),
  defaultSelectValidCdConfig,
  selectValidCd,
  selectValidCdEvaluation,
  selectValidCdTask,
  newSelectValidCdInstances,
  ) where

import qualified Data.Bimap                       as BM (fromList)
import qualified Data.Map                         as M (
  foldrWithKey,
  fromAscList,
  insert,
  toList,
  )

import Modelling.Auxiliary.Output (
  OutputMonad (..),
  hoveringInformation,
  multipleChoice,
  simplifiedInformation,
  LangM,
  )
import Modelling.CdOd.RepairCd (
  AllowedProperties,
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
import Control.Monad.Random             (evalRandT, mkStdGen)
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
    printNames       :: Bool,
    printNavigations :: Bool,
    timeout          :: Maybe Int
  } deriving (Generic, Read, Show)

defaultSelectValidCdConfig :: SelectValidCdConfig
defaultSelectValidCdConfig = SelectValidCdConfig {
    allowedProperties = allowEverything,
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
    timeout          = Nothing
  }

data SelectValidCdInstance = SelectValidCdInstance {
    classDiagrams   :: Map Int (Bool, Syntax),
    withNames       :: Bool,
    withNavigations :: Bool
  } deriving (Generic, Read, Show)

selectValidCdTask
  :: (OutputMonad m, MonadIO m)
  => FilePath
  -> SelectValidCdInstance
  -> LangM m
selectValidCdTask path task = do
  paragraph $ text [i|Consider the following class diagram candidates.|]
  cds      <- lift $ liftIO $ sequence $
    M.foldrWithKey drawCd mempty $ classDiagrams task
  images show snd cds
  paragraph $ text
    [i|Which of these class diagram candidates are valid class diagrams?
Please state your answer by giving a list of numbers, indicating all valid class diagrams.|]
  paragraph $ do
    text [i|For example, |]
    code "[1, 2]"
    text [i| would indicate that only class diagram candidates 1 and 2 of the given ones are valid class diagrams.|]
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
  -> LangM m
selectValidCdEvaluation = multipleChoice "class diagrams" . classDiagrams

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
