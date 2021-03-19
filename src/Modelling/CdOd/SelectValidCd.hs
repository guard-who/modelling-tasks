{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
module Modelling.CdOd.SelectValidCd (
  SelectValidCdConfig (..),
  SelectValidCdInstance (..),
  defaultSelectValidCdConfig,
  selectValidCd,
  selectValidCdEvaluation,
  selectValidCdTask,
  ) where

import qualified Data.Map                         as M (fromList)

import Modelling.Auxiliary.Output (
  OutputMonad (..),
  hoveringInformation,
  multipleChoice,
  simplifiedInformation,
  LangM,
  )
import Modelling.CdOd.RepairCd          (repairIncorrect)
import Modelling.CdOd.Output            (drawCdFromSyntax)
import Modelling.CdOd.Types             (ClassConfig (..))

import Control.Monad.Random             (evalRandT, mkStdGen)
import Data.Bifunctor                   (second)
import Data.GraphViz                    (GraphvizOutput (Svg))
import Data.Map                         (Map)
import Data.String.Interpolate          (i)
import GHC.Generics                     (Generic)

data SelectValidCdConfig = SelectValidCdConfig {
    classConfig      :: ClassConfig,
    maxInstances     :: Maybe Integer,
    printNames       :: Bool,
    printNavigations :: Bool,
    timeout          :: Maybe Int
  } deriving Generic

defaultSelectValidCdConfig :: SelectValidCdConfig
defaultSelectValidCdConfig = SelectValidCdConfig {
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

newtype SelectValidCdInstance = SelectValidCdInstance {
    classDiagrams   :: Map Int (Bool, FilePath)
  } deriving (Generic, Show)

selectValidCdTask :: OutputMonad m => SelectValidCdInstance -> LangM m
selectValidCdTask task = do
  paragraph $ text [i|Consider the following class diagram candidates.|]
  images show snd $ classDiagrams task
  paragraph $ text
    [i|Which of these class diagram candidates are valid class diagrams?
Please state your answer by giving a list of numbers, indicating all valid class diagrams.|]
  paragraph $ do
    text [i|For example, |]
    code "[0, 9]"
    text [i| would indicate that only class diagram candidates 0 and 9 of the given ones are valid class diagrams.|]
  paragraph simplifiedInformation
  paragraph hoveringInformation

selectValidCdEvaluation
  :: OutputMonad m
  => SelectValidCdInstance
  -> [Int]
  -> LangM m
selectValidCdEvaluation = multipleChoice "class diagrams" . classDiagrams

selectValidCd
  :: SelectValidCdConfig
  -> FilePath
  -> Int
  -> Int
  -> IO SelectValidCdInstance
selectValidCd config path segment seed = do
  let g = mkStdGen $ (segment +) $ 4 * seed
  (_, chs)  <- evalRandT (repairIncorrect (classConfig config) (maxInstances config) (timeout config)) g
  let cds = map (second snd) chs
  cds'      <- foldl drawCd (return []) $ zip [1 ..] cds
  return $ SelectValidCdInstance $ M.fromList cds'
  where
    drawCd cds (x, (b, cd)) = do
      f <- drawCdFromSyntax
        (printNavigations config)
        (printNames config)
        Nothing
        cd
        [i|#{path}-#{show x}|]
        Svg
      ((x, (b, f)) :) <$> cds
