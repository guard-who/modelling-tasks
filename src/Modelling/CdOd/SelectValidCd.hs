{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
module Modelling.CdOd.SelectValidCd where

import qualified Data.Map                         as M (fromList)

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
