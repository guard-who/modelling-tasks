-- |

module Modelling.CdOd.DifferentNamesSpec where

import qualified Data.Bimap                       as BM

import Modelling.Auxiliary.Output (
  LangM'(withLang),
  Language (English),
  )
import Modelling.CdOd.DifferentNames (
  differentNamesEvaluation,
  DifferentNamesInstance (..)
  )
import Modelling.Common                 ()

import Control.Monad.Random             (mkStdGen, randomRIO)
import Data.Bifunctor                   (Bifunctor (bimap))
import Data.Either                      (isLeft)
import Data.List (nub)
import Test.Hspec
import Test.QuickCheck                  (Testable (property), (==>), (===), ioProperty)
import System.Random                    (setStdGen)
import System.Random.Shuffle            (shuffleM)

spec :: Spec
spec =
  describe "differentNamesEvaluation" $ do
    it "accepts correct solutions" $
      property $ \cs g -> isValidMapping cs
        ==> ioProperty $ do
          cs' <- flipCoin g `mapM` cs >>= shuffleM
          return $ Right () === evaluateDifferentNames cs cs'
    it "rejects too short solutions" $
      property $ \cs n ->
        let cs' = drop n cs
        in n > 0 && not (null cs) && isValidMapping cs
           ==> isLeft $ evaluateDifferentNames cs cs'
    it "rejects too long solutions" $
      property $ \cs w ->
        let cs' = cs ++ w
        in not (null w) && isValidMapping cs
           ==> isLeft $ evaluateDifferentNames cs cs'

flipCoin :: Int -> (Char, Char) -> IO (Char, Char)
flipCoin g p = do
  setStdGen $ mkStdGen g
  b <- randomRIO (False, True)
  return $ (if b then (\(x, y) -> (y, x)) else id) p

evaluateDifferentNames
  :: [(Char, Char)]
  -- ^ task instance mapping
  -> [(Char, Char)]
  -- ^ submitted mapping
  -> Either String ()
evaluateDifferentNames cs cs' = do
  flip withLang English $ differentNamesEvaluation
    DifferentNamesInstance {
      cDiagram = error "cDiagram is undefined",
      oDiagram = error "oDiagram is undefined",
      mapping = BM.fromList $ bimap (:[]) (:[]) <$> cs
      }
    cs'

isValidMapping :: (Eq a, Eq b) => [(a, b)] -> Bool
isValidMapping cs
  | l > length (nub $ fst <$> cs)
  = False
  | l > length (nub $ snd <$> cs)
  = False
  | otherwise
  = True
  where
    l = length cs
