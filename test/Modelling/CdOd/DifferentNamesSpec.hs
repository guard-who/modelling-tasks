-- |

module Modelling.CdOd.DifferentNamesSpec where

import qualified Data.Bimap                       as BM

import Modelling.Auxiliary.Output (
  LangM'(withLang),
  Language (English),
  )
import Modelling.CdOd.DifferentNames (
  differentNamesEvaluation,
  differentNamesInitial,
  differentNamesSyntax,
  DifferentNamesInstance (..)
  )
import Modelling.CdOd.Types             (Name (Name, unName), toNameMapping)
import Modelling.Common                 ()

import Control.Monad.Random             (mkStdGen, randomRIO)
import Data.Bifunctor                   (Bifunctor (bimap))
import Data.Either                      (isLeft)
import Data.List (nub)
import Data.Ratio                       ((%))
import Data.Tuple                       (swap)
import Test.Hspec
import Test.QuickCheck                  (Testable (property), (==>), (===), ioProperty)
import System.Random                    (setStdGen)
import System.Random.Shuffle            (shuffleM)

spec :: Spec
spec =
  describe "differentNamesEvaluation" $ do
    it "accepts the initial example" $
      let cs = bimap unName unName <$> differentNamesInitial
      in Right 1 == evaluateDifferentNames cs cs
    it "accepts correct solutions" $
      property $ \cs g -> not (null cs) && isValidMapping cs
        ==> ioProperty $ do
          cs' <- flipCoin g `mapM` cs >>= shuffleM
          return $ Right 1 === evaluateDifferentNames cs cs'
    it "accepts with percentage or rejects too short solutions" $
      property $ \cs n -> not (null cs) && isValidMapping cs
        ==> ioProperty $ do
          let n' = abs n
              l = fromIntegral $ length cs
              r = (l - fromIntegral n') % l
          cs' <- drop n' <$> shuffleM cs
          return $ (if r >= 0.5 then (Right r ==) else isLeft)
            $ evaluateDifferentNames cs cs'
    it "rejects too long solutions" $
      property $ \cs w ->
        let cs' = cs ++ w
        in not (null w) && isValidMapping cs
           ==> isLeft $ evaluateDifferentNames cs cs'

flipCoin :: Int -> (String, String) -> IO (String, String)
flipCoin g p = do
  setStdGen $ mkStdGen g
  b <- randomRIO (False, True)
  return $ (if b then swap else id) p

evaluateDifferentNames
  :: [(String, String)]
  -- ^ task instance mapping
  -> [(String, String)]
  -- ^ submitted mapping
  -> Either String Rational
evaluateDifferentNames cs cs' = flip withLang English $ do
  let i = DifferentNamesInstance {
        anonymousObjects = error "anonymousObjects is undefined",
        cDiagram = error "cDiagram is undefined",
        generatorValue = 1,
        oDiagram = error "oDiagram is undefined",
        mapping = toNameMapping $ BM.fromList cs
        }
      cs'' = bimap Name Name <$> cs'
  differentNamesSyntax i cs''
  differentNamesEvaluation i cs''

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
