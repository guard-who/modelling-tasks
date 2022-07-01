-- |

module Modelling.CdOd.DifferentNamesSpec where

import qualified Data.Bimap                       as BM

import Modelling.CdOd.DifferentNames (
  differentNamesEvaluation,
  differentNamesInitial,
  differentNamesSyntax,
  DifferentNamesInstance (..),
  defaultDifferentNamesInstance,
  renameInstance,
  )
import Modelling.CdOd.Types (
  Name (Name, unName),
  associationNames,
  classNames,
  fromNameMapping,
  linkNames,
  toNameMapping,
  )
import Modelling.Common                 ()

import Control.Monad.Output (
  LangM' (withLang),
  Language (English),
  )
import Control.Monad.Random             (mkStdGen, randomRIO)
import Data.Bifunctor                   (Bifunctor (bimap))
import Data.Char                        (toUpper)
import Data.Containers.ListUtils        (nubOrd)
import Data.Either                      (isLeft)
import Data.List (nub)
import Data.Maybe                       (fromJust)
import Data.Ratio                       ((%))
import Data.Tuple                       (swap)
import Test.Hspec
import Test.QuickCheck (
  (===),
  (==>),
  Arbitrary (arbitrary),
  Property,
  Testable (property),
  ioProperty,
  oneof,
  sized,
  vectorOf,
  )
import System.Random                    (setStdGen)
import System.Random.Shuffle            (shuffleM)

spec :: Spec
spec = do
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
  describe "renameInstance" $ do
    it "is reversable" $ renameProperty $ \inst mrinst _ _ ->
        let cd = cDiagram inst
            od = oDiagram inst
            names = classNames cd
            assocs = associationNames cd
            links  = linkNames od
        in (Just inst ==)
           $ mrinst
           >>= (\x -> renameInstance x names assocs links)
    it "renames solution" $ renameProperty $ \inst mrinst as ls ->
      let rename xs ys = Name . fromJust . (`lookup` zip xs ys)
          origMap = bimap
            (rename (associationNames $ cDiagram inst) as)
            (rename (linkNames $ oDiagram inst) ls)
            <$> BM.toList (fromNameMapping $ mapping inst)
      in (Right 1 ==)
         $ maybe (Left "instance could not be renamed") return mrinst
         >>= \rinst -> differentNamesEvaluation rinst origMap `withLang` English
           :: Either String Rational

renameProperty ::
  Testable prop =>
  (DifferentNamesInstance
    -> Maybe DifferentNamesInstance
    -> [String]
    -> [String]
    -> prop)
  -> Property
renameProperty p = property $ \n1 n2 n3 n4 a1 a2 a3 l1 l2 l3 ->
  let inst = defaultDifferentNamesInstance
      ns = map unName [n1, n2, n3, n4]
      as = map unName [a1, a2, a3]
      ls = map unName [l1, l2, l3]
      distinct xs = length (nubOrd xs) == length xs
      mrinst = renameInstance inst ns as ls
  in distinct ns && distinct as && distinct ls
     ==> p inst mrinst as ls

instance Arbitrary Name where
  arbitrary = sized $ \s -> Name <$> (
    (:)
    <$> oneof (map return letters)
    <*> vectorOf s (oneof $ map return $ letters ++ ['0'..'9'])
    )
    where
      lowers = ['a'..'z']
      uppers = map toUpper lowers
      letters = lowers ++ uppers

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
        showSolution = True,
        mapping = toNameMapping $ BM.fromList cs,
        usesAllRelationships = Just True
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
