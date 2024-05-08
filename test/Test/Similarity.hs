{-# LANGUAGE NamedFieldPuns #-}
-- | Provides the ability to allow specific deviations of expectations.
module Test.Similarity (
  Deviation (..),
  assertSimilar,
  debugAssertEqual,
  shouldReturnSimilar,
  ) where

import Control.Applicative              (Alternative ((<|>)), optional)
import Control.Monad                    (unless, when)
import Data.Bifunctor                   (first)
import Data.Char                        (isDigit)
import Data.Functor                     (($>))
import Data.List.Extra                  (repeatedly, takeEnd)
import Data.Maybe                       (fromMaybe)
import Data.Tuple.Extra                 (both)
import System.Directory                 (createDirectoryIfMissing)
import System.FilePath                  (takeDirectory)
import Test.HUnit.Base                  (Assertion, assertEqual)
import Text.ParserCombinators.Parsec (
  Parser,
  anyChar,
  char,
  digit,
  eof,
  many,
  many1,
  notFollowedBy,
  parse,
  string,
  try,
  )

{-|
A datatype used to define similarity of values
by setting specific allowed deviations.
-}
data Deviation
  = Deviation {
    -- | Absolute value change that is allowed
    absoluteDeviation :: Double,
    -- | Relative change that is allowed,
    -- e.g., 0.1 times of the original value
    relativeDeviation :: Double
    }

{-|
A datatype for checking deviations of numerical values.
-}
data SomeXMLString
  = SomeString String
  -- ^ Strings that have to match exactly
  | SomeNumber String Double
  -- ^ Numbers that may differ (by 'Deviation')
  deriving Show

{-|
For regaining the original representation
-}
showOriginal :: [SomeXMLString] -> String
showOriginal [] = ""
showOriginal (SomeString x : xs) = x ++ showOriginal xs
showOriginal (SomeNumber x _ : xs) = x ++ showOriginal xs

parseXML :: Parser [SomeXMLString]
parseXML =
  (:) <$> parseXMLString <*> parseXML
  <|> eof $> []

parseXMLString :: Parser SomeXMLString
parseXMLString = parseNumber <|> parseString

{-|
These specific numbers always have a digit before and after the dot
-}
parseNumber :: Parser SomeXMLString
parseNumber = uncurry SomeNumber <$> do
  sign <- optional $ string "-"
  x <- many1 digit
  y <- (:) <$> char '.' <*> many1 digit
  let number = fromMaybe "" sign ++ x ++ y
  return (number, read number)

{-|
Parses a complete string without dotted numbers.
-}
parseString :: Parser SomeXMLString
parseString = fmap SomeString $ many1 $ try $ do
  x <- anyChar
  when (x == '-') (notFollowedBy $ many digit >> char '.' >> digit)
  when (isDigit x) (notFollowedBy $ many digit >> char '.' >> digit)
  return x

shouldReturnSimilar
  :: Maybe FilePath
  -- ^ Whether to write the test result to a file
  -- using the provided file name (and sub-directories)
  -> Int
  -- ^ the length of context before and after differences
  -> Deviation
  -- ^ when to assume values are different
  -> IO String
  -- ^ the expected XML String
  -> String
  -- ^ the actual XML String
  -> Assertion
shouldReturnSimilar debugFile context deviation action expected =
  action >>= assertSimilar debugFile context deviation expected

isSimilar :: Deviation -> SomeXMLString -> SomeXMLString -> Bool
isSimilar _ (SomeString x1) (SomeString x2) = x1 == x2
isSimilar
  Deviation {absoluteDeviation, relativeDeviation}
  (SomeNumber _ x1)
  (SomeNumber _ x2)
  = x1 - abs x1 * relativeDeviation - absoluteDeviation <= x2
  && x1 + abs x1 * relativeDeviation + absoluteDeviation >= x2
isSimilar _ (SomeString []) (SomeNumber [] _) = True
isSimilar _ (SomeNumber [] _) (SomeString []) = True
isSimilar _ _ _ = False

{-|
Asserts similarity between the given XML Strings by using the specified
Deviations.
-}
assertSimilar
  :: Maybe FilePath
  -- ^ Whether to write the test result to a file
  -- using the provided file name (and sub-directories)
  -> Int
  -- ^ the length of context before and after differences
  -> Deviation
  -- ^ when to assume values are different
  -> String
  -- ^ the expected XML String
  -> String
  -- ^ the actual XML String
  -> Assertion
assertSimilar debugFile context deviation expected actual = do
  let es = either (error . show) id $ parse parseXML "expected" expected
      as = either (error . show) id $ parse parseXML "actual" actual
      similars = zipWith (isSimilar deviation) es as
  unless (and similars) $ do
    let xs = rejoin $ repeatedly breakNonEmpty $ zip similars (zip es as)
        e = showOriginal $ map fst $ head xs
        preE
          | length e > context = ".." ++ takeEnd context e
          | otherwise          = e
        (esDiff, asDiff) = (preE ++) `both` diffWithContext context (tail xs)
    debugWriteFile debugFile expected actual
    assertEqual "" esDiff asDiff
  let lengthEs = length es
      lengthAs = length as
  unless (lengthEs == lengthAs) $ do
    let (expectedEnd, actualEnd) =
          if lengthEs < lengthAs
          then ("..", ".." ++ showOriginal (drop lengthEs as))
          else (".." ++ showOriginal (drop lengthAs es), "..")
    debugWriteFile debugFile expected actual
    assertEqual "" expectedEnd actualEnd
  where
    rejoin (xs : different : same : zs) = xs : rejoin ((different ++ same) : zs)
    rejoin xs = xs
    breakNonEmpty xs = first (map snd) $
      case break fst xs of
        ([], _) -> span fst xs
        broken -> broken
    diffWithContext _ [] = ("", "")
    diffWithContext c ([] : xss) = diffWithContext c xss
    diffWithContext c (((e, a) : xs) : xss) =
      let same = showOriginal $ map fst xs
          lengthSame = length same
          (es, as) = diffWithContext (c - lengthSame) xss
          sameWithDots = take context same ++ ".."
          originalE = showOriginal [e]
          originalA = showOriginal [a]
      in if lengthSame > context
         then (originalE ++ sameWithDots, originalA ++ sameWithDots)
         else (originalE ++ same ++ es, originalA ++ same ++ as)

debugAssertEqual :: Maybe FilePath -> String -> String -> String -> IO ()
debugAssertEqual debugFile preface expected actual = do
  unless (actual == expected) $ debugWriteFile debugFile expected actual
  assertEqual preface expected actual

debugWriteFile :: Maybe FilePath -> String -> String -> IO ()
debugWriteFile debugFile expected actual
  | Just file <- debugFile = do
      let expectedFile = "artifacts/expected/" ++ file
          actualFile = "artifacts/actual/" ++ file
      createDirectoryIfMissing True $ takeDirectory expectedFile
      createDirectoryIfMissing True $ takeDirectory actualFile
      writeFile expectedFile expected
      writeFile actualFile actual
  | otherwise = pure ()
