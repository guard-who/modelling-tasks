module Modelling.Auxiliary.Common where

import Control.Monad.Random (MonadRandom (getRandomR))
import Data.Char                        (digitToInt, isSpace, toLower, toUpper)
import Data.Foldable                    (Foldable (foldl'))
import Text.ParserCombinators.Parsec (
  Parser,
  digit,
  many,
  many1,
  optional,
  satisfy,
  )

oneOf :: MonadRandom m => [a] -> m a
oneOf xs = do
      x <- getRandomR (0, length xs - 1)
      return $ xs !! x

skipSpaces :: Parser ()
skipSpaces = optional $ many $ satisfy isSpace

parseInt :: Parser Int
parseInt = foldl' (\a i -> a * 10 + digitToInt i) 0 <$> many1 digit

lowerFirst :: String -> String
lowerFirst []     = []
lowerFirst (x:xs) = toLower x : xs

upperFirst :: String -> String
upperFirst []     = []
upperFirst (x:xs) = toUpper x : xs
