module Modelling.Auxiliary.Common where

import Control.Monad.Random (MonadRandom (getRandomR))
import Data.Char                        (isSpace)
import Text.ParserCombinators.Parsec    (Parser, many, optional, satisfy)

oneOf :: MonadRandom m => [a] -> m a
oneOf xs = do
      x <- getRandomR (0, length xs - 1)
      return $ xs !! x

skipSpaces :: Parser ()
skipSpaces = optional $ many $ satisfy isSpace
