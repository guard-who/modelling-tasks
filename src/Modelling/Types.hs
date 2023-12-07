{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

-- | This module provides basic types

module Modelling.Types (
  Change (..),
  Letters (..),
  Name (..),
  NameMapping (..),
  fromNameMapping,
  parseLettersPrec,
  parseNamePrec,
  showLetters,
  showName,
  toNameMapping,
  ) where

import qualified Data.Bimap                       as BM

import Modelling.Auxiliary.Common       (skipSpaces)

import Data.Bimap                       (Bimap)
import Data.Char                        (isAlpha, isAlphaNum)
import Data.String                      (IsString (fromString))
import GHC.Generics                     (Generic)
import Text.ParserCombinators.Parsec (
  Parser,
  many1,
  satisfy,
  endBy,
  )

newtype Name = Name { unName :: String }
  deriving (Eq, Generic, Ord, Read, Show)

instance IsString Name where
  fromString = Name

showName :: Name -> String
showName = unName

parseNamePrec :: Int -> Parser Name
parseNamePrec _ = do
  skipSpaces
  Name <$> many1 (satisfy isAlphaNum) <* skipSpaces

newtype Letters = Letters { lettersList :: String }
  deriving (Eq, Generic, Ord, Read, Show)

instance IsString Letters where
  fromString = Letters

showLetters :: Letters -> String
showLetters = lettersList

parseLettersPrec :: Int -> Parser Letters
parseLettersPrec _ = do
  skipSpaces
  Letters <$> endBy (satisfy isAlpha) skipSpaces

newtype NameMapping = NameMapping { nameMapping :: Bimap Name Name }
  deriving (Eq, Generic)

fromNameMapping :: NameMapping -> Bimap String String
fromNameMapping = BM.mapMonotonic unName . BM.mapMonotonicR unName . nameMapping

toNameMapping :: Bimap String String -> NameMapping
toNameMapping = NameMapping . BM.mapMonotonic Name . BM.mapMonotonicR Name

instance Show NameMapping where
  show = show . BM.toList . nameMapping

instance Read NameMapping where
  readsPrec p xs = [(NameMapping $ BM.fromList y, ys) | (y, ys) <- readsPrec p xs]

data Change a = Change {
    add    :: Maybe a,
    remove :: Maybe a
  } deriving (Eq, Foldable, Functor, Generic, Read, Show, Traversable)
