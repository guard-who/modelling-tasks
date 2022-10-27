module Modelling.ActivityDiagram.Auxiliary.Parser (
  ParseValue(..),
  parseMappingSequence
) where

import qualified Data.Map as M (fromList)

import Control.Applicative (Alternative((<|>)))
import Data.Char (isControl, isSpace)
import Data.Maybe (catMaybes)
import Data.Map (Map)
import Modelling.Auxiliary.Common (parseInt)
import Text.Parsec.String (Parser)
import Text.Parsec(
  endOfLine,
  satisfy,
  skipMany,
  sepEndBy,
  sepBy,
  choice,
  many1,
  char,
  letter,
  digit,
  between
  )

data ParseValue =
  ParseInt Int |
  ParseString String |
  ParseTuple (ParseValue, ParseValue) |
  ParseList [ParseValue]
  deriving (Show)

parseMappingSequence :: Parser (Map String ParseValue)
parseMappingSequence = M.fromList . catMaybes <$> (parseMapping `sepEndBy` endOfLine)

parseMapping :: Parser (Maybe (String, ParseValue))
parseMapping = skipSpaceChars *> text
  where
    parseLine = (,) <$> (parseString <* char ':' <* skipSpaceChars) <*> parseValue
    emptyLine = skipSpaceChars
    text = (Just <$> parseLine) <|> (Nothing <$ emptyLine)

parseValue :: Parser ParseValue
parseValue = value <* skipSpaceChars
  where value = choice [ ParseList <$> parseList
                        ,ParseTuple <$> parseTuple
                        ,ParseInt <$> parseInt
                        ,ParseString <$> parseString
                        ]

parseString :: Parser String
parseString =
  between (char '"') (char '"') parseAlphaNums <|>
  parseAlphaNums
  where parseAlphaNums = many1 (letter <|> digit)

parseTuple :: Parser (ParseValue, ParseValue)
parseTuple = between (char '(' <* skipSpaceChars) (char ')')
  $ do x <- parseValue <* char ',' <* skipSpaceChars
       y <- parseValue
       return (x,y)

parseList :: Parser [ParseValue]
parseList = between (char '[' <* skipSpaceChars) (char ']')
  $ (parseValue <* skipSpaceChars) `sepBy` (char ',' <* skipSpaceChars)

skipSpaceChars :: Parser ()
skipSpaceChars = skipMany $ satisfy (\c -> isSpace c && not (isControl c))