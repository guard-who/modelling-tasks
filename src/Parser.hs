{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
module Parser where

import qualified Data.Set                         as S (fromList)
import qualified Data.Map                         as M
  (alter, empty, insert, singleton)

import Control.Monad                    (void)
import Data.Functor                     (($>))
import Data.Map                         (Map)
import Data.Set                         (Set)
import Text.Parsec
import Text.Parsec.String               (Parser)

data Signature = Signature {
    scope    :: Maybe String,
    sigName  :: String
  } deriving (Eq, Ord)

data Object =
    Object {
      objSig     :: Signature,
      identifier :: Int
    }
  | NumberObject {
      number :: Int
    }
  | NamedObject {
      objName :: String
    } deriving (Eq, Ord)

data Relation a =
    EmptyRelation
  | Single (a Object)
  | Double (a (Object, Object))
  | Triple (a (Object, Object, Object))

data Annotation = Skolem deriving Eq

type Entries a = a Signature (Entry a Set)

data Entry a b = Entry {
    annotation :: Maybe Annotation,
    relation   :: a String (Relation b)
  }

single :: Monoid (a Object) => Relation a -> Either String (a Object)
single EmptyRelation = Right mempty
single (Single r)    = Right r
single _             = Left "Relation is (unexpectedly) a mapping"

double
  :: Monoid (a (Object, Object))
  => Relation a
  -> Either String (a (Object, Object))
double EmptyRelation = Right mempty
double (Double r)    = Right r
double _             = Left "Relation is not a binary mapping"

triple
  :: Monoid (a (Object, Object, Object))
  => Relation a
  -> Either String (a (Object, Object, Object))
triple EmptyRelation = Right mempty
triple (Triple r)    = Right r
triple _             = Left "Relation is not a ternary mapping"

combineEntries :: [Entries (,)] -> Entries Map
combineEntries = foldl createOrInsert M.empty
  where
    createOrInsert ys (s, e) = M.alter (Just . alterSig e) s ys
    alterSig e Nothing  = e { relation = uncurry M.singleton $ relation e}
    alterSig e (Just y) = y { relation = uncurry M.insert (relation e) (relation y) }

alloyInstance :: Parser [Entries (,)]
alloyInstance = (try (void $ string "---INSTANCE---" *> newline) <|> return ())
  *> many entry

entry :: Parser (Entries (,))
entry = do
  entryAnnotation <- try (string "skolem " $> Just Skolem) <|> pure Nothing
  entrySignature <- sig
  (entrySignature,)
    <$> (Entry
         <$> pure entryAnnotation
         <*> ((,)
              <$> ((string "<:" *> word) <|> pure "")
              <*> parseRelations <* (void newline <|> eof)))

sig :: Parser Signature
sig =
  try (Signature <$> (Just <$> word) <* char '/' <*> word)
  <|> Signature <$> pure Nothing <*> word

parseRelations :: Parser (Relation Set)
parseRelations = char '='
  *> (try (string "{}" $> EmptyRelation)
      <|> fmap Triple (try $ sep tripleRel)
      <|> fmap Double (try $ sep doubleRel)
      <|> fmap Single (sep singleRel))
  where
    sep rel = S.fromList
      <$> between (char '{') (char '}') (rel `sepBy` string ", ")
    tripleRel = (,,) <$> nextObject <*> nextObject <*> object
    doubleRel = (,) <$> nextObject <*> object
    singleRel = object
    nextObject = object <* string "->"

object :: Parser Object
object =
  try (Object <$> sig <* char '$' <*> (read <$> many1 digit))
  <|> try (NumberObject <$> int)
  <|> NamedObject <$> word

int :: Parser Int
int = fmap read $ (++)
  <$> (try (string "-") <|> pure "")
  <*> many1 digit

word :: Parser String
word = (:)
  <$> (letter <|> char '$')
  <*> many1 (letter <|> digit <|> char '_')
