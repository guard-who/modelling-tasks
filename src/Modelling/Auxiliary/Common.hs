module Modelling.Auxiliary.Common where

import qualified Data.ByteString                  as BS (readFile, writeFile)
import qualified Data.ByteString.Lazy             as LBS (fromStrict)
import qualified Data.ByteString.UTF8             as BS (fromString)
import qualified Data.Map                         as M (
  Map,
  empty,
  insertWith,
  )
import qualified Data.Set                         as S (
  Set,
  singleton,
  union,
  )

import Control.Arrow                    (ArrowChoice (left))
import Control.Monad                    (when)
import Control.Monad.IO.Class           (MonadIO (liftIO))
import Control.Monad.Random (MonadRandom (getRandomR))
import Data.Char                        (digitToInt, isSpace, toLower, toUpper)
import Data.Digest.Pure.SHA             (sha256, showDigest)
import Data.Foldable                    (Foldable (foldl'))
import Data.Function                    ((&))
import Control.Lens (
  LensRules,
  (.~),
  lensField,
  lensRules,
  mappingNamer,
  )
import System.Directory                 (doesFileExist)
import Text.Parsec                      (parse)
import Text.ParserCombinators.Parsec (
  Parser,
  digit,
  many,
  many1,
  optional,
  satisfy,
  )

data Object = Object {
  oName :: String,
  oIndex :: Int
  } deriving (Eq, Ord, Show)

toMap :: (Ord a, Ord b) => S.Set (a, b) -> M.Map a (S.Set b)
toMap = foldr (\(x, y) -> M.insertWith S.union x (S.singleton y)) M.empty

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

lensRulesL :: LensRules
lensRulesL = lensRules & lensField .~ mappingNamer (pure . ('l':) . upperFirst)

parseWith :: (Int -> Parser a) -> String -> Either String a
parseWith f = left show . parse (f 0) ""

cacheIO
  :: (MonadIO m, Show a)
  => FilePath
  -- ^ base file path (prefix of file name)
  -> String
  -- ^ path prefix (including dot and extension)
  -> String
  -- ^ some identifying name for what (part of file name)
  -> a
  -- ^ what
  -> (FilePath -> a -> m b)
  -- ^ how to create something from what
  -> m FilePath
cacheIO path ext name what how = (file <$) . cache $ how file what
  where
    cache create = do
      let create' = create >> liftIO (BS.writeFile whatFile what')
      isFile <- liftIO $ doesFileExist file
      if isFile
        then do
          f <- liftIO $ BS.readFile whatFile
          when (f /= what') $ do
            liftIO $ appendFile (path ++ "busted.txt") whatId
            create'
        else create'
    what' = BS.fromString $ show what
    whatId = path ++ name ++ showDigest (sha256 $ LBS.fromStrict what')
    whatFile = whatId ++ ".hs"
    file = whatId ++ ext

short :: Enum a => a -> String
short x = show $ fromEnum x
