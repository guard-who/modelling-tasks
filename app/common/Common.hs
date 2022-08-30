{-# OPTIONS_GHC -Wno-orphans #-}
-- | Common functions for application modules
module Common (
   instanceInput
  ) where

import qualified Data.Map                         as M (lookup)

import Control.Monad                    (unless)
import Control.Monad.Output             (LangM' (LangM), OutputMonad (..))
import Control.Monad.Trans              (MonadTrans (lift))
import Data.Map                         (foldrWithKey)
import Data.Maybe                       (fromMaybe)

instanceInput :: IO Int
instanceInput = do
  putStr "Seed of wanted Instance: "
  read <$> getLine

instance OutputMonad IO where
  assertion b m = unless b $ m >> error ""
  image         = lift . putStr . ("file: " ++)
  images g f    = lift . putStrLn . foldrWithKey
    (\k x rs -> g k ++ ". file: " ++ f x ++ '\n' : rs)
    ""
  paragraph     = (>> lift (putStrLn ""))
  text          = lift . putStr
  enumerateM p  = foldl
    (\o (x, e) -> paragraph $ do o; p x; lift $ putStr "  "; e)
    (return ())
  itemizeM      = foldl
    (\o x -> paragraph $ do o; lift $ putStr " -  "; x)
    (return ())
  indent xs     = do
    lift $ putStr ">>>>"
    xs
    lift $ putStrLn "<<<<"
  refuse xs     = do
    xs
    indent $ text "No"
    error "refused"
  latex         = lift . putStrLn . ("LaTeX: " ++)
  code          = lift . putStr . (\xs -> " <" ++ xs ++ "> ")
  translated lm = do
    l <- LangM return
    text . fromMaybe "" $ M.lookup l lm
