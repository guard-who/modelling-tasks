{-# OPTIONS_GHC -Wno-orphans #-}
-- | Common functions for application modules
module Common (
  forceErrors, instanceInput, printNetAndInfo, renderPetriNet,
  ) where

import Modelling.Auxiliary.Output       (OutputMonad (..))

import Control.Monad                    (unless)
import Control.Monad.Trans              (MonadTrans (lift))
import Control.Monad.Trans.Except       (ExceptT, runExceptT)
import Data.Map                         (foldrWithKey)
import Diagrams.Prelude                 (Diagram, mkWidth)
import Diagrams.Backend.SVG             (B, renderSVG)

instanceInput :: IO Int
instanceInput = do
  putStr "Seed of wanted Instance: "
  read <$> getLine

forceErrors :: Monad m => ExceptT String m () -> m ()
forceErrors m = do
  () <- either error id <$> runExceptT m
  return ()

printNetAndInfo :: Show a => String -> (Diagram B, a) -> IO ()
printNetAndInfo x (dia, concurrent) = do
  renderPetriNet x dia
  print concurrent

renderPetriNet :: String -> Diagram B -> IO ()
renderPetriNet x dia = do
  renderSVG name (mkWidth 800) dia
  putStrLn $ "wrote file" ++ name
  where
    name = x ++ "petri.svg"

instance OutputMonad IO where
  assertion b m = unless b $ error m
  enumerate g f = lift . putStrLn . foldrWithKey
    (\k x rs -> g k ++ ". " ++ f x ++ "\n" ++ rs)
    ""
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
