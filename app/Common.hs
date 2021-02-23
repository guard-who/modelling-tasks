-- | Common functions for application modules
module Common (
  forceErrors, instanceInput, printNetAndInfo, renderPetriNet,
  ) where

import Modelling.Auxiliary.Output       (OutputMonad (..))

import Control.Monad.Trans.Except       (ExceptT, runExceptT)
import Data.Map                         (foldrWithKey)
import Diagrams.Prelude                 (Diagram, mkWidth)
import Diagrams.Backend.SVG             (B, renderSVG)
import Control.Monad                    (unless)

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
  enumerate g f = putStrLn . foldrWithKey
    (\k x rs -> g k ++ ". " ++ f x ++ "\n" ++ rs)
    ""
  image         = putStrLn . ("file: " ++)
  images g f    = putStrLn . foldrWithKey
    (\k x rs -> g k ++ ". file: " ++ f x ++ rs)
    ""
  paragraph     = putStrLn
  text          = putStr
  enumerateM p  = foldl (\o (x, e) -> do o; p x; putStr "  "; e) (return ())
  itemizeM      = foldl (\o x -> do o; putStr " -  "; x) (return ())
