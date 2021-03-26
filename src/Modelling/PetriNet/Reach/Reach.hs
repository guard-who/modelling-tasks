{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
originally from Autotool (https://gitlab.imn.htwk-leipzig.de/autotool/all0)
based on revision: ad25a990816a162fdd13941ff889653f22d6ea0a
based on file: collection/src/Petri/Reach.hs
-}
module Modelling.PetriNet.Reach.Reach where

import qualified Data.Set                         as S (toList)

import Modelling.Auxiliary.Output (
  LangM,
  OutputMonad (assertion, code, image, indent, paragraph, text),
  )
import Modelling.PetriNet.Reach.Draw    (drawToFile)
import Modelling.PetriNet.Reach.Property (Property (Default), validate)
import Modelling.PetriNet.Reach.Roll    (net)
import Modelling.PetriNet.Reach.Step    (execute, levels)
import Modelling.PetriNet.Reach.Type (
  Transition (..),
  Place(..),
  Net (transitions, start),
  Capacity (Unbounded),
  State,
  mark,
  )

import Control.Monad                    (foldM, forM)
import Control.Monad.IO.Class           (MonadIO)
import Control.Monad.Random             (mkStdGen)
import Control.Monad.Trans.Random       (evalRand)
import Data.List                        (minimumBy)
import Data.Ord                         (comparing)
import Data.Typeable                    (Typeable)
import GHC.Generics                     (Generic)

data PetriReach = PetriReach
  deriving (Generic, Typeable)

verifyReach :: (OutputMonad m, Show a, Show t, Ord t, Ord a)
  => PetriReach
  -> (Net a t, State a)
  -> LangM m
verifyReach PetriReach (n,s) = do
  validate Default n
  validate Default $ n { start = s }

reportReach
  :: (MonadIO m, OutputMonad m, Ord s, Ord t, Show s, Show t, Show a)
  => FilePath
  -> (Net s t, a)
  -> LangM m
reportReach path (n,goal) = do
  paragraph $ text "Gesucht ist für das Petrinetz"
  i <- drawToFile False path 0 n
  image i
  paragraph $ do
    text "eine Transitionsfolge, durch die die folgende Markierung erreicht wird:"
    text $ show goal
  paragraph $ text "Geben Sie Ihre Lösung als Auflistung der folgenden Art an:"
  code $ show [Transition 0, Transition 2, Transition 99]
  paragraph $ text $ concat [
    "Wobei diese Angabe bedeuten soll, dass nach dem Schalten von ",
    show (Transition 0), ", danach ", show (Transition 2),
    ", und schließlich ", show (Transition 99),
    " (in genau dieser Reihenfolge), die gesuchte Markierung erreicht wird."
    ]

initialReach :: p -> (Net s a, b) -> [a]
initialReach _ (n,_) = reverse $ S.toList $ transitions n

totalReach :: (MonadIO m, OutputMonad m, Show s, Show t, Ord s, Ord t)
  => FilePath
  -> (Net s t, State s)
  -> [t]
  -> LangM m
totalReach path (n,goal) ts = do
  paragraph $ text "Startmarkierung"
  indent $ text $ show (start n)
  out <- foldM
      (\z (k,t) -> do
         paragraph $ text $ "Schritt" ++ show k
         Modelling.PetriNet.Reach.Step.execute path False k n t z)
      (start n)
      (zip [1 :: Int ..] ts)
  assertion (out == goal) "Zielmarkierung erreicht?"

data Config = Config {
  numPlaces :: Int,
  numTransitions :: Int,
  capacity :: Capacity Place,
  transitionLength :: Int
  }
  deriving (Typeable, Generic)

defaultReachConfig :: Config
defaultReachConfig = Config {
  numPlaces = 4,
  numTransitions = 4,
  Modelling.PetriNet.Reach.Reach.capacity = Unbounded,
  transitionLength = 8
  }

generateReach :: Config -> Int -> (Net Place Transition, State Place)
generateReach conf seed =
  let tries = eval $ forM [1 :: Int .. 1000] $ const $ do
        let ps = [Place 1 .. Place (numPlaces conf)]
            ts = [Transition 1 .. Transition (numTransitions conf)]
        n <- Modelling.PetriNet.Reach.Roll.net
            ps
            ts
            (Modelling.PetriNet.Reach.Reach.capacity conf)
        return $ do
          (l,zs) <-
            take (transitionLength conf + 1) $ zip [0 :: Int ..] $ levels n
          z' <- zs
          let d = sum $ do
                p <- ps
                return $ abs (mark (start n) p - mark z' p)
          return ((negate l, d), (n, z'))
  in snd $ minimumBy (comparing fst) $ concat tries
  where
    eval f = evalRand f $ mkStdGen seed
