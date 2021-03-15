{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
originally from Autotool (https://gitlab.imn.htwk-leipzig.de/autotool/all0)
based on revision: ad25a990816a162fdd13941ff889653f22d6ea0a
based on file: collection/src/Petri/Property.hs
-}
module Modelling.PetriNet.Reach.Property where

import qualified Data.Map                         as M (
  elems,
  filter,
  fromListWith,
  keysSet,
  null,
  )
import qualified Data.Set                         as S (
  difference,
  member,
  null,
  size,
  )


import Modelling.Auxiliary.Output (
  OutputMonad(indent, paragraph, refuse, text),
  )
import Modelling.PetriNet.Reach.Type (
  Net (capacity, connections, places, start, transitions),
  Capacity (..),
  State (unState),
  allNonNegative,
  conforms,
  )

import Control.Monad                    (forM, forM_, unless, when)
import Data.Typeable                    (Typeable)
import GHC.Generics                     (Generic)

data Property
  = Default
  | MaxNumPlaces Int
  | MaxNumTransitions Int
  | MaxEdgeMultiplicity Int
  | MaxInitialTokens Int
  | Capacity (Capacity ())
  deriving (Typeable, Generic)

validates
  :: (Foldable f, OutputMonad m, Show a, Show b, Ord b, Ord a)
  => f Property
  -> Net a b
  -> m ()
validates props n = forM_ props $ \prop -> validate prop n

validate
  :: (OutputMonad f, Show a, Show t, Ord t, Ord a)
  => Property
  -> Net a t
  -> f ()
validate p n = case p of
  MaxNumPlaces m -> guardBound
    "Anzahl der Stellen"
    (S.size $ places n)
    m
  MaxNumTransitions m -> guardBound
    "Anzahl der Transitionen"
    (S.size $ transitions n)
    m
  MaxInitialTokens m -> guardBound
    "Anzahl der Token in Startzustand"
    (sum $ M.elems $ unState $ start n)
    m
  MaxEdgeMultiplicity m ->
    forM_ (connections n) $ \c@(vor, _, nach) -> do
      let badVor =
            M.filter (> m) $
              M.fromListWith (+) $ zip vor $ repeat 1
          badNach =
            M.filter (> m) $
              M.fromListWith (+) $ zip nach $ repeat 1
      unless (M.null badVor) $
        refuse $ do
          paragraph $ text $ unlines [
            "Verbindung: " ++ show c,
            "Vielfachheit der Eingangskanten zu hoch:"
            ]
          indent $ text $ show badVor
      unless (M.null badNach) $
        refuse $ do
          paragraph $ text $ unlines [
            "Verbindung: " ++ show c,
            "Vielfachheit der Ausgangskanten zu hoch:"]
          indent $ text $ show badNach
  Capacity cap -> case cap of
    Bounded _ -> undefined -- TODO: Is this case required?
    Unbounded ->
      when (capacity n /= Unbounded) $
        refuse $ text $ "Als Kapazität ist vorgeschrieben:" ++ show cap
    AllBounded b ->
      when (capacity n /= AllBounded b) $
        refuse $ text $ "Als Kapazität ist vorgeschrieben:" ++ show cap
  Default -> do
    unless (allNonNegative $ start n) $
      refuse $ text "Startzustand enthält negative Markierungen"
    unless (conforms (capacity n) (start n)) $
      refuse $
        text "Startzustand überschreitet Kapazitäten"

    forM_ (connections n) $ \c@(vor, t, nach) -> do
      unless (S.member t $ transitions n) $
        refuse $ do
          paragraph $ text $ "Verbindung:" ++ show c
          paragraph $ text $ "nicht deklarierte Transition:" ++ show t
      forM_ vor $ \v ->
        unless (S.member v $ places n) $
          refuse $ do
            paragraph $ text $ "Verbindung:" ++ show c
            paragraph $ text $
              "nicht deklarierte Stelle im Vorbereich:" ++ show v
      forM nach $ \a ->
        unless (S.member a $ places n) $
          refuse $ do
            paragraph $ text $ "Verbindung:" ++ show c
            paragraph $ text $
              "nicht deklarierte Stelle im Nachbereich:" ++ show a

    case capacity n of
      Bounded f -> do
        let out = S.difference (M.keysSet f) (places n)
        unless (S.null out) $
          refuse $ do
            paragraph $ text "nicht definierte Stellen in Kapazitätsfunktion:"
            paragraph $ text $ show out
      _ -> return ()

    let out = S.difference (M.keysSet $ unState $ start n) (places n)
    unless (S.null out) $
      refuse $ do
        paragraph $ text "nicht definierte Stellen im Startzustand:"
        paragraph $ text $ show out

guardBound :: (Ord a, OutputMonad f, Show a) => String -> a -> a -> f ()
guardBound name actual bound =
  when (actual > bound) $
    refuse $ do
      paragraph $ text $ name ++ '(' : show actual ++ ")"
      paragraph $ text $ " ist größer als die Schranke" ++ show bound
