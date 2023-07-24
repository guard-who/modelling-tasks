{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

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

import Modelling.PetriNet.Reach.Step    (execute)
import Modelling.PetriNet.Reach.Type (
  Net (capacity, connections, places, start, transitions),
  Capacity (..),
  State (unState),
  allNonNegative,
  conforms,
  )

import Control.Monad                    (foldM, unless, when)
import Control.Monad.Identity           (Identity (runIdentity))
import Control.Monad.Output (
  GenericOutputMonad (indent, paragraph, refuse, text),
  LangM,
  Language,
  OutputMonad,
  )
import Control.Monad.Output.Generic (
  evalLangM,
  )
import Data.Either                      (fromLeft)
import Data.Foldable                    (for_)
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
  -> LangM m
validates props n = for_ props $ \prop -> validate prop n

validate
  :: (OutputMonad m, Show a, Show t, Ord t, Ord a)
  => Property
  -> Net a t
  -> LangM m
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
    "Anzahl der Token in Startmarkierung"
    (sum $ M.elems $ unState $ start n)
    m
  MaxEdgeMultiplicity m ->
    for_ (connections n) $ \c@(vor, _, nach) -> do
      let badVor =
            M.filter (> m) $
              M.fromListWith (+) $ map (,1) vor
          badNach =
            M.filter (> m) $
              M.fromListWith (+) $ map (,1) nach
      unless (M.null badVor) $
        refuse $ do
          paragraph $ text $ unlines [
            "Verbindung: " ++ show c,
            "Vielfachheit der Eingangskanten zu hoch:"
            ]
          indent $ text $ show badVor
          pure ()
      unless (M.null badNach) $
        refuse $ do
          paragraph $ text $ unlines [
            "Verbindung: " ++ show c,
            "Vielfachheit der Ausgangskanten zu hoch:"]
          indent $ text $ show badNach
          pure ()
      pure ()
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
      refuse $ text "Startmarkierung enthält negative Anzahl an Marken"
    unless (conforms (capacity n) (start n)) $
      refuse $
        text "Startmarkierung überschreitet Kapazitäten"

    for_ (connections n) $ \c@(vor, t, nach) -> do
      unless (S.member t $ transitions n) $
        refuse $ do
          paragraph $ text $ "Verbindung:" ++ show c
          paragraph $ text $ "nicht deklarierte Transition:" ++ show t
          pure ()
      for_ vor $ \v ->
        unless (S.member v $ places n) $
          refuse $ do
            paragraph $ text $ "Verbindung:" ++ show c
            paragraph $ text $
              "nicht deklarierte Stelle im Vorbereich:" ++ show v
            pure ()
      for_ nach $ \a ->
        unless (S.member a $ places n) $
          refuse $ do
            paragraph $ text $ "Verbindung:" ++ show c
            paragraph $ text $
              "nicht deklarierte Stelle im Nachbereich:" ++ show a
            pure ()
      pure ()

    case capacity n of
      Bounded f -> do
        let out = S.difference (M.keysSet f) (places n)
        unless (S.null out) $
          refuse $ do
            paragraph $ text "nicht definierte Stellen in Kapazitätsfunktion:"
            paragraph $ text $ show out
            pure ()
      _ -> pure ()

    let out = S.difference (M.keysSet $ unState $ start n) (places n)
    unless (S.null out) $
      refuse $ do
        paragraph $ text "nicht definierte Stellen der Startmarkierung:"
        paragraph $ text $ show out
        pure ()
    pure ()

guardBound :: (Ord a, OutputMonad m, Show a) => String -> a -> a -> LangM m
guardBound name actual bound =
  when (actual > bound) $
    refuse $ do
      paragraph $ text $ name ++ '(' : show actual ++ ")"
      paragraph $ text $ " ist größer als die Schranke " ++ show bound
      pure ()

{-|
Checks if the given predicate @p@ is satisfied after (partial) execution
of the given sequence.
-}
satisfiesAtAnyState
  :: (Ord s, Ord t, Show s, Show t)
  => (State s -> Bool)
  -> Net s t
  -> [t]
  -> Bool
satisfiesAtAnyState p n ts =
  (p (start n) ||) .
  fromLeft False $ foldM
    (\z t -> case runIdentity (evalLangM @Language @Maybe (execute n t z)) of
        Nothing -> Left False
        Just z' -> if p z' then Left True else return z'
    )
    (start n)
    ts
