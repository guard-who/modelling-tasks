{-# LANGUAGE LambdaCase #-}
-- | provide phrasing functions for different languages
module Modelling.CdOd.Phrasing (
  phraseChange,
  phraseRelationship,
  trailingCommaGerman,
  ) where

import qualified Modelling.CdOd.Phrasing.German    as German
import qualified Modelling.CdOd.Phrasing.English   as English

import Control.OutputCapable.Blocks (
  ArticleToUse,
  Language (English, German),
  )

import Modelling.Types (
  Change,
  )
import Modelling.CdOd.Types (
  AnyRelationship,
  )

phraseChange :: Language
  -> ArticleToUse
  -> Bool
  -> Bool
  -> Change (AnyRelationship String String)
  -> String
phraseChange = \case
  English -> English.phraseChange
  German -> German.phraseChange

phraseRelationship
  :: Language
  -> ArticleToUse
  -> Bool
  -> Bool
  -> AnyRelationship String String -> String
phraseRelationship = \case
  English -> English.phraseRelationship
  German -> German.phraseRelationship

trailingCommaGerman :: String -> String
trailingCommaGerman = German.trailingComma
