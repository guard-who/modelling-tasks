{-# LANGUAGE LambdaCase #-}
-- | provide phrasing functions for different languages
module Modelling.CdOd.Phrasing (
  phraseChange,
  phraseRelationship,
  trailingCommaGerman,
  ) where

import qualified Modelling.CdOd.Phrasing.German    as German
import qualified Modelling.CdOd.Phrasing.English   as English

import Control.Monad.Output (
  Language (English, German),
  )

import Modelling.Types (
  Change,
  )
import Modelling.CdOd.Types (
  ArticleToUse,
  Relationship,
  )

phraseChange :: Language
  -> ArticleToUse
  -> Bool
  -> Bool
  -> Change (Relationship String String)
  -> String
phraseChange = \case
  English -> English.phraseChange
  German -> German.phraseChange

phraseRelationship
  :: Language
  -> ArticleToUse
  -> Bool
  -> Bool
  -> Relationship String String -> String
phraseRelationship = \case
  English -> English.phraseRelationship
  German -> German.phraseRelationship

trailingCommaGerman :: String -> String
trailingCommaGerman = German.trailingComma
