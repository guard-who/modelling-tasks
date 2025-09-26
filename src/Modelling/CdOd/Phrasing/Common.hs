-- | Common phrasing logic for CdOd tasks
module Modelling.CdOd.Phrasing.Common (
  PhrasingStrings (..),
  phraseChangeWith
) where

import Modelling.Types (
  Change (..),
  )
import Modelling.CdOd.Types (
  AnyRelationship,
  NonInheritancePhrasing (..),
  OmittedDefaultMultiplicities (..),
  PhrasingKind (..),
  toPhrasing,
  )

import Control.OutputCapable.Blocks     (ArticleToUse (..))

-- | Language-specific strings for phrasing
data PhrasingStrings = PhrasingStrings
  { changeNothing :: String
  , addPrefix :: String
  , removePrefix :: String
  , replacePrefix :: String
  , byInfix :: String
  , postProcess :: String -> String  -- ^ Post-processing function for things like trailing commas
  , phraseRelationWith :: OmittedDefaultMultiplicities
                     -> ArticleToUse
                     -> PhrasingKind
                     -> NonInheritancePhrasing
                     -> AnyRelationship String String
                     -> String
  }



-- | Common change phrasing logic parameterized by language strings
phraseChangeWith
  :: PhrasingStrings
  -> OmittedDefaultMultiplicities
  -> ArticleToUse
  -> Bool
  -> Bool
  -> Change (AnyRelationship String String)
  -> String
phraseChangeWith strings defaultMultiplicities article byName withDir c =
  case (add c, remove c) of
  (Nothing, Nothing) -> changeNothing strings
  (Just e,  Nothing) -> addPrefix strings ++ postProcess strings (phrasingNew e)
  (Nothing, Just e ) -> removePrefix strings ++ phrasingOld e
  (Just e1, Just e2) ->
    replacePrefix strings ++ postProcess strings (phrasingOld e2)
    ++ byInfix strings ++ phrasingNew e1
  where
    phrasingOld = phraseRelationWith strings
      defaultMultiplicities
      article
      Denoted
      $ toPhrasing byName withDir
    phrasingNew = phraseRelationWith strings
      defaultMultiplicities
      IndefiniteArticle
      Participations
      $ toPhrasing False withDir
