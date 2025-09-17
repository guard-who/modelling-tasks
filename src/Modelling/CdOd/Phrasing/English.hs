{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
-- | Phrasing relationships and changes in English
module Modelling.CdOd.Phrasing.English (
  phraseChange,
  phraseRelationship,
  ) where

import Modelling.Types (
  Change (..),
  )
import Modelling.CdOd.Auxiliary.Util    (oneAndOther)
import Modelling.CdOd.Phrasing.Common   (phraseChangeWith, PhrasingStrings (..))
import Modelling.CdOd.Types (
  AnyRelationship,
  DefaultedLimitedLinking (..),
  InvalidRelationship (..),
  LimitedLinking (..),
  NonInheritancePhrasing (..),
  OmittedDefaultMultiplicities (..),
  PhrasingKind (..),
  Relationship (..),
  defaultedLimitedLinking,
  sortLimits,
  toPhrasing,
  )

import Control.OutputCapable.Blocks     (ArticleToUse (..))
import Data.String.Interpolate          (iii)
import Data.Tuple.Extra                 (curry3)

phraseChange
  :: OmittedDefaultMultiplicities
  -> ArticleToUse
  -> Bool
  -> Bool
  -> Change (AnyRelationship String String)
  -> String
phraseChange = phraseChangeWith englishStrings

-- | English phrasing strings
englishStrings :: PhrasingStrings
englishStrings = PhrasingStrings
  { changeNothing = "change nothing"
  , addPrefix = "add "
  , removePrefix = "remove "
  , replacePrefix = "replace "
  , byInfix = " by "
  , postProcess = id
  , phraseRelationWith = phraseRelation
  }

consonantArticle :: ArticleToUse -> String
consonantArticle = \case
  DefiniteArticle -> "the"
  IndefiniteArticle -> "a"

vowelArticle :: ArticleToUse -> String
vowelArticle = \case
  DefiniteArticle -> "the"
  IndefiniteArticle -> "an"

phraseRelationship
  :: OmittedDefaultMultiplicities
  -> ArticleToUse
  -> PhrasingKind
  -> Bool
  -> Bool
  -> AnyRelationship String String
  -> String
phraseRelationship defaultMultiplicities article kind byName withDir =
  phraseRelation defaultMultiplicities article kind phrasing
  where
    phrasing = toPhrasing byName withDir

phraseRelation
  :: OmittedDefaultMultiplicities
  -> ArticleToUse
  -> PhrasingKind
  -> NonInheritancePhrasing
  -> AnyRelationship String String
  -> String
phraseRelation OmittedDefaultMultiplicities {..} article = curry3 $ \case
  (kind,_, Left InvalidInheritance {..}) -> [iii|
    #{vowelArticle article} inheritance
    where #{linking invalidSubClass} inherits from #{linking invalidSuperClass}
    and #{phraseParticipations
      kind
      (defaultedInheritance invalidSubClass)
      (defaultedInheritance invalidSuperClass)
      }
    |]
  (_, _, Right Inheritance {..}) -> [iii|
    #{vowelArticle article} inheritance
    where #{subClass} inherits from #{superClass}
    |]
  (_, ByName, Right Association {..}) -> "association " ++ associationName
  (_, ByName, Right Aggregation {..}) -> "aggregation " ++ aggregationName
  (_, ByName, Right Composition {..}) -> "composition " ++ compositionName
  (kind, how, Right Association {..})
    | from <- defaultedAssociation associationFrom
    , to <- defaultedAssociation associationTo
    -> case (how, kind, linking associationFrom == linking associationTo) of
      (Lengthy, Participations, True)
        | fromIt <- from {defaultedLinking = "it"}
        -> [iii|
          #{consonantArticle article} self-association
          for #{linking associationFrom}
          where #{participates fromIt} at one end
          and #{phraseLimitDefault $ defaultedLimits to} at the other end
          |]
      (Lengthy, Denoted, True)
        | denoted <- uncurry denotions
          $ oneAndOther "one end" "the other end"
          $ sortLimits from to
        -> [iii|
          #{consonantArticle article} self-association
          for #{linking associationFrom} #{denoted}
          |]
      (Lengthy, _, False) -> [iii|
        #{vowelArticle article} association
        #{phraseParticipations kind from to}
        |]
      (ByDirection, Participations, True)
        | fromIt <- from {defaultedLinking = "it"}
        -> [iii|
          #{consonantArticle article} self-association
          for #{linking associationFrom}
          where #{participates fromIt} at its beginning
          and #{phraseLimitDefault $ defaultedLimits to} at its arrow end
          |]
      (ByDirection, Denoted, True)
        | denoted <- uncurry denotions
          $ uncurry sortLimits
          $ oneAndOther "its beginning" "its arrow end" (from, to)
        -> [iii|
          #{consonantArticle article} self-association
          for #{linking associationFrom} #{denoted}
          |]
      (ByDirection, _, False) -> [iii|
        #{vowelArticle article} association from #{linking associationFrom}
        to #{linking associationTo}
        #{phraseParticipations kind from to}
        |]
  (kind, _, Right Aggregation {..})
    | part <- defaultedAssociation aggregationPart
    , whole <- defaultedAssociation aggregationWhole
    ->
      if linking aggregationPart == linking aggregationWhole
      then [iii|
        #{consonantArticle article} self-aggregation
        #{selfParticipatesPartWhole kind part whole}
        |]
      else [iii|
        #{consonantArticle article} relationship
        that makes #{linking aggregationWhole}
        an aggregation of #{linking aggregationPart}s
        #{phraseParticipations kind whole part}
        |]
  (kind, _, Right Composition {..})
    | part <- defaultedAssociation compositionPart
    , whole <- defaultedCompositionWhole compositionWhole
    ->
      if linking compositionPart == linking compositionWhole
      then [iii|
        #{consonantArticle article} self-composition
        #{selfParticipatesPartWhole kind part whole}
        |]
      else [iii|
        #{consonantArticle article} relationship
        that makes #{linking compositionWhole}
        a composition of #{linking compositionPart}s
        #{phraseParticipations kind whole part}
        |]
  where
    defaultedCompositionWhole =
      defaultedLimitedLinking compositionWholeOmittedDefaultMultiplicity
    defaultedAssociation =
      defaultedLimitedLinking associationOmittedDefaultMultiplicity
    defaultedInheritance = defaultedLimitedLinking Nothing

selfParticipatesPartWhole
  :: PhrasingKind
  -> DefaultedLimitedLinking
  -> DefaultedLimitedLinking
  -> String
selfParticipatesPartWhole Denoted part whole = [iii|
  for #{defaultedLinking part}
  #{which}
  |]
  where
    which = uncurry denotions $ sortLimits
      part {defaultedLinking = "its part end"}
      whole {defaultedLinking = "its whole end"}
selfParticipatesPartWhole Participations part whole = [iii|
  for #{defaultedLinking part}
  where #{participates partIt} as part
  and #{phraseLimitDefault $ defaultedLimits whole} as whole
  |]
  where
    partIt = part {defaultedLinking = "it"}

phraseParticipations
  :: PhrasingKind
  -> DefaultedLimitedLinking
  -> DefaultedLimitedLinking
  -> String
phraseParticipations = \case
  Denoted -> denotions
  Participations -> participations

denotions
  :: DefaultedLimitedLinking
  -> DefaultedLimitedLinking
  -> String
denotions one other = case (defaultedRange one, defaultedRange other) of
  (Nothing, Nothing) -> [iii|which has not denoted multiplicities at all|]
  (Nothing, Just otherRange) -> [iii|
     which has no multiplicity denoted near #{defaultedLinking one}
     and #{otherRange} near #{defaultedLinking other}
     |]
  (Just oneRange, Nothing) -> [iii|
     which has no multiplicity denoted near #{defaultedLinking other}
     and #{oneRange} near #{defaultedLinking one}
     |]
  (Just oneRange, Just otherRange) -> [iii|
     which has denoted the multiplicity
     #{oneRange} near #{defaultedLinking one}
     and #{otherRange} near #{defaultedLinking other}
     |]

participations
  :: DefaultedLimitedLinking
  -> DefaultedLimitedLinking
  -> String
participations one other = [iii|
  where #{participates one}
  and #{participates other}
  |]

participates :: DefaultedLimitedLinking -> String
participates DefaultedLimitedLinking {..}
  = defaultedLinking ++ " participates "
  ++ phraseLimitDefault defaultedLimits

phraseLimitDefault :: Maybe (Int, Maybe Int) -> String
phraseLimitDefault = maybe "with the default multiplicity" phraseLimit

phraseLimit :: (Int, Maybe Int) -> String
phraseLimit (0, Just 0)  = "not at all"
phraseLimit (1, Just 1)  = "exactly once"
phraseLimit (2, Just 2)  = "exactly twice"
phraseLimit (-1, Just n) = "*.." ++ show n ++ " times"
phraseLimit (m, Nothing) = show m ++ "..* times"
phraseLimit (m, Just n)  = show m ++ ".." ++ show n ++ " times"
