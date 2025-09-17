{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
-- | Phrasing relationships and changes in German
module Modelling.CdOd.Phrasing.German (
  phraseChange,
  phraseRelationship,
  trailingComma,
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
phraseChange = phraseChangeWith germanStrings

-- | German phrasing strings
germanStrings :: PhrasingStrings
germanStrings = PhrasingStrings
  { changeNothing = "verändere nichts"
  , addPrefix = "ergänze "
  , removePrefix = "entferne "
  , replacePrefix = "ersetze "
  , byInfix = " durch "
  , postProcess = \xs -> if ',' `elem` xs then xs ++ "," else xs
  , phraseRelationWith = phraseRelation
  }

trailingComma :: String -> String
trailingComma xs
      | ',' `elem` xs = xs ++ ","
      | otherwise     = xs

femaleArticle :: ArticleToUse -> String
femaleArticle = \case
  DefiniteArticle -> "die"
  IndefiniteArticle -> "eine"

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
    #{femaleArticle article} Vererbung,
    bei der #{linking invalidSubClass} von #{linking invalidSuperClass} erbt
    |]
    ++ phraseParticipations
      kind
      (defaultedInheritance invalidSubClass)
      (defaultedInheritance invalidSuperClass)
  (_, _, Right Inheritance {..}) -> [iii|
    #{femaleArticle article} Vererbung,
    bei der #{subClass} von #{superClass} erbt
    |]
  (_, ByName, Right Association {..}) -> "Assoziation " ++ associationName
  (_, ByName, Right Aggregation {..}) -> "Aggregation " ++ aggregationName
  (_, ByName, Right Composition {..}) -> "Komposition " ++ compositionName
  (kind, how, Right Association {..})
    | from <- defaultedAssociation associationFrom
    , to <- defaultedAssociation associationTo
    -> case (how, kind, linking associationFrom == linking associationTo) of
      (Lengthy, Participations, True) -> [iii|
        #{femaleArticle article} Selbst-Assoziation
        für #{linking associationFrom},
        bei der #{linking associationFrom}
        an einem Ende #{phraseLimitDefault $ defaultedLimits from}
        und am anderen Ende #{phraseLimitDefault $ defaultedLimits to}
        beteiligt ist
        |]
      (Lengthy, Denoted, True)
        | denoted <- uncurry denotions
          $ oneAndOther "einem Ende" "dem anderen Ende"
          $ sortLimits from to
        -> [iii|
          #{femaleArticle article} Selbst-Assoziation
          für #{linking associationFrom}
          |] ++ denoted
      (Lengthy, _, False) -> femaleArticle article ++ " Assoziation"
        ++ phraseParticipations kind from to
      (ByDirection, Participations, True) -> [iii|
        #{femaleArticle article} Selbst-Assoziation
        für #{linking associationFrom},
        bei der #{linking associationFrom}
        am Anfang #{phraseLimitDefault $ defaultedLimits from}
        und am Ende #{phraseLimitDefault $ defaultedLimits to} beteiligt ist
        |]
      (ByDirection, Denoted, True)
        | denoted <- uncurry denotions
          $ uncurry sortLimits
          $ oneAndOther "seinem Anfang" "seinem Pfeilende" (from, to)
        -> [iii|
          #{femaleArticle article} Selbst-Assoziation
          für #{linking associationFrom}
          |] ++ denoted
      (ByDirection, _, False) -> [iii|
        #{femaleArticle article} Assoziation von #{linking associationFrom}
        nach #{linking associationTo}
        |] ++ phraseParticipations kind from to
  (kind, _, Right Aggregation {..})
    | part <- defaultedAssociation aggregationPart
    , whole <- defaultedAssociation aggregationWhole
    ->
      if linking aggregationPart == linking aggregationWhole
      then [iii|
        #{femaleArticle article} Selbst-Aggregation
        #{selfParticipatesPartWhole kind part whole}
        |]
      else [iii|
        #{femaleArticle article} Beziehung, die #{linking aggregationWhole}
        eine Aggregation aus #{linking aggregationPart}s macht
        |] ++ phraseParticipations kind whole part
  (kind, _, Right Composition {..})
    | part <- defaultedAssociation compositionPart
    , whole <- defaultedCompositionWhole compositionWhole
    ->
      if linking compositionPart == linking compositionWhole
      then [iii|
        #{femaleArticle article} Selbst-Komposition
        #{selfParticipatesPartWhole kind part whole}
        |]
      else [iii|
        #{femaleArticle article} Beziehung, die #{linking compositionWhole}
        eine Komposition aus #{linking compositionPart}s macht
        |] ++ phraseParticipations kind whole part
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
  für #{defaultedLinking part},
  #{which}
  |]
  where
    which = uncurry denotions $ sortLimits
      part {defaultedLinking = "dem Teil-Ende"}
      whole {defaultedLinking = "dem Ganzen-Ende"}
selfParticipatesPartWhole Participations part whole = [iii|
  für #{defaultedLinking part},
  wobei es #{phraseLimitDefault $ defaultedLimits part} als Teil
  und #{phraseLimitDefault $ defaultedLimits whole} als Ganzes beteiligt ist
  |]

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
  (Nothing, Nothing) -> [iii|, bei der keine Multiplizitäten angegeben sind|]
  (Nothing, Just otherRange) -> [iii|
     , bei der keine Multiplizität neben #{defaultedLinking one}
     und #{otherRange} neben #{defaultedLinking other} angegeben ist
     |]
  (Just oneRange, Nothing) -> [iii|
     , bei der keine Multiplizität neben #{defaultedLinking other}
     und #{oneRange} neben #{defaultedLinking one} angegeben ist
     |]
  (Just oneRange, Just otherRange) -> [iii|
     , bei der die Multiplizität
     #{oneRange} neben #{defaultedLinking one}
     und #{otherRange} neben #{defaultedLinking other} angegeben ist
     |]

participations
  :: DefaultedLimitedLinking
  -> DefaultedLimitedLinking
  -> String
participations one other = [iii|
  , wobei #{defaultedLinking one} #{phraseLimitDefault $ defaultedLimits one}
  und #{defaultedLinking other} #{phraseLimitDefault $ defaultedLimits other}
  beteiligt ist
  |]

phraseLimitDefault :: Maybe (Int, Maybe Int) -> String
phraseLimitDefault = maybe "mit der Standardmultiplizität" phraseLimit

phraseLimit :: (Int, Maybe Int) -> String
phraseLimit (0, Just 0)  = "gar nicht"
phraseLimit (1, Just 1)  = "genau einmal"
phraseLimit (2, Just 2)  = "genau zweimal"
phraseLimit (-1, Just n) = "*.." ++ show n ++ "-mal"
phraseLimit (m, Nothing) = show m ++ "..*-mal"
phraseLimit (m, Just n)  = show m ++ ".." ++ show n ++ "-mal"
