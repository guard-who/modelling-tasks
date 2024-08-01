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
import Modelling.CdOd.Types (
  AnyRelationship,
  InvalidRelationship (..),
  LimitedLinking (..),
  NonInheritancePhrasing (..),
  Relationship (..),
  toPhrasing,
  )

import Control.OutputCapable.Blocks     (ArticleToUse (..))
import Data.String.Interpolate          (iii)

phraseChange
  :: ArticleToUse
  -> Bool
  -> Bool
  -> Change (AnyRelationship String String)
  -> String
phraseChange article byName withDir c = case (add c, remove c) of
  (Nothing, Nothing) -> "verändere nichts"
  (Just a,  Nothing) -> "ergänze "
    ++ trailingComma (phrasingNew a)
  (Nothing, Just e)  -> "entferne " ++ phrasingOld e
  (Just a,  Just e)  ->
    "ersetze " ++ trailingComma (phrasingOld e)
    ++ " durch " ++ phrasingNew a
  where
    phrasingOld = phraseRelation article $ toPhrasing byName withDir
    phrasingNew = phraseRelation IndefiniteArticle $ toPhrasing False withDir

trailingComma :: String -> String
trailingComma xs
      | ',' `elem` xs = xs ++ ","
      | otherwise     = xs

femaleArticle :: ArticleToUse -> String
femaleArticle = \case
  DefiniteArticle -> "die"
  IndefiniteArticle -> "eine"

phraseRelationship
  :: ArticleToUse
  -> Bool
  -> Bool
  -> AnyRelationship String String
  -> String
phraseRelationship article byName withDir = phraseRelation article phrasing
  where
    phrasing = toPhrasing byName withDir

phraseRelation
  :: ArticleToUse
  -> NonInheritancePhrasing
  -> AnyRelationship String String
  -> String
phraseRelation article = curry $ \case
  (_, Left InvalidInheritance {..}) -> [iii|
    #{femaleArticle article} Vererbung,
    bei der #{linking invalidSubClass} von #{linking invalidSuperClass} erbt
    |] ++ participations invalidSubClass invalidSuperClass
  (_, Right Inheritance {..}) -> [iii|
    #{femaleArticle article} Vererbung,
    bei der #{subClass} von #{superClass} erbt
    |]
  (ByName, Right Association {..}) -> "Assoziation " ++ associationName
  (ByName, Right Aggregation {..}) -> "Aggregation " ++ aggregationName
  (ByName, Right Composition {..}) -> "Komposition " ++ compositionName
  (Lengthy, Right Association {..})
    | linking associationFrom == linking associationTo -> [iii|
    #{femaleArticle article} Selbst-Assoziation für #{linking associationFrom},
    bei der #{linking associationFrom}
    an einem Ende #{phraseLimit $ limits associationFrom}
    und am anderen Ende #{phraseLimit $ limits associationTo} beteiligt ist
    |]
    | otherwise -> femaleArticle article ++ " Assoziation"
      ++ participations associationFrom associationTo
  (ByDirection, Right Association {..})
    | linking associationFrom == linking associationTo -> [iii|
    #{femaleArticle article} Selbst-Assoziation für #{linking associationFrom},
    bei der {linking associationFrom}
    am Anfang #{phraseLimit $ limits associationFrom}
    und am Ende #{phraseLimit $ limits associationTo} beteiligt ist
    |]
    | otherwise -> [iii|
    #{femaleArticle article} Assoziation von #{linking associationFrom}
    nach #{linking associationTo}
    |] ++ participations associationFrom associationTo
  (_, Right Aggregation {..})
    | linking aggregationPart == linking aggregationWhole -> [iii|
    #{femaleArticle article} Selbst-Aggregation
    #{selfParticipatesPartWhole aggregationPart aggregationWhole}
    |]
    | otherwise -> [iii|
    #{femaleArticle article} Beziehung, die #{linking aggregationWhole}
    eine Aggregation aus #{linking aggregationPart}s macht
    |] ++ participations aggregationWhole aggregationPart
  (_, Right Composition {..})
    | linking compositionPart == linking compositionWhole -> [iii|
    #{femaleArticle article} Selbst-Komposition
    #{selfParticipatesPartWhole compositionPart compositionWhole}
    |]
    | otherwise -> [iii|
    #{femaleArticle article} Beziehung, die #{linking compositionWhole}
    eine Komposition aus #{linking compositionPart}s macht
    |] ++ participations compositionWhole compositionPart

selfParticipatesPartWhole
  :: LimitedLinking String
  -> LimitedLinking String
  -> String
selfParticipatesPartWhole part whole = [iii|
  für #{linking part}, wobei #{linking part} #{phraseLimit $ limits part}
  als Teil and #{phraseLimit $ limits whole} als Ganzes beteiligt ist
  |]

participations
  :: LimitedLinking String
  -> LimitedLinking String
  -> String
participations from to = [iii|
  , wobei #{linking from} #{phraseLimit $ limits from}
  und #{linking to} #{phraseLimit $ limits to} beteiligt ist
  |]

phraseLimit :: (Int, Maybe Int) -> String
phraseLimit (0, Just 0)  = "gar nicht"
phraseLimit (1, Just 1)  = "genau einmal"
phraseLimit (2, Just 2)  = "genau zweimal"
phraseLimit (-1, Just n) = "*.." ++ show n ++ "-mal"
phraseLimit (m, Nothing) = show m ++ "..*-mal"
phraseLimit (m, Just n)  = show m ++ ".." ++ show n ++ "-mal"
