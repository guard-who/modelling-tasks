{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Types
import Output

import Data.List
import Data.Maybe
import Data.GraphViz

import System.Random

newtype Arrow = Arrow (String, String, Connection)
type DiagramEdge = (String, String, Connection)

connections :: Syntax -> [DiagramEdge]
connections (is, as) =
  [(s, e, Inheritance) | (s, Just e) <- is]
  ++ [(s, e, Assoc t m1 m2 False) | (t, _, m1, s, e, m2) <- as]

selfEdges :: [DiagramEdge] -> [DiagramEdge]
selfEdges es = [x | x@(s, e, _) <- es, e == s]

doubleConnections :: [DiagramEdge] -> [(DiagramEdge, DiagramEdge)]
doubleConnections es =
  [(x, y) | x@(s1, e1, _) <- es, s1 /= e1
          , y@(s2, e2, _) <- filterFirst x es, s2 /= e2
          , s1 == s2 && e1 == e2 || s1 == e2 && s2 == e1]

multipleInheritances :: [DiagramEdge] -> [(DiagramEdge, DiagramEdge)]
multipleInheritances es =
  [(x, y) | x@(s1, e1, Inheritance) <- es, s1 /= e1
          , y@(s2, e2, Inheritance) <- filterFirst x es, s2 /= e2
          , s1 == s2 && e1 /= e2]

inheritanceCycles :: [DiagramEdge] -> [[DiagramEdge]]
inheritanceCycles = cycles isInheritance
  where
    isInheritance Inheritance = True
    isInheritance _           = False

compositionCycles :: [DiagramEdge] -> [[DiagramEdge]]
compositionCycles = cycles isComposition
  where
    isComposition (Assoc Composition _ _ _) = True
    isComposition _                         = False

wrongLimits :: [DiagramEdge] -> [DiagramEdge]
wrongLimits es =
  [c | c@(_, _, Assoc t s@(sl, sh) e _) <- es
     , isComposition t && (sh /= Just 1 || sl < 0 || sl > 1)
       || not (inLimit s)
       || not (inLimit e)]
  where
    isComposition Composition = True
    isComposition _           = False
    inLimit (l, Nothing)
      | 0 <= l && l <= 2 = True
      | otherwise        = False
    inLimit (l, Just h)
      | l == 0 && l <  h && h <= 2 = True
      | l >  0 && l <= h && h <= 2 = True
      | otherwise                  = False

cycles :: (Connection -> Bool) -> [DiagramEdge] -> [[DiagramEdge]]
cycles connectionFilter es =
  [c:cs |   (s , e, cs) <- getPaths connectionFilter es
        , c@(s', e', _) <- edges, s' == e, s == e']
  where
    edges               = filter (connectionFilter . connection) es
    connection  (_,_,c) = c

getPaths :: (Connection -> Bool) -> [DiagramEdge] -> [(String, String, [DiagramEdge])]
getPaths connectionFilter es =
  [path | c@(s, e, _) <- edges, s /= e, path <- getPath s e [c] edges]
  where
    edges               = filter (connectionFilter . connection) es
    start       (s,_,_) = s
    end         (_,e,_) = e
    connection  (_,_,c) = c
    getPath :: String
            -> String
            -> [DiagramEdge]  -- edges within the current path
            -> [DiagramEdge]  -- still available edges
            -> [(String, String, [DiagramEdge])]
    getPath s e ps es' =
      (s, e, ps) :
      -- prevent cyclic paths
      let es'' = filter ((s /=) . start) $ filter ((s /=) . end) es'
      in [path | p@(s', e', _) <- es', s' /= e', s' /= e, s == e'
               , path <- getPath s' e (p:ps) es'']

filterFirst :: Eq a => a -> [a] -> [a]
filterFirst _ []     = []
filterFirst x (y:ys) = if x == y then ys else y : filterFirst x ys

data Config = Config {
    classes      :: (Maybe Int, Maybe Int),
    aggregations :: (Maybe Int, Maybe Int),
    associations :: (Maybe Int, Maybe Int),
    compositions :: (Maybe Int, Maybe Int),
    inheritances :: (Maybe Int, Maybe Int)
  } deriving (Eq)

main :: IO ()
main =
  generate (Config { classes = (Just 4, Just 4), aggregations = (Nothing, Nothing), associations = (Nothing, Nothing), compositions = (Nothing, Nothing), inheritances = (Nothing, Nothing) })
  >>= drawCdFromSyntax "output" Pdf

searchSpace = 10

generate :: Config -> IO Syntax
generate c = do
  ncls <- oneOfFirst searchSpace $ toAvailable $ classes c
  nins <- oneOfFirst searchSpace $ toAvailable $ inheritances c
  ncos <- oneOfFirst searchSpace $ toAvailable $ compositions c
  nass <- oneOfFirst searchSpace $ toAvailable $ associations c
  nags <- oneOfFirst searchSpace $ toAvailable $ aggregations c
  if isPossible ncls nins ncos nass nags
    then do
      es <- generateEdges (classs ncls) nins ncos nass nags
      let isInheritance (_, _, Inheritance) = True
          isInheritance (_, _, _          ) = False
          (ihs, ass) = partition isInheritance es
          classes' = (\x -> (x, foldl (\p (s, e, Inheritance) -> if s == x then Just e else p) Nothing ihs)) <$> (classs ncls)
          assocs   = [(t, s ++ "and" ++ e, m1, s, e, m2) | (s, e, Assoc t m1 m2 False) <- ass]
      return (classes', assocs)
    else if minimalC == c
         then error "it seems to be impossible to generate such a model; check your configuration"
         else generate minimalC
  where
    minimalC = minimise c
    classs cla = (:[]) <$> take cla ['A'..]
    toAvailable :: (Maybe Int, Maybe Int) -> [Int]
    toAvailable (mx, Nothing) = [fromMaybe 0 mx..]
    toAvailable (mx, Just  y) = [fromMaybe 0 mx.. y]
    oneOfFirst :: Int -> [a] -> IO a
    oneOfFirst s xs = do
      x <- randomRIO (0, maxLength s xs - 1)
      return $ xs !! x
    -- a size function that terminates on infinite lists
    -- returns the size of the list if it is smaller than @limit@ or limit
    maxLength :: Int -> [a] -> Int
    maxLength 0 _      = 0
    maxLength _ []     = 0
    maxLength s (_:xs) = 1 + maxLength (s - 1) xs
    -- checks for cases which surely lead to unsolvable problems
    isPossible cla inh com ass agg
      | inh >= cla                                      = False
      | cla * (cla - 1) `div` 2 < inh + com + ass + agg = False
      | otherwise                                       = True

generateEdges classes inh com ass agg = do
  xs <- foldl (\es t -> es >>= flip generateEdge t) (return []) $
    replicate inh Nothing
    ++ replicate com (Just Composition)
    ++ replicate ass (Just Association)
    ++ replicate agg (Just Aggregation)
  return xs
  where
    oneOf :: [a] -> IO a
    oneOf xs = do
      x <- randomRIO (0, length xs - 1)
      return $ xs !! x
    generateEdge :: [DiagramEdge] -> Maybe AssociationType -> IO [DiagramEdge]
    generateEdge cs mt = do
      s <- oneOf classes
      e <- oneOf $ filter (s /=) classes
      l <- generateLimits mt
      let c = (s, e, l)
      if checkConstraints $ c:cs
        then return $ c:cs
        else generateEdge cs mt
    generateLimits :: (Maybe AssociationType) -> IO Connection
    generateLimits Nothing            = return Inheritance
    generateLimits (Just Composition) = do
      ll1 <- randomRIO (0, 1)
      l2  <- generateLimit
      return $ Assoc Composition (ll1, Just 1) l2 False
    generateLimits (Just t          ) = do
      l1 <- generateLimit
      l2 <- generateLimit
      return $ Assoc t l1 l2 False
    generateLimit :: IO (Int, Maybe Int)
    generateLimit = do
      l <- randomRIO (0, 2)
      h <- oneOf $ drop (l - 1) [Just 1, Just 2, Nothing]
      return (l, h)
    checkConstraints cs =
      null (doubleConnections cs)
      -- && null (selfEdges cs)
      && null (multipleInheritances cs)
      && null (inheritanceCycles cs)
      && null (compositionCycles cs)
      -- && null (wrongLimits cs)

minimise :: Config -> Config
minimise c = c {
    classes      = increase $ classes c,
    aggregations = decrease $ aggregations c,
    associations = decrease $ associations c,
    compositions = decrease $ compositions c,
    inheritances = decrease $ inheritances c
  }
  where
    increase (Nothing, my     ) = (Just 1, my)
    increase (Just  x, Nothing) = (Just $ x + 1, Nothing)
    increase (Just  x, Just  y)
      | x < y     = (Just $ x + 1, Just y)
      | otherwise = (Just x, Just y)
    decrease (Nothing, my     ) = (Just 0, my)
    decrease (Just  x, Nothing) = (Just x, Just $ x + searchSpace)
    decrease (Just  x, Just  y)
      | x < y     = (Just x, Just $ y - 1)
      | otherwise = (Just x, Just y)
