module Alloy.CdOd.Generate where

import Alloy.CdOd.Edges
import Alloy.CdOd.Types

import Control.Arrow                    (second)
import Control.Monad.Random             (MonadRandom, getRandomR)

generate :: MonadRandom m => ClassConfig -> Int -> m ([String], [DiagramEdge])
generate c searchSpace = do
  ncls <- oneOfFirst searchSpace $ toAvailable $ second Just $ classes c
  nins <- oneOfFirst searchSpace $ toAvailable $ inheritances c
  ncos <- oneOfFirst searchSpace $ toAvailable $ compositions c
  nass <- oneOfFirst searchSpace $ toAvailable $ associations c
  nags <- oneOfFirst searchSpace $ toAvailable $ aggregations c
  if isPossible ncls nins ncos nass nags
    then do
      let names = classNames ncls
      es <- generateEdges names nins ncos nass nags
      return (names, nameEdges es)
    else if smallerC == c
         then error "it seems to be impossible to generate such a model; check your configuration"
         else generate smallerC searchSpace
  where
    smallerC = shrink c searchSpace
    classNames x = (:[]) <$> take x ['A'..]
    toAvailable :: (Int, Maybe Int) -> [Int]
    toAvailable (x, Nothing) = [x ..]
    toAvailable (x, Just  y) = [x .. y]
    oneOfFirst :: MonadRandom m => Int -> [a] -> m a
    oneOfFirst s xs = do
      x <- getRandomR (0, maxLength s xs - 1)
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

nameEdges :: [DiagramEdge] -> [DiagramEdge]
nameEdges es =
     [e | e@(_, _, Inheritance) <- es]
  ++ [(s, e, Assoc k (n:[]) m1 m2 b)
     | (n, (s, e, Assoc k _ m1 m2 b)) <- zip ['z', 'y' ..] es]

generateEdges :: MonadRandom m => [String] -> Int -> Int -> Int -> Int -> m [DiagramEdge]
generateEdges classs inh com ass agg =
  foldl (\es t -> es >>= flip generateEdge t) (return []) $
    replicate inh Nothing
    ++ replicate com (Just Composition)
    ++ replicate ass (Just Association)
    ++ replicate agg (Just Aggregation)
  where
    oneOf :: MonadRandom m => [a] -> m a
    oneOf xs = do
      x <- getRandomR (0, length xs - 1)
      return $ xs !! x
    generateEdge :: MonadRandom m => [DiagramEdge] -> Maybe AssociationType -> m [DiagramEdge]
    generateEdge cs mt = do
      s <- oneOf classs
      e <- oneOf $ filter (s /=) classs
      l <- generateLimits mt
      let c = (s, e, l)
      if checkMultiEdge $ c:cs
        then return $ c:cs
        else generateEdge cs mt
    generateLimits :: MonadRandom m => (Maybe AssociationType) -> m Connection
    generateLimits Nothing            = return Inheritance
    generateLimits (Just Composition) = do
      ll1 <- getRandomR (0, 1)
      l2  <- generateLimit
      return $ Assoc Composition "" (ll1, Just 1) l2 False
    generateLimits (Just t          ) = do
      l1 <- generateLimit
      l2 <- generateLimit
      return $ Assoc t "" l1 l2 False
    generateLimit :: MonadRandom m => m (Int, Maybe Int)
    generateLimit = do
      l <- getRandomR (0, 2)
      h <- oneOf $ drop (l - 1) [Just 1, Just 2, Nothing]
      return (l, h)

shrink :: ClassConfig -> Int -> ClassConfig
shrink c searchSpace = c {
    classes      = increase $ classes c,
    aggregations = decrease $ aggregations c,
    associations = decrease $ associations c,
    compositions = decrease $ compositions c,
    inheritances = decrease $ inheritances c
  }
  where
    increase (x, y)
      | x < y     = (x + 1, y)
      | otherwise = (x, y)
    decrease (x, Nothing) = (x, Just $ x + searchSpace)
    decrease (x, Just  y)
      | x < y     = (x, Just $ y - 1)
      | otherwise = (x, Just y)
