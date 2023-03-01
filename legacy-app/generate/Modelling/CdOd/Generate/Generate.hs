{-# LANGUAGE TupleSections #-}
module Modelling.CdOd.Generate.Generate (
  generate,
  ) where


import Modelling.Auxiliary.Common       (oneOf)
import Modelling.CdOd.Generate.Edges (
  AssociationType (..),
  Connection (..),
  DiagramEdge,
  checkMultiEdge,
  hasAssociationAtOneSuperclass,
  nameEdges,
  )
import Modelling.CdOd.Types             (ClassConfig (..))

import Control.Arrow                    (second)
import Control.Monad.Random             (MonadRandom, getRandomR)
import Data.List                        (delete, nub)
import Data.Maybe                       (isNothing)
import Data.Tuple                       (swap)
import System.Random.Shuffle            (shuffleM)

generate
  :: MonadRandom m
  => Maybe Bool
  -> ClassConfig
  -> Int
  -> m ([String], [DiagramEdge])
generate withNonTrivialInheritance c searchSpace = do
  ncls <- oneOfFirst searchSpace $ toAvailable $ second Just $ classLimits c
  nins <- oneOfFirst searchSpace $ toAvailable $ inheritanceLimits c
  ncos <- oneOfFirst searchSpace $ toAvailable $ compositionLimits c
  nass <- oneOfFirst searchSpace $ toAvailable $ associationLimits c
  nags <- oneOfFirst searchSpace $ toAvailable $ aggregationLimits c
  if isPossible ncls nins ncos nass nags
    then do
      names <- shuffleM $ classNames ncls
      es <- generateEdges withNonTrivialInheritance names nins ncos nass nags
      cd <- maybe retry (return . (names,) . nameEdges) es
      if maybe False (&& not (uncurry hasAssociationAtOneSuperclass cd)) withNonTrivialInheritance
        then retry
        else return cd
    else retry
  where
    retry =
      if smallerC == c
      then error $ "it seems to be impossible to generate such a model"
           ++ "; check your configuration"
      else generate withNonTrivialInheritance smallerC searchSpace
    smallerC = shrink c searchSpace
    classNames x = map (:[]) $ take x ['A'..]
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
      | withNonTrivialInheritance == Just True
      , inh == 0 || com + ass + agg == 0                = False
      | withNonTrivialInheritance == Just False
      , (cla - inh) * (cla - inh - 1) `div` 2 < com + ass + agg = False
      | otherwise                                       = True

data GenerationConfig = GenerationConfig {
  available               :: [(String, String)],
  -- ^ still available edges
  withNoFurtherConnection :: Bool,
  -- ^ if superclasses shall have no further connections
  withConnection          :: Maybe (String, Int)
  -- ^ a class name which shall be superclass
  --   and how many steps before connection shall be created
  --   in order to choose any of composition / association / aggregation
  } deriving Show

next :: GenerationConfig -> GenerationConfig
next conf = conf {
  withConnection = second (\x -> x - 1) <$> withConnection conf
  }

deletePair :: (String, String) -> GenerationConfig -> GenerationConfig
deletePair t conf = conf { available = delete t $ available conf }

deleteAllForClass :: String -> GenerationConfig -> GenerationConfig
deleteAllForClass c conf = conf {
  available = [(x, y) | (x, y) <- available conf, c /= x, c /= y]
  }

generateEdges
  :: MonadRandom m
  => Maybe Bool
  -- ^ whether superclasses shall have non trivial inheritances
  -> [String]
  -> Int
  -> Int
  -> Int
  -> Int
  -> m (Maybe [DiagramEdge])
generateEdges wnti classs inh com ass agg = fmap (fmap snd) $ foldl
  (\es t -> es >>= maybe (return Nothing) (`generateEdge` t))
  (Just . (, []) <$> getConfig)
  $ replicate inh Nothing
    ++ replicate com (Just Composition')
    ++ replicate ass (Just Association')
    ++ replicate agg (Just Aggregation')
  where
    getConfig = do
      step <- oneOf $ nub [inh, inh + com, inh + com + ass]
      return $ GenerationConfig {
        available = [(x, y) | x <- classs, y <- classs, x > y],
        withNoFurtherConnection = maybe False not wnti,
        withConnection          = case wnti of
          Just True -> Just (head classs, step)
          _         -> Nothing
        }

generateEdge
      :: MonadRandom m
      => (GenerationConfig, [DiagramEdge])
      -> Maybe AssociationType
      -> m (Maybe (GenerationConfig, [DiagramEdge]))
generateEdge (conf, cs) mt
  | null cs, isNothing mt, Just (cl, _) <- withConnection conf = do
      t <- oneOf [ (x, y) | (x, y) <- available conf, x == cl || y == cl]
      let (s, e) = if fst t == cl then swap t else t
      finish (deletePair t conf, [(s, e, Inheritance')])
  | otherwise = do
      t <- oneOf $ case withConnection conf of
        Just (cl, 0) ->
          let required = [(x, y) | (x, y) <- available conf, x == cl || y == cl]
              hasRequiredAlready = null required
          in if hasRequiredAlready then available conf else required
        _            -> available conf
      b <- oneOf [True, False]
      let (s, e) = if b then t else swap t
      l <- generateLimits mt
      let c = (s, e, l)
      if checkMultiEdge $ c:cs
        then do
        let del = if isNothing mt && withNoFurtherConnection conf
              then deleteAllForClass e
              else deletePair t
        finish (del conf, c:cs)
        else generateEdge (conf, cs) mt
  where
    finish (gc, des)
      | null (available conf) = return Nothing
      | otherwise             = return $ Just (next gc, des)
    generateLimits :: MonadRandom m => Maybe AssociationType -> m Connection
    generateLimits Nothing            = return Inheritance'
    generateLimits (Just Composition') = do
      ll1 <- getRandomR (0, 1)
      l2  <- generateLimit
      return $ Assoc Composition' "" (ll1, Just 1) l2 False
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
    classLimits       = increase $ classLimits c,
    aggregationLimits = decrease $ aggregationLimits c,
    associationLimits = decrease $ associationLimits c,
    compositionLimits = decrease $ compositionLimits c,
    inheritanceLimits = decrease $ inheritanceLimits c
  }
  where
    increase (x, y)
      | x < y     = (x + 1, y)
      | otherwise = (x, y)
    decrease (x, Nothing) = (x, Just $ x + searchSpace)
    decrease (x, Just  y)
      | x < y     = (x, Just $ y - 1)
      | otherwise = (x, Just y)
