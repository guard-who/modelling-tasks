module Mutation (
  -- * Types
  Mutation (..), Targets, Target (..), Alteration (..),
  -- * Perform mutation operations
  getAllMutationResults, getMutationResults
  ) where

import Types
import Edges

import Data.Function (on)
import Data.List     ((\\))
import Data.Maybe    (maybeToList)
import Data.Set      (Set, delete, intersection, member, singleton, toList)

getAllMutationResults :: [String] -> [DiagramEdge] -> [[DiagramEdge]]
getAllMutationResults vs es =
  let singleTargets   = [singleton t | t <- [minBound :: Target ..]]
      mutations       = [m | t <- singleTargets, m <- [Add t, Remove t]]
        ++ [Transform t1 t2 | t1 <- singleTargets, t2 <- singleTargets]
        ++ [m | a <- [minBound :: Alteration ..], t <- singleTargets
              , m <- [LimitRange a t, LimitShift a t]]
  in concatMap (getMutationResults vs es) mutations

data Mutation =
    Add       Targets
  | Remove    Targets
  | Transform Targets Targets
  | LimitRange Alteration Targets
  | LimitShift Alteration Targets

data Target = TAssociation | TAggregation | TComposition | TInheritance
  deriving (Bounded, Enum, Eq, Ord)

type Targets = Set Target

data Alteration = Increase | Decrease
  deriving (Bounded, Enum, Eq, Ord)

getMutationResults :: [String] -> [DiagramEdge] -> Mutation -> [[DiagramEdge]]
getMutationResults vs es m = case m of
  Add                 t -> allAdds t vs es
  Remove              t -> allRemoves t es
  Transform         s t -> transform s t es
  LimitRange Increase t -> allIncreaseLimitsRange t es
  LimitRange Decrease t -> allDecreaseLimitsRange t es
  LimitShift Increase t -> allShiftDownLimitsRange t es
  LimitShift Decrease t -> allShiftUpLimitsRange t es

transform :: Targets -> Targets -> [DiagramEdge] -> [[DiagramEdge]]
transform s t es =
  (concatMap (flip allFlipTransformations es) $ toList (s `intersection` t))
  ++ addWhen (TInheritance `member` s) (allFromInheritances ti es)
  ++ addWhen (TInheritance `member` t) (allToInheritances si es)
  ++ addWhen (TComposition `member` si) (allFromCompositions tc es)
  ++ addWhen (TComposition `member` ti) (allToCompositions sc es)
  ++ concat [allOtherTransformations sa ta es
            | sa <- toList sc, ta <- toList tc, sa /= ta]
  where
    addWhen b xs = if b then xs else []
    ti = delete TInheritance t
    si = delete TInheritance s
    tc = delete TComposition ti
    sc = delete TComposition si

isTarget :: Connection -> Target -> Bool
isTarget (Assoc Association _ _ _) TAssociation = True
isTarget (Assoc Aggregation _ _ _) TAggregation = True
isTarget (Assoc Composition _ _ _) TComposition = True
isTarget Inheritance               TInheritance = True
isTarget _                         _            = False

isTargetEdge :: DiagramEdge -> Target -> Bool
isTargetEdge (_, _, t) = isTarget t

isTargetsEdge :: DiagramEdge -> Targets -> Bool
isTargetsEdge x ts = any (x `isTargetEdge`) ts

targets :: Targets -> [DiagramEdge] -> [DiagramEdge]
targets ts es =
  [e | e <- es, isTargetsEdge e ts]

allRemoves :: Targets -> [DiagramEdge] -> [[DiagramEdge]]
allRemoves ts es =
  [filter (e /=) es | e <- targets ts es]

nonEdges :: [String] -> [DiagramEdge] -> [(String, String)]
nonEdges vs es = [(x, y) | x <- vs, y <- vs, x < y] \\ connections
  where
    connections = [e | (x, y, _) <- es, e <- [(x, y), (y, x)]]

type Limit = (Int, Maybe Int)

allAdds :: Targets -> [String] -> [DiagramEdge] -> [[DiagramEdge]]
allAdds ts vs es =
  [x:es | (s, e) <- nonEdges vs es, t <- toList ts
        , sl <- fst $ allLimits t, el <- snd $ allLimits t
        , x <- addEdges s e t sl el]
  where
    addEdges s e TInheritance _  _  = [(s, e, Inheritance), (e, s, Inheritance)]
    addEdges s e TAssociation sl el = addEdge s e TAssociation sl el
    addEdges s e t            sl el = addEdge s e t sl el ++ addEdge e s t sl el
    addEdge s e t sl el =
      (\k -> (s, e, Assoc k sl el False)) <$> maybeToList (assocType t)

assocType :: Target -> Maybe AssociationType
assocType TAssociation = Just Association
assocType TAggregation = Just Aggregation
assocType TComposition = Just Composition
assocType TInheritance = Nothing

{-|
Generates a list of all limits (i.e. multiplicities) for the given target.
The resulting tuple contains the list of all multiplicities at the edges start
and the list of all multiplicities at the edges end.
-}
allLimits :: Target -> ([Limit], [Limit])
allLimits t = (allStartLimits, allEndLimits)
  where
    allStartLimits = case t of
      TInheritance -> []
      TComposition -> [(0, Just 1), (1, Just 1)]
      _            -> allPossibleLimits
    allEndLimits   = case t of
      TInheritance -> []
      _            -> allPossibleLimits
    allPossibleLimits = [(l, h) | l <- [0, 1, 2], h <- [Just 1, Just 2, Nothing]
                                , maybe True (l <=) h]

allIncreaseLimitsRange :: Targets -> [DiagramEdge] -> [[DiagramEdge]]
allIncreaseLimitsRange = allLimitsWith ((>) `on` limitSize)

allDecreaseLimitsRange :: Targets -> [DiagramEdge] -> [[DiagramEdge]]
allDecreaseLimitsRange = allLimitsWith ((<) `on` limitSize)

allShiftDownLimitsRange :: Targets -> [DiagramEdge] -> [[DiagramEdge]]
allShiftDownLimitsRange =
  allLimitsWith (\x y -> limitSize x == limitSize y && fst x > fst y)

allShiftUpLimitsRange :: Targets -> [DiagramEdge] -> [[DiagramEdge]]
allShiftUpLimitsRange =
  allLimitsWith (\x y -> limitSize x == limitSize y && fst x > fst y)

{-|
Returns all possible sets of edges by modifying the limits on one side of one
edge by the given modification op on applying targets.
-}
allLimitsWith :: (Limit -> Limit -> Bool) -> Targets -> [DiagramEdge] -> [[DiagramEdge]]
allLimitsWith op ts es =
  [ (sv, ev, Assoc k sl'  el' False) : filter (e /=) es
  | e@(sv, ev, Assoc k sl el _) <- targets ts es, t <- toList ts
  , (sl', el') <- bothLimits sl el t]
  where
    bothLimits s e t = zip (repeat s) (endLimits e t)
                    ++ zip (startLimits s t) (repeat e)
    startLimits l t = [l' | l' <- fst $ allLimits t, l' `op` l]
    endLimits   l t = [l' | l' <- snd $ allLimits t, l' `op` l]

{-|
Beware! This function just takes a constant value (at the moment 10) to measure
the size of unlimited upper bounds.
-}
limitSize :: Limit -> Int
limitSize (x, Nothing) = 10 - x
limitSize (x, Just y ) = y - x

allFlipTransformations :: Target -> [DiagramEdge] -> [[DiagramEdge]]
allFlipTransformations t es =
  [ e' : filter (e /=) es | e <- es, isTargetEdge e t
                          , e'<- maybeToList $ maybeFlipEdge e]
  where
    maybeFlipEdge (s, e, k)
      | Assoc Association sl el _ <- k, sl == el = Nothing
      | otherwise                                = Just (e, s, k)

allFromInheritances :: Targets -> [DiagramEdge] -> [[DiagramEdge]]
allFromInheritances ts es =
  [ (se, ee, Assoc k sl el False) : filter (e /=) es
  | e@(se, ee, Inheritance) <- es, t <- toList ts
  , sl <- fst $ allLimits t, el <- snd $ allLimits t
  , k <- maybeToList $ assocType t]

allToInheritances :: Targets -> [DiagramEdge] -> [[DiagramEdge]]
allToInheritances ts es =
  [ (se, ee, Inheritance) : filter (e /=) es
  | e@(se, ee, Assoc {}) <- targets ts es]

allFromCompositions :: Targets -> [DiagramEdge] -> [[DiagramEdge]]
allFromCompositions ts es =
  [ (se, ee, Assoc k sl el False) : filter (e /=) es
  | e@(se, ee, Assoc Composition sl el _) <- es, t <- toList ts
  , k <- maybeToList $ assocType t]

allToCompositions :: Targets -> [DiagramEdge] -> [[DiagramEdge]]
allToCompositions ts es =
  [ (se, ee, Assoc Composition (reduce sl) el False) : filter (e /=) es
  | e@(se, ee, Assoc k sl el _) <- targets ts es, k /= Composition]
  where
    reduce (0, _) = (0, Just 1)
    reduce _      = (1, Just 1)

allOtherTransformations :: Target -> Target -> [DiagramEdge] -> [[DiagramEdge]]
allOtherTransformations st tt es =
  [ (se, ee, Assoc k sl el False) : filter (e /=) es
  | st /= tt, e@(se, ee, Assoc _ sl el _) <- es, isTargetEdge e st
  , k <- maybeToList $ assocType tt]
