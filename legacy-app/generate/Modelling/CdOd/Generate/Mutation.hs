module Modelling.CdOd.Generate.Mutation (
  -- * Types
  Mutation (..), Targets, Target (..), Alteration (..),
  -- * Perform mutation operations
  getAllMutationResults, getMutationResults,
  -- * Utility functions
  nonTargets,
  ) where

import Modelling.CdOd.Types
  (AssociationType (..), ClassConfig (..), Connection (..))
import Modelling.CdOd.Edges             (DiagramEdge)

import Data.Function                    (on)
import Data.List                        ((\\))
import Data.Maybe                       (maybeToList)
import Data.Set
  (Set, delete, intersection, member, singleton, toList)

getAllMutationResults
  :: ClassConfig -> [String] -> [DiagramEdge] -> [[DiagramEdge]]
getAllMutationResults c vs es =
  let singleTargets   = [singleton t | t <- [minBound :: Target ..]]
      mutations       = [m | t <- singleTargets, m <- [Add t, Remove t]]
        ++ [Transform t1 t2 | t1 <- singleTargets, t2 <- singleTargets]
        ++ [m | a <- [minBound :: Alteration ..], t <- singleTargets
              , m <- [LimitRange a t, LimitShift a t]]
  in concatMap (getMutationResults c vs es) mutations

data Mutation =
    Add       Targets
  | Remove    Targets
  | Transform Targets Targets
  | LimitRange Alteration Targets
  | LimitShift Alteration Targets

data Target = TAssociation | TAggregation | TComposition | TInheritance
  deriving (Bounded, Enum, Eq, Ord, Show)

type Targets = Set Target

data Alteration = Increase | Decrease
  deriving (Bounded, Enum, Eq, Ord)

getMutationResults
  :: ClassConfig -> [String] -> [DiagramEdge] -> Mutation -> [[DiagramEdge]]
getMutationResults c vs es m = map newName $ case m of
  Add                 t -> allAdds c t vs es
  Remove              t -> allRemoves c t es
  Transform         s t -> transform c s t es
  LimitRange Increase t -> allIncreaseLimitsRange c t es
  LimitRange Decrease t -> allDecreaseLimitsRange c t es
  LimitShift Increase t -> allShiftDownLimitsRange c t es
  LimitShift Decrease t -> allShiftUpLimitsRange c t es
  where
    newName xs = case xs of
      []                           -> []
      (s,e,Assoc k "" sl se b):xs' ->
        (s,e,Assoc k (firstFree (allNames xs') $ map (:[]) ['z','y'..]) sl se b):xs'
      xs'                          -> xs'
    allNames xs = [n | (_, _, Assoc _ n _ _ _) <- xs]
    firstFree _  []     = error "There are no free variables left"
    firstFree xs (y:ys) =
      if y `elem` xs then firstFree xs ys else y

transform
  :: ClassConfig -> Targets -> Targets -> [DiagramEdge] -> [[DiagramEdge]]
transform c s t es =
  concatMap (flip (allFlipTransformations c) es) (toList (s `intersection` t))
  ++ addWhen (TInheritance `member` s) (allFromInheritances c ti es)
  ++ addWhen (TInheritance `member` t) (allToInheritances c si es)
  ++ addWhen (TComposition `member` si) (allFromCompositions c tc es)
  ++ addWhen (TComposition `member` ti) (allToCompositions c sc es)
  ++ concat [allOtherTransformations c sa ta es
            | sa <- toList sc, ta <- toList tc, sa /= ta]
  where
    addWhen b xs = if b then xs else []
    ti = delete TInheritance t
    si = delete TInheritance s
    tc = delete TComposition ti
    sc = delete TComposition si

isTarget :: Connection -> Target -> Bool
isTarget (Assoc Association' _ _ _ _) TAssociation = True
isTarget (Assoc Aggregation' _ _ _ _) TAggregation = True
isTarget (Assoc Composition' _ _ _ _) TComposition = True
isTarget Inheritance'                 TInheritance = True
isTarget _                           _            = False

isTargetEdge :: DiagramEdge -> Target -> Bool
isTargetEdge (_, _, t) = isTarget t

isTargetsEdge :: DiagramEdge -> Targets -> Bool
isTargetsEdge x = any (x `isTargetEdge`)

targets :: Targets -> [DiagramEdge] -> [DiagramEdge]
targets ts es =
  [e | e <- es, isTargetsEdge e ts]

nonTargets :: Targets -> [DiagramEdge] -> [DiagramEdge]
nonTargets ts es =
  [e | e <- es, not $ isTargetsEdge e ts]

targetEdgesCount :: Target -> [DiagramEdge] -> Int
targetEdgesCount t es = length [e | e <- es, isTargetEdge e t]

configTarget :: Target -> ClassConfig -> (Int, Maybe Int)
configTarget t = case t of
  TAssociation -> associationLimits
  TAggregation -> aggregationLimits
  TComposition -> compositionLimits
  TInheritance -> inheritanceLimits

isLessThan :: Int -> (Int, a) -> Bool
isLessThan x (y , _) = x < y

isGreaterThan :: Int -> (a, Maybe Int) -> Bool
isGreaterThan _ (_, Nothing) = False
isGreaterThan x (_, Just y ) = x > y

isRemovable :: ClassConfig -> Target -> [DiagramEdge] -> Bool
isRemovable c t es =
  not $ (targetEdgesCount t es - 1) `isLessThan` configTarget t c

isAddable :: ClassConfig -> Target -> [DiagramEdge] -> Bool
isAddable c t es =
  not $ (targetEdgesCount t es + 1) `isGreaterThan` configTarget t c

removableTargets :: ClassConfig -> Targets -> [DiagramEdge] -> [DiagramEdge]
removableTargets c ts es =
  [e | t <- toList ts, isRemovable c t es
     , e <- es, isTargetEdge e t]

allRemoves :: ClassConfig -> Targets -> [DiagramEdge] -> [[DiagramEdge]]
allRemoves c ts es =
  [filter (e /=) es | e <- removableTargets c ts es]

nonEdges :: [String] -> [DiagramEdge] -> [(String, String)]
nonEdges vs es = [(x, y) | x <- vs, y <- vs, x < y] \\ connections
  where
    connections = [e | (x, y, _) <- es, e <- [(x, y), (y, x)]]

type Limit = (Int, Maybe Int)

allAdds
  :: ClassConfig -> Targets -> [String] -> [DiagramEdge] -> [[DiagramEdge]]
allAdds c ts vs es =
  [x:es | (s, e) <- nonEdges vs es, t <- toList ts, isAddable c t es
        , sl <- fst $ allLimits c t, el <- snd $ allLimits c t
        , x <- addEdges s e t sl el]
  where
    addEdges s e TInheritance _  _  = [(s, e, Inheritance'), (e, s, Inheritance')]
    addEdges s e TAssociation sl el = addEdge s e TAssociation sl el
    addEdges s e t            sl el = addEdge s e t sl el ++ addEdge e s t sl el
    addEdge s e t sl el =
      map (\k -> (s, e, Assoc k "" sl el False)) $ maybeToList (assocType t)

assocType :: Target -> Maybe AssociationType
assocType TAssociation = Just Association'
assocType TAggregation = Just Aggregation'
assocType TComposition = Just Composition'
assocType TInheritance = Nothing

{-|
Generates a list of all limits (i.e. multiplicities) for the given target.
The resulting tuple contains the list of all multiplicities at the edges start
and the list of all multiplicities at the edges end.

For now: ignores 'ClassConfig' parameter.
-}
allLimits :: ClassConfig -> Target -> ([Limit], [Limit])
allLimits _ t = (allStartLimits, allEndLimits)
  where
    allStartLimits = case t of
      TInheritance -> []
      TComposition -> [(0, Just 1), (1, Just 1)]
      _            -> allPossibleLimits
    allEndLimits   = case t of
      TInheritance -> []
      _            -> allPossibleLimits
    allPossibleLimits = [(l, h) | l <- [0, 1, 2]
                                , h <- [Just 1, Just 2, Nothing]
                                , maybe True (l <=) h]

allIncreaseLimitsRange
  :: ClassConfig -> Targets -> [DiagramEdge] -> [[DiagramEdge]]
allIncreaseLimitsRange c = allLimitsWith c ((>) `on` limitSize)

allDecreaseLimitsRange
  :: ClassConfig -> Targets -> [DiagramEdge] -> [[DiagramEdge]]
allDecreaseLimitsRange c = allLimitsWith c ((<) `on` limitSize)

allShiftDownLimitsRange
  :: ClassConfig -> Targets -> [DiagramEdge] -> [[DiagramEdge]]
allShiftDownLimitsRange c =
  allLimitsWith c (\x y -> limitSize x == limitSize y && fst x < fst y)

allShiftUpLimitsRange
  :: ClassConfig -> Targets -> [DiagramEdge] -> [[DiagramEdge]]
allShiftUpLimitsRange c =
  allLimitsWith c (\x y -> limitSize x == limitSize y && fst x > fst y)

{-|
Returns all possible sets of edges by modifying the limits on one side of one
edge by the given modification op on applying targets.
-}
allLimitsWith
  :: ClassConfig
  -> (Limit -> Limit -> Bool)
  -> Targets
  -> [DiagramEdge]
  -> [[DiagramEdge]]
allLimitsWith c op ts es =
  [ (sv, ev, Assoc k n sl' el' False) : filter (e /=) es
  | e@(sv, ev, Assoc k n sl el _) <- targets ts es, t <- toList ts
  , (sl', el') <- bothLimits sl el t]
  where
    bothLimits s e t = zip (repeat s) (endLimits e t)
                    ++ zip (startLimits s t) (repeat e)
    startLimits l t = [l' | l' <- fst $ allLimits c t, l' `op` l]
    endLimits   l t = [l' | l' <- snd $ allLimits c t, l' `op` l]

{-|
Beware! This function just takes a constant value (at the moment 10) to measure
the size of unlimited upper bounds.
-}
limitSize :: Limit -> Int
limitSize (x, Nothing) = 10 - x
limitSize (x, Just y ) = y - x

allFlipTransformations
  :: ClassConfig -> Target -> [DiagramEdge] -> [[DiagramEdge]]
allFlipTransformations _c t es =
  [ flipEdge e : filter (e /=) es | e <- es, isTargetEdge e t ]
  where
    flipEdge (s, e, k) = (e, s, k)

allFromInheritances
  :: ClassConfig -> Targets -> [DiagramEdge] -> [[DiagramEdge]]
allFromInheritances c ts es =
  [ (se, ee, Assoc k "" sl el False) : filter (e /=) es
  | isRemovable c TInheritance es, e@(se, ee, Inheritance') <- es
  , t <- toList ts, isAddable c t es
  , sl <- fst $ allLimits c t, el <- snd $ allLimits c t
  , k <- maybeToList $ assocType t]

allToInheritances :: ClassConfig -> Targets -> [DiagramEdge] -> [[DiagramEdge]]
allToInheritances c ts es =
  [ (se, ee, Inheritance') : filter (e /=) es
  | isAddable c TInheritance es
  , e@(se, ee, Assoc {}) <- removableTargets c ts es]

allFromCompositions
  :: ClassConfig -> Targets -> [DiagramEdge] -> [[DiagramEdge]]
allFromCompositions c ts es =
  [ (se, ee, Assoc k "" sl el False) : filter (e /=) es
  | isRemovable c TComposition es, e@(se, ee, Assoc Composition' _ sl el _) <- es
  , t <- toList ts, isAddable c t es
  , k <- maybeToList $ assocType t]

allToCompositions :: ClassConfig -> Targets -> [DiagramEdge] -> [[DiagramEdge]]
allToCompositions c ts es =
  [ (se, ee, Assoc Composition' "" (reduce sl) el False) : filter (e /=) es
  | isAddable c TComposition es
  , e@(se, ee, Assoc k _ sl el _) <- removableTargets c ts es, k /= Composition']
  where
    reduce (0, _) = (0, Just 1)
    reduce _      = (1, Just 1)

allOtherTransformations
  :: ClassConfig -> Target -> Target -> [DiagramEdge] -> [[DiagramEdge]]
allOtherTransformations c st tt es =
  [ (se, ee, Assoc k "" sl el False) : filter (e /=) es
  | st /= tt, isRemovable c st es, isAddable c tt es
  , e@(se, ee, Assoc _ _ sl el _) <- es, isTargetEdge e st
  , k <- maybeToList $ assocType tt]
