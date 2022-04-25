{-|
originally from Autotool (https://gitlab.imn.htwk-leipzig.de/autotool/all0)
based on revision: ad25a990816a162fdd13941ff889653f22d6ea0a
based on file: collection/src/Petri/Step.hs
-}
module Modelling.PetriNet.Reach.Step where

import qualified Data.Map                         as M (
  insert,
  findWithDefault,
  fromList,
  toList,
  )
import qualified Data.Set                         as S (
  difference,
  empty,
  fromList,
  member,
  toList,
  union,
  )

import Modelling.Auxiliary.Output (
  LangM',
  OutputMonad (image, indent, paragraph, refuse, text),
  english,
  german,
  recoverWith,
  translate,
  )
import Modelling.PetriNet.Reach.Draw    (drawToFile)
import Modelling.PetriNet.Reach.Type (
  Net (capacity, connections, start),
  State (State),
  allNonNegative,
  conforms,
  )

import Control.Applicative              (Alternative)
import Control.Monad                    (foldM, guard, unless)
import Control.Monad.IO.Class           (MonadIO)
import Data.GraphViz                    (GraphvizCommand)

deadlocks :: Ord s => Net s t -> [[State s]]
deadlocks n = do
  zs <- levels n
  return $ filter (null . successors n) zs

deadlocks' :: Ord s => Net s t -> [[(State s, [t])]]
deadlocks' n = do
  zs <- levels' n
  return $ filter (\(s, _) -> null $ successors n s) zs

levels :: Ord s => Net s t -> [[State s]]
levels n =
  let f _    [] = []
      f done xs =
        let done' = S.union done $ S.fromList xs
            next = S.fromList $ do
              x <- xs
              (_, y) <- successors n x
              return y
         in xs :
            f
              done'
              (S.toList $ S.difference next done')
  in f S.empty [start n]

levels'
  :: Ord s
  => Net s t
  -> [[(State s, [t])]]
levels' n =
  let f _    [] = []
      f done xs =
        let done' = S.union done $ S.fromList $ map fst xs
            next = M.toList $
              M.fromList $ do
                (x, p) <- xs
                (t, y) <- successors n x
                guard $ not $ S.member y done'
                return (y, t : p)
         in xs : f done' next
  in f S.empty [(start n, [])]

equalling :: Eq a => (t -> a) -> t -> t -> Bool
equalling f x y = f x == f y

--executesPlain n ts = result $ executes n ts

executes
  :: (Alternative m, MonadIO m, OutputMonad m, Show t, Ord s, Show s, Ord t)
  => FilePath
  -> GraphvizCommand
  -> Net s t
  -> [t]
  -> LangM' m (Either Int (State s))
executes path cmd n ts = foldM
  (\z (k, t) -> either (return . Left) (step k t) z)
  (Right $ start n)
  (zip [1 :: Int ..] ts)
  where
    step k t z = recoverWith (k - 1) $ do
      paragraph $ translate $ do
        english $ "Step " ++ show k
        german $ "Schritt " ++ show k
      executeIO path cmd k n t z

executeIO
  :: (MonadIO m, OutputMonad m, Show a, Show k, Ord a, Ord k)
  => FilePath
  -> GraphvizCommand
  -> Int
  -> Net k a
  -> a
  -> State k
  -> LangM' m (State k)
executeIO path cmd i n t z0 = do
  z2 <- execute n t z0
  g <- drawToFile False path cmd i $ n {start = z2}
  image g
  return z2

execute
  :: (OutputMonad m, Show a, Show k, Ord a, Ord k)
  => Net k a
  -> a
  -> State k
  -> LangM' m (State k)
execute n t z0 = do
  paragraph $ text $ "Transition " ++ show t
  let cs = do
        c@(_, t', _) <- connections n
        guard $ t' == t
        return c
  case cs of
    [] -> do
      refuse $ translate $ do
        english "does not exist!"
        german "existiert nicht!"
      return z0
    [(vor, _, nach)] -> do
      let z1 = change pred vor z0
      paragraph $ translate $ do
        english "Intermediate marking (after collecting tokens)"
        german "Zwischenmarkierung (nach Einziehen der Marken im Vorbereich)"
      indent $ text $ show z1
      unless (allNonNegative z1) $ refuse $ translate $ do
        english "contains negative markings (transition was not activated)!"
        german "enthält negative Markierungen (Transition war nicht aktiviert)!"
      let z2 = change succ nach z1
      paragraph $ translate $ do
        english "Final marking (after distributing tokens)"
        german "Endmarkierung (nach Austeilen der Marken im Nachbereich)"
      indent $ text $ show z2
      unless (conforms (capacity n) z2) $ refuse $ translate $ do
        english "contains more tokens than capacity permits!"
        german "enthält mehr Marken, als die Kapazität zulässt!"
      return z2
    _ -> undefined -- TODO Patern match not required?

successors
  :: Ord s
  => Net s t
  -> State s
  -> [(t, State s)]
successors n z0 = do
  (vor, t, nach) <- connections n
  let z1 = change pred vor z0
  guard $ allNonNegative z1
  let z2 = change succ nach z1
  guard $ conforms (capacity n) z2
  return (t, z2)

change
  :: Ord s
  => (Int -> Int)
  -> [s]
  -> State s
  -> State s
change f ps (State z) =
  State $ foldl
    (\z' p -> M.insert p (f $ M.findWithDefault 0 p z') z')
    z
    ps
