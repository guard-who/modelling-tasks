{-# LANGUAGE TupleSections #-}
module Alloy.CdOd.CdAndChanges.Instance (
  fromInstance,
  ) where

import qualified Data.Map                         as M (lookup)
import qualified Data.Set                         as S (findMin, size, toList)

import Alloy.CdOd.Types
  (AssociationType (..), Change (..), Connection (..), DiagramEdge)

import Data.List                        (stripPrefix)
import Data.Map                         (Map)
import Data.Maybe                       (catMaybes, fromMaybe, isJust)
import Language.Alloy.Call

fromInstance
  :: AlloyInstance
  -> Either String (([String], [String]), [DiagramEdge], [Change DiagramEdge])
fromInstance insta = do
  es <- instanceToEdges insta
  cs <- instanceToChanges insta
  ns <- instanceToNames insta
  return (ns,
          [e | (o, e) <- es, o `notElem` catMaybes (map add cs)],
          [Change a r | c <- cs
                      , a <- lookupM (add c) es
                      , r <- lookupM (remove c) es])
  where
    lookupM :: Eq a => Maybe a -> [(a, b)] -> [Maybe b]
    lookupM Nothing  _  = [Nothing]
    lookupM (Just k) ms = [v | let v = lookup k ms, isJust v]

instanceToNames
  :: AlloyInstance -> Either String ([String], [String])
instanceToNames insta = do
  c' <- lookupSig (scoped "this" "Class") insta
  cs <- map objectName . S.toList <$> getSingle "" c'
  a' <- lookupSig (scoped "this" "Assoc") insta
  as <- map objectName . S.toList <$> getSingle "" a'
  return (cs, as)

instanceToChanges
  :: AlloyInstance
  -> Either String [Change Object]
instanceToChanges insta = do
  c'      <- lookupSig (scoped "this" "Change") insta
  cs      <- S.toList <$> getSingle "" c'
  cAdd    <- getRelation "add" c'
  cRemove <- getRelation "remove" c'
  return $ map (change cAdd cRemove) $ cs
  where
    change cAdd cRemove c =
      Change (M.lookup c cAdd) (M.lookup c cRemove)

getRelation :: String -> AlloySig -> Either String (Map Object Object)
getRelation n i = getDouble n i >>= relToMap id >>= mapM single
  where
    single x
      | S.size x == 1 = return $ S.findMin x
      | otherwise     = fail $ "Relation " ++ n ++ " matches at least one"
        ++ "member of its domain to multiple values of the codomain."

instanceToEdges ::
  AlloyInstance
  -> Either String [(Object, DiagramEdge)]
instanceToEdges insta = do
  r'         <- lookupSig (scoped "this" "Relationship") insta
  rFrom      <- getRelation "from" r'
  rTo        <- getRelation "to" r'
  a'         <- lookupSig (scoped "this" "Assoc") insta
  aFromLower <- getRelation "fromLower" a'
  aFromUpper <- getRelation "fromUpper" a'
  aToLower   <- getRelation "toLower" a'
  aToUpper   <- getRelation "toUpper" a'
  instanceToEdges' insta rFrom rTo aFromLower aFromUpper aToLower aToUpper

instanceToEdges'
  :: AlloyInstance
  -> Map Object Object
  -> Map Object Object
  -> Map Object Object
  -> Map Object Object
  -> Map Object Object
  -> Map Object Object
  -> Either String [(Object, DiagramEdge)]
instanceToEdges' insta rFrom rTo aFromLower aFromUpper aToLower aToUpper = do
  inh' <- lookupSig (scoped "this" "Inheritance") insta
  inh  <- S.toList <$> getSingle "" inh'
  inhs <- (\i -> (i,) <$> rel False i Inheritance) `mapM` inh
  coms <- lookupAssocs True Composition "Composition"
  asss <- lookupAssocs False Association "Association"
  aggs <- lookupAssocs True Aggregation "Aggregation"
  return $ inhs ++ coms ++ asss ++ aggs
  where
    lookupObj k m = case M.lookup k m of
      Nothing -> Left "Missing object "
      Just v  -> Right $ objectName v
    lookupLimit k m = case M.lookup k m of
      Nothing -> Left "Missing limit"
      Just o -> case objectName o of
        n | Just _ <- stripPrefix "Star" n -> Right Nothing
          | Just _ <- stripPrefix "Zero" n -> Right $ Just 0
          | Just _ <- stripPrefix "One"  n -> Right $ Just 1
          | Just _ <- stripPrefix "Two"  n -> Right $ Just 2
        l      -> Left $ "Unknown limit " ++ l
    rel flipRel r c = do
      rFrom' <- lookupObj r rFrom
      rTo'   <- lookupObj r rTo
      if flipRel
        then return (rTo', rFrom', c)
        else return (rFrom', rTo', c)
    assoc flipRel t a = do
      aFromLower' <- lookupLimit a aFromLower
      aFromUpper' <- lookupLimit a aFromUpper
      aToLower'   <- lookupLimit a aToLower
      aToUpper'   <- lookupLimit a aToUpper
      let lower = (fromMaybe (-1) aFromLower', aFromUpper')
          upper = (fromMaybe (-1) aToLower', aToUpper')
          (l, h) = if flipRel then (upper, lower) else (lower, upper)
      fmap (a,) $ rel flipRel a
        $ Assoc t (objectName a) l h False
    lookupAssocs flipRel t n = do
      a' <- lookupSig (scoped "this" n) insta
      a  <- S.toList <$> getSingle "" a'
      assoc flipRel t `mapM` a
