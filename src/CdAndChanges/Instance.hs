{-# LANGUAGE TupleSections #-}
module CdAndChanges.Instance where

import qualified Data.Map                         as M (fromList, lookup)
import qualified Data.Set                         as S (toList)

import Parser (
  Entry (..),
  Object (..),
  Relation,
  Signature (..),
  alloyInstance,
  combineEntries,
  double,
  single,
  )
import Types
  (AssociationType (..), Connection (..), DiagramEdge)

import Data.Bifunctor                   (first)
import Data.Map                         (Map)
import Data.Maybe                       (catMaybes, fromJust, isJust)
import Data.Set                         (Set)
import Text.Parsec                      (parse)

scoped :: String -> String -> Signature
scoped = Signature . Just

relToMap :: Relation Set -> Either String (Map Object Object)
relToMap = fmap (M.fromList . S.toList) . double

fromInstance :: String -> Either String ([DiagramEdge], [Change DiagramEdge])
fromInstance output = do
  insta      <-
    first show $ combineEntries <$> parse alloyInstance "Alloy" output
  es <- instanceToEdges insta
  cs <- instanceToChanges insta
  return ([e | (o, e) <- es, o `notElem` catMaybes (add <$> cs)],
          [Change a r | c <- cs
                      , a <- lookupM (add c) es
                      , r <- lookupM (remove c) es])
  where
    lookupM :: Eq a => Maybe a -> [(a, b)] -> [Maybe b]
    lookupM Nothing  _  = [Nothing]
    lookupM (Just k) ms = [v | let v = lookup k ms, isJust v]

data Change a = Change {
    add    :: Maybe a,
    remove :: Maybe a
  }

instanceToChanges
  :: Map Signature (Entry Map Set)
  -> Either String [Change Object]
instanceToChanges insta = do
  c'      <- lookupEntry (scoped "this" "Change") insta
  cs      <- S.toList <$> lookupRel single "" c'
  cAdd    <- lookupRel relToMap "add" c'
  cRemove <- lookupRel relToMap "remove" c'
  return $ change cAdd cRemove <$> cs
  where
    change cAdd cRemove c =
      Change (M.lookup c cAdd) (M.lookup c cRemove)

instanceToEdges ::
  Map Signature (Entry Map Set)
  -> Either String [(Object, DiagramEdge)]
instanceToEdges insta = do
  r'         <- lookupEntry (scoped "this" "Relationship") insta
  rFrom      <- lookupRel relToMap "from" r'
  rTo        <- lookupRel relToMap "to" r'
  a'         <- lookupEntry (scoped "this" "Assoc") insta
  aFromLower <- lookupRel relToMap "fromLower" a'
  aFromUpper <- lookupRel relToMap "fromUpper" a'
  aToLower   <- lookupRel relToMap "toLower" a'
  aToUpper   <- lookupRel relToMap "toUpper" a'
  instanceToEdges' insta rFrom rTo aFromLower aFromUpper aToLower aToUpper

instanceToEdges'
  :: Map Signature (Entry Map Set)
  -> Map Object Object
  -> Map Object Object
  -> Map Object Object
  -> Map Object Object
  -> Map Object Object
  -> Map Object Object
  -> Either String [(Object, DiagramEdge)]
instanceToEdges' insta rFrom rTo aFromLower aFromUpper aToLower aToUpper = do
  inh' <- lookupEntry (scoped "this" "Inheritance") insta
  inh  <- S.toList <$> lookupRel single "" inh'
  inhs <- (\i -> (i,) <$> rel True i Inheritance) `mapM` inh
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
      Just (Object sig _) -> case sigName sig of
        "Star" -> Right Nothing
        "Zero" -> Right $ Just 0
        "One"  -> Right $ Just 1
        "Two"  -> Right $ Just 2
        l      -> Left $ "Unknown limit " ++ l
      Just o -> Left $ "Unknown object name " ++ objectName o
    rel flipRel r c = do
      rFrom' <- lookupObj r rFrom
      rTo'   <- lookupObj r rTo
      if flipRel
        then return (rFrom', rTo', c)
        else return (rTo', rFrom', c)
    assoc flipRel t a = do
      aFromLower' <- lookupLimit a aFromLower
      aFromUpper' <- lookupLimit a aFromUpper
      aToLower'   <- lookupLimit a aToLower
      aToUpper'   <- lookupLimit a aToUpper
      fmap (a,) $ rel flipRel a
        $ Assoc t (objectName a)
          (fromJust aFromLower', aFromUpper')
          (fromJust aToLower', aToUpper') False
    lookupAssocs flipRel t n = do
      a' <- lookupEntry (scoped "this" n) insta
      a  <- S.toList <$> lookupRel single "" a'
      assoc flipRel t `mapM` a

lookupRel
  :: (Relation a -> Either String b)
  -> String
  -> Entry Map a
  -> Either String b
lookupRel kind rel e = case M.lookup rel (relation e) of
  Nothing -> Left $ "relation " ++ rel
    ++ " is missing in your Alloy instance"
  Just r -> kind r

lookupEntry :: Signature -> Map Signature e -> Either String e
lookupEntry sig insta = case M.lookup sig insta of
  Nothing -> Left $ maybe "" (++ "/") (scope sig) ++ sigName sig
    ++ " is missing in your Alloy instance"
  Just e  -> Right e

objectName :: Object -> String
objectName o = case o of
  Object sig n   -> sigName sig ++ show n
  NumberObject n -> show n
  NamedObject n  -> n
