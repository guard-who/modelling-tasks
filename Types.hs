module Types where

data Token
  = Keyword String
  | Id String
  | Num Int
  | Symbol String
  deriving Show

type Association = (AssociationType, String, (Int, Maybe Int), String, String, (Int, Maybe Int))

data AssociationType = Association | Aggregation | Composition
  deriving Eq
