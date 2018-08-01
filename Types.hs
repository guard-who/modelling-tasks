module Types where

data Token
  = Keyword String
  | Id String
  | Num Int
  | Symbol String
  deriving Show
