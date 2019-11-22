module Types where

type Association = (AssociationType, String, (Int, Maybe Int), String, String, (Int, Maybe Int))

data AssociationType = Association | Aggregation | Composition
  deriving Eq

data Connection = Inheritance | Assoc AssociationType String (Int, Maybe Int) (Int, Maybe Int) Bool
  deriving Eq

type Syntax = ([(String, Maybe String)], [Association])

data ClassConfig = ClassConfig {
    classes      :: (Int, Int),
    aggregations :: (Int, Maybe Int),
    associations :: (Int, Maybe Int),
    compositions :: (Int, Maybe Int),
    inheritances :: (Int, Maybe Int)
  } deriving Eq
