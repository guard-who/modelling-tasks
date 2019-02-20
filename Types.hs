module Types where

type Association = (AssociationType, String, (Int, Maybe Int), String, String, (Int, Maybe Int))

data AssociationType = Association | Aggregation | Composition
  deriving Eq

data Connection = Inheritance | Assoc AssociationType String (Int, Maybe Int) (Int, Maybe Int) Bool
  deriving Eq

type Syntax = ([(String, Maybe String)], [Association])

data ClassConfig = ClassConfig {
    classes      :: (Maybe Int, Maybe Int),
    aggregations :: (Maybe Int, Maybe Int),
    associations :: (Maybe Int, Maybe Int),
    compositions :: (Maybe Int, Maybe Int),
    inheritances :: (Maybe Int, Maybe Int)
  } deriving Eq
