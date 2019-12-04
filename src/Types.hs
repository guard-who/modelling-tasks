module Types where

type Association = (AssociationType, String, (Int, Maybe Int), String, String, (Int, Maybe Int))

data AssociationType = Association | Aggregation | Composition
  deriving Eq

data Connection = Inheritance | Assoc AssociationType String (Int, Maybe Int) (Int, Maybe Int) Bool
  deriving Eq

type Syntax = ([(String, Maybe String)], [Association])

type DiagramEdge = (String, String, Connection)

data Change a = Change {
    add    :: Maybe a,
    remove :: Maybe a
  } deriving Show

data ClassConfig = ClassConfig {
    classes      :: (Int, Int),
    aggregations :: (Int, Maybe Int),
    associations :: (Int, Maybe Int),
    compositions :: (Int, Maybe Int),
    inheritances :: (Int, Maybe Int)
  } deriving Eq

data RelationshipProperties = RelationshipProperties {
    wrongAssocs             :: Int,
    wrongCompositions       :: Int,
    selfRelationships       :: Int,
    hasDoubleRelationships  :: Bool,
    hasReverseRelationships :: Bool,
    hasMultipleInheritances :: Bool,
    hasInheritanceCycles    :: Bool,
    hasCompositionCycles    :: Bool,
    hasMarkedEdges          :: Maybe Bool
  }

defaultProperties :: RelationshipProperties
defaultProperties = RelationshipProperties {
    wrongAssocs             = 0,
    wrongCompositions       = 0,
    selfRelationships       = 0,
    hasDoubleRelationships  = False,
    hasReverseRelationships = False,
    hasMultipleInheritances = False,
    hasInheritanceCycles    = False,
    hasCompositionCycles    = False,
    hasMarkedEdges          = Nothing
  }
