-- |

module Modelling.CdOd.DifferentNamesSpec where

import qualified Data.Bimap                       as BM

import Modelling.CdOd.DifferentNames (
  DifferentNamesConfig (maxInstances, objectConfig),
  ShufflingOption (..),
  differentNames,
  checkDifferentNamesConfig,
  checkDifferentNamesInstance,
  differentNamesEvaluation,
  differentNamesInitial,
  differentNamesSyntax,
  DifferentNamesInstance (..),
  defaultDifferentNamesConfig,
  defaultDifferentNamesInstance,
  getDifferentNamesTask,
  renameInstance,
  )
import Modelling.Auxiliary.Common       (oneOf)
import Modelling.CdOd.Types (
  Cd,
  ClassDiagram (..),
  LimitedLinking (..),
  Link (..),
  Name (Name, unName),
  Object (..),
  ObjectConfig (..),
  ObjectDiagram (..),
  Od,
  Relationship (..),
  associationNames,
  classNames,
  fromNameMapping,
  linkNames,
  toNameMapping,
  )
import Modelling.Common                 ()

import Control.Monad.Output (
  LangM' (withLang),
  Language (English),
  )
import Control.Monad.Trans.Except       (runExceptT)
import Control.Monad.Random (
  evalRandT,
  mkStdGen,
  randomIO,
  randomRIO,
  )
import Data.Bifunctor                   (Bifunctor (bimap))
import Data.Char                        (toUpper)
import Data.Containers.ListUtils        (nubOrd)
import Data.Either                      (isLeft, isRight)
import Data.List (nub)
import Data.Maybe                       (fromJust)
import Data.Ratio                       ((%))
import Data.Tuple                       (swap)
import Test.Hspec
import Test.QuickCheck (
  (===),
  (==>),
  Arbitrary (arbitrary),
  Property,
  Testable (property),
  ioProperty,
  oneof,
  sized,
  vectorOf,
  )
import System.Random                    (getStdGen, setStdGen)
import System.Random.Shuffle            (shuffleM)

spec :: Spec
spec = do
  describe "defaultDifferentNamesConfig" $
    it "is valid" $
      checkDifferentNamesConfig defaultDifferentNamesConfig `shouldBe` Nothing
  describe "defaultDifferentNamesInstance" $ do
    it "is valid" $
      checkDifferentNamesInstance defaultDifferentNamesInstance
      `shouldBe` Nothing
    context "using WithAdditionalNames" $
      it "is valid" $
        checkDifferentNamesInstance defaultDifferentNamesInstance {
          linkShuffling = WithAdditionalNames ["v"]
          }
        `shouldBe` Nothing
  describe "differentNames" $
    context "using defaultDifferentNamesConfig with limited instances" $
      it "generates an instance" $ do
        inst <- runExceptT $ do
          segment <- oneOf [0 .. 3]
          seed <- randomIO
          differentNames cfg segment seed
        inst `shouldSatisfy` isRight
  describe "differentNamesEvaluation" $ do
    it "accepts the initial example" $
      let cs = bimap unName unName <$> differentNamesInitial
      in Right 1 == evaluateDifferentNames cs cs
    it "accepts correct solutions" $
      property $ \cs g -> not (null cs) && isValidMapping cs
        ==> ioProperty $ do
          cs' <- flipCoin g `mapM` cs >>= shuffleM
          return $ Right 1 === evaluateDifferentNames cs cs'
    it "accepts with percentage or rejects too short solutions" $
      property $ \cs n -> not (null cs) && isValidMapping cs
        ==> ioProperty $ do
          let n' = abs n
              l = fromIntegral $ length cs
              r = (l - fromIntegral n') % l
          cs' <- drop n' <$> shuffleM cs
          return $ (if r >= 0.5 then (Right r ==) else isLeft)
            $ evaluateDifferentNames cs cs'
    it "rejects too long solutions" $
      property $ \cs w ->
        let cs' = cs ++ w
        in not (null w) && isValidMapping cs
           ==> isLeft $ evaluateDifferentNames cs cs'
  describe "renameInstance" $ do
    it "is reversable" $ renameProperty $ \inst mrinst _ _ ->
        let cd = cDiagram inst
            od = oDiagram inst
            names = classNames cd
            assocs = associationNames cd
            linkNs = linkNames od
        in (Just inst ==)
           $ mrinst
           >>= (\x -> renameInstance x names assocs linkNs)
    it "renames solution" $ renameProperty $ \inst mrinst as ls ->
      let rename xs ys = Name . fromJust . (`lookup` zip xs ys)
          origMap = bimap
            (rename (associationNames $ cDiagram inst) as)
            (rename (linkNames $ oDiagram inst) ls)
            <$> BM.toList (fromNameMapping $ mapping inst)
      in (Right 1 ==)
         $ maybe (Left "instance could not be renamed") return mrinst
         >>= \rinst -> differentNamesEvaluation rinst origMap `withLang` English
           :: Either String Rational
  describe "getDifferentNamesTask" $ do
    it "generates matching OD for association circle" $
      odFor (cdSimpleCircle association association association)
      `shouldReturn` simpleCircleOd
    it "generates matching OD for aggregation circle" $
      odFor (cdSimpleCircle aggregation aggregation aggregation)
      `shouldReturn` simpleCircleOd
    it "generates matching OD for composition and association circle" $
      odFor (cdSimpleCircle composition composition association)
      `shouldReturn` simpleCircleOd
    it "generates matching OD for composition and aggregation circle" $
      odFor (cdSimpleCircle composition composition aggregation)
      `shouldReturn` simpleCircleOd
    it "generates matching OD for association and aggregation circle" $
      odFor (cdSimpleCircle association association aggregation)
      `shouldReturn` simpleCircleOd
    it "generates matching OD for association, aggregation and composition circle" $
      odFor (cdSimpleCircle association aggregation composition)
      `shouldReturn` simpleCircleOd
    it "generates matching OD for circle with inheritance" $
      odFor cdBCCircle
      `shouldReturn` ObjectDiagram {
        objects = [
          Object {objectName = "a", objectClass = "A"},
          Object {objectName = "c", objectClass = "C"},
          Object {objectName = "c1", objectClass = "C"}
          ],
        links = [
          Link {linkName = "y", linkFrom = "a", linkTo = "c"},
          Link {linkName = "y", linkFrom = "a", linkTo = "c1"},
          Link {linkName = "x", linkFrom = "c", linkTo = "a"},
          Link {linkName = "x", linkFrom = "c1", linkTo = "a"}
          ]
        }
  where
    cfg = defaultDifferentNamesConfig {
      maxInstances = Just 1000
      }

odFor :: Cd -> IO Od
odFor cd = oDiagram <$> do
  g <- getStdGen
  evalRandT (getDifferentNamesTask failed fewObjects cd) g
  where
    failed = error "failed generating instance"
    fewObjects = defaultDifferentNamesConfig { objectConfig = oc }
    oc = ObjectConfig {
      linkLimits = (0, Just 4),
      linksPerObjectLimits = (0, Just 4),
      objectLimits = (3, 3)
      }

cdBCCircle :: Cd
cdBCCircle = ClassDiagram {
  classNames = ["A", "B", "C"],
  relationships = [
    Inheritance {subClass = "A", superClass = "B"},
    Aggregation {
      aggregationName = "x",
      aggregationPart = LimitedLinking {
        linking = "B",
        limits = (1, Just 1)
        },
      aggregationWhole = LimitedLinking {
        linking = "C",
        limits = (2, Just 2)
        }
       },
    Association {
      associationName = "y",
      associationFrom = LimitedLinking {
        linking = "C",
        limits = (0, Nothing)
        },
      associationTo = LimitedLinking {
        linking = "A",
        limits = (1, Just 1)
        }
       }
    ]
  }

type ToRelationship = String -> String -> String -> Relationship String String

cdSimpleCircle
  :: ToRelationship
  -> ToRelationship
  -> ToRelationship
  -> Cd
cdSimpleCircle edgeX edgeY edgeZ = ClassDiagram {
  classNames = ["A", "B", "C"],
  relationships = [edgeX "x" "A" "B", edgeY "y" "B" "C", edgeZ "z" "C" "A"]
  }

lcOne :: nodeName -> LimitedLinking nodeName
lcOne c = LimitedLinking {linking = c, limits = one}
  where
    one = (1, Just 1)

association :: r -> c -> c -> Relationship c r
association name from to= Association {
  associationName = name,
  associationFrom = lcOne from,
  associationTo = lcOne to
  }

aggregation :: r -> c -> c -> Relationship c r
aggregation name part whole = Aggregation {
  aggregationName = name,
  aggregationPart = lcOne part,
  aggregationWhole = lcOne whole
  }

composition :: r -> c -> c -> Relationship c r
composition name part whole = Composition {
  compositionName = name,
  compositionPart = lcOne part,
  compositionWhole = lcOne whole
  }

simpleCircleOd :: Od
simpleCircleOd = ObjectDiagram {
  objects = [
    Object {objectName = "a", objectClass = "A"},
    Object {objectName = "b", objectClass = "B"},
    Object {objectName = "c", objectClass = "C"}
    ],
  links = [
    Link {linkName = "z", linkFrom = "a", linkTo = "c"},
    Link {linkName = "x", linkFrom = "b", linkTo = "a"},
    Link {linkName = "y", linkFrom = "c", linkTo = "b"}
    ]
  }

renameProperty ::
  Testable prop =>
  (DifferentNamesInstance
    -> Maybe DifferentNamesInstance
    -> [String]
    -> [String]
    -> prop)
  -> Property
renameProperty p = property $ \n1 n2 n3 n4 a1 a2 a3 l1 l2 l3 ->
  let inst = defaultDifferentNamesInstance
      ns = map unName [n1, n2, n3, n4]
      as = map unName [a1, a2, a3]
      ls = map unName [l1, l2, l3]
      distinct xs = length (nubOrd xs) == length xs
      mrinst = renameInstance inst ns as ls
  in distinct ns && distinct as && distinct ls
     ==> p inst mrinst as ls

instance Arbitrary Name where
  arbitrary = sized $ \s -> Name <$> (
    (:)
    <$> oneof (map return letters)
    <*> vectorOf s (oneof $ map return $ letters ++ ['0'..'9'])
    )
    where
      lowers = ['a'..'z']
      uppers = map toUpper lowers
      letters = lowers ++ uppers

flipCoin :: Int -> (String, String) -> IO (String, String)
flipCoin g p = do
  setStdGen $ mkStdGen g
  b <- randomRIO (False, True)
  return $ (if b then swap else id) p

evaluateDifferentNames
  :: [(String, String)]
  -- ^ task instance mapping
  -> [(String, String)]
  -- ^ submitted mapping
  -> Either String Rational
evaluateDifferentNames cs cs' = flip withLang English $ do
  let i = DifferentNamesInstance {
        anonymousObjects = error "anonymousObjects is undefined",
        cDiagram = error "cDiagram is undefined",
        generatorValue = 1,
        oDiagram = error "oDiagram is undefined",
        showSolution = True,
        mapping = toNameMapping $ BM.fromList cs,
        linkShuffling = ConsecutiveLetters,
        usesAllRelationships = True
        }
      cs'' = bimap Name Name <$> cs'
  differentNamesSyntax i cs''
  differentNamesEvaluation i cs''

isValidMapping :: (Eq a, Eq b) => [(a, b)] -> Bool
isValidMapping cs
  | l > length (nub $ fst <$> cs)
  = False
  | l > length (nub $ snd <$> cs)
  = False
  | otherwise
  = True
  where
    l = length cs
