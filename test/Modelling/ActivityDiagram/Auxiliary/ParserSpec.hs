{-# LANGUAGE QuasiQuotes #-}

module Modelling.ActivityDiagram.Auxiliary.ParserSpec where

import qualified Data.Map as M (fromList)

import Modelling.ActivityDiagram.Auxiliary.Parser (ParseValue(..), parseMappingSequence)

import Data.Either (isLeft)
import Data.Map (Map)
import Data.String.Interpolate ( i )
import Text.Parsec (parse)
import Test.Hspec (Spec, context, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec =
  describe "parseMappingSequence" $ do
    context "Given lines of input of the form <key>: <value>" $ do
      it "parses the input and constructs a corresponding mapping" $
        parse parseMappingSequence "" testKeyValBasic `shouldBe` Right testMapping
      it "parses the input regardless of leading/trailing spaces" $
        parse parseMappingSequence "" testKeyValLeadTrail `shouldBe` Right testMapping
      it "is robust against usage of unorthodox spacing between symbols" $
        parse parseMappingSequence "" testKeyValSpacing `shouldBe` Right testMapping
      it "is robust against the use of empty lines" $
        parse parseMappingSequence "" testKeyValEmptyLines `shouldBe` Right testMapping
      it "parses quoted strings as well as normal strings" $
        parse parseMappingSequence "" testKeyValQuotedStrings `shouldBe` Right testMapping
      it "rejects input with non-alphanumeric keys" $
        parse parseMappingSequence "" testNonAlphaKey `shouldSatisfy` isLeft
      it "rejects input with non-alphanumeric string values" $
        parse parseMappingSequence "" testNonAlphaValue `shouldSatisfy` isLeft
    context "Given lines of input which are not of the form <key>: <value>" $
      it "rejects the input" $
        parse parseMappingSequence "" testNotKeyVal `shouldSatisfy` isLeft

testKeyValBasic :: String
testKeyValBasic = [i|test1: [1,2]
test2: hello
test3: (42,7)
|]

testKeyValLeadTrail :: String
testKeyValLeadTrail = [i|   test1: [1,2]
test2: hello
    test3: (42,7)
|]

testKeyValSpacing :: String
testKeyValSpacing = [i|test1: [  1 , 2   ]
test2 :hello
test3  :  ( 42 ,   7 )
|]

testKeyValEmptyLines :: String
testKeyValEmptyLines = [i|
test1: [1,2]


test2: hello

test3: (42,7)

|]

testKeyValQuotedStrings :: String
testKeyValQuotedStrings = [i|test1: [1,2]
test2: "hello"
test3: (42,7)
|]

testMapping :: Map String ParseValue
testMapping =
  M.fromList [
    ("test1", ParseList [ParseInt 1, ParseInt 2]),
    ("test2", ParseString "hello"),
    ("test3", ParseTuple (ParseInt 42, ParseInt 7))
  ]

testNonAlphaKey :: String
testNonAlphaKey = [i|te st: 1|]

testNonAlphaValue :: String
testNonAlphaValue = [i|test: "aa aaa"|]

testNotKeyVal :: String
testNotKeyVal = [i|test1: 1
test2 2
test3: 3|]