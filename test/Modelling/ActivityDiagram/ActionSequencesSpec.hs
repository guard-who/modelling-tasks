module Modelling.ActivityDiagram.ActionSequencesSpec where

import Modelling.ActivityDiagram.ActionSequences (generateActionSequence, validActionSequence)

import Modelling.ActivityDiagram.Datatype (
  UMLActivityDiagram(..),
  AdNode (..),
  AdConnection (..)
  )

import Modelling.ActivityDiagram.Alloy (
  adConfigToAlloy,
  moduleActionSequencesRules,
  )
import Modelling.ActivityDiagram.Config (defaultAdConfig)
import Modelling.ActivityDiagram.Instance (parseInstance)
import Language.Alloy.Call (getInstances)

import Test.Hspec(Spec, context, describe, it, shouldBe)


spec :: Spec
spec =
  describe "validActionSequence" $ do
    context "on a specific diagram" $ do
      it "accepts valid input sequences of actions, therefore one leading to the termination of all flows of the diagram" $
        validActionSequence ["A", "E", "C", "B"] testDiagram `shouldBe` (True::Bool)
      it "accepts valid input sequences traversing a different path at decision nodes" $
        validActionSequence ["A", "E", "D", "B"] testDiagram `shouldBe` (True::Bool)
      it "accept valid input sequences traversing nodes in different order due to fork nodes" $
        validActionSequence ["A", "B", "E", "C"] testDiagram `shouldBe` (True::Bool)
      it "accepts valid input sequences of actions traversing one cycle" $
        validActionSequence ["A", "E", "C", "B", "D", "B", "E"] testDiagram `shouldBe` (True::Bool)
      it "accepts valid input sequences of actions traversing more than one cycle" $
        validActionSequence ["A", "E", "D", "B", "B", "C", "E", "E", "C", "B"] testDiagram `shouldBe` (True::Bool)
      it "accepts much deferred flow ends" $
        validActionSequence ["A", "E", "D", "C", "E", "E", "C", "E", "C", "B", "B", "B", "B"] testDiagram `shouldBe` (True::Bool)
      it "rejects an empty input sequence" $
        validActionSequence [] testDiagram `shouldBe` (False::Bool)
      it "rejects input sequences with actions which dont exist in the diagram" $
        validActionSequence ["A", "X", "E", "C", "B"] testDiagram `shouldBe` (False::Bool)
      it "rejects input sequences that are too short to terminate any flows of the diagram" $
        validActionSequence ["A"] testDiagram `shouldBe` (False::Bool)
      it "rejects input sequences that dont terminate all flows of the diagram" $
        validActionSequence ["A", "E", "C"] testDiagram `shouldBe` (False::Bool)
      it "rejects input sequences that contain actions that arent traversed in that order" $
        validActionSequence ["A", "E", "A", "C", "B"] testDiagram `shouldBe` (False::Bool)
      it "rejects input sequences where a prefix, but not the whole sequence, would terminate all flows of the diagram" $
        validActionSequence ["A", "B", "D", "E", "B"] testDiagram `shouldBe` (False::Bool)
      it "is consistent with the function generateActionSequence" $
        validActionSequence (generateActionSequence testDiagram) testDiagram `shouldBe` (True::Bool)
    context "on a list of generated diagrams" $
      it "is consistent with the function generateActionSequence" $ do
        let spec' = adConfigToAlloy modules predicates defaultAdConfig
            depth = 10
        inst <- getInstances (Just $ fromIntegral depth) spec'
        ad <- mapM parseInstance inst
        (length inst, all p ad) `shouldBe` (depth, True)
      where
        modules = moduleActionSequencesRules
        predicates = "someActionNodesExistInEachBlock"
        p x = validActionSequence (generateActionSequence x) x

testDiagram :: UMLActivityDiagram
testDiagram = UMLActivityDiagram
      { nodes =
        [ AdActionNode
            { label = 1
            , name = "A"
            }
        , AdActionNode
            { label = 2
            , name = "B"
            }
        , AdActionNode
            { label = 3
            , name = "C"
            }
        , AdActionNode
            { label = 4
            , name = "D"
            }
        , AdActionNode
            { label = 5
            , name = "E"
            }
        , AdObjectNode
            { label = 6
            , name = "F"
            }
        , AdDecisionNode { label = 7 }
        , AdDecisionNode { label = 8 }
        , AdMergeNode { label = 9 }
        , AdMergeNode { label = 10 }
        , AdForkNode { label = 11 }
        , AdJoinNode { label = 12 }
        , AdFlowFinalNode { label = 13 }
        , AdFlowFinalNode { label = 14 }
        , AdInitialNode { label = 15 }
        ]
    , connections =
        [ AdConnection
            { from = 1
            , to = 10
            , guard = ""
            }
        , AdConnection
            { from = 2
            , to = 14
            , guard = ""
            }
        , AdConnection
            { from = 3
            , to = 9
            , guard = ""
            }
        , AdConnection
            { from = 4
            , to = 9
            , guard = ""
            }
        , AdConnection
            { from = 5
            , to = 12
            , guard = ""
            }
        , AdConnection
            { from = 6
            , to = 1
            , guard = ""
            }
        , AdConnection
            { from = 7
            , to = 10
            , guard = "b"
            }
        , AdConnection
            { from = 7
            , to = 13
            , guard = "a"
            }
        , AdConnection
            { from = 8
            , to = 3
            , guard = "b"
            }
        , AdConnection
            { from = 8
            , to = 4
            , guard = "a"
            }
        , AdConnection
            { from = 9
            , to = 12
            , guard = ""
            }
        , AdConnection
            { from = 10
            , to = 11
            , guard = ""
            }
        , AdConnection
            { from = 11
            , to = 2
            , guard = ""
            }
        , AdConnection
            { from = 11
            , to = 5
            , guard = ""
            }
        , AdConnection
            { from = 11
            , to = 8
            , guard = ""
            }
        , AdConnection
            { from = 12
            , to = 7
            , guard = ""
            }
        , AdConnection
            { from = 15
            , to = 6
            , guard = ""
            }
        ]
    }
