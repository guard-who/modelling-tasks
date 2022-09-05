module Modelling.ActivityDiagram.ActionSequencesSpec where

import Modelling.ActivityDiagram.ActionSequences (generateActionSequence, validActionSequence)

import Modelling.ActivityDiagram.Datatype (
  UMLActivityDiagram(..),
  ADNode(..),
  ADConnection(..)
  )

import Modelling.ActivityDiagram.Alloy (moduleActionSequencesRules)
import Modelling.ActivityDiagram.Config (adConfigToAlloy, defaultADConfig, ADConfig(..))
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
        let spec = adConfigToAlloy modules preds defaultADConfig{minActions=5, maxActions=8, minObjectNodes=0, maxObjectNodes=1}
        inst <- getInstances (Just 50) spec
        let ad = map (failWith id .parseInstance) inst
        all p ad `shouldBe` (True::Bool)
      where
        modules = moduleActionSequencesRules
        preds = "someActionNodesExistInEachBlock"
        p x = validActionSequence (generateActionSequence x) x


failWith :: (a -> String) -> Either a c -> c
failWith f = either (error . f) id

testDiagram :: UMLActivityDiagram
testDiagram = UMLActivityDiagram
      { nodes =
        [ ADActionNode
            { label = 1
            , name = "A"
            }
        , ADActionNode
            { label = 2
            , name = "B"
            }
        , ADActionNode
            { label = 3
            , name = "C"
            }
        , ADActionNode
            { label = 4
            , name = "D"
            }
        , ADActionNode
            { label = 5
            , name = "E"
            }
        , ADObjectNode
            { label = 6
            , name = "F"
            }
        , ADDecisionNode { label = 7 }
        , ADDecisionNode { label = 8 }
        , ADMergeNode { label = 9 }
        , ADMergeNode { label = 10 }
        , ADForkNode { label = 11 }
        , ADJoinNode { label = 12 }
        , ADFlowFinalNode { label = 13 }
        , ADFlowFinalNode { label = 14 }
        , ADInitialNode { label = 15 }
        ]
    , connections =
        [ ADConnection
            { from = 1
            , to = 10
            , guard = ""
            }
        , ADConnection
            { from = 2
            , to = 14
            , guard = ""
            }
        , ADConnection
            { from = 3
            , to = 9
            , guard = ""
            }
        , ADConnection
            { from = 4
            , to = 9
            , guard = ""
            }
        , ADConnection
            { from = 5
            , to = 12
            , guard = ""
            }
        , ADConnection
            { from = 6
            , to = 1
            , guard = ""
            }
        , ADConnection
            { from = 7
            , to = 10
            , guard = "b"
            }
        , ADConnection
            { from = 7
            , to = 13
            , guard = "a"
            }
        , ADConnection
            { from = 8
            , to = 3
            , guard = "b"
            }
        , ADConnection
            { from = 8
            , to = 4
            , guard = "a"
            }
        , ADConnection
            { from = 9
            , to = 12
            , guard = ""
            }
        , ADConnection
            { from = 10
            , to = 11
            , guard = ""
            }
        , ADConnection
            { from = 11
            , to = 2
            , guard = ""
            }
        , ADConnection
            { from = 11
            , to = 5
            , guard = ""
            }
        , ADConnection
            { from = 11
            , to = 8
            , guard = ""
            }
        , ADConnection
            { from = 12
            , to = 7
            , guard = ""
            }
        , ADConnection
            { from = 15
            , to = 6
            , guard = ""
            }
        ]
    }
