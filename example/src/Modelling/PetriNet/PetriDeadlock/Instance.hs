module Modelling.PetriNet.PetriDeadlock.Instance where

import qualified Data.Map                         as M (fromList)
import qualified Data.Set                         as S (fromList)

import Modelling.PetriNet.Reach.Deadlock (DeadlockInstance (..))
import Modelling.PetriNet.Reach.Type    (Capacity (..), Net (..), State (..))
import Data.GraphViz.Commands           (GraphvizCommand (Circo))

examWs2024 :: DeadlockInstance String String
examWs2024 = DeadlockInstance {
  drawUsing = Circo,
  minLength = 14,
  noLongerThan = Nothing,
  petriNet = Net {
    places = S.fromList ["s1", "s2", "s3", "s4", "s5", "s6"],
    transitions = S.fromList ["t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8"],
    connections = [
      (["s3"], "t1", ["s6"]),
      (["s3"], "t2", ["s4", "s5"]),
      (["s4"], "t3", ["s2"]),
      (["s5"], "t4", ["s4"]),
      (["s1"], "t5", ["s1"]),
      (["s2"], "t6", ["s1", "s3"]),
      (["s2", "s6"], "t7", ["s4", "s5"]),
      (["s1"], "t8", ["s3"])
      ],
    capacity = Unbounded,
    start = State {
      unState = M.fromList [
        ("s1", 1),
        ("s2", 0),
        ("s3", 1),
        ("s4", 1),
        ("s5", 1),
        ("s6", 1)
        ]
      }
    },
  showSolution = True,
  withLengthHint = Just 14,
  withMinLengthHint = Just 14
  }
