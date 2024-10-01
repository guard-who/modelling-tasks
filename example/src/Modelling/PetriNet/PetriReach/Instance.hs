-- |

module Modelling.PetriNet.PetriReach.Instance where

import qualified Data.Map                         as M (fromList)
import qualified Data.Set                         as S (fromList)

import Modelling.PetriNet.Reach.Reach   (ReachInstance (..))
import Modelling.PetriNet.Reach.Type    (Capacity (..), Net (..), State (..))
import Data.GraphViz                    (GraphvizCommand (Circo))

task5 :: ReachInstance String String
task5 = ReachInstance {
  drawUsing = Circo,
  goal = State {
    unState = M.fromList
      [("s1", 1), ("s2", 1), ("s3", 1), ("s4", 0), ("s5", 4), ("s6", 0)]
    },
  minLength = 12,
  noLongerThan = Nothing,
  petriNet = Net {
    places = S.fromList ["s1", "s2", "s3", "s4", "s5", "s6"],
    transitions = S.fromList ["t1", "t2", "t3", "t4", "t5", "t6"],
    connections = [
      (["s1", "s5"], "t1", ["s6", "s2", "s1"]),
      (["s3", "s1"], "t2", ["s5", "s6"]),
      (["s4", "s5"], "t3", ["s3", "s5"]),
      (["s6", "s4"], "t4", ["s5", "s4"]),
      (["s2", "s5"], "t5", ["s4", "s2", "s1"]),
      (["s2", "s3"], "t6", ["s2", "s3"])
     ],
    capacity = Unbounded,
    start = State {
      unState = M.fromList [
        ("s1", 1),
        ("s2", 1),
        ("s3", 1),
        ("s4", 0),
        ("s5", 1),
        ("s6", 0)
       ]
      }
    },
  showGoalNet = True,
  showSolution = True,
  withLengthHint = Just 12,
  withMinLengthHint = Just 12
  }
