module Modelling.PetriNet.FindConflictsSpec where

import Modelling.PetriNet.FindConflicts
import Modelling.PetriNet.Types

import Control.Monad  (void)
import Test.Hspec

spec :: Spec
spec = do
  describe "findConflicts" $
    context "out of a given basic-config and a Boolean for the tasktype" $
      it "everything needed to create the Task is generated" $ 
        void (findConflicts True defaultPetriBasicConfig) `shouldReturn` ()