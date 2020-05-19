module Modelling.PetriNet.FindConflictsSpec where

import Modelling.PetriNet.FindConflicts
import Modelling.PetriNet.Types

import Diagrams.Backend.SVG              (B)
import Diagrams.Prelude                  (Diagram)
import Test.Hspec

spec :: Spec
spec = do
  describe "findConflicts" $
    context "out of a given basic-config and a Boolean for the tasktype" $
      it "everything needed to create the Task is generated" $ do
        (tex,diaConfl) <- findConflicts True defaultPetriBasicConfig 
        print (get2ndElements diaConfl) `shouldReturn` ()
        
get2ndElements :: [(a,b)] -> [b]
get2ndElements list = [x| (y,x) <- list]