module Modelling.PetriNet.FindConflictsSpec where

import Modelling.PetriNet.FindConflicts
import Modelling.PetriNet.Types          (defaultBasicConfig)

import Diagrams.Backend.SVG              (B)
import Diagrams.Prelude                  (Diagram)
import Test.Hspec

spec :: Spec
spec = do
  describe "findConflicts" $
    context "out of a given basic-config, count of Nets and a Boolean for the tasktype" $
      it "everything needed to create the Task is generated" $ do
        (tex,diaConfl) <- findConflicts True 3 defaultBasicConfig 
        print (get2ndElements diaConfl) `shouldReturn` ()
        
get2ndElements :: [(a,b)] -> [b]
get2ndElements list = [x| (y,x) <- list]