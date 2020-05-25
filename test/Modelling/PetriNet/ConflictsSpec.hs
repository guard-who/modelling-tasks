module Modelling.PetriNet.ConflictsSpec where

import Modelling.PetriNet.Conflicts
import Modelling.PetriNet.Types          
  (defaultFindConflictConfig,defaultPickConflictConfig)

import Test.Hspec

spec :: Spec
spec = do
  describe "findConflicts" $
    context "creates, out of a given Config," $
      it "everything needed to create the Task is generated" $ do
        (_,diaConfl) <- findConflicts defaultFindConflictConfig 
        print (map snd diaConfl) `shouldReturn` ()
  describe "pickConflicts" $
    context "creates, out of a given Config," $
      it "everything needed to create the Task is generated" $ do
        (_,diaConfl) <- pickConflicts defaultPickConflictConfig 
        print (map snd diaConfl) `shouldReturn` ()
        