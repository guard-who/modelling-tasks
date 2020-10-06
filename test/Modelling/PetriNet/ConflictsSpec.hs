module Modelling.PetriNet.ConflictsSpec where

import Modelling.PetriNet.Conflicts
import Modelling.PetriNet.Types          
  (defaultFindConflictConfig,defaultPickConflictConfig)

import Test.Hspec
import Control.Monad.Trans.Except       (runExceptT)

spec :: Spec
spec = do
  describe "findConflicts" $
    context "creates, out of a given Config," $
      it "everything needed to create the Task is generated" $ do
        Right diaConfl <- runExceptT $ findConflicts 0 defaultFindConflictConfig
        print (snd diaConfl) `shouldReturn` ()
  describe "pickConflicts" $
    context "creates, out of a given Config," $
      it "everything needed to create the Task is generated" $ do
        Right diaConfls <- runExceptT $ pickConflicts 0 defaultPickConflictConfig
        print (map snd diaConfls) `shouldReturn` ()
