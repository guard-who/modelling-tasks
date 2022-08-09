module Modelling.ActivityDiagram.PetrinetSpec where

import qualified Data.Map as M (keys)

import Modelling.ActivityDiagram.Petrinet (PetriKey (..), convertToPetrinet)

import Modelling.ActivityDiagram.Alloy (getAlloyInstancesWith, moduleActionSequencesRules)
import Modelling.ActivityDiagram.Config (adConfigToAlloy, defaultADConfig, ADConfig(..))
import Modelling.ActivityDiagram.Instance (parseInstance)

import Modelling.PetriNet.Types (PetriLike(allNodes), petriLikeToPetri)

import Data.Either (isRight)
import Data.List (sort)
import Test.Hspec(Spec, context, describe, it, shouldBe)

spec :: Spec
spec =
  describe "convertToPetrinet" $
    context "on a list of generated diagrams" $ do
      let spec = adConfigToAlloy "" "" defaultADConfig
      it "generates a petrinet with ascending labels" $ do
        inst <- getAlloyInstancesWith (Just 50) spec
        let petri = map (convertToPetrinet . failWith id . parseInstance "this" "this") inst
        all checkLabels petri `shouldBe` (True::Bool)
      it "generates only valid petrinets" $ do
        inst <- getAlloyInstancesWith (Just 50) spec
        let petri = map (petriLikeToPetri . convertToPetrinet . failWith id . parseInstance "this" "this") inst
        all isRight petri `shouldBe` (True::Bool)

failWith :: (a -> String) -> Either a c -> c
failWith f = either (error . f) id

checkLabels :: PetriLike PetriKey -> Bool
checkLabels petri =
  let labels = sort $ map label $ M.keys $ allNodes petri
  in labels == [1..(length labels)]