module Modelling.ActivityDiagram.PetriNetSpec where

import qualified Data.Map as M (keys)

import Modelling.ActivityDiagram.PetriNet (
  PetriKey (..),
  convertToPetriNet,
  convertToSimple,
  )

import Modelling.ActivityDiagram.Config (adConfigToAlloy, defaultAdConfig)
import Modelling.ActivityDiagram.Instance (parseInstance)
import Modelling.PetriNet.Types (PetriLike(allNodes), petriLikeToPetri)

import Language.Alloy.Call (getInstances)

import Data.Either (isRight)
import Data.List (sort)
import Test.Hspec(Spec, context, describe, it, shouldBe)

spec :: Spec
spec =
  describe "convertToPetriNet" $
    context "on a list of generated diagrams" $ do
      let spec' = adConfigToAlloy "" "" defaultAdConfig
      it "generates a Petri net with ascending labels" $ do
        inst <- getInstances (Just 50) spec'
        petri <- mapM (fmap convertToSimple . parseInstance) inst
        all checkLabels petri `shouldBe` (True::Bool)
      it "generates only valid Petri nets" $ do
        inst <- getInstances (Just 50) spec'
        petri <- mapM
          (fmap (petriLikeToPetri . convertToPetriNet) . parseInstance)
          inst
        all isRight petri `shouldBe` (True::Bool)

checkLabels :: PetriLike n PetriKey -> Bool
checkLabels petri =
  let labels = sort $ map label $ M.keys $ allNodes petri
  in labels == [1..(length labels)]
