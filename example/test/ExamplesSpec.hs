-- |

module ExamplesSpec where

import Modelling.ActivityDiagram.EnterAS (
  checkEnterASConfig,
  )
import Modelling.ActivityDiagram.EnterAs.Config (
  task35,
  task36,
  )
import Modelling.ActivityDiagram.FindAuxiliaryPetriNodes (
  checkFindAuxiliaryPetriNodesConfig,
  )
import Modelling.ActivityDiagram.FindAuxiliaryPetriNodes.Config (
  task41,
  task42,
  )
import Modelling.ActivityDiagram.MatchAd (
  checkMatchAdConfig,
  )
import Modelling.ActivityDiagram.MatchAd.Config (
  task31,
  task32,
  )
import Modelling.ActivityDiagram.MatchPetri (
  checkMatchPetriConfig,
  )
import Modelling.ActivityDiagram.MatchPetri.Config (
  task39,
  task40,
  )
import Modelling.ActivityDiagram.SelectAS (
  checkSelectASConfig,
  )
import Modelling.ActivityDiagram.SelectAs.Config (
  task33,
  task34,
  )
import Modelling.ActivityDiagram.SelectPetri (
  checkSelectPetriConfig,
  )
import Modelling.ActivityDiagram.SelectPetri.Config (
  task37,
  task38,
  )
import Modelling.Auxiliary.Common (
  ShuffleInstance (taskInstance),
  )
import Modelling.CdOd.DifferentNames (
  checkDifferentNamesConfig,
  )
import Modelling.CdOd.DifferentNames.Config (
  task12,
  task13,
  task25,
  )
import Modelling.CdOd.MatchCdOd (
  checkMatchCdOdConfig,
  )
import Modelling.CdOd.MatchCdOd.Config (
  task14,
  task15,
  )
import Modelling.CdOd.NameCdError (
  checkNameCdErrorConfig,
  )
import Modelling.CdOd.NameCdError.Config (
  task09,
  task10,
  )
import Modelling.CdOd.RepairCd (
  checkRepairCdConfig,
  )
import Modelling.CdOd.RepairCd.Config (
  task07,
  task08,
  )
import Modelling.CdOd.SelectValidCd (
  checkSelectValidCdConfig,
  checkSelectValidCdInstance,
  )
import Modelling.CdOd.SelectValidCd.Config (
  task2023_05,
  task2023_06,
  task2024_06,
  task2024_07,
  task2024_08,
  )
import Modelling.CdOd.SelectValidCd.Instance (
  task2024_05,
  task2024_06picked,
  task2024_09,
  )
import Modelling.PetriNet.Concurrency (
  checkFindConcurrencyConfig,
  checkPickConcurrencyConfig,
  )
import Modelling.PetriNet.Conflict (
  checkPickConflictConfig,
  )
import Modelling.PetriNet.ConflictPlaces (
  checkFindConflictPlacesConfig,
  )
import Modelling.PetriNet.MatchToMath (
  checkMathConfig,
  )
import Modelling.PetriNet.PetriFindConcurrency.Config (
  task23,
  )
import Modelling.PetriNet.PetriFindConflictPlaces.Config (
  task24,
  task26,
  )
import Modelling.PetriNet.PetriGraphToMath.Config (
  task17,
  task18,
  )
import Modelling.PetriNet.PetriMathToGraph.Config (
  task19,
  task20,
  )
import Modelling.PetriNet.PetriPickConcurrency.Config (
  task21,
  )
import Modelling.PetriNet.PetriPickConflict.Config (
  task22,
  task16,
  )

import Test.Hspec

spec :: Spec
spec =
  describe "check config" $ do
    describe "2023" $ do
      it "task05" $
        checkSelectValidCdConfig task2023_05 `shouldBe` Nothing
      it "task06" $
        checkSelectValidCdConfig task2023_06 `shouldBe` Nothing
      it "task07" $
        checkRepairCdConfig task07 `shouldBe` Nothing
      it "task08" $
        checkRepairCdConfig task08 `shouldBe` Nothing
      it "task09" $
        checkNameCdErrorConfig task09 `shouldBe` Nothing
      it "task10" $
        checkNameCdErrorConfig task10 `shouldBe` Nothing
      it "task12" $
        checkDifferentNamesConfig task12 `shouldBe` Nothing
      it "task13" $
        checkDifferentNamesConfig task13 `shouldBe` Nothing
      it "task14" $
        checkMatchCdOdConfig task14 `shouldBe` Nothing
      it "task15" $
        checkMatchCdOdConfig task15 `shouldBe` Nothing
      it "task16" $
        checkPickConflictConfig task16 `shouldBe` Nothing
      it "task17" $
        checkMathConfig task17 `shouldBe` Nothing
      it "task18" $
        checkMathConfig task18 `shouldBe` Nothing
      it "task19" $
        checkMathConfig task19 `shouldBe` Nothing
      it "task20" $
        checkMathConfig task20 `shouldBe` Nothing
      it "task21" $
        checkPickConcurrencyConfig task21 `shouldBe` Nothing
      it "task22" $
        checkPickConflictConfig task22 `shouldBe` Nothing
      it "task23" $
        checkFindConcurrencyConfig task23 `shouldBe` Nothing
      it "task24" $
        checkFindConflictPlacesConfig task24 `shouldBe` Nothing
      it "task25" $
        checkDifferentNamesConfig task25 `shouldBe` Nothing
      it "task26" $
        checkFindConflictPlacesConfig task26 `shouldBe` Nothing
      it "task31" $
        checkMatchAdConfig task31 `shouldBe` Nothing
      it "task32" $
        checkMatchAdConfig task32 `shouldBe` Nothing
      it "task33" $
        checkSelectASConfig task33 `shouldBe` Nothing
      it "task34" $
        checkSelectASConfig task34 `shouldBe` Nothing
      it "task35" $
        checkEnterASConfig task35 `shouldBe` Nothing
      it "task36" $
        checkEnterASConfig task36 `shouldBe` Nothing
      it "task37" $
        checkSelectPetriConfig task37 `shouldBe` Nothing
      it "task38" $
        checkSelectPetriConfig task38 `shouldBe` Nothing
      it "task39" $
        checkMatchPetriConfig task39 `shouldBe` Nothing
      it "task40" $
        checkMatchPetriConfig task40 `shouldBe` Nothing
      it "task41" $
        checkFindAuxiliaryPetriNodesConfig task41 `shouldBe` Nothing
      it "task42" $
        checkFindAuxiliaryPetriNodesConfig task42 `shouldBe` Nothing
    describe "2024" $ do
      it "task05" $
        checkSelectValidCdInstance task2024_05 `shouldBe` Nothing
      it "task06" $
        checkSelectValidCdConfig task2024_06 `shouldBe` Nothing
      it "task06 picked" $
        checkSelectValidCdInstance (taskInstance task2024_06picked)
        `shouldBe` Nothing
      it "task07" $
        checkSelectValidCdConfig task2024_07 `shouldBe` Nothing
      it "task08" $
        checkSelectValidCdConfig task2024_08 `shouldBe` Nothing
      it "task09" $
        checkSelectValidCdInstance (taskInstance task2024_09) `shouldBe` Nothing
