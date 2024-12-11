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
  task2023_12,
  task2023_13,
  task2023_25,
  task2024_15,
  task2024_16,
  )
import Modelling.CdOd.MatchCdOd (
  checkMatchCdOdConfig,
  )
import Modelling.CdOd.MatchCdOd.Config (
  task2023_14,
  task2023_15,
  task2024_17,
  task2024_18,
  task2024_19,
  task2024_20,
  )
import Modelling.CdOd.NameCdError (
  checkNameCdErrorConfig,
  checkNameCdErrorInstance,
  )
import Modelling.CdOd.NameCdError.Config (
  task2023_09,
  task2023_10,
  task2024_10,
  task2024_11,
  )
import Modelling.CdOd.NameCdError.Instance (
  task2024_14,
  )
import Modelling.CdOd.RepairCd (
  checkRepairCdConfig,
  )
import Modelling.CdOd.RepairCd.Config (
  task2023_07,
  task2023_08,
  task2024_12,
  task2024_13,
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
  checkGraphToMathConfig,
  checkMathConfig,
  )
import Modelling.PetriNet.PetriFindConcurrency.Config (
  task2023_23,
  task2024_32,
  task2024_33,
  )
import Modelling.PetriNet.PetriFindConflictPlaces.Config (
  task2023_24,
  task2023_26,
  task2024_34,
  task2024_35,
  task2024_36,
  )
import Modelling.PetriNet.PetriGraphToMath.Config (
  task2023_17,
  task2023_18,
  task2024_21,
  task2024_22,
  )
import Modelling.PetriNet.PetriMathToGraph.Config (
  task2023_19,
  task2023_20,
  task2024_23,
  task2024_24,
  )
import Modelling.PetriNet.PetriPickConcurrency.Config (
  task2023_21,
  task2024_29,
  )
import Modelling.PetriNet.PetriPickConflict.Config (
  task2023_22,
  task2023_16,
  task2024_30,
  task2024_31,
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
        checkRepairCdConfig task2023_07 `shouldBe` Nothing
      it "task08" $
        checkRepairCdConfig task2023_08 `shouldBe` Nothing
      it "task09" $
        checkNameCdErrorConfig task2023_09 `shouldBe` Nothing
      it "task10" $
        checkNameCdErrorConfig task2023_10 `shouldBe` Nothing
      it "task12" $
        checkDifferentNamesConfig task2023_12 `shouldBe` Nothing
      it "task13" $
        checkDifferentNamesConfig task2023_13 `shouldBe` Nothing
      it "task14" $
        checkMatchCdOdConfig task2023_14 `shouldBe` Nothing
      it "task15" $
        checkMatchCdOdConfig task2023_15 `shouldBe` Nothing
      it "task16" $
        checkPickConflictConfig task2023_16 `shouldBe` Nothing
      it "task17" $
        checkGraphToMathConfig task2023_17 `shouldBe` Nothing
      it "task18" $
        checkGraphToMathConfig task2023_18 `shouldBe` Nothing
      it "task19" $
        checkMathConfig task2023_19 `shouldBe` Nothing
      it "task20" $
        checkMathConfig task2023_20 `shouldBe` Nothing
      it "task21" $
        checkPickConcurrencyConfig task2023_21 `shouldBe` Nothing
      it "task22" $
        checkPickConflictConfig task2023_22 `shouldBe` Nothing
      it "task23" $
        checkFindConcurrencyConfig task2023_23 `shouldBe` Nothing
      it "task24" $
        checkFindConflictPlacesConfig task2023_24 `shouldBe` Nothing
      it "task25" $
        checkDifferentNamesConfig task2023_25 `shouldBe` Nothing
      it "task26" $
        checkFindConflictPlacesConfig task2023_26 `shouldBe` Nothing
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
      it "task10" $
        checkNameCdErrorConfig task2024_10 `shouldBe` Nothing
      it "task11" $
        checkNameCdErrorConfig task2024_11 `shouldBe` Nothing
      it "task12" $
        checkRepairCdConfig task2024_12 `shouldBe` Nothing
      it "task13" $
        checkRepairCdConfig task2024_13 `shouldBe` Nothing
      it "task14" $
        checkNameCdErrorInstance task2024_14 `shouldBe` Nothing
      it "task15" $
        checkDifferentNamesConfig task2024_15 `shouldBe` Nothing
      it "task16" $
        checkDifferentNamesConfig task2024_16 `shouldBe` Nothing
      it "task17" $
        checkMatchCdOdConfig task2024_17 `shouldBe` Nothing
      it "task18" $
        checkMatchCdOdConfig task2024_18 `shouldBe` Nothing
      it "task19" $
        checkMatchCdOdConfig task2024_19 `shouldBe` Nothing
      it "task20" $
        checkMatchCdOdConfig task2024_20 `shouldBe` Nothing
      it "task21" $
        checkGraphToMathConfig task2024_21 `shouldBe` Nothing
      it "task22" $
        checkGraphToMathConfig task2024_22 `shouldBe` Nothing
      it "task23" $
        checkMathConfig task2024_23 `shouldBe` Nothing
      it "task24" $
        checkMathConfig task2024_24 `shouldBe` Nothing
      it "task29" $
        checkPickConcurrencyConfig task2024_29 `shouldBe` Nothing
      it "task30" $
        checkPickConflictConfig task2024_30 `shouldBe` Nothing
      it "task31" $
        checkPickConflictConfig task2024_31 `shouldBe` Nothing
      it "task32" $
        checkFindConcurrencyConfig task2024_32 `shouldBe` Nothing
      it "task33" $
        checkFindConcurrencyConfig task2024_33 `shouldBe` Nothing
      it "task34" $
        checkFindConflictPlacesConfig task2024_34 `shouldBe` Nothing
      it "task35" $
        checkFindConflictPlacesConfig task2024_35 `shouldBe` Nothing
      it "task36" $
        checkFindConflictPlacesConfig task2024_36 `shouldBe` Nothing
