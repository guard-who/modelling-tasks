-- |

module ExamplesSpec where

import qualified Modelling.Auxiliary.Shuffle.All as All (
  ShuffleInstance (taskInstance),
  )

import Modelling.ActivityDiagram.EnterAS (
  checkEnterASConfig,
  )
import Modelling.ActivityDiagram.EnterAs.Config (
  task2023_35,
  task2023_36,
  task2024_41,
  task2024_42,
  task2024_68,
  task2024_69,
  )
import Modelling.ActivityDiagram.FindAuxiliaryPetriNodes (
  checkFindAuxiliaryPetriNodesConfig,
  )
import Modelling.ActivityDiagram.FindAuxiliaryPetriNodes.Config (
  task2023_41,
  task2023_42,
  task2024_47,
  task2024_48,
  task2024_72,
  task2024_73,
  )
import Modelling.ActivityDiagram.MatchAd (
  checkMatchAdConfig,
  )
import Modelling.ActivityDiagram.MatchAd.Config (
  task2023_31,
  task2023_32,
  task2024_37,
  task2024_38,
  task2024_67,
  )
import Modelling.ActivityDiagram.MatchPetri (
  checkMatchPetriConfig,
  )
import Modelling.ActivityDiagram.MatchPetri.Config (
  task2023_39,
  task2023_40,
  task2024_45,
  task2024_46,
  task2024_70,
  task2024_71,
  )
import Modelling.ActivityDiagram.SelectAS (
  checkSelectASConfig,
  )
import Modelling.ActivityDiagram.SelectAs.Config (
  task2023_33,
  task2023_34,
  task2024_39,
  task2024_40,
  )
import Modelling.ActivityDiagram.SelectPetri (
  checkSelectPetriConfig,
  )
import Modelling.ActivityDiagram.SelectPetri.Config (
  task2023_37,
  task2023_38,
  task2024_43,
  task2024_44,
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
  task2024_56,
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
  task2024_57,
  task2024_58,
  task2024_59,
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
  task2024_54,
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
  task2024_55,
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
  task2024_51,
  task2024_52,
  )
import Modelling.CdOd.SelectValidCd.Instance (
  task2024_05,
  task2024_06picked,
  task2024_09,
  task2024_53,
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
  task2024_62,
  task2024_63,
  )
import Modelling.PetriNet.PetriFindConflictPlaces.Config (
  task2023_24,
  task2023_26,
  task2024_34,
  task2024_35,
  task2024_36,
  task2024_64,
  task2024_65,
  task2024_66,
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
        checkMatchAdConfig task2023_31 `shouldBe` Nothing
      it "task32" $
        checkMatchAdConfig task2023_32 `shouldBe` Nothing
      it "task33" $
        checkSelectASConfig task2023_33 `shouldBe` Nothing
      it "task34" $
        checkSelectASConfig task2023_34 `shouldBe` Nothing
      it "task35" $
        checkEnterASConfig task2023_35 `shouldBe` Nothing
      it "task36" $
        checkEnterASConfig task2023_36 `shouldBe` Nothing
      it "task37" $
        checkSelectPetriConfig task2023_37 `shouldBe` Nothing
      it "task38" $
        checkSelectPetriConfig task2023_38 `shouldBe` Nothing
      it "task39" $
        checkMatchPetriConfig task2023_39 `shouldBe` Nothing
      it "task40" $
        checkMatchPetriConfig task2023_40 `shouldBe` Nothing
      it "task41" $
        checkFindAuxiliaryPetriNodesConfig task2023_41 `shouldBe` Nothing
      it "task42" $
        checkFindAuxiliaryPetriNodesConfig task2023_42 `shouldBe` Nothing
    describe "2024" $ do
      it "task05" $
        checkSelectValidCdInstance task2024_05 `shouldBe` Nothing
      it "task06" $
        checkSelectValidCdConfig task2024_06 `shouldBe` Nothing
      it "task06 picked" $
        checkSelectValidCdInstance (All.taskInstance task2024_06picked)
        `shouldBe` Nothing
      it "task07" $
        checkSelectValidCdConfig task2024_07 `shouldBe` Nothing
      it "task08" $
        checkSelectValidCdConfig task2024_08 `shouldBe` Nothing
      it "task09" $
        checkSelectValidCdInstance (All.taskInstance task2024_09)
        `shouldBe` Nothing
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
      it "task37" $
        checkMatchAdConfig task2024_37 `shouldBe` Nothing
      it "task38" $
        checkMatchAdConfig task2024_38 `shouldBe` Nothing
      it "task39" $
        checkSelectASConfig task2024_39 `shouldBe` Nothing
      it "task40" $
        checkSelectASConfig task2024_40 `shouldBe` Nothing
      it "task41" $
        checkEnterASConfig task2024_41 `shouldBe` Nothing
      it "task42" $
        checkEnterASConfig task2024_42 `shouldBe` Nothing
      it "task43" $
        checkSelectPetriConfig task2024_43 `shouldBe` Nothing
      it "task44" $
        checkSelectPetriConfig task2024_44 `shouldBe` Nothing
      it "task45" $
        checkMatchPetriConfig task2024_45 `shouldBe` Nothing
      it "task46" $
        checkMatchPetriConfig task2024_46 `shouldBe` Nothing
      it "task47" $
        checkFindAuxiliaryPetriNodesConfig task2024_47 `shouldBe` Nothing
      it "task48" $
        checkFindAuxiliaryPetriNodesConfig task2024_48 `shouldBe` Nothing
      it "task51" $
        checkSelectValidCdConfig task2024_51 `shouldBe` Nothing
      it "task52" $
        checkSelectValidCdConfig task2024_52 `shouldBe` Nothing
      it "task53" $
        checkSelectValidCdInstance (All.taskInstance task2024_53)
        `shouldBe` Nothing
      it "task54" $
        checkNameCdErrorConfig task2024_54 `shouldBe` Nothing
      it "task55" $
        checkRepairCdConfig task2024_55 `shouldBe` Nothing
      it "task56" $
        checkDifferentNamesConfig task2024_56 `shouldBe` Nothing
      it "task57" $
        checkMatchCdOdConfig task2024_57 `shouldBe` Nothing
      it "task58" $
        checkMatchCdOdConfig task2024_58 `shouldBe` Nothing
      it "task59" $
        checkMatchCdOdConfig task2024_59 `shouldBe` Nothing
      it "task62" $
        checkFindConcurrencyConfig task2024_62 `shouldBe` Nothing
      it "task63" $
        checkFindConcurrencyConfig task2024_63 `shouldBe` Nothing
      it "task64" $
        checkFindConflictPlacesConfig task2024_64 `shouldBe` Nothing
      it "task65" $
        checkFindConflictPlacesConfig task2024_65 `shouldBe` Nothing
      it "task66" $
        checkFindConflictPlacesConfig task2024_66 `shouldBe` Nothing
      it "task67" $
        checkMatchAdConfig task2024_67 `shouldBe` Nothing
      it "task68" $
        checkEnterASConfig task2024_68 `shouldBe` Nothing
      it "task69" $
        checkEnterASConfig task2024_69 `shouldBe` Nothing
      it "task70" $
        checkMatchPetriConfig task2024_70 `shouldBe` Nothing
      it "task71" $
        checkMatchPetriConfig task2024_71 `shouldBe` Nothing
      it "task72" $
        checkFindAuxiliaryPetriNodesConfig task2024_72 `shouldBe` Nothing
      it "task73" $
        checkFindAuxiliaryPetriNodesConfig task2024_73 `shouldBe` Nothing
