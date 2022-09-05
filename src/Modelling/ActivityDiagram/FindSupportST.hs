{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Modelling.ActivityDiagram.FindSupportST (
  FindSupportSTInstance(..),
  FindSupportSTConfig(..),
  FindSupportSTSolution(..),
  defaultFindSupportSTConfig,
  checkFindSupportSTConfig,
  findSupportSTSolution,
  findSupportSTAlloy,
  findSupportSTTaskDescription,
  findSupportSTText,
  findSupportSTTask,
  findSupportSTEvaluation,
  findSupportST,
  defaultFindSupportSTInstance
) where

import qualified Data.Map as M ((!), null, size, filter, filterWithKey, keys, fromList)

import Modelling.ActivityDiagram.Datatype (UMLActivityDiagram(..), ADNode(..), ADConnection(..))
import Modelling.ActivityDiagram.Petrinet (PetriKey(..), convertToPetrinet)
import Modelling.ActivityDiagram.Shuffle (shuffleADNames)
import Modelling.ActivityDiagram.Config (ADConfig(..), defaultADConfig, checkADConfig, adConfigToAlloy)
import Modelling.ActivityDiagram.Alloy (modulePetrinet)
import Modelling.ActivityDiagram.Instance (parseInstance)
import Modelling.ActivityDiagram.PlantUMLConverter (defaultPlantUMLConvConf, drawADToFile)

import Modelling.Auxiliary.Output (addPretext)
import Modelling.PetriNet.Types (PetriLike(..), Node(..), isPlaceNode, isTransitionNode)

import Control.Applicative (Alternative ((<|>)))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Output (
  LangM,
  Rated,
  OutputMonad (..),
  english,
  german,
  translate,
  translations,
  multipleChoice
  )
import Control.Monad.Random (
  MonadRandom (getRandom),
  RandT,
  RandomGen,
  evalRandT,
  mkStdGen
  )
import Data.Map (Map)
import Data.String.Interpolate ( i )
import Language.Alloy.Call (getInstances)
import System.Random.Shuffle (shuffleM)

data FindSupportSTInstance = FindSupportSTInstance {
  activityDiagram :: UMLActivityDiagram,
  seed :: Int
} deriving (Show)

data FindSupportSTConfig = FindSupportSTConfig {
  adConfig :: ADConfig,
  maxInstances :: Maybe Integer,
  activityFinalsExist :: Maybe Bool,        -- Option to disallow activity finals to reduce semantic confusion
  avoidAddingSinksForFinals :: Maybe Bool   -- Avoid having to add new sink transitions for representing finals
} deriving (Show)

defaultFindSupportSTConfig :: FindSupportSTConfig
defaultFindSupportSTConfig = FindSupportSTConfig
  { adConfig = defaultADConfig,
    maxInstances = Just 50,
    activityFinalsExist = Nothing,
    avoidAddingSinksForFinals = Nothing
  }

checkFindSupportSTConfig :: FindSupportSTConfig -> Maybe String
checkFindSupportSTConfig conf =
  checkADConfig (adConfig conf)
  <|> findSupportSTConfig' conf

findSupportSTConfig' :: FindSupportSTConfig -> Maybe String
findSupportSTConfig' FindSupportSTConfig {
    adConfig,
    activityFinalsExist,
    avoidAddingSinksForFinals
  }
  | activityFinalsExist == Just True && activityFinalNodes adConfig < 1
    = Just "Setting the parameter 'activityFinalsExist' to True implies having at least 1 Activity Final Node"
  | activityFinalsExist == Just False && activityFinalNodes adConfig > 0
    = Just "Setting the parameter 'activityFinalsExist' to False prohibits having more than 0 Activity Final Node"
  | avoidAddingSinksForFinals == Just True && minActions adConfig + forkJoinPairs adConfig < 1
    = Just "The option 'avoidAddingSinksForFinals' can only be achieved if the number of Actions, Fork Nodes and Join Nodes together is positive"
  | otherwise
    = Nothing

findSupportSTAlloy :: FindSupportSTConfig -> String
findSupportSTAlloy FindSupportSTConfig {
  adConfig,
  activityFinalsExist,
  avoidAddingSinksForFinals
}
  = adConfigToAlloy modules preds adConfig
  where modules = modulePetrinet
        preds =
          [i|
            not supportSTAbsent
            #{f activityFinalsExist "activityFinalsExist"}
            #{f avoidAddingSinksForFinals "avoidAddingSinksForFinals"}
          |]
        f opt s =
          case opt of
            Just True -> s
            Just False -> [i| not #{s}|]
            _ -> ""

findSupportSTTaskDescription :: String
findSupportSTTaskDescription =
  [i|
    Look at the given Activity Diagram and convert it to a petri net by hand,
    then:

    a) Enter the number of nodes of the resulting petri net
    b) Enter the number of support places of the resulting petri net
    c) Enter the number of support transitions of the resulting petri net
  |]

findSupportSTText :: FindSupportSTInstance -> (UMLActivityDiagram, String)
findSupportSTText inst =
  let (ad, solution) = findSupportSTSolution inst
      soltext = [i|
      Solutions for the FindSupportST-Task:

      a) Number of nodes of the resulting petri net: #{numberOfPetriNodes solution}
      b) Number of support places in the resulting petri net: #{numberOfSupportPlaces solution}
      c) Number of support transitions in the resulting petri net: #{numberOfSupportTransitions solution}
      |]
  in (ad, soltext)

data FindSupportSTSolution = FindSupportSTSolution {
  numberOfPetriNodes :: Int,
  numberOfSupportPlaces :: Int,
  numberOfSupportTransitions :: Int
} deriving (Show, Eq)

findSupportSTSolution :: FindSupportSTInstance -> (UMLActivityDiagram, FindSupportSTSolution)
findSupportSTSolution FindSupportSTInstance {
  activityDiagram,
  seed
} =
  let ad =  snd $ shuffleADNames seed activityDiagram
      solution = findSupportSTSolution' $ convertToPetrinet ad
  in (ad, solution)

findSupportSTSolution' :: PetriLike PetriKey -> FindSupportSTSolution
findSupportSTSolution' petri = FindSupportSTSolution {
    numberOfPetriNodes = M.size $ allNodes petri,
    numberOfSupportPlaces = M.size $ M.filter isPlaceNode supportSTMap,
    numberOfSupportTransitions = M.size $ M.filter isTransitionNode supportSTMap
  }
  where
    supportSTMap = M.filterWithKey (\k _ -> isSupportST k && not (isSinkST k petri)) $ allNodes petri

isSinkST :: PetriKey -> PetriLike PetriKey -> Bool
isSinkST key petri = M.null $ flowOut $ allNodes petri M.! key

isSupportST :: PetriKey -> Bool
isSupportST key =
  case key of
    SupportST {} -> True
    _ -> False

findSupportSTTask
  :: (OutputMonad m, MonadIO m)
  => FilePath
  -> FindSupportSTInstance
  -> LangM m
findSupportSTTask path task = do
  let (diag, _) = findSupportSTSolution task
  ad <- liftIO $ drawADToFile path defaultPlantUMLConvConf diag
  paragraph $ translate $ do
    english "Consider the following activity diagram."
    german "Betrachten Sie das folgende Aktivitätsdiagramm."
  image ad
  paragraph $ translate $ do
    english [i|Translate the given activity diagram into a petrinet, then state the total number of nodes,
the number of support places and the number of support transitions of the net.|]
    german [i|Übersetzen Sie das gegebene Aktivitätsdiagramm in ein Petrinetz, geben Sie anschließend die Gesamtanzahl
an Knoten, die Anzahl der Hilfsstellen und die Anzahl der Hilfstransitionen des Netzes an.|]
  paragraph $ do
    translate $ do
      english [i|To do this, enter your answer as in the following example.|]
      german [i|Geben Sie dazu Ihre Antwort wie im folgenden Beispiel an.|]
    code $ show findSupportSTInitial
    translate $ do
      english [i|In this example, the resulting net contains 10 nodes in total, with 2 support places and 3 support transitions.|]
      german [i|In diesem Beispiel enthält das entstehende Netz etwa 10 Knoten, davon 2 Hilfsstellen und 3 Hilfstransition.|]

findSupportSTInitial :: FindSupportSTSolution
findSupportSTInitial = FindSupportSTSolution {
  numberOfPetriNodes = 10,
  numberOfSupportPlaces = 2,
  numberOfSupportTransitions = 3
}

findSupportSTEvaluation
  :: OutputMonad m
  => FindSupportSTInstance
  -> FindSupportSTSolution
  -> Rated m
findSupportSTEvaluation task sub = addPretext $ do
  let as = translations $ do
        english "partial answers"
        german "Teilantworten"
      (_, sol) = findSupportSTSolution task
      solution = findSupportSTSolutionMap sol
      sub' = M.keys $ findSupportSTSolutionMap sub
  multipleChoice as (Just $ show sol) solution sub'

findSupportSTSolutionMap
  :: FindSupportSTSolution
  -> Map (Int, Int) Bool
findSupportSTSolutionMap sol =
  let xs = [
        numberOfPetriNodes sol,
        numberOfSupportPlaces sol,
        numberOfSupportTransitions sol
        ]
  in M.fromList $ zipWith (curry (,True)) [1..] xs

findSupportST
  :: FindSupportSTConfig
  -> Int
  -> Int
  -> IO FindSupportSTInstance
findSupportST config segment seed = do
  let g = mkStdGen $ (segment +) $ 4 * seed
  evalRandT (getFindSupportSTTask config) g

getFindSupportSTTask
  :: (RandomGen g, MonadIO m)
  => FindSupportSTConfig
  -> RandT g m FindSupportSTInstance
getFindSupportSTTask config = do
  instas <- liftIO $ getInstances (maxInstances config) $ findSupportSTAlloy config
  rinstas <- shuffleM instas
  let ad = map (failWith id . parseInstance "this" "this") rinstas
  g' <- getRandom
  return $ FindSupportSTInstance {
    activityDiagram=head ad,
    seed=g'
  }

failWith :: (a -> String) -> Either a c -> c
failWith f = either (error . f) id

defaultFindSupportSTInstance :: FindSupportSTInstance
defaultFindSupportSTInstance = FindSupportSTInstance {
  activityDiagram = UMLActivityDiagram {
    nodes = [
      ADActionNode {label = 1, name = "A"},
      ADActionNode {label = 2, name = "B"},
      ADActionNode {label = 3, name = "C"},
      ADActionNode {label = 4, name = "D"},
      ADObjectNode {label = 5, name = "E"},
      ADObjectNode {label = 6, name = "F"},
      ADObjectNode {label = 7, name = "G"},
      ADObjectNode {label = 8, name = "H"},
      ADDecisionNode {label = 9},
      ADDecisionNode {label = 10},
      ADMergeNode {label = 11},
      ADMergeNode {label = 12},
      ADForkNode {label = 13},
      ADJoinNode {label = 14},
      ADActivityFinalNode {label = 15},
      ADFlowFinalNode {label = 16},
      ADInitialNode {label = 17}
    ],
    connections = [
      ADConnection {from = 1, to = 14, guard = ""},
      ADConnection {from = 2, to = 11, guard = ""},
      ADConnection {from = 3, to = 14, guard = ""},
      ADConnection {from = 4, to = 9, guard = ""},
      ADConnection {from = 5, to = 11, guard = ""},
      ADConnection {from = 6, to = 10, guard = ""},
      ADConnection {from = 7, to = 16, guard = ""},
      ADConnection {from = 8, to = 4, guard = ""},
      ADConnection {from = 9, to = 2, guard = "a"},
      ADConnection {from = 9, to = 5, guard = "b"},
      ADConnection {from = 10, to = 8, guard = "b"},
      ADConnection {from = 10, to = 12, guard = "a"},
      ADConnection {from = 11, to = 15, guard = ""},
      ADConnection {from = 12, to = 6, guard = ""},
      ADConnection {from = 13, to = 1, guard = ""},
      ADConnection {from = 13, to = 3, guard = ""},
      ADConnection {from = 13, to = 7, guard = ""},
      ADConnection {from = 14, to = 12, guard = ""},
      ADConnection {from = 17, to = 13, guard = ""}
    ]
  },
  seed = 5508675034223564747
}