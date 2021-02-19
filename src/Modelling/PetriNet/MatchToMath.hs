{-# LANGUAGE DeriveGeneric #-}
{-# Language DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# Language QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Modelling.PetriNet.MatchToMath (
  Graph,
  Math,
  MathConfig (..),
  checkConfig,
  defaultMathConfig,
  graphToMath,
  matchToMathTask,
  mathToGraph,
  petriNetRnd,
  )  where

import qualified Data.Map                         as M (
  foldrWithKey, keys, lookup, partition
  )

import Modelling.PetriNet.Alloy (
  compAdvConstraints,
  compBasicConstraints,
  compChange,
  moduleHelpers,
  modulePetriAdditions,
  modulePetriConcepts,
  modulePetriConstraints,
  modulePetriSignature,
  petriScopeBitwidth,
  petriScopeMaxSeq,
  taskInstance,
  )
import Modelling.PetriNet.BasicNetFunctions (
  checkBasicConfig, checkChangeConfig
  )
import Modelling.PetriNet.Diagram       (drawNet)
import Modelling.PetriNet.LaTeX         (toPetriMath)
import Modelling.PetriNet.Parser (
  parseChange, parseRenamedPetriLike,
  )
import Modelling.PetriNet.Types (
  AdvConfig,
  AlloyConfig,
  BasicConfig (..),
  Change,
  ChangeConfig (..),
  PetriLike (..),
  PetriMath,
  defaultAdvConfig,
  defaultAlloyConfig,
  defaultBasicConfig,
  defaultChangeConfig,
  flowIn, initial, isPlaceNode,
  mapChange,
  )

import Control.Applicative              (Alternative ((<|>)))
import Control.Monad.Random             (StdGen, evalRandT)
import Control.Monad.Trans.Class        (lift)
import Control.Monad.Trans.Except       (ExceptT, except)
import Data.GraphViz                    (GraphvizCommand)
import Data.String.Interpolate          (i)
import Diagrams.Backend.SVG             (B)
import Diagrams.Prelude                     (Diagram)
import GHC.Generics                     (Generic)
import Image.LaTeX.Render               (Formula)
import Language.Alloy.Call (
  AlloyInstance, getInstances, objectName,
  )
import System.Random.Shuffle            (shuffleM)

type Math  = PetriMath Formula
type Graph = Diagram B

data MathConfig = MathConfig {
  basicConfig :: BasicConfig,
  advConfig :: AdvConfig,
  changeConfig :: ChangeConfig,
  generatedWrongInstances :: Int,
  wrongInstances :: Int,
  alloyConfig :: AlloyConfig
  } deriving (Generic, Show)

defaultMathConfig :: MathConfig
defaultMathConfig = MathConfig {
  basicConfig = defaultBasicConfig,
  advConfig = defaultAdvConfig,
  changeConfig = defaultChangeConfig {
    tokenChangeOverall = 0,
    maxTokenChangePerPlace = 0
    },
  generatedWrongInstances = 50,
  wrongInstances = 3,
  alloyConfig = defaultAlloyConfig
  }

graphToMath
  :: MathConfig
  -> Int
  -> Int
  -> ExceptT String IO (Diagram B, Math, [(PetriMath Formula, Change)])
graphToMath = matchToMath toMath
  where
    toMath x = except $
      toPetriMath <$> parseRenamedPetriLike "flow" "tokens" x

mathToGraph
  :: MathConfig
  -> Int
  -> Int
  -> ExceptT String IO (Diagram B, Math, [(Diagram B, Change)])
mathToGraph c = matchToMath draw c
  where
    draw x = do
      pl <- except $ parseRenamedPetriLike "flow" "tokens" x
      drawNet id pl (graphLayout $ basicConfig c)

matchToMath
  :: (AlloyInstance -> ExceptT String IO a)
  -> MathConfig
  -> Int
  -> Int
  -> ExceptT String IO (Diagram B, Math, [(a, Change)])
matchToMath toOutput config segment seed = do
  ((f, net, math), g) <- netMath config segment seed
  fList <- lift $ getInstances (Just $ toInteger $ generatedWrongInstances config) f
  fList' <- take (wrongInstances config) <$> evalRandT (shuffleM fList) g
  alloyChanges <- except $ mapM addChange fList'
  changes <- firstM toOutput `mapM` alloyChanges
  return (net, math, changes)

netMath
  :: MathConfig
  -> Int
  -> Int
  -> ExceptT String IO ((String, Diagram B, Math), StdGen)
netMath config = taskInstance
  mathInstance
  (\c -> petriNetRnd (basicConfig c) (advConfig c))
  config
  (\c -> graphLayout $ basicConfig (c :: MathConfig))
  (\c -> alloyConfig (c :: MathConfig))
  config

mathInstance
  :: MathConfig
  -> AlloyInstance
  -> GraphvizCommand
  -> ExceptT String IO (String, Diagram B, Math)
mathInstance config inst gc = do
  petriLike <- except $ parseRenamedPetriLike "flow" "tokens" inst
  rightNet  <- drawNet id petriLike gc
  let math = toPetriMath petriLike
  let f = renderFalse petriLike config
  return (f, rightNet, math)

matchToMathTask :: Bool -> String
matchToMathTask switch =
  if switch
  then "Which of the presented Petri nets shows the mathematical expression?"
  else "Which of the presented mathematical expressions shows the given Petri net?"

firstM :: Monad m => (a -> m b) -> (a, c) -> m (b, c)
firstM f (p, c) = (,c) <$> f p

checkConfig :: MathConfig -> Maybe String
checkConfig c@MathConfig {
  basicConfig,
  changeConfig
  } = checkBasicConfig basicConfig
  <|> checkChangeConfig basicConfig changeConfig
  <|> checkMathConfig c

checkMathConfig :: MathConfig -> Maybe String
checkMathConfig MathConfig {
  generatedWrongInstances,
  wrongInstances
  }
  | wrongInstances < 1
  = Just "There has to be at least one wrongInstance"
  | generatedWrongInstances < wrongInstances
  = Just "generatedWrongInstances has to be higher than wrongInstances"
  | otherwise
  = Nothing

addChange :: AlloyInstance -> Either String (AlloyInstance, Change)
addChange alloy = do
  change <- parseChange alloy
  return (alloy, mapChange (takeWhile (/= '$') . objectName) change)

petriNetRnd :: BasicConfig -> AdvConfig -> String
petriNetRnd basicC@BasicConfig{places,transitions} advConfig = [i|module PetriNetRnd

#{modulePetriSignature}
#{modulePetriAdditions}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}

fact{
  no givenPlaces
  no givenTransitions
}

pred showNets[#{activated} : set Transitions] {
  \#Places = #{places}
  \#Transitions = #{transitions}
  #{compBasicConstraints activated basicC}
  #{compAdvConstraints advConfig}
}
run showNets for exactly #{petriScopeMaxSeq basicC} Nodes, #{petriScopeBitwidth basicC} Int
|]
  where
    activated = "activatedTrans"

renderFalse :: PetriLike String -> MathConfig -> String
renderFalse
  PetriLike  {allNodes}
  MathConfig {basicConfig, advConfig, changeConfig} = [i|module FalseNet

#{modulePetriSignature}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}

#{places}
#{transitions}

fact{
#{initialMark}
#{defaultFlow}
}

pred showFalseNets[#{activated} : set Transitions]{
  #{compBasicConstraints activated basicConfig}
  #{compAdvConstraints advConfig}
  #{compChange changeConfig}
}

run showFalseNets for exactly #{petriScopeMaxSeq basicConfig} Nodes, #{petriScopeBitwidth basicConfig} Int
|]
  where
    (ps, ts)    = M.partition isPlaceNode allNodes
    activated   = "activatedTrans"
    places      = unlines [extendLine p "givenPlaces" | p <- M.keys ps]
    transitions = unlines [extendLine t "givenTransitions" | t <- M.keys ts]
    initialMark = M.foldrWithKey (\k -> (++) . tokenLine k) "" $ initial <$> ps
    defaultFlow = M.foldrWithKey (\k _ -> (printFlow k ++)) "" allNodes
    printFlow :: String -> String
    printFlow x = M.foldrWithKey
      (\y flow -> (++) $ flowLine x y $ M.lookup x $ flowIn flow)
      ""
      allNodes
    extendLine :: String -> String -> String
    extendLine n k = [i|one sig #{n} extends #{k}{}
|]
    tokenLine :: String -> Int -> String
    tokenLine k l = [i|  #{k}.defaultTokens = #{l}
|]
    flowLine :: String -> String -> Maybe Int -> String
    flowLine from to Nothing  = [i|  no #{from}.defaultFlow[#{to}]
|]
    flowLine from to (Just f) = [i|  #{from}.defaultFlow[#{to}] = #{f}
|]
