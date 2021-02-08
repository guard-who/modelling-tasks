{-# Language DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# Language QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Modelling.PetriNet.MatchToMath (
  Graph, Math,
  checkConfig, matchToMath, matchToMathTask,
  petriNetRnd,
  )  where

import qualified Data.Map                         as M (
  foldrWithKey, keys, lookup, partition
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
  BasicConfig (..),
  Change,
  MathConfig (..),
  PetriMath,
  PetriLike (..),
  flowIn, initial, isPlaceNode,
  mapChange,
  )

import Control.Applicative              (Alternative ((<|>)))
import Control.Monad.Trans.Class        (lift)
import Control.Monad.Trans.Except       (ExceptT, except)
import Data.String.Interpolate          (i)
import Diagrams.Backend.SVG             (B)
import Diagrams.Prelude                     (Diagram)
import Image.LaTeX.Render               (Formula)
import Language.Alloy.Call (
  AlloyInstance, getInstances, objectName,
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
  )

type Math  = PetriMath Formula
type Graph = Diagram B
--True Task1 <-> False Task1a
matchToMath
  :: Int
  -> Bool
  -> MathConfig
  -> ExceptT String IO (Graph, Math, Either [(Graph, Change)] [(Math, Change)])
matchToMath indInst switch config@MathConfig{basicTask,advTask} = do
  list <- lift $ getInstances (Just (toInteger (indInst+1))) (petriNetRnd basicTask advTask)
  petriLike <- except $ parseRenamedPetriLike "flow" "tokens" (list !! indInst)
  rightNet  <- drawNet id petriLike (graphLayout basicTask)
  let math = toPetriMath petriLike
  let f = renderFalse petriLike config
  fList <- lift $ getInstances (Just 3) f
  alloyChanges <- except $ mapM addChange fList
  if switch
    then do
    let draw x = do
          pl <- except $ parseRenamedPetriLike "flow" "tokens" x
          drawNet id pl (graphLayout basicTask)
    drawChanges <- firstM draw `mapM` alloyChanges
    return (rightNet, math, Left drawChanges)
    else do
    let toMath x = toPetriMath <$> parseRenamedPetriLike "flow" "tokens" x
    mathChanges <- except $ firstM toMath `mapM` alloyChanges
    return (rightNet, math, Right mathChanges)

matchToMathTask :: Bool -> String
matchToMathTask switch =
  if switch
  then "Which of the presented Petri nets shows the mathematical expression?"
  else "Which of the presented mathematical expressions shows the given Petri net?"

firstM :: Monad m => (a -> m b) -> (a, c) -> m (b, c)
firstM f (p, c) = (,c) <$> f p

checkConfig :: MathConfig -> Maybe String
checkConfig MathConfig{basicTask,changeTask} = 
  checkBasicConfig basicTask <|> checkChangeConfig basicTask changeTask

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
  MathConfig {basicTask, advTask, changeTask} = [i|module FalseNet

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
  #{compBasicConstraints activated basicTask}
  #{compAdvConstraints advTask}
  #{compChange changeTask}
}

run showFalseNets for exactly #{petriScopeMaxSeq basicTask} Nodes, #{petriScopeBitwidth basicTask} Int
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
