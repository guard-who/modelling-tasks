{-# Language DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Modelling.PetriNet.MatchToMath (
  checkConfig, matchToMath, matchToMathTask,
  )  where

import Modelling.PetriNet.Alloy             (petriNetRnd, renderFalse)
import Modelling.PetriNet.BasicNetFunctions (checkBasicConfig,checkChangeConfig)
import Modelling.PetriNet.Diagram       (drawNet)
import Modelling.PetriNet.LaTeX         (toPetriMath)
import Modelling.PetriNet.Parser (
  convertPetri, parseChange, parsePetriLike, simpleRename,
  )
import Modelling.PetriNet.Types (
  traversePetriLike,
  BasicConfig (graphLayout),
  Change,
  MathConfig (..),
  PetriMath,
  )

import Control.Monad.Trans.Class        (lift)
import Control.Monad.Trans.Except       (ExceptT, except)
import Diagrams.Backend.SVG             (B)
import Diagrams.Prelude                     (Diagram)
import Image.LaTeX.Render               (Formula)
import Language.Alloy.Call                  (AlloyInstance,getInstances)
import Maybes                               (firstJusts)

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
  petriLike <- except $ parsePetriLike "flow" "tokens" (list !! indInst)
  named     <- except $ simpleRename `traversePetriLike` petriLike
  petri     <- except $ convertPetri "flow" "tokens" (list !! indInst)
  rightNet  <- drawNet petriLike (graphLayout basicTask)
  let math = toPetriMath petri
  let f = renderFalse named config
  fList <- lift $ getInstances (Just 3) f
  alloyChanges <- except $ mapM addChange fList
  if switch
    then do
    let draw x = do
          pl <- except $ parsePetriLike "flow" "tokens" x
          drawNet pl (graphLayout basicTask)
    drawChanges <- firstM draw `mapM` alloyChanges
    return (rightNet, math, Left drawChanges)
    else do
    let toMath x = toPetriMath <$> convertPetri "flow" "tokens" x
    mathChanges <- except $ firstM toMath `mapM` alloyChanges
    return (rightNet, math, Right mathChanges)

matchToMathTask :: Bool -> String
matchToMathTask switch =
  if switch
  then "Which of the presented petrinets shows the mathematical expression?"
  else "Which of the presented mathematical expressions shows the given petrinet?"

firstM :: Monad m => (a -> m b) -> (a, c) -> m (b, c)
firstM f (p, c) = f p >>= (return . (,c))

checkConfig :: MathConfig -> Maybe String
checkConfig MathConfig{basicTask,changeTask} = 
  firstJusts [checkBasicConfig basicTask, checkChangeConfig basicTask changeTask]

addChange :: AlloyInstance -> Either String (AlloyInstance, Change)
addChange alloy = do
  change <- parseChange alloy
  return (alloy, change)
