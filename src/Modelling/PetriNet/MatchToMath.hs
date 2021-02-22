{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# Language DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# Language QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Modelling.PetriNet.MatchToMath (
  Math,
  MathConfig (..),
  MatchInstance (..),
  checkMathConfig,
  defaultMathConfig,
  graphToMath,
  graphToMathGenerate,
  matchToMathTask,
  mathToGraph,
  mathToGraphGenerate,
  petriNetRnd,
  renderFormula,
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
  PetriMath (..),
  defaultAdvConfig,
  defaultAlloyConfig,
  defaultBasicConfig,
  defaultChangeConfig,
  flowIn, initial, isPlaceNode,
  mapChange,
  )

import Control.Applicative              (Alternative ((<|>)))
import Control.Monad.Random             (RandT, RandomGen, StdGen, evalRandT, mkStdGen)
import Control.Monad.Trans.Class        (lift)
import Control.Monad.Trans.Except       (ExceptT (ExceptT), except)
import Data.Bifoldable                  (Bifoldable (bifoldMap))
import Data.Bifunctor                   (Bifunctor (bimap, second))
import Data.Bitraversable               (Bitraversable, bimapM)
import Data.GraphViz                    (GraphvizCommand)
import Data.Map                         (Map, fromList)
import Data.String.Interpolate          (i)
import Diagrams.Backend.SVG             (B, renderSVG)
import Diagrams.Prelude                 (Diagram, mkWidth)
import GHC.Generics                     (Generic)
import Image.LaTeX.Render               (alterForHTML, imageForFormula, defaultFormulaOptions, defaultEnv, SVG, Formula)
import Language.Alloy.Call (
  AlloyInstance, getInstances, objectName,
  )
import System.Random.Shuffle            (shuffleM)

type Math = PetriMath Formula

class NamedParts n where
  addPartNames :: n a -> n (String, a)

instance NamedParts PetriMath where
  addPartNames pm = PetriMath {
    netMath            = ("net", netMath pm),
    placesMath         = ("places", placesMath pm),
    transitionsMath    = ("transitions", transitionsMath pm),
    tokenChangeMath    =
        [(("in" ++ show n, x), ("out" ++ show n, y))
        | (n, (x, y)) <- zip [1 :: Integer ..] $ tokenChangeMath pm],
    initialMarkingMath = ("marking", initialMarkingMath pm)
    }

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

data MatchInstance a b = MatchInstance {
  from :: a,
  to :: Map Int (Bool, b)
  }
  deriving (Bitraversable, Generic, Show)

instance Bifoldable MatchInstance where
  bifoldMap f g mi = f (from mi) <> foldMap (g . snd) (to mi)

instance Bifunctor MatchInstance where
  bimap f g mi = MatchInstance (f $ from mi) (second g <$> to mi)

renderFormula :: String -> ExceptT String IO SVG
renderFormula = ExceptT . (bimap show alterForHTML <$>)
  . imageForFormula defaultEnv defaultFormulaOptions

evalRandTWith
  :: Int
  -> RandT StdGen (ExceptT String IO) a
  -> ExceptT String IO a
evalRandTWith = flip evalRandT . mkStdGen

graphToMathGenerate
  :: MathConfig
  -> String
  -> Int
  -> Int
  -> ExceptT String IO (MatchInstance FilePath (PetriMath FilePath))
graphToMathGenerate config path segment seed = do
  inst <- graphToMath config segment seed
  bimapM (writeGraph path) (mapM (writeFormula path) . addPartNames) inst

mathToGraphGenerate
  :: MathConfig
  -> String
  -> Int
  -> Int
  -> ExceptT String IO (MatchInstance (PetriMath FilePath) FilePath)
mathToGraphGenerate config path segment seed = do
  inst <- mathToGraph config segment seed
  bimapM (mapM (writeFormula path) . addPartNames) (writeGraph path) inst

writeGraph
  :: String
  -> Diagram B
  -> ExceptT String IO FilePath
writeGraph path d = do
  let file = path ++ "graph.svg"
  lift $ renderSVG file (mkWidth 250) d >> return file

writeFormula
  :: String
  -> (String, String)
  -> ExceptT String IO FilePath
writeFormula path (name, f) = do
  let file = path ++ name ++ ".svg"
  svg <- renderFormula f
  lift $ writeFile file svg
  return file

graphToMath
  :: MathConfig
  -> Int
  -> Int
  -> ExceptT String IO (MatchInstance (Diagram B) Math)
graphToMath c segment seed = evalRandTWith seed $ do
  (d, m, ms) <- matchToMath toMath c segment
  ms' <- shuffleM $ (True, m) : [(False, x) | (x, _) <- ms]
  return $ MatchInstance d $ fromList $ zip [1..] ms'
  where
    toMath x = except $
      toPetriMath <$> parseRenamedPetriLike "flow" "tokens" x

mathToGraph
  :: MathConfig
  -> Int
  -> Int
  -> ExceptT String IO (MatchInstance Math (Diagram B))
mathToGraph c segment seed = evalRandTWith seed $ do
  (d, m, ds) <- matchToMath draw c segment
  ds' <- shuffleM $ (True, d) : [(False, x) | (x, _) <- ds]
  return $ MatchInstance m $ fromList $ zip [1..] ds'
  where
    draw x = do
      pl <- except $ parseRenamedPetriLike "flow" "tokens" x
      drawNet id pl (graphLayout $ basicConfig c)

matchToMath
  :: RandomGen g
  => (AlloyInstance -> ExceptT String IO a)
  -> MathConfig
  -> Int
  -> RandT g (ExceptT String IO) (Diagram B, Math, [(a, Change)])
matchToMath toOutput config segment = do
  (f, net, math) <- netMathInstance config segment
  fList <- lift $ lift $ getInstances (Just $ toInteger $ generatedWrongInstances config) f
  fList' <- take (wrongInstances config) <$> shuffleM fList
  alloyChanges <- lift $ except $ mapM addChange fList'
  changes <- lift $ firstM toOutput `mapM` alloyChanges
  return (net, math, changes)

netMathInstance
  :: RandomGen g
  => MathConfig
  -> Int
  -> RandT g (ExceptT String IO) (String, Diagram B, Math)
netMathInstance config = taskInstance
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

checkMathConfig :: MathConfig -> Maybe String
checkMathConfig c@MathConfig {
  basicConfig,
  changeConfig
  } = checkBasicConfig basicConfig
  <|> checkChangeConfig basicConfig changeConfig
  <|> checkConfig c

checkConfig :: MathConfig -> Maybe String
checkConfig MathConfig {
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
