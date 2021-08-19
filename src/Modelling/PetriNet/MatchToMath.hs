{-# LANGUAGE DeriveGeneric #-}
{-# Language DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# Language QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Modelling.PetriNet.MatchToMath (
  GraphToMathInstance,
  Math,
  MathConfig (..),
  MatchInstance (..),
  MathToGraphInstance,
  checkMathConfig,
  defaultMathConfig,
  formulaFilesFrom,
  formulaFilesTo,
  graphToMath,
  graphToMathEvaluation,
  graphToMathGenerate,
  graphToMathTask,
  mathToGraph,
  mathToGraphEvaluation,
  mathToGraphGenerate,
  mathToGraphTask,
  petriNetRnd,
  renderFormula,
  )  where

import qualified Data.Map                         as M (
  filter, foldrWithKey, keys, lookup, partition,
  )

import Modelling.Auxiliary.Output       (
  LangM,
  OutputMonad (..),
  english,
  singleChoice,
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
import Data.Bitraversable               (Bitraversable (bitraverse), bimapM)
import Data.GraphViz                    (GraphvizCommand)
import Data.Map                         (Map, fromList, mapWithKey, toList)
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

type GraphToMathInstance = MatchInstance FilePath (PetriMath String)
type MathToGraphInstance = MatchInstance (PetriMath String) FilePath

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
    initialMarkingMath = ("marking", initialMarkingMath pm),
    placeOrderMath     = ("order",) <$> placeOrderMath pm
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
  deriving (Generic, Show)

instance Bifoldable MatchInstance where
  bifoldMap f g (MatchInstance x y) = f x `mappend` foldMap (g . snd) y

instance Bifunctor MatchInstance where
  bimap f g (MatchInstance x y) = MatchInstance (f x) (second g <$> y)

instance Bitraversable MatchInstance where
  bitraverse f g (MatchInstance x y) = MatchInstance
    <$> f x
    <*> traverse (traverse g) y

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
  -> ExceptT String IO GraphToMathInstance
graphToMathGenerate config path segment seed = do
  inst <- graphToMath config segment seed
  writeDia path inst

writeDia
  :: FilePath
  -> MatchInstance (Diagram B) b
  -> ExceptT String IO (MatchInstance FilePath b)
writeDia path = bimapM (writeGraph path "") return

mathToGraphGenerate
  :: MathConfig
  -> String
  -> Int
  -> Int
  -> ExceptT String IO MathToGraphInstance
mathToGraphGenerate config path segment seed = do
  inst <- mathToGraph config segment seed
  writeDias path inst

writeDias
  :: FilePath
  -> MatchInstance a (Diagram B)
  -> ExceptT String IO (MatchInstance a FilePath)
writeDias path inst =
  let inst' = MatchInstance {
        from = from inst,
        to   = mapWithKey (\k -> second (show k,)) $ to inst
        }
  in bimapM return (uncurry $ writeGraph path) inst'

{-# DEPRECATED formulaFilesTo "not used anymore writeDia used instead" #-}
formulaFilesTo
  :: (Traversable t, NamedParts t)
  => FilePath
  -> MatchInstance (Diagram B) (t String)
  -> ExceptT String IO (MatchInstance FilePath (t FilePath))
formulaFilesTo path inst =
  let inst' = MatchInstance {
        from = from inst,
        to   = mapWithKey (\k -> second ((show k,) . addPartNames)) $ to inst
        }
  in bimapM (writeGraph path "") (\(n, x) -> mapM (writeFormula path n) x) inst'

{-# DEPRECATED formulaFilesFrom "not used anymore writeDias used instead" #-}
formulaFilesFrom
  :: (NamedParts t, Traversable t)
  => FilePath
  -> MatchInstance (t String) (Diagram B)
  -> ExceptT String IO (MatchInstance (t FilePath) FilePath)
formulaFilesFrom path inst =
  let inst' = MatchInstance {
        from = addPartNames $ from inst,
        to   = mapWithKey (\k -> second (show k,)) $ to inst
        }
  in bimapM (mapM $ writeFormula path "") (uncurry $ writeGraph path) inst'

writeGraph
  :: FilePath
  -> String
  -> Diagram B
  -> ExceptT String IO FilePath
writeGraph path index d = do
  let file = path ++ "graph" ++ index ++ ".svg"
  lift $ renderSVG file (mkWidth 250) d
  return file

writeFormula
  :: FilePath
  -> String
  -> (String, String)
  -> ExceptT String IO FilePath
writeFormula path index (name, f) = do
  let file = path ++ index ++ "-" ++ name ++ ".svg"
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
      drawNet
        id
        pl
        (hidePlaceNames $ basicConfig c)
        (hideTransitionNames $ basicConfig c)
        (hideWeight1 $ basicConfig c)
        (graphLayout $ basicConfig c)

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
  if wrongInstances config == length fList'
    then do
    alloyChanges <- lift $ except $ mapM addChange fList'
    changes <- lift $ firstM toOutput `mapM` alloyChanges
    return (net, math, changes)
    else matchToMath toOutput config segment

firstM :: Monad m => (a -> m b) -> (a, c) -> m (b, c)
firstM f (p, c) = (,c) <$> f p

netMathInstance
  :: RandomGen g
  => MathConfig
  -> Int
  -> RandT g (ExceptT String IO) (String, Diagram B, Math)
netMathInstance config = taskInstance
  mathInstance
  (\c -> petriNetRnd (basicConfig c) (advConfig c))
  config
  (\c -> basicConfig (c :: MathConfig))
  (\c -> alloyConfig (c :: MathConfig))
  config

mathInstance
  :: MathConfig
  -> AlloyInstance
  -> Bool
  -- ^ whether to hide place names
  -> Bool
  -- ^ whether to hide transition names
  -> Bool
  -- ^ whether to hide weight of 1
  -> GraphvizCommand
  -> ExceptT String IO (String, Diagram B, Math)
mathInstance config inst hidePNames hideTNames hide1 gc = do
  petriLike <- except $ parseRenamedPetriLike "flow" "tokens" inst
  rightNet  <- drawNet id petriLike hidePNames hideTNames hide1 gc
  let math = toPetriMath petriLike
  let f = renderFalse petriLike config
  return (f, rightNet, math)

graphToMathTask :: OutputMonad m => GraphToMathInstance -> LangM m
graphToMathTask task = do
  paragraph $ english "Consider this graphical representation of a Petri net:"
  image $ from task
  paragraph $ text
    "Which of the following mathematical representations denotes this Petri net?"
  enumerateM
    (text . (++ ". ") . show)
    $ second (mathToOutput latex . snd) <$> toList (to task)
  paragraph $ text
    [i|Please state your answer by giving the number of the matching representation only.|]
  paragraph $ do
    text [i|Stating |]
    code "1"
    text [i| as answer would indicate that representation 1 matches the given graphical representation (and the other mathematical representations don't!).|]

mathToOutput :: OutputMonad m => (a -> LangM m) -> PetriMath a -> LangM m
mathToOutput f pm = paragraph $ do
  f $ netMath pm
  english ", where "
  f $ placesMath pm
  english " and "
  f $ transitionsMath pm
  english ", as well as"
  case placeOrderMath pm of
    Nothing -> return ()
    Just o  -> do
      english " using the place ordering "
      f o
  english ":"
  itemizeM $ f . fst <$> tokenChangeMath pm
  itemizeM $ f . snd <$> tokenChangeMath pm
  english "Moreover, "
  f $ initialMarkingMath pm

mathToGraphTask :: OutputMonad m => MathToGraphInstance -> LangM m
mathToGraphTask task = do
  paragraph $ text "Consider this mathematical representation of a Petri net:"
  mathToOutput latex $ from task
  paragraph $ text "Which of the following diagrams represents this Petri net?"
  images show snd $ to task
  paragraph $ text
    [i|Please state your answer by giving the number of the matching diagram only.|]
  paragraph $ do
    text [i|Stating |]
    code "1"
    text [i| as answer would indicate that diagram 1 matches the given mathematical representation (and the other diagrams don't!).|]

graphToMathEvaluation
  :: OutputMonad m
  => GraphToMathInstance
  -> Int
  -> LangM m
graphToMathEvaluation = singleChoice "mathematical representation" . head . M.keys . M.filter fst . to

mathToGraphEvaluation
  :: OutputMonad m
  => MathToGraphInstance
  -> Int
  -> LangM m
mathToGraphEvaluation = singleChoice "graphical representation" . head . M.keys . M.filter fst . to

checkMathConfig :: MathConfig -> Maybe String
checkMathConfig c@MathConfig {
  basicConfig,
  changeConfig
  } = checkBasicConfig basicConfig
  <|> prohibitHideNames basicConfig
  <|> checkChangeConfig basicConfig changeConfig
  <|> checkConfig c

prohibitHideNames :: BasicConfig -> Maybe String
prohibitHideNames bc
  | hidePlaceNames bc
  = Just "Place names are required for this task type"
  | hideTransitionNames bc
  = Just "Transition names are required for this task type"
  | otherwise
  = Nothing

checkConfig :: MathConfig -> Maybe String
checkConfig MathConfig {
  generatedWrongInstances,
  wrongInstances
  }
  | wrongInstances < 1
  = Just "There has to be at least one wrongInstance"
  | generatedWrongInstances < wrongInstances
  = Just "generatedWrongInstances must not be lower than wrongInstances"
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
