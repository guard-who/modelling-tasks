{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# Language DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# Language QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Modelling.PetriNet.MatchToMath (
  GraphToMathInstance,
  Math,
  MathConfig (..),
  MatchInstance (..),
  MathToGraphInstance,
  addPartNames,
  checkGraphToMathConfig,
  checkMathConfig,
  defaultGraphToMathInstance,
  defaultMathConfig,
  defaultMathToGraphInstance,
  graphToMath,
  graphToMathEvaluation,
  graphToMathSyntax,
  graphToMathTask,
  matchSolution,
  mathToGraph,
  mathToGraphEvaluation,
  mathToGraphSyntax,
  mathToGraphTask,
  petriNetRnd,
  )  where

import qualified Data.Map                         as M (
  empty,
  filter,
  foldrWithKey,
  keys,
  partition,
  )

import Capabilities.Alloy               (MonadAlloy, getInstances)
import Capabilities.Cache               (MonadCache)
import Capabilities.Diagrams            (MonadDiagrams)
import Capabilities.Graphviz            (MonadGraphviz)
import Modelling.Auxiliary.Common       (Object (oName), oneOf)
import Modelling.Auxiliary.Output       (
  hoveringInformation,
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
  petriScopeBitWidth,
  petriScopeMaxSeq,
  signatures,
  taskInstance,
  )
import Modelling.PetriNet.Diagram       (cacheNet)
import Modelling.PetriNet.LaTeX         (toPetriMath)
import Modelling.PetriNet.Parser (
  parseChange,
  parseRenamedNet,
  )
import Modelling.PetriNet.Types (
  AdvConfig,
  AlloyConfig,
  BasicConfig (..),
  Change,
  ChangeConfig (..),
  Drawable,
  DrawSettings (..),
  GraphConfig (..),
  Net (..),
  PetriLike (..),
  PetriMath (..),
  PetriNode (..),
  SimpleNode (..),
  SimplePetriLike,
  checkBasicConfig,
  checkChangeConfig,
  checkGraphLayouts,
  defaultAdvConfig,
  defaultAlloyConfig,
  defaultBasicConfig,
  defaultChangeConfig,
  defaultGraphConfig,
  drawSettingsWithCommand,
  isPlaceNode,
  manyRandomDrawSettings,
  mapChange,
  randomDrawSettings,
  shuffleNames,
  )

import Control.Applicative              (Alternative ((<|>)))
import Control.Arrow                    (first)
import Control.Monad.Catch              (MonadThrow)
import Control.OutputCapable.Blocks       (
  ArticleToUse (DefiniteArticle),
  GenericOutputCapable (..),
  LangM,
  Language,
  OutputCapable,
  Rated,
  ($=<<),
  english,
  german,
  singleChoice,
  translate,
  translations,
  )
import Control.Monad.Random             (
  MonadRandom,
  RandT,
  RandomGen,
  StdGen,
  evalRandT,
  mkStdGen,
  )
import Data.Bifoldable                  (Bifoldable (bifoldMap))
import Data.Bifunctor                   (Bifunctor (bimap, second))
import Data.Bitraversable               (Bitraversable (bitraverse), bimapM)
import Data.GraphViz                    (GraphvizCommand (Circo, Dot, Fdp, Sfdp))
import Data.Map                         (Map, fromList, mapWithKey, toList)
import Data.String.Interpolate          (i)
import GHC.Generics                     (Generic)
import Image.LaTeX.Render               (Formula)
import Language.Alloy.Call (
  AlloyInstance,
  )
import System.Random.Shuffle            (shuffleM)

type Math = PetriMath Formula

type GraphToMathInstance = MatchInstance (Drawable (SimplePetriLike String)) Math
type MathToGraphInstance = MatchInstance Math (Drawable (SimplePetriLike String))

{-# DEPRECATED addPartNames "the whole type class NamedParts will be removed" #-}
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
  graphConfig :: GraphConfig,
  printSolution :: Bool,
  useDifferentGraphLayouts :: Bool,
  wrongInstances :: Int,
  alloyConfig :: AlloyConfig
  } deriving (Generic, Read, Show)

defaultMathConfig :: MathConfig
defaultMathConfig = MathConfig {
  basicConfig = defaultBasicConfig,
  advConfig = defaultAdvConfig,
  changeConfig = defaultChangeConfig {
    tokenChangeOverall = 0,
    maxTokenChangePerPlace = 0
    },
  generatedWrongInstances = 50,
  graphConfig = defaultGraphConfig,
  printSolution = False,
  useDifferentGraphLayouts = False,
  wrongInstances = 3,
  alloyConfig = defaultAlloyConfig
  }

data MatchInstance a b = MatchInstance {
  from :: a,
  showSolution :: Bool,
  to :: Map Int (Bool, b)
  }
  deriving (Functor, Generic, Read, Show)

instance Bifoldable MatchInstance where
  bifoldMap f g m@MatchInstance {} = f (from m) `mappend` foldMap (g . snd) (to m)

instance Bifunctor MatchInstance where
  bimap f g m@MatchInstance {} = m {
    from = f $ from m,
    to   = second g <$> to m
    }

instance Bitraversable MatchInstance where
  bitraverse f g m@MatchInstance {} = MatchInstance
    <$> f (from m)
    <*> pure (showSolution m)
    <*> traverse (traverse g) (to m)

evalWithStdGen
  :: Monad m
  => Int
  -> RandT StdGen m a
  -> m a
evalWithStdGen = flip evalRandT . mkStdGen

writeDia
  :: (MonadCache m, MonadDiagrams m, MonadGraphviz m, MonadThrow m, Net p n)
  => FilePath
  -> MatchInstance (Drawable (p n String)) b
  -> m (MatchInstance FilePath b)
writeDia path = bimapM (\(n, ds) -> writeGraph ds path "" n) pure

writeDias
  :: (MonadCache m, MonadDiagrams m, MonadGraphviz m, MonadThrow m, Net p n)
  => FilePath
  -> MatchInstance a (Drawable (p n String))
  -> m (MatchInstance a FilePath)
writeDias path inst =
  let inst' = inst {
        from = from inst,
        to   = mapWithKey (\k -> second (show k,)) $ to inst
        }
  in bimapM pure (\(l, (n, d)) -> writeGraph d path l n) inst'

writeGraph
  :: (MonadCache m, MonadDiagrams m, MonadGraphviz m, MonadThrow m, Net p n)
  => DrawSettings
  -> FilePath
  -> String
  -> p n String
  -> m FilePath
writeGraph drawSettings path index pl =
  cacheNet
    (path ++ "graph" ++ index)
    id
    pl
    drawSettings

graphToMath
  :: (MonadAlloy m, MonadThrow m, Net p n)
  => MathConfig
  -> Int
  -> Int
  -> m (MatchInstance (Drawable (p n String)) Math)
graphToMath c segment seed = evalWithStdGen seed $ do
  ds <- randomDrawSettings (graphConfig c)
  (d, m, ms) <-
    matchToMath ds (map toPetriMath) c segment
  matchMathInstance c d m $ fst <$> ms

mathToGraph
  :: (MonadAlloy m, MonadFail m, MonadThrow m, Net p n)
  => MathConfig
  -> Int
  -> Int
  -> m (MatchInstance Math (Drawable (p n String)))
mathToGraph c segment seed = evalWithStdGen seed $ do
  (x, xs) <- second (flip zip) <$>
    if useDifferentGraphLayouts c
    then do
      (x':xs') <- manyRandomDrawSettings (graphConfig c) (wrongInstances c + 1)
      return (x', xs')
    else do
      s <- drawSettingsWithCommand (graphConfig c)
        <$> oneOf (graphLayouts $ graphConfig c)
      return (s, replicate (wrongInstances c) s)
  (d, m, ds) <- matchToMath x xs c segment
  matchMathInstance c m d $ fst <$> ds

matchMathInstance
  :: MonadRandom m
  => MathConfig
  -> a
  -> b
  -> [b]
  -> m (MatchInstance a b)
matchMathInstance c x y ys = do
  ys' <- shuffleM $ (True, y) : ((False,) <$> ys)
  return $ MatchInstance {
    from = x,
    showSolution = printSolution c,
    to = fromList $ zip [1..] ys'
    }

matchToMath
  :: (MonadAlloy m, MonadThrow m, Net p n, RandomGen g)
  => DrawSettings
  -> ([p n String] -> [a])
  -> MathConfig
  -> Int
  -> RandT g m (Drawable (p n String), Math, [(a, Change)])
matchToMath ds toOutput config segment = do
  (f, net, math) <- netMathInstance config segment
  fList <- getInstances
    (Just $ toInteger $ generatedWrongInstances config)
    Nothing
    f
  fList' <- take (wrongInstances config) <$> shuffleM fList
  if wrongInstances config == length fList'
    then do
    alloyChanges <- mapM addChange fList'
    changes <- firstM parse `mapM` alloyChanges
    let changes' = uncurry zip $ first toOutput (unzip changes)
    return ((net, ds), math, changes')
    else matchToMath ds toOutput config segment
  where
    parse = parseRenamedNet "flow" "tokens"

firstM :: Monad m => (a -> m b) -> (a, c) -> m (b, c)
firstM f (p, c) = (,c) <$> f p

netMathInstance
  :: (MonadAlloy m, MonadThrow m, Net p n, RandomGen g)
  => MathConfig
  -> Int
  -> RandT g m (String, p n String, Math)
netMathInstance config = taskInstance
  mathInstance
  (\c -> petriNetRnd (basicConfig c) (advConfig c))
  config
  (\c -> alloyConfig (c :: MathConfig))
  config

mathInstance
  :: (MonadAlloy m, MonadThrow m, Net p n, RandomGen g)
  => MathConfig
  -> AlloyInstance
  -> RandT g m (String, p n String, Math)
mathInstance config inst = do
  petriLike <- parseRenamedNet "flow" "tokens" inst
  petriLike' <- fst <$> shuffleNames petriLike
  let math = toPetriMath petriLike'
  let f = renderFalse petriLike' config
  return (f, petriLike', math)

graphToMathTask
  :: (MonadCache m, MonadDiagrams m, MonadGraphviz m, MonadThrow m, OutputCapable m)
  => FilePath
  -> GraphToMathInstance
  -> LangM m
graphToMathTask path task = do
  paragraph $ translate $ do
    english "Consider the following graphical representation of a Petri net:"
    german "Betrachten Sie folgende grafische Darstellung eines Petrinetzes:"
  image $=<< from <$> writeDia path task
  paragraph $ translate $ do
    english "Which of the following mathematical representations denotes this Petri net?"
    german "Welche der folgenden mathematischen Repräsentationen formalisiert dieses Petrinetz?"
  enumerateM
    (text . (++ ". ") . show)
    $ second (mathToOutput latex . snd) <$> toList (to task)
  paragraph $ translate $ do
    english [i|Please state your answer by giving the number of the matching representation only.|]
    german [i|Geben Sie Ihre Antwort durch Angabe der Nummer der passenden Repräsentation an.|]
  paragraph $ do
    translate $ do
      english [i|Stating |]
      german [i|Die Angabe von |]
    code "1"
    translate $ do
      english [i| as answer would indicate that representation 1 matches the given graphical representation (and the other mathematical representations don't).|]
      german [i| als Antwort würde bedeuten, dass Repräsentation 1 zur gegebenen grafischen Darstellung passt (und die anderen mathematischen Repräsentationen nicht).|]
    pure ()
  paragraph hoveringInformation
  pure ()

mathToOutput :: OutputCapable m => (a -> LangM m) -> PetriMath a -> LangM m
mathToOutput f pm = paragraph $ do
  f $ netMath pm
  translate $ do
    english ", where "
    german ", mit "
  f $ placesMath pm
  translate $ do
    english " and "
    german " und "
  f $ transitionsMath pm
  translate $ do
    english ", as well as"
    german ", sowie"
  case placeOrderMath pm of
    Nothing -> pure ()
    Just o  -> do
      translate $ do
        english " using the place ordering "
        german " mit der Stellenreihenfolge "
      f o
      pure ()
  translate $ english ":"
  itemizeM $ f . fst <$> tokenChangeMath pm
  itemizeM $ f . snd <$> tokenChangeMath pm
  translate $ do
    english "Moreover, "
    german "und "
  f $ initialMarkingMath pm
  pure ()

mathToGraphTask
  :: (MonadCache m, MonadDiagrams m, MonadGraphviz m, MonadThrow m, OutputCapable m)
  => FilePath
  -> MathToGraphInstance
  -> LangM m
mathToGraphTask path task = do
  paragraph $ translate $ do
    english "Consider the following mathematical representation of a Petri net:"
    german "Betrachten Sie folgende mathematische Repräsentation eines Petrinetzes:"
  mathToOutput latex $ from task
  paragraph $ translate $ do
    english "Which of the following diagrams represents this Petri net?"
    german "Welches der folgenden Diagramme stellt dieses Petrinetz dar?"
  images show snd $=<< to <$> writeDias path task
  paragraph $ translate $ do
    english [i|Please state your answer by giving the number of the matching diagram only.|]
    german [i|Geben Sie Ihre Antwort durch Angabe der Nummer des passenden Diagramms an.|]
  paragraph $ do
    translate $ do
      english [i|Stating |]
      german [i|Die Angabe von |]
    code "1"
    translate $ do
      english [i| as answer would indicate that diagram 1 matches the given mathematical representation (and the other diagrams don't).|]
      german [i| als Antwort würde bedeuten, dass Diagramm 1 zur gegebenen mathematischen Repräsentation passt (und die anderen Diagramme nicht).|]
    pure ()
  paragraph hoveringInformation
  pure ()

graphToMathSyntax
  :: OutputCapable m
  => GraphToMathInstance
  -> Int
  -> LangM m
graphToMathSyntax task x =
  assertion (1 <= x && x <= length (to task)) $ translate $ do
    english "The given mathematical representation is part of the task?"
    german "Die angegebene mathematische Repräsentation ist Bestandteil der Aufgabenstellung?"

graphToMathEvaluation
  :: OutputCapable m
  => GraphToMathInstance
  -> Int
  -> Rated m
graphToMathEvaluation = do
  let what = translations $ do
        english "mathematical representation"
        german "mathematische Repräsentation"
  evaluation what

matchSolution :: MatchInstance a b -> Int
matchSolution = head . M.keys . M.filter fst . to

mathToGraphSyntax
  :: OutputCapable m
  => MathToGraphInstance
  -> Int
  -> LangM m
mathToGraphSyntax task x =
  assertion (1 <= x && x <= length (to task)) $ translate $ do
    english "Given graphical representation is part of the task?"
    german "Die angegebene grafische Darstellung ist Bestandteil der Aufgabenstellung?"

mathToGraphEvaluation
  :: OutputCapable m
  => MathToGraphInstance
  -> Int
  -> Rated m
mathToGraphEvaluation = do
  let what = translations $ do
        english "graphical representation"
        german "grafische Darstellung"
  evaluation what

evaluation
  :: OutputCapable m
  => Map Language String
  -> MatchInstance a b
  -> Int
  -> Rated m
evaluation what task = do
  let solution = matchSolution task
      maybeSolution =
        if showSolution task
        then Just $ show solution
        else Nothing
  singleChoice DefiniteArticle what maybeSolution solution

checkGraphToMathConfig :: MathConfig -> Maybe String
checkGraphToMathConfig c@MathConfig {
  useDifferentGraphLayouts
  }
  | useDifferentGraphLayouts
  = Just "Setting useDifferentGraphLayouts to True is not supported for this task type."
  | otherwise
  = checkMathConfig c

checkMathConfig :: MathConfig -> Maybe String
checkMathConfig c@MathConfig {
  basicConfig,
  changeConfig,
  graphConfig,
  useDifferentGraphLayouts,
  wrongInstances
  } = checkBasicConfig basicConfig
  <|> prohibitHideNames graphConfig
  <|> checkChangeConfig basicConfig changeConfig
  <|> checkConfig c
  <|> checkGraphLayouts useDifferentGraphLayouts wrongInstances graphConfig

prohibitHideNames :: GraphConfig -> Maybe String
prohibitHideNames gc
  | hidePlaceNames gc
  = Just "Place names are required for this task type"
  | hideTransitionNames gc
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

addChange :: MonadThrow m => AlloyInstance -> m (AlloyInstance, Change)
addChange alloy = do
  change <- parseChange alloy
  return (alloy, mapChange oName change)

petriNetRnd :: BasicConfig -> AdvConfig -> String
petriNetRnd basicC@BasicConfig{places,transitions} advConfig = [i|module PetriNetRnd

#{modulePetriSignature}
#{modulePetriAdditions}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}
#{signatures "added" places transitions}

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
run showNets for exactly #{petriScopeMaxSeq basicC} Nodes, #{petriScopeBitWidth basicC} Int
|]
  where
    activated = "activatedTrans"

renderFalse :: Net p n => p n String -> MathConfig -> String
renderFalse
  net
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

run showFalseNets for exactly #{petriScopeMaxSeq basicConfig} Nodes, #{petriScopeBitWidth basicConfig} Int
|]
  where
    allNodes    = nodes net
    (ps, ts)    = M.partition isPlaceNode allNodes
    activated   = "activatedTrans"
    places      = unlines [extendLine p "givenPlaces" | p <- M.keys ps]
    transitions = unlines [extendLine t "givenTransitions" | t <- M.keys ts]
    initialMark = M.foldrWithKey (\k -> (++) . tokenLine k) "" $ initialTokens <$> ps
    defaultFlow = M.foldrWithKey (\k _ -> (printFlow k ++)) "" allNodes
    printFlow :: String -> String
    printFlow x = M.foldrWithKey
      (\y _ -> (++) $ flowLine x y $ flow x y net)
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

defaultGraphToMathInstance :: GraphToMathInstance
defaultGraphToMathInstance = MatchInstance {
  from = (
    PetriLike {
      allNodes = fromList [
        ("s1",SimplePlace {initial = 2, flowOut = fromList [("t3",1)]}),
        ("s2",SimplePlace {initial = 0, flowOut = fromList [("t1",1),("t2",1)]}),
        ("s3",SimplePlace {initial = 1, flowOut = M.empty}),
        ("s4",SimplePlace {initial = 0, flowOut = M.empty}),
        ("t1",SimpleTransition {flowOut = fromList [("s3",1)]}),
        ("t2",SimpleTransition {flowOut = fromList [("s4",1)]}),
        ("t3",SimpleTransition {flowOut = fromList [("s3",1)]})
        ]
      },
    DrawSettings {
      withAnnotatedLabels = False,
      withPlaceNames = True,
      withTransitionNames = True,
      with1Weights = False,
      withGraphvizCommand = Sfdp
      }
    ),
  showSolution = False,
  to = fromList [
    (1,(False,PetriMath {
      netMath = "N = \\left(S, T, \\vphantom{()}^{\\bullet}(), ()^{\\bullet}, m_0\\right)",
      placesMath = "S = \\left\\{s_{1},s_{2},s_{3},s_{4}\\right\\}",
      transitionsMath = "T = \\left\\{t_{1},t_{2},t_{3}\\right\\}",
      tokenChangeMath = [
        ("^{\\bullet}t_{1} = \\left(0,1,0,0\\right)","t_{1}^{\\bullet} = \\left(0,0,1,0\\right)"),
        ("^{\\bullet}t_{2} = \\left(0,1,0,0\\right)","t_{2}^{\\bullet} = \\left(1,0,0,1\\right)"),
        ("^{\\bullet}t_{3} = \\left(1,0,0,0\\right)","t_{3}^{\\bullet} = \\left(0,0,1,1\\right)")
        ],
      initialMarkingMath = "m_0 = \\left(2,0,1,0\\right)",
      placeOrderMath = Just "\\left(s_{1},s_{2},s_{3},s_{4}\\right)"
      })),
    (2,(False,PetriMath {
      netMath = "N = \\left(S, T, \\vphantom{()}^{\\bullet}(), ()^{\\bullet}, m_0\\right)",
      placesMath = "S = \\left\\{s_{1},s_{2},s_{3},s_{4}\\right\\}",
      transitionsMath = "T = \\left\\{t_{1},t_{2},t_{3}\\right\\}",
      tokenChangeMath = [
        ("^{\\bullet}t_{1} = \\left(0,1,0,0\\right)","t_{1}^{\\bullet} = \\left(1,0,1,0\\right)"),
        ("^{\\bullet}t_{2} = \\left(0,1,0,0\\right)","t_{2}^{\\bullet} = \\left(0,0,0,1\\right)"),
        ("^{\\bullet}t_{3} = \\left(1,0,0,0\\right)","t_{3}^{\\bullet} = \\left(0,0,1,1\\right)")
        ],
      initialMarkingMath = "m_0 = \\left(2,0,1,0\\right)",
      placeOrderMath = Just "\\left(s_{1},s_{2},s_{3},s_{4}\\right)"
      })),
    (3,(False,PetriMath {
      netMath = "N = \\left(S, T, \\vphantom{()}^{\\bullet}(), ()^{\\bullet}, m_0\\right)",
      placesMath = "S = \\left\\{s_{1},s_{2},s_{3},s_{4}\\right\\}",
      transitionsMath = "T = \\left\\{t_{1},t_{2},t_{3}\\right\\}",
      tokenChangeMath = [
        ("^{\\bullet}t_{1} = \\left(0,1,0,0\\right)","t_{1}^{\\bullet} = \\left(1,0,0,0\\right)"),
        ("^{\\bullet}t_{2} = \\left(0,1,0,0\\right)","t_{2}^{\\bullet} = \\left(0,0,0,1\\right)"),
        ("^{\\bullet}t_{3} = \\left(1,0,0,0\\right)","t_{3}^{\\bullet} = \\left(0,0,1,0\\right)")
        ],
      initialMarkingMath = "m_0 = \\left(2,0,1,0\\right)",
      placeOrderMath = Just "\\left(s_{1},s_{2},s_{3},s_{4}\\right)"
      })),
    (4,(True,PetriMath {
      netMath = "N = \\left(S, T, \\vphantom{()}^{\\bullet}(), ()^{\\bullet}, m_0\\right)",
      placesMath = "S = \\left\\{s_{1},s_{2},s_{3},s_{4}\\right\\}",
      transitionsMath = "T = \\left\\{t_{1},t_{2},t_{3}\\right\\}",
      tokenChangeMath = [
        ("^{\\bullet}t_{1} = \\left(0,1,0,0\\right)","t_{1}^{\\bullet} = \\left(0,0,1,0\\right)"),
        ("^{\\bullet}t_{2} = \\left(0,1,0,0\\right)","t_{2}^{\\bullet} = \\left(0,0,0,1\\right)"),
        ("^{\\bullet}t_{3} = \\left(1,0,0,0\\right)","t_{3}^{\\bullet} = \\left(0,0,1,0\\right)")
        ],
      initialMarkingMath = "m_0 = \\left(2,0,1,0\\right)",
      placeOrderMath = Just "\\left(s_{1},s_{2},s_{3},s_{4}\\right)"
      }))
    ]
  }

defaultMathToGraphInstance :: MathToGraphInstance
defaultMathToGraphInstance = MatchInstance {
  from = PetriMath {
    netMath = "N = \\left(S, T, \\vphantom{()}^{\\bullet}(), ()^{\\bullet}, m_0\\right)",
    placesMath = "S = \\left\\{s_{1},s_{2},s_{3},s_{4}\\right\\}",
    transitionsMath = "T = \\left\\{t_{1},t_{2},t_{3}\\right\\}",
    tokenChangeMath = [
      ("^{\\bullet}t_{1} = \\left(0,0,1,0\\right)","t_{1}^{\\bullet} = \\left(1,0,0,0\\right)"),
      ("^{\\bullet}t_{2} = \\left(0,0,0,1\\right)","t_{2}^{\\bullet} = \\left(0,1,0,0\\right)"),
      ("^{\\bullet}t_{3} = \\left(0,0,1,0\\right)","t_{3}^{\\bullet} = \\left(0,0,0,1\\right)")
      ],
    initialMarkingMath = "m_0 = \\left(1,1,0,1\\right)",
    placeOrderMath = Just "\\left(s_{1},s_{2},s_{3},s_{4}\\right)"
    },
  showSolution = False,
  to = fromList [
    (1,(True,(
      PetriLike {
        allNodes = fromList [
          ("s1",SimplePlace {initial = 1, flowOut = M.empty}),
          ("s2",SimplePlace {initial = 1, flowOut = M.empty}),
          ("s3",SimplePlace {initial = 0, flowOut = fromList [("t1",1),("t3",1)]}),
          ("s4",SimplePlace {initial = 1, flowOut = fromList [("t2",1)]}),
          ("t1",SimpleTransition {flowOut = fromList [("s1",1)]}),
          ("t2",SimpleTransition {flowOut = fromList [("s2",1)]}),
          ("t3",SimpleTransition {flowOut = fromList [("s4",1)]})
          ]
        },
      DrawSettings {
        withAnnotatedLabels = False,
        withPlaceNames = True,
        withTransitionNames = True,
        with1Weights = False,
        withGraphvizCommand = Dot
        }
      ))),
    (2,(False,(
      PetriLike {
        allNodes = fromList [
          ("s1",SimplePlace {initial = 1, flowOut = M.empty}),
          ("s2",SimplePlace {initial = 1, flowOut = M.empty}),
          ("s3",SimplePlace {initial = 0, flowOut = fromList [("t1",2),("t3",2)]}),
          ("s4",SimplePlace {initial = 1, flowOut = fromList [("t2",1)]}),
          ("t1",SimpleTransition {flowOut = fromList [("s1",1)]}),
          ("t2",SimpleTransition {flowOut = fromList [("s2",1)]}),
          ("t3",SimpleTransition {flowOut = fromList [("s4",1)]})
          ]
        },
      DrawSettings {
        withAnnotatedLabels = False,
        withPlaceNames = True,
        withTransitionNames = True,
        with1Weights = False,
        withGraphvizCommand = Sfdp
        }
      ))),
    (3,(False,(
      PetriLike {
        allNodes = fromList [
          ("s1",SimplePlace {initial = 1, flowOut = M.empty}),
          ("s2",SimplePlace {initial = 1, flowOut = M.empty}),
          ("s3",SimplePlace {initial = 0, flowOut = fromList [("t1",1),("t3",1)]}),
          ("s4",SimplePlace {initial = 1, flowOut = fromList [("t2",1)]}),
          ("t1",SimpleTransition {flowOut = fromList [("s1",2),("s2",1)]}),
          ("t2",SimpleTransition {flowOut = fromList [("s2",1)]}),
          ("t3",SimpleTransition {flowOut = fromList [("s4",1)]})
          ]
        },
      DrawSettings {
        withAnnotatedLabels = False,
        withPlaceNames = True,
        withTransitionNames = True,
        with1Weights = False,
        withGraphvizCommand = Circo}
      ))),
    (4,(False,(
      PetriLike {
        allNodes = fromList [
          ("s1",SimplePlace {initial = 1, flowOut = M.empty}),
          ("s2",SimplePlace {initial = 1, flowOut = M.empty}),
          ("s3",SimplePlace {initial = 0, flowOut = fromList [("t1",2),("t3",1)]}),
          ("s4",SimplePlace {initial = 1, flowOut = fromList [("t2",1)]}),
          ("t1",SimpleTransition {flowOut = fromList [("s1",2)]}),
          ("t2",SimpleTransition {flowOut = fromList [("s2",1)]}),
          ("t3",SimpleTransition {flowOut = fromList [("s4",1)]})
          ]
        },
      DrawSettings {
        withAnnotatedLabels = False,
        withPlaceNames = True,
        withTransitionNames = True,
        with1Weights = False,
        withGraphvizCommand = Fdp
        }
      )))
    ]
  }
