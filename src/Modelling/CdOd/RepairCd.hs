{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Modelling.CdOd.RepairCd (
  AllowedProperties (..),
  InValidOption (..),
  RelationshipChangeWithArticle,
  RepairCdConfig (..),
  RepairCdInstance (..),
  checkClassConfigAndChanges,
  checkRepairCdConfig,
  checkRepairCdInstance,
  classAndAssocNames,
  defaultRepairCdConfig,
  defaultRepairCdInstance,
  allowEverything,
  mapInValidOption,
  mapInValidOptionM,
  renameInstance,
  repairCd,
  repairCdEvaluation,
  repairCdSolution,
  repairCdSyntax,
  repairCdTask,
  repairIncorrect,
  PropertyChange (..),
  (.&.),
  illegalChanges,
  legalChanges,
  toProperty,
  ) where

import qualified Modelling.CdOd.CdAndChanges.Transform as Changes (
  transformChanges,
  transformImproveCd,
  )
import qualified Modelling.CdOd.Types             as T (
  RelationshipProperties (selfInheritances, selfRelationships),
  )

import qualified Data.Bimap                       as BM (fromList)
import qualified Data.Map                         as M (
  elems,
  filter,
  fromAscList,
  keys,
  toList,
  traverseWithKey,
  )

import Modelling.Auxiliary.Common (
  Randomise (randomise),
  RandomiseLayout (randomiseLayout),
  shuffleEverything,
  )
import Modelling.Auxiliary.Output (
  addPretext,
  hoveringInformation,
  rerefuse,
  simplifiedInformation,
  )
import Modelling.CdOd.Auxiliary.Util    (alloyInstanceToOd, getInstances)
import Modelling.CdOd.CD2Alloy.Transform (
  combineParts,
  createRunCommand,
  transform,
  )
import Modelling.CdOd.CdAndChanges.Instance (
  AnnotatedChangeAndCd (..),
  ChangeAndCd (..),
  GenericClassDiagramInstance (..),
  uniformlyAnnotateChangeAndCd,
  )
import Modelling.CdOd.MatchCdOd         (getChangesAndCds)
import Modelling.CdOd.Output (
  cacheCd,
  drawCd,
  drawOd,
  )
import Modelling.CdOd.Phrasing (
  phraseChange,
  )
import Modelling.CdOd.Types (
  Annotation (..),
  ArticleToUse (DefiniteArticle),
  Cd,
  ClassConfig (..),
  ClassDiagram (..),
  LimitedLinking (..),
  ObjectProperties (..),
  Od,
  Relationship (..),
  RelationshipProperties (..),
  associationNames,
  checkClassConfig,
  checkClassConfigWithProperties,
  classNames,
  defaultProperties,
  maxObjects,
  relationshipName,
  renameClassesAndRelationships,
  reverseAssociation,
  shuffleClassAndConnectionOrder,
  )
import Modelling.Types                  (Change (..))

import Control.Applicative              (Alternative ((<|>)))
import Control.Monad                    ((>=>), forM, join, void, when, zipWithM)
import Control.Monad.Catch              (MonadThrow)
import Control.Monad.Except             (runExceptT)
import Control.Monad.IO.Class           (MonadIO (liftIO))
import Control.Monad.Output (
  GenericOutputMonad (..),
  LangM,
  Language (English, German),
  OutputMonad,
  Rated,
  ($=<<),
  english,
  enumerateM,
  german,
  multipleChoice,
  singleChoiceSyntax,
  translate,
  )
import Control.Monad.Random
  (MonadRandom, RandT, RandomGen, StdGen, evalRandT, getStdGen, mkStdGen)
import Data.Bifunctor                   (bimap, first, second)
import Data.Bitraversable               (bimapM)
import Data.Containers.ListUtils        (nubOrd)
import Data.Either                      (isRight)
import Data.Either.Extra                (eitherToMaybe)
import Data.Foldable                    (for_)
import Data.GraphViz                    (DirType (..))
import Data.Map                         (Map)
import Data.Maybe                       (catMaybes, listToMaybe, mapMaybe)
import Data.String.Interpolate          (i, iii)
import GHC.Generics                     (Generic)
import System.Random.Shuffle            (shuffleM)

debug :: Bool
debug = False

data PropertyChange = PropertyChange {
    changeName     :: String,
    operation      :: RelationshipProperties -> RelationshipProperties,
    validityChange :: Bool -> Bool
  }

toProperty :: PropertyChange -> RelationshipProperties
toProperty p = operation p defaultProperties

isValid :: PropertyChange -> Bool
isValid p = validityChange p True

type RelationshipChangeWithArticle
  = Annotation ArticleToUse (Change (Relationship String String))

type CdChangeAndCd = InValidOption
  (AnnotatedChangeAndCd ArticleToUse String String)
  RelationshipChangeWithArticle
  Od

type RelationshipChange = InValidOption
  RelationshipChangeWithArticle
  Cd
  Cd

data InValidOption option forInvalidity forValidity = InValidOption {
  hint :: Either forInvalidity forValidity,
  option :: option
  } deriving (Eq, Generic, Read, Show)

mapInValidOption
  :: (a -> b)
  -> (c -> d)
  -> (e -> f)
  -> InValidOption a c e
  -> InValidOption b d f
mapInValidOption f g h InValidOption {..} = InValidOption {
  hint = bimap g h hint,
  option = f option
  }

mapInValidOptionM
  :: Applicative m
  => (a -> m b)
  -> (c -> m d)
  -> (e -> m f)
  -> InValidOption a c e
  -> m (InValidOption b d f)
mapInValidOptionM f g h InValidOption {..} = InValidOption
  <$> bimapM g h hint
  <*> f option

data RepairCdConfig = RepairCdConfig {
    allowedProperties :: AllowedProperties,
    classConfig      :: ClassConfig,
    maxInstances     :: Maybe Integer,
    objectProperties :: ObjectProperties,
    printExtendedFeedback :: Bool,
    printNames       :: Bool,
    printNavigations :: Bool,
    printSolution    :: Bool,
    timeout          :: Maybe Int,
    useNames         :: Bool
  } deriving (Generic, Read, Show)

defaultRepairCdConfig :: RepairCdConfig
defaultRepairCdConfig = RepairCdConfig {
    allowedProperties = allowEverything {
        reverseInheritances    = False,
        Modelling.CdOd.RepairCd.selfInheritances = False
        },
    classConfig = ClassConfig {
        classLimits        = (4, 4),
        aggregationLimits  = (1, Just 1),
        associationLimits  = (0, Just 1),
        compositionLimits  = (2, Just 3),
        inheritanceLimits  = (0, Just 0),
        relationshipLimits = (4, Just 5)
      },
    maxInstances     = Just 200,
    objectProperties = ObjectProperties {
      completelyInhabited = Just True,
      hasLimitedIsolatedObjects = False,
      hasSelfLoops = Nothing,
      usesEveryRelationshipName = Just True
      },
    printExtendedFeedback = False,
    printNames       = True,
    printNavigations = True,
    printSolution    = False,
    timeout          = Nothing,
    useNames         = False
  }

checkRepairCdConfig :: RepairCdConfig -> Maybe String
checkRepairCdConfig RepairCdConfig {..}
  | not printNames && useNames
  = Just "use names is only possible when printing names"
  | completelyInhabited objectProperties /= Just True
  = Just "completelyInhabited needs to be set to 'Just True' for this task type"
  | usesEveryRelationshipName objectProperties /= Just True
  = Just [iii|
      usesEveryRelationshipName needs to be set to 'Just True' for this task type
      |]
  | printExtendedFeedback && not printSolution
  = Just [iii|
      printExtendedFeedback leaks the correct solution
      and thus can only be enabled when printSolution is set to True
      |]
  | otherwise
  = checkClassConfigAndChanges classConfig allowedProperties

checkClassConfigAndChanges
  :: ClassConfig
  -> AllowedProperties
  -> Maybe String
checkClassConfigAndChanges classConfig allowedProperties =
  checkClassConfig classConfig
  <|> onlyFirst (map checkChange $ legalChanges allowedProperties)
  where
    checkProp = checkClassConfigWithProperties classConfig
    onlyFirst = listToMaybe . catMaybes
    checkChange c =
      ([iii|
         You should amend your class configuration for
         or disable the property change "#{changeName c}":|] ++)
      <$> checkProp (toProperty c)

repairCdTask
  :: (OutputMonad m, MonadIO m)
  => FilePath
  -> RepairCdInstance
  -> LangM m
repairCdTask path task = do
  paragraph $ translate $ do
    english "Consider the following class diagram, which unfortunately is invalid:"
    german "Betrachten Sie folgendes Klassendiagramm, welches leider ungültig ist:"
  image $=<< liftIO $ cacheCd
    (withDirections task)
    (withNames task)
    mempty
    (classDiagram task)
    path
  paragraph $ translate $ do
    english [i|Which of the following changes would repair the class diagram?|]
    german [i|Welche der folgenden Änderungen würden das Klassendiagramm reparieren?|]
  let phrase x y Annotation {..} = translate $ do
        english $ phraseChange English annotation x y annotated
        german $ phraseChange German annotation x y annotated
  enumerateM (text . show)
    $ second (phrase (withNames task) (withDirections task) . option)
    <$> M.toList (changes task)
  paragraph $ translate $ do
    english [i|Please state your answer by giving a list of numbers, indicating all changes each resulting in a valid class diagram.|]
    german [i|Bitte geben Sie Ihre Antwort als Liste aller Zahlen an, deren Änderungen jeweils in einem gültigen Klassendiagramm resultieren.|]
  paragraph $ do
    translate $ do
      english [i|Answer by giving a comma separated list of all valid options, e.g., |]
      german [i|Antworten Sie durch Angabe einer durch Komma separierten Liste aller gültigen Optionen. Zum Beispiel |]
    code "[1, 2]"
    translate $ do
      english [i| would indicate that options 1 and 2 each repair the given class diagram.|]
      german [i| als Angabe würde bedeuten, dass die Optionen 1 und 2 jeweils das gegebene Klassendiagramm reparieren.|]
    pure ()
  paragraph simplifiedInformation
  paragraph hoveringInformation
  pure ()

repairCdSyntax :: OutputMonad m => RepairCdInstance -> [Int] -> LangM m
repairCdSyntax inst xs =
  for_ xs $ singleChoiceSyntax False (M.keys $ changes inst)

repairCdEvaluation
  :: (Alternative m, MonadIO m, OutputMonad m)
  => FilePath
  -> RepairCdInstance
  -> [Int]
  -> Rated m
repairCdEvaluation path inst xs = addPretext $ do
  let chs = M.fromAscList [
        (English, "changes"),
        (German, "Änderungen")
        ]
      solution = isRight . hint <$> changes inst
      correctAnswer
        | showSolution inst = Just $ show $ repairCdSolution inst
        | otherwise = Nothing
  rerefuse
    (multipleChoice chs correctAnswer solution xs)
    $ when (showExtendedFeedback inst)
    $ void $ M.traverseWithKey
      (repairCdFeedback path (withDirections inst) (withNames inst) xs)
      (changes inst)

repairCdFeedback
  :: (MonadIO m, OutputMonad m)
  => FilePath
  -> Bool
  -> Bool
  -> [Int]
  -> Int
  -> RelationshipChange
  -> LangM m
repairCdFeedback path withDir byName xs x cdChange =
  case hint cdChange of
    Left cd
      | x `elem` xs -> notCorrect *> makesIncorrect *> showCd cd
      | otherwise   -> correct *> makesIncorrect *> showCd cd
    Right cd
      | x `elem` xs -> correct *> makesCorrect *> showCd cd
      | otherwise   -> notCorrect *> makesCorrect *> showCd cd
  where
    correct = paragraph $ translate $ do
      english [iii|Your answer about change #{x} is correct.|]
      german [iii|Ihre Antwort zu Änderung #{x} ist richtig.|]
    notCorrect = paragraph $ translate $ do
      english [iii|Your answer about change #{x} is not correct.|]
      german [iii|Ihre Antwort zu Änderung #{x} ist nicht richtig.|]
    makesCorrect = paragraph $ translate $ do
      english [iii|The change repairs the class diagram as it results in:|]
      german [iii|
        Die Änderung repariert das Klassendiagramm, da es dann so aussieht:
        |]
    makesIncorrect = paragraph $ translate $ do
      english [iii|The change does not repair the class diagram as it results in:|]
      german [iii|
        Die Änderung repariert das Klassendiagramm nicht, da es dann so aussieht:
        |]
    showCd cd = paragraph $
      image $=<< liftIO $ cacheCd withDir byName mempty cd path

repairCdSolution :: RepairCdInstance -> [Int]
repairCdSolution = M.keys . M.filter id . fmap (isRight . hint) . changes

data RepairCdInstance = RepairCdInstance {
    changes        :: Map Int RelationshipChange,
    classDiagram   :: Cd,
    showExtendedFeedback :: Bool,
    showSolution   :: Bool,
    withDirections :: Bool,
    withNames      :: Bool
  } deriving (Eq, Generic, Read, Show)

checkRepairCdInstance :: RepairCdInstance -> Maybe String
checkRepairCdInstance RepairCdInstance {..}
  | showExtendedFeedback && not showSolution
  = Just [iii|
      showExtendedFeedback leaks the correct solution
      and thus can only be enabled when showSolution is set to True
      |]
  | otherwise
  = Nothing

classAndAssocNames :: RepairCdInstance -> ([String], [String])
classAndAssocNames inst =
  let cd = classDiagram inst
      allChs = M.elems $ changes inst
      cds = map (either id id . hint) allChs
      chs = map option allChs
      names = nubOrd $ classNames cd
        ++ concatMap classNames cds
      assocs = nubOrd $ associationNames cd
        ++ mapMaybe (add . annotated >=> relationshipName) chs
        ++ mapMaybe (remove . annotated >=> relationshipName) chs
        ++ concatMap associationNames cds
  in (names, assocs)

instance Randomise RepairCdInstance where
  randomise inst = do
    let (names, assocs) = classAndAssocNames inst
    names' <- shuffleM names
    assocs' <- shuffleM assocs
    renameInstance inst names' assocs'
      >>= shuffleInstance

instance RandomiseLayout RepairCdInstance where
  randomiseLayout RepairCdInstance {..} = do
    cd <- shuffleClassAndConnectionOrder classDiagram
    changes' <- mapInValidOptionM
      pure
      shuffleClassAndConnectionOrder
      shuffleClassAndConnectionOrder
      `mapM` changes
    return RepairCdInstance {
      changes = changes',
      classDiagram = cd,
      showExtendedFeedback = showExtendedFeedback,
      showSolution = showSolution,
      withDirections = withDirections,
      withNames = withNames
      }

shuffleInstance :: MonadRandom m => RepairCdInstance -> m RepairCdInstance
shuffleInstance inst = do
  chs <- M.fromAscList . zip [1..] <$> shuffleM (M.elems $ changes inst)
  return $ RepairCdInstance {
    changes = chs,
    classDiagram = classDiagram inst,
    showExtendedFeedback = showExtendedFeedback inst,
    showSolution = showSolution inst,
    withDirections = withDirections inst,
    withNames = withNames inst
    }

renameInstance
  :: MonadThrow m
  => RepairCdInstance
  -> [String]
  -> [String]
  -> m RepairCdInstance
renameInstance inst names' assocs' = do
  let (names, assocs) = classAndAssocNames inst
      bmNames  = BM.fromList $ zip names names'
      bmAssocs = BM.fromList $ zip assocs assocs'
      renameCd = renameClassesAndRelationships bmNames bmAssocs
      renameEdge = renameClassesAndRelationships bmNames bmAssocs
  cd <- renameCd $ classDiagram inst
  chs <- mapM (mapInValidOptionM (mapM $ mapM renameEdge) renameCd renameCd)
    $ changes inst
  return $ RepairCdInstance {
    changes        = chs,
    classDiagram   = cd,
    showExtendedFeedback = showExtendedFeedback inst,
    showSolution   = showSolution inst,
    withDirections = withDirections inst,
    withNames      = withNames inst
    }

repairCd
  :: RepairCdConfig
  -> Int
  -> Int
  -> IO RepairCdInstance
repairCd config segment seed = do
  let g = mkStdGen $ (segment +) $ 4 * seed
  (cd, chs) <- flip evalRandT g $ repairIncorrect
    (allowedProperties config)
    (classConfig config)
    (objectProperties config)
    (maxInstances config)
    (timeout config)
  let chs' = map cdAsHint chs
  shuffleEverything $ RepairCdInstance
    (M.fromAscList $ zip [1..] chs')
    cd
    (printExtendedFeedback config)
    (printSolution config)
    (printNavigations config)
    (printNames config && useNames config)
  where
    cdAsHint x =
      let cd _ = annotatedChangeClassDiagram $ option x
      in mapInValidOption annotatedRelationshipChange cd cd x

defaultRepairCdInstance :: RepairCdInstance
defaultRepairCdInstance = RepairCdInstance {
  changes = M.fromAscList [
    (1, InValidOption {
      hint = Left $ ClassDiagram {
        classNames = ["A", "D", "B", "C"],
        relationships = [
          Composition {
            compositionName = "x",
            compositionPart =
              LimitedLinking {linking = "B", limits = (0, Just 1)},
            compositionWhole =
              LimitedLinking {linking = "D", limits = (0, Just 1)}
            },
          Composition {
            compositionName = "v",
            compositionPart =
              LimitedLinking {linking = "A", limits = (1, Just 0)},
            compositionWhole =
              LimitedLinking {linking = "C", limits = (0, Just 1)}
            },
          Aggregation {
            aggregationName = "z",
            aggregationPart =
              LimitedLinking {linking = "A", limits = (0, Just 1)},
            aggregationWhole =
              LimitedLinking {linking = "B", limits = (0, Just 1)}
            }
          ]
        },
      option = Annotation {
        annotated = Change {
          add = Nothing,
          remove = Just $ Composition {
            compositionName = "y",
            compositionPart = LimitedLinking {linking = "D", limits = (0, Just 2)},
            compositionWhole = LimitedLinking {linking = "A", limits = (0, Just 1)}
            }
          },
        annotation = DefiniteArticle
        }
      }),
    (2, InValidOption {
      hint = Right $ ClassDiagram {
        classNames = ["D", "B", "A", "C"],
        relationships = [
          Composition {
            compositionName = "x",
            compositionPart =
              LimitedLinking {linking = "B", limits = (0, Just 1)},
            compositionWhole =
              LimitedLinking {linking = "D", limits = (0, Just 1)}
            },
          Aggregation {
            aggregationName = "z",
            aggregationPart =
              LimitedLinking {linking = "A", limits = (0, Just 1)},
            aggregationWhole =
              LimitedLinking {linking = "B", limits = (0, Just 1)}
            },
          Composition {
            compositionName = "v",
            compositionPart =
              LimitedLinking {linking = "A", limits = (1, Just 1)},
            compositionWhole =
              LimitedLinking {linking = "C", limits = (0, Just 1)}
            },
          Composition {
            compositionName = "y",
            compositionPart =
              LimitedLinking {linking = "D", limits = (0, Just 2)},
            compositionWhole =
              LimitedLinking {linking = "A", limits = (0, Just 1)}
            }
          ]
        },
      option = Annotation {
        annotated = Change {
          add = Just $ Composition {
            compositionName = "w",
            compositionPart = LimitedLinking {linking = "A", limits = (1, Just 1)},
            compositionWhole = LimitedLinking {linking = "C", limits = (0, Just 1)}
            },
          remove = Just $ Composition {
            compositionName = "v",
            compositionPart = LimitedLinking {linking = "A", limits = (1, Just 0)},
            compositionWhole = LimitedLinking {linking = "C", limits = (0, Just 1)}
            }
          },
        annotation = DefiniteArticle
        }
      }),
    (3, InValidOption {
      hint = Right $ ClassDiagram {
        classNames = ["C", "A", "B", "D"],
        relationships = [
          Composition {
            compositionName = "y",
            compositionPart =
              LimitedLinking {linking = "D", limits = (0, Just 2)},
            compositionWhole =
              LimitedLinking {linking = "A", limits = (0, Just 1)}
            },
          Aggregation {
            aggregationName = "z",
            aggregationPart =
              LimitedLinking {linking = "A", limits = (0, Just 1)},
            aggregationWhole =
              LimitedLinking {linking = "B", limits = (0, Just 1)}
            },
          Composition {
            compositionName = "x",
            compositionPart =
              LimitedLinking {linking = "B", limits = (0, Just 1)},
            compositionWhole =
              LimitedLinking {linking = "D", limits = (0, Just 1)}
            }
          ]
        },
      option = Annotation {
        annotated = Change {
          add = Nothing,
          remove = Just $ Composition {
            compositionName = "v",
            compositionPart = LimitedLinking {linking = "A", limits = (1, Just 0)},
            compositionWhole = LimitedLinking {linking = "C", limits = (0, Just 1)}
            }
          },
        annotation = DefiniteArticle
        }
      }),
    (4, InValidOption {
      hint = Left $ ClassDiagram {
        classNames = ["A", "D", "B", "C"],
        relationships = [
          Association {
            associationName = "u",
            associationFrom =
              LimitedLinking {linking = "C", limits = (2, Just 2)},
            associationTo =
              LimitedLinking {linking = "A", limits = (2, Just 2)}
            },
          Composition {
            compositionName = "y",
            compositionPart =
              LimitedLinking {linking = "D", limits = (0, Just 2)},
            compositionWhole =
              LimitedLinking {linking = "A", limits = (0, Just 1)}
            },
          Aggregation {
            aggregationName = "z",
            aggregationPart =
              LimitedLinking {linking = "A", limits = (0, Just 1)},
            aggregationWhole =
              LimitedLinking {linking = "B", limits = (0, Just 1)}
            },
          Composition {
            compositionName = "v",
            compositionPart =
              LimitedLinking {linking = "A", limits = (1, Just 0)},
            compositionWhole =
              LimitedLinking {linking = "C", limits = (0, Just 1)}
            },
          Composition {
            compositionName = "x",
            compositionPart =
              LimitedLinking {linking = "B", limits = (0, Just 1)},
            compositionWhole =
              LimitedLinking {linking = "D", limits = (0, Just 1)}
            }
          ]
        },
      option = Annotation {
        annotated = Change {
          add = Just $ Association {
            associationName = "u",
            associationFrom = LimitedLinking {linking = "C", limits = (2, Just 2)},
            associationTo = LimitedLinking {linking = "A", limits = (2, Just 2)}
            },
          remove = Nothing
          },
        annotation = DefiniteArticle
        }
      })
    ],
  classDiagram = ClassDiagram {
    classNames = ["D", "A", "C", "B"],
    relationships = [
      Composition {
        compositionName = "x",
        compositionPart = LimitedLinking {linking = "B", limits = (0, Just 1)},
        compositionWhole = LimitedLinking {linking = "D", limits = (0, Just 1)}
        },
      Aggregation {
        aggregationName = "z",
        aggregationPart = LimitedLinking {linking = "A", limits = (0, Just 1)},
        aggregationWhole = LimitedLinking {linking = "B", limits = (0, Just 1)}
        },
      Composition {
        compositionName = "v",
        compositionPart = LimitedLinking {linking = "A", limits = (1, Just 0)},
        compositionWhole = LimitedLinking {linking = "C", limits = (0, Just 1)}
        },
      Composition {
        compositionName = "y",
        compositionPart = LimitedLinking {linking = "D", limits = (0, Just 2)},
        compositionWhole = LimitedLinking {linking = "A", limits = (0, Just 1)}
        }
      ]
    },
  showExtendedFeedback = False,
  showSolution = False,
  withDirections = False,
  withNames = True
  }

repairIncorrect
  :: RandomGen g
  => AllowedProperties
  -> ClassConfig
  -> ObjectProperties
  -> Maybe Integer
  -> Maybe Int
  -> RandT g IO (Cd, [CdChangeAndCd])
repairIncorrect allowed config objectProperties maxInsts to = do
  e0:_    <- shuffleM $ illegalChanges allowed
  l0:ls   <- shuffleM $ legalChanges allowed
  let addLegals
        | l1:_ <- ls = (l1 .&. noChange :) . (l1 :)
        | otherwise  = id
  c0:_    <- shuffleM $ allChanges allowed
  csm     <- shuffleM $ c0 : noChange : addLegals [e0]
  cs      <- shuffleM $ l0 .&. e0 : noChange : take 2 csm
  let alloyCode = Changes.transformChanges config (toProperty e0) (Just config)
        $ map toProperty cs
  when debug $ liftIO $ do
    putStrLn $ changeName e0
    print $ map changeName cs
    writeFile "repair.als" alloyCode
  instas  <- liftIO $ getInstances maxInsts to alloyCode
  rinstas <- shuffleM instas
  getInstanceWithODs cs rinstas
  where
    drawCd' :: Cd -> Integer -> IO FilePath
    drawCd' cd' n =
      drawCd True True mempty cd' ("cd-" ++ show n ++ ".svg")
    drawOd' :: Od -> Integer -> RandT StdGen IO FilePath
    drawOd' od x =
      drawOd od 0 Back True ("od-" ++ show x)
    getInstanceWithODs _  [] =
      repairIncorrect allowed config objectProperties maxInsts to
    getInstanceWithODs propertyChanges (rinsta:rinstas) = do
      cdInstance <- liftIO $ getChangesAndCds rinsta
      let cd = instanceClassDiagram cdInstance
          chs = instanceChangesAndCds cdInstance
      hints <- liftIO $ zipWithM getOdOrImprovedCd propertyChanges chs
      case sequenceA hints of
        Nothing -> getInstanceWithODs propertyChanges rinstas
        Just odsAndCds -> do
          when debug $ liftIO $ do
            void $ drawCd' cd 0
            uncurry drawCd' `mapM_` zip (map changeClassDiagram chs) [1 ..]
            g <- getStdGen
            flip evalRandT g
              $ uncurry (either (const $ const $ return "") drawOd')
              `mapM_` zip odsAndCds [1 ..]
          let odsAndCdWithArticle = map (first addArticle) odsAndCds
              chs' = map (uniformlyAnnotateChangeAndCd DefiniteArticle) chs
          return (cd, zipWith InValidOption odsAndCdWithArticle chs')
    addArticle = (`Annotation` DefiniteArticle)
    getOdOrImprovedCd propertyChange change
      | isValid propertyChange = fmap Right <$> getOD (changeClassDiagram change)
      | otherwise = fmap Left
        <$> getImprovedCd (changeClassDiagram change) (toProperty propertyChange)
    getImprovedCd cd properties = do
      let alloyCode = Changes.transformImproveCd cd config properties
      changes <- listToMaybe <$> getInstances (Just 1) to alloyCode
      fmap (relationshipChange . head . instanceChangesAndCds)
        <$> traverse getChangesAndCds changes
    getOD :: Cd -> IO (Maybe Od)
    getOD cd = do
      let reversedRelationships = map reverseAssociation $ relationships cd
          maxNObjects = maxObjects $ snd $ classLimits config
          parts = transform
            (cd {relationships = reversedRelationships})
            []
            maxNObjects
            objectProperties
            ""
            ""
          command = createRunCommand
            "cd"
            (length $ classNames cd)
            maxNObjects
            reversedRelationships
            parts
      od <- listToMaybe
        <$> getInstances (Just 1) to (combineParts parts ++ command)
      fmap join $ forM od
        $ runExceptT . alloyInstanceToOd >=> return . eitherToMaybe

data AllowedProperties = AllowedProperties {
  compositionCycles      :: Bool,
  doubleRelationships    :: Bool,
  inheritanceCycles      :: Bool,
  reverseInheritances    :: Bool,
  reverseRelationships   :: Bool,
  selfInheritances       :: Bool,
  selfRelationships      :: Bool,
  wrongAssociationLimits :: Bool,
  wrongCompositionLimits :: Bool
  } deriving (Generic, Read, Show)

allowEverything :: AllowedProperties
allowEverything = AllowedProperties {
  compositionCycles = True,
  doubleRelationships    = True,
  inheritanceCycles      = True,
  reverseInheritances = True,
  reverseRelationships   = True,
  selfInheritances       = True,
  selfRelationships      = True,
  wrongAssociationLimits = True,
  wrongCompositionLimits = True
  }

allChanges :: AllowedProperties -> [PropertyChange]
allChanges c = legalChanges c ++ illegalChanges c

noChange :: PropertyChange
noChange = PropertyChange "none" id id

infixl 9 .&.
(.&.) :: PropertyChange -> PropertyChange -> PropertyChange
PropertyChange n1 o1 v1 .&. PropertyChange n2 o2 v2 = PropertyChange
  (n1 ++ " + " ++ n2)
  (o1 . o2)
  (v1 . v2)

legalChanges :: AllowedProperties -> [PropertyChange]
legalChanges allowed = noChange : [
    PropertyChange "add one self relationship" addSelfRelationships id
  | Modelling.CdOd.RepairCd.selfRelationships allowed] ++ [
    PropertyChange "force double relationships" withDoubleRelationships id
  | doubleRelationships allowed] ++ [
    PropertyChange "force reverse relationships" withReverseRelationships id
  | reverseRelationships allowed]
--    PropertyChange "force multiple inheritances" withMultipleInheritances id
  where
    addSelfRelationships :: RelationshipProperties -> RelationshipProperties
    addSelfRelationships config@RelationshipProperties {..}
      = config { T.selfRelationships = selfRelationships + 1 }
    withDoubleRelationships :: RelationshipProperties -> RelationshipProperties
    withDoubleRelationships config
      = config { hasDoubleRelationships = Just True }
    withReverseRelationships :: RelationshipProperties -> RelationshipProperties
    withReverseRelationships config
      = config { hasReverseRelationships = Just True }
    -- withMultipleInheritances :: RelationshipProperties -> RelationshipProperties
    -- withMultipleInheritances config
    --   = config { hasMultipleInheritances = True }

illegalChanges :: AllowedProperties -> [PropertyChange]
illegalChanges allowed = map ($ const False) $ [
    PropertyChange "add wrong association" addWrongAssocs
  | wrongAssociationLimits allowed] ++ [
    PropertyChange "add wrong composition" addWrongCompositions
  | wrongCompositionLimits allowed] ++ [
    PropertyChange "force inheritance cycles" withNonTrivialInheritanceCycles
  | inheritanceCycles allowed] ++ [
    PropertyChange "force reverse inheritances" withReverseInheritances
  | reverseInheritances allowed] ++ [
    PropertyChange "add self inheritance" addSelfInheritance
  | Modelling.CdOd.RepairCd.selfInheritances allowed] ++ [
    PropertyChange "force composition cycles" withCompositionCycles
  | compositionCycles allowed]
  where
    addWrongAssocs :: RelationshipProperties -> RelationshipProperties
    addWrongAssocs config@RelationshipProperties {..}
      = config { wrongAssocs = wrongAssocs + 1 }
    addWrongCompositions :: RelationshipProperties -> RelationshipProperties
    addWrongCompositions config@RelationshipProperties {..}
      = config { wrongCompositions = wrongCompositions + 1 }
    addSelfInheritance :: RelationshipProperties -> RelationshipProperties
    addSelfInheritance config@RelationshipProperties {..}
      = config { T.selfInheritances = selfInheritances + 1 }
    withReverseInheritances :: RelationshipProperties -> RelationshipProperties
    withReverseInheritances config
      = config { hasReverseInheritances = True }
    withNonTrivialInheritanceCycles
      :: RelationshipProperties
      -> RelationshipProperties
    withNonTrivialInheritanceCycles config
      = config { hasNonTrivialInheritanceCycles = True }
    withCompositionCycles :: RelationshipProperties -> RelationshipProperties
    withCompositionCycles config
      = config { hasCompositionCycles = True }
