module Main (main) where

import Modelling.CdOd.Auxiliary.Lexer (lexer)
import Modelling.CdOd.Auxiliary.Parser (parser)
import Modelling.CdOd.CD2Alloy.Transform (
  LinguisticReuse (None),
  Parts (..),
  createRunCommand,
  transform,
  )
import Modelling.CdOd.Types (
  ClassDiagram (..),
  ObjectProperties (..),
  Relationship (..),
  maxFiveObjects,
  )

import Control.Monad
import Data.Maybe                       (mapMaybe)
import Data.Ratio                       ((%))
import Data.Time.LocalTime
import System.Environment (getArgs)

run :: String -> Maybe FilePath -> Bool -> String -> IO ()
run input output template index = do
  let tokens = lexer input
  let parsed = parser tokens
  let cd = uncurry toCd parsed
  time <- getZonedTime
  let parts = transform
        None
        cd
        Nothing
        []
        maxFiveObjects
        objectProperties
        index
        (show time)
      p1 = part1 parts
      p2 = part2 parts
      p3 = part3 parts
      p4 = createRunCommand
        ("cd" ++ index)
        (length $ classNames cd)
        maxFiveObjects
        (relationships cd)
  case output of
    Just file -> do
      when template $ let out = file ++ ".part1" in writeFile out p1 >> putStrLn ("Some output written to " ++ out)
      let out = file ++ ".part2" in writeFile out p2 >> putStrLn ("Some output written to " ++ out)
      let out = file ++ ".part3" in writeFile out p3 >> putStrLn ("Some output written to " ++ out)
      when template $ let out = file ++ ".part4" in writeFile out p4 >> putStrLn ("Some output written to " ++ out)
    Nothing -> putStrLn $ (if template then p1 else "") ++ p2 ++ p3 ++ (if template then p4 else "")
  where
    toCd cs es = ClassDiagram {
      classNames = map fst cs,
      relationships = mapMaybe (uncurry toInheritance) cs ++ es
      }
    toInheritance sub super = Inheritance sub <$> super
    objectProperties = ObjectProperties {
      anonymousObjectProportion = 0 % 1,
      completelyInhabited = Nothing,
      hasLimitedIsolatedObjects = True,
      hasSelfLoops = Nothing,
      usesEveryRelationshipName = Nothing
      }

main :: IO ()
main = do
  args <- getArgs
  case args of
   [] -> getContents >>= \contents -> run contents Nothing False ""
   [input] -> readFile input >>= \contents -> run contents Nothing False ""
   [input, output] -> readFile input >>= \contents -> run contents (Just output) True ""
   [input, output, index] -> readFile input >>= \contents -> run contents (Just output) True (show (read index :: Int))
   _ -> error "zu viele Parameter"
