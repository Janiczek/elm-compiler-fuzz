{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Lib
import Data.Set (Set)
import qualified Data.Set as Set
import Project
import System.Console.CmdArgs.Implicit
import qualified Test.QuickCheck as QC

main :: IO ()
main = do
  putStrLn "The fuzzer is starting"
  args <- cmdArgs arguments
  main' args Set.empty

testMain :: IO ()
testMain = do
  main' (Arguments (Just "/home/martin/.bin/elm-0.19-master")) Set.empty

data Arguments =
  Arguments
    { elmPath :: Maybe String
    }
  deriving (Show, Data, Typeable)


arguments :: Arguments
arguments =
  Arguments
    { elmPath = def &= help "Path to the Elm 0.19 executable"
    }
    &= summary "elm-compiler-fuzz: Fuzzer for the Elm compiler, trying to provoke caching errors."


main' :: Arguments -> Set Project -> IO ()
main' args crashingProjects = do
  putStr "."
  project <- generateElmProject
  dirPath <- writeElmProject project
  result <- runElmMake (elmPath args) dirPath project
  if didCrash result then do
    putStr " CRASH! "
    handleCrash args crashingProjects dirPath project
  else do
    project' <- mutate project
    result' <- runElmMake (elmPath args) dirPath project'
    if didCrash result' then
      handleCrash args crashingProjects dirPath project'
    else
      loop args crashingProjects dirPath

handleCrash :: Arguments -> Set Project -> FilePath -> Project -> IO ()
handleCrash args crashingProjects dirPath project = do
  putStrLn "Handling crash, how exciting!"
  project' <- shrinkFully project
  if isNewCrash crashingProjects project'
    then do
      report project'
      let crashingProjects' = save project' crashingProjects
      loop args crashingProjects' dirPath
    else
      loop args crashingProjects dirPath

loop :: Arguments -> Set Project -> FilePath -> IO ()
loop args crashingProjects dirPath = do
  cleanup dirPath
  main' args crashingProjects


isNewCrash :: Set Project -> Project -> Bool
isNewCrash crashingProjects project =
  Set.member project crashingProjects

save :: Project -> Set Project -> Set Project
save project crashingProjects =
  Set.insert project crashingProjects

shrinkFully :: Project -> IO Project
shrinkFully project = do
  putStrLn "Shrinking fully"
  let currentShrinks = QC.shrink project
  undefined
