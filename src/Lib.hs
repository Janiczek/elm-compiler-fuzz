{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( generateElmProject
  , writeElmProject
  , runElmMake
  , didCrash
  , mutate
  , report
  , cleanup
  ) where

import Helpers
import Project
import System.IO.Temp
import Turtle hiding (FilePath, (</>))
import Data.List as List
import Test.QuickCheck
import Control.Monad
import System.FilePath.Posix
import Templates
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as T
import System.Directory


data CompilationResult
  = Crash
  | NoCrash
  deriving (Eq)


data ElmFile
  = ElmJson
  | ElmSourceCode
      { module_ :: String
      , contents :: String
      }
  deriving (Eq)

generateElmProject :: IO Project
generateElmProject = do
  generate arbitrary

writeElmProject :: Project -> IO FilePath
writeElmProject project = do
  tmpDirPath <- getCanonicalTemporaryDirectory
  dirPath <- createTempDirectory tmpDirPath "elm-compiler-fuzz"
  -- TODO remove dirPath
  write dirPath ElmJson
  let files = modules project |> Map.toList
  forM files (\(moduleName, code) -> write dirPath (ElmSourceCode moduleName (codeString code)))
  return dirPath
  where
    write :: FilePath -> ElmFile -> IO ()
    write dirPath elmFile = do
      let (fileName, fileContents) = case elmFile of
            ElmJson ->
              ("elm.json", elmJsonContents)
            ElmSourceCode moduleName contents ->
              ( moduleName ++ ".elm"
              , contents
              )
      let filePath = dirPath </> fileName
      writeFile filePath fileContents


runElmMake :: Maybe String -> FilePath -> Project -> IO CompilationResult
runElmMake maybeElmPath dirPath project = do
  let elmPath = maybeElmPath |> fromMaybe "elm"
  cd (dirPath |> T.pack |> fromText)
  (exitCode, _, stderr) <- shellStrictWithErr (T.pack (elmPath ++ " make *.elm")) empty
  return <|
    if exitCode == ExitFailure 1 && List.isInfixOf "CallStack" (T.unpack stderr) then
      Crash
    else
      NoCrash

didCrash :: CompilationResult -> Bool
didCrash result =
  result == Crash

mutate :: Project -> IO Project
mutate project = do
  putStrLn "Mutating"
  mutation <- undefined :: IO Mutation
  let project' = addMutation mutation project
  return project'
  where
    addMutation :: Mutation -> Project -> Project
    addMutation mutation project =
      project {mutations = mutations project ++ [mutation]}

report :: Project -> IO ()
report project = do
  putStrLn "Reporting the project"
  undefined

cleanup :: FilePath -> IO ()
cleanup dirPath =
  removeDirectoryRecursive dirPath
