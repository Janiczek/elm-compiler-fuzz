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

writeElmProject :: Project -> IO FilePath
writeElmProject project = do
  tmpDirPath <- getCanonicalTemporaryDirectory
  dirPath <- createTempDirectory tmpDirPath "elm-compiler-fuzz"
  write dirPath ElmJson
  let files = modules project |> Map.toList
  forM files (\(moduleName, code) -> write dirPath (ElmSourceCode moduleName (codeString code)))
  return dirPath


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

mutate :: FilePath -> Project -> IO Project
mutate dirPath project = do
  (mutations', project') <- generate (arbitraryMutations project)
  applyMutationsIO dirPath mutations'
  return project'

report :: Project -> IO ()
report project = do
  putStrLn "Reporting the project"
  putStrLn (show project)

cleanup :: FilePath -> IO ()
cleanup dirPath =
  removeDirectoryRecursive dirPath

arbitraryMutation :: Project -> Gen (Mutation, Project)
arbitraryMutation project = do
  mutation <- oneof
                [ renameFile
                , removeFile
                , addFile
                ]
  let project' = project { mutations = mutations project ++ [mutation] }
  let project'' = applyMutation mutation project'
  return (mutation, project'')
    where
      renameFile :: Gen Mutation
      renameFile = do
        let filenames = Map.keys (modules project)
        if filenames == [] then
          addFile
        else do
          old <- elements filenames
          new <- elements uppercaseTemplates
          return (RenameFile { oldName = old, newName = new })

      removeFile :: Gen Mutation
      removeFile = do
        let filenames = Map.keys (modules project)
        if filenames == [] then
          addFile
        else do
          filename <- elements filenames
          return (RemoveFile { name = filename })

      addFile :: Gen Mutation
      addFile = do
        filename <- moduleNameGen (Map.keys (modules project))
        dependencies <- arbitraryDependencies (filename : Map.keys (modules project))
        contents <- arbitraryCode dependencies filename
        let filename = findModuleName contents
        return (AddFile filename contents)

arbitraryMutations :: Project -> Gen ([Mutation], Project)
arbitraryMutations project = do
  count <- arbitrarySizedNatural |> resize 10
  arbitraryMutations' count project []
  where
    arbitraryMutations' :: Int -> Project -> [Mutation] -> Gen ([Mutation], Project)
    arbitraryMutations' count project mutations =
      if count == 0 then
        return (mutations, project)
      else do
        let count' = count - 1
        (mutation, project') <- arbitraryMutation project
        let mutations' = mutations ++ [mutation]
        arbitraryMutations' count' project' mutations'

applyMutation :: Mutation -> Project -> Project
applyMutation mutation project =
  case mutation of
    RenameFile oldName newName ->
      project { modules = modules project
                  |> Map.mapKeys (\name -> if name == oldName then newName else name)
              }

    RemoveFile name ->
      project { modules = modules project
                  |> Map.delete name
              }

    AddFile name contents ->
      project { modules = modules project
                  |> Map.insert name contents
              }

applyMutationsIO :: FilePath -> [Mutation] -> IO ()
applyMutationsIO dirPath mutations =
  forM_ mutations (applyMutationIO dirPath)


applyMutationIO :: FilePath -> Mutation -> IO ()
applyMutationIO dirPath mutation =
  case mutation of
    RenameFile old new ->
      renameFile
        (dirPath </> (old ++ ".elm"))
        (dirPath </> (new ++ ".elm"))

    RemoveFile name ->
      removeFile (dirPath </> name ++ ".elm")

    AddFile name contents ->
      write dirPath (ElmSourceCode name (codeString contents))
