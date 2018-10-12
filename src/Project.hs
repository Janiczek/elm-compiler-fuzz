{-# LANGUAGE DeriveGeneric #-}


module Project
  ( Project(..)
  , Mutation(..)
  , Code(..)
  , CodeChunk(..)
  , codeString
  , findModuleName
  , arbitraryCode
  , arbitraryDependencies
  , moduleNameGen
  ) where

import GHC.Generics (Generic)
import Test.QuickCheck
import qualified Data.List as List
import Helpers
import CodeChunk
import Templates
import Data.Graph (Graph)
import qualified Data.Graph as Graph
import Data.Maybe
import Data.Map (Map, (!))
import qualified Data.Map as Map

data Project = Project
  { modules :: Map String Code
  , dependencies :: [(String, String)]
  , mutations :: [Mutation]
  } deriving (Eq, Generic, Show, Ord)

data Mutation
  = RenameFile { oldName :: String
               , newName :: String }
  | RemoveFile { name :: String }
  | AddFile { name :: String
            , contents :: Code }
  deriving (Eq, Generic, Show, Ord)

data Code =
  Code String
  deriving (Eq, Generic, Show, Ord)


codeString :: Code -> String
codeString (Code string) = string


instance Arbitrary Project where
  arbitrary = do
    filenames <- listOf1 (moduleNameGen [])
    dependencies <- arbitraryDependencies filenames

    pairs <- mapM (\filename -> do
                code <- arbitraryCode dependencies filename
                return (filename, code)
             ) filenames
    return
      (Project
        { modules = Map.fromList pairs
        , dependencies = dependencies
        , mutations = []
        }
      )

  shrink project =
    [project {mutations = mutations'} | mutations' <- shrink (mutations project)]

instance Arbitrary Mutation where
  arbitrary = error "use Lib.arbitraryMutation* instead of arbitrary :: Gen Mutation"
  shrink mutation =
    case mutation of
      RenameFile _ _ -> []
      RemoveFile _ -> []
      AddFile name' contents' ->
        [ AddFile {name = name', contents = contents''}
        | contents'' <- shrink contents'
        ]

instance Arbitrary Code where
  arbitrary = error "use Project.arbitraryCode instead of arbitrary :: Gen Code"


arbitraryCode :: [(String, String)] -> String -> Gen Code
arbitraryCode dependencies moduleName = do
    expand (startingTemplate moduleName)
    where
      expand :: [CodeChunk] -> Gen Code
      expand chunks = do
        chunks' <- expandChunks chunks
        let string = chunks' |> mapMaybe onlyText |> concat
        return (Code string)

      expandChunks :: [CodeChunk] -> Gen [CodeChunk]
      expandChunks chunks =
        chunks |> map expandChunk |> sequence |> fmap concat

      expandChunk :: CodeChunk -> Gen [CodeChunk]
      expandChunk chunk =
        case chunk of
          T _ -> return [chunk]
          UI -> expandFromStrings uppercaseTemplates
          LI -> expandFromStrings lowercaseTemplates
          OP -> expandFromStrings opTemplates
          D -> expandFromList definitionTemplates
          E -> expandFromList exprTemplates
          TY -> expandFromList typeTemplates
          I -> expandFromList importTemplates
          IS -> return imports 

      expandFromStrings :: [String] -> Gen [CodeChunk]
      expandFromStrings list =
        elements list
          |> fmap (\string -> [T string])

      expandFromList :: [[CodeChunk]] -> Gen [CodeChunk]
      expandFromList templates = do
        template <- elements templates
        expandChunks template

      imports :: [CodeChunk]
      imports = 
        concatMap importTemplate allowedDependencies

      allowedDependencies :: [String]
      allowedDependencies =
        dependencies
        |> mapMaybe (\(from, to) ->
          if from == moduleName then
            Just to
          else
            Nothing
        )

findModuleName :: Code -> String
findModuleName (Code string) =
  -- second word of the first line
  string
  |> lines
  |> head
  |> words
  |> tail
  |> head

moduleNameGen :: [String] -> Gen String
moduleNameGen existingModules =
  gen
  |> resize 4
  |> flip suchThat (\name -> not (elem name existingModules))
  where
    gen :: Gen String
    gen = do
      firstChar <- elements ['A'..'Z']
      rest <- listOf (elements ['a'..'z'])
      return (firstChar : rest)

arbitraryDependencies :: [String] -> Gen [(String, String)]
arbitraryDependencies modules =
  if length modules < 2 then
    return []
  else do
    tries <- resize 10 arbitrary :: Gen Int
    arbitraryDependencies' tries modules []
      where
        -- generate dependencies graph without a cycle
        arbitraryDependencies' :: Int -> [String] -> [(String, String)] -> Gen [(String, String)]
        arbitraryDependencies' triesLeft modules acc =
          if triesLeft <= 0 then
            return acc
          else do
            let triesLeft' = triesLeft - 1

            pairing@(from,to) <- arbitraryPairing modules

            let acc' = if wouldCreateCycle pairing acc then
                          acc
                       else
                         pairing : acc

            arbitraryDependencies' triesLeft' modules acc'

        wouldCreateCycle :: (String, String) -> [(String, String)] -> Bool
        wouldCreateCycle pairing@(from,to) pairings =
          -- does a path in the opposite direction exist in the current graph?
          Graph.path graph to' from'
          where
            allVertices :: [String]
            allVertices =
              pairing : pairings
              |> concatMap (\(from, to) -> [from,to])
              |> List.nub
            
            maxId :: Map String Int -> Int
            maxId mapping =
              Map.elems mapping
              |> foldl max 0

            -- mapping from String (filenames) to Int (Graph needs them)
            mapping :: Map String Int
            mapping =
              allVertices
                |> foldl (\acc vertex ->
                    if Map.member vertex acc then
                      acc
                    else
                      acc |> Map.insert vertex (maxId acc + 1)
                    ) Map.empty

            bounds = (0, maxId mapping)

            allPairings :: [(String, String)]
            allPairings =
              pairing : pairings

            edges :: [(Int, Int)]
            edges =
              allPairings
                |> map (\(from, to) -> (mapping ! from, mapping ! to))

            graph :: Graph
            graph =
              Graph.buildG bounds edges

            to' :: Int
            to' = mapping ! to

            from' :: Int
            from' = mapping ! from


arbitraryPairing :: [String] -> Gen (String, String)
arbitraryPairing modules = do
  first <- elements modules
  let restOfModules = List.delete first modules
  second <- elements restOfModules
  return (first, second)

