{-# LANGUAGE DeriveGeneric #-}

module Project
  ( Project(..)
  , Mutation(..)
  , Code(..)
  , CodeChunk(..)
  , codeString
  , findModuleName
  , arbitraryCode
  , moduleNameGen
  ) where

import GHC.Generics (Generic)
import Test.QuickCheck
import Helpers
import CodeChunk
import Templates
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

data Project = Project
  { modules :: Map String Code
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
    filenames <- listOf moduleNameGen
    codes <- mapM arbitraryCode filenames 
    let pairs = codes |> map (\code' -> (findModuleName code', code'))
    return
      (Project
        { modules = Map.fromList pairs
        , mutations = []
        }
      )

  shrink project =
    [project {mutations = mutations'} | mutations' <- shrink (mutations project)]

instance Arbitrary Mutation where
  arbitrary = undefined -- use Lib.arbitraryMutation* instead
  shrink mutation =
    case mutation of
      RenameFile _ _ -> []
      RemoveFile _ -> []
      AddFile name' contents' ->
        [ AddFile {name = name', contents = contents''}
        | contents'' <- shrink contents'
        ]

instance Arbitrary Code where
  arbitrary = undefined -- use arbitraryCode


arbitraryCode :: String -> Gen Code
arbitraryCode moduleName = do
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
          EX -> expandFromList exposingTemplates
          I -> expandFromList importTemplates

      expandFromStrings :: [String] -> Gen [CodeChunk]
      expandFromStrings list =
        elements list
          |> fmap (\string -> [T string])

      expandFromList :: [[CodeChunk]] -> Gen [CodeChunk]
      expandFromList templates = do
        template <- elements templates
        expandChunks template


findModuleName :: Code -> String
findModuleName (Code string) =
  -- second word of the first line
  string
  |> lines
  |> head
  |> words
  |> tail
  |> head

moduleNameGen :: Gen String
moduleNameGen = resize 4 <| do
  firstChar <- elements ['A'..'Z']
  rest <- listOf (elements ['a'..'z'])
  return (firstChar : rest)
