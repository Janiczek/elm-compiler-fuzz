{-# LANGUAGE DeriveGeneric #-}

module Project (Project(..), Mutation(..), Code(..), CodeChunk(..), codeString) where

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
    codes <- arbitrary :: Gen [Code]
    let pairs = codes |> map (\code' -> (findModuleName code', code'))
    return
      (Project
        { modules = Map.fromList pairs
        , mutations = []
        }
      )
    where
      findModuleName :: Code -> String
      findModuleName (Code string) =
        -- second word of the first line
        string
        |> lines
        |> head
        |> words
        |> tail
        |> head

  shrink project =
    [project {mutations = mutations'} | mutations' <- shrink (mutations project)]

instance Arbitrary Mutation where
  arbitrary = undefined
  shrink mutation =
    case mutation of
      RenameFile _ _ -> []
      RemoveFile _ -> []
      AddFile name' contents' ->
        [ AddFile {name = name', contents = contents''}
        | contents'' <- shrink contents'
        ]

instance Arbitrary Code where
  -- TODO sometimes it loops a bit too much.
  -- Let's thread an expansion count through and stop all expansions after a limit?
  arbitrary = do
    expand startingTemplate
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
