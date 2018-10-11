{-# LANGUAGE DeriveGeneric #-}

module CodeChunk (CodeChunk(..), onlyText) where

import GHC.Generics (Generic)

{-|
    T = Text
    UI = Uppercase Identifier ("Abcde", "Hello", "Foo")
    LI = Lowercase Identifier ("abcde", "hello", "foo")
    D = Top-level definition
    E = Expression
    TY = Type
    EX = Exposing
    I = Import
-}
data CodeChunk
  = T String
  | UI
  | LI
  | D
  | E
  | TY
  | EX
  | I
  deriving (Eq, Generic)

onlyText :: CodeChunk -> Maybe String
onlyText chunk =
  case chunk of
      T text -> Just text
      _ -> Nothing
