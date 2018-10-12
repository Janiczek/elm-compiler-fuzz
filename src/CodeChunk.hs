{-# LANGUAGE DeriveGeneric #-}

module CodeChunk (CodeChunk(..), onlyText) where

import GHC.Generics (Generic)

{-|
    T = Text
    UI = Uppercase Identifier ("Abcde", "Hello", "Foo")
    LI = Lowercase Identifier ("abcde", "hello", "foo")
    OP = Operator
    D = Top-level definition
    E = Expression
    TY = Type
    EX = Exposing
    I = Import
    IS = Imports (all of them, according to generated dependencies)
-}
data CodeChunk
  = T String
  | UI
  | LI
  | OP
  | D
  | E
  | TY
  | EX
  | I
  | IS
  deriving (Eq, Generic)

onlyText :: CodeChunk -> Maybe String
onlyText chunk =
  case chunk of
      T text -> Just text
      _ -> Nothing
