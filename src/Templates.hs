{-# LANGUAGE QuasiQuotes #-}

module Templates
  ( startingTemplate
  , exposingTemplates
  , importTemplate
  , importTemplates
  , uppercaseTemplates
  , lowercaseTemplates
  , typeTemplates
  , definitionTemplates
  , exprTemplates
  , elmJsonContents
  ) where

import CodeChunk
import Helpers
import Text.RawString.QQ

startingTemplate :: [CodeChunk]
startingTemplate =
  [T "module ", UI, T " exposing (", EX, T ")\n"] -- module Foo exposing (<EXPOSING>)
    ++ importTemplate
    ++ [T "\n\n", D] -- surprise me!

exposingTemplates :: [[CodeChunk]]
exposingTemplates = [[T ".."], [LI], [UI], [LI, T ", ", UI]]

importTemplate :: [CodeChunk]
importTemplate = [T "\nimport ", UI, I] -- import Foo <IMPORT>

importTemplates :: [[CodeChunk]]
importTemplates =
  [ []
  , [T " as ", UI] -- as Foo
  , [T " exposing (", EX, T ")"] -- exposing (<EXPOSING>)
  , [T " as ", UI, T " exposing (", EX, T ")"] -- as Foo exposing (<EXPOSING>)
  ]
  |> map (\template -> [template, template ++ importTemplate]) -- allow more than one import
  |> concat

uppercaseTemplates :: [String]
uppercaseTemplates =
  [ "Foo"
  , "Bar"
  , "Baz"
  , "Quux"
  , "Main"
  , "Another"
  , "Lalala"
  , "Haha"
  , "Yay"
  , "LoveElm"
  , "MyType"
  , "AnotherType"
  ]

lowercaseTemplates :: [String]
lowercaseTemplates =
  [ "main"
  , "foo"
  , "bar"
  , "baz"
  , "quux"
  , "update"
  , "view"
  , "subscriptions"
  , "add"
  , "init"
  , "x"
  , "y"
  ]

definitionTemplates :: [[CodeChunk]]
definitionTemplates =
  [ [LI, T " = ", E] -- foo = <EXPR>
  , [LI, T " ", LI, T " = ", E] -- foo bar = <EXPR>

  , [LI, T " : ", TY] -- foo : <TYPE>

  , [T "type alias ", UI, T " = ", TY] -- type alias Foo = <TYPE>
  , [T "type alias ", UI, T " ", LI, T " = ", TY] -- type alias Foo bar = <TYPE>

  , [T "type ", UI, T " = ", UI] -- type Foo = Bar
  , [T "type ", UI, T " = ", UI, T " ", TY] -- type Foo = Bar <TYPE>
  , [T "type ", UI, T " = ", UI, T " | " , UI] -- type Foo = Bar | Baz
  , [T "type ", UI, T " = ", UI, T " ", TY, T " | " , UI] -- type Foo = Bar <TYPE> | Baz

  , [T "type ", UI, T " ", LI, T " = ", UI] -- type Foo a = Bar
  , [T "type ", UI, T " ", LI, T " = ", UI, T " ", TY] -- type Foo a = Bar <TYPE>
  , [T "type ", UI, T " ", LI, T " = ", UI, T " | " , UI] -- type Foo a = Bar | Baz
  , [T "type ", UI, T " ", LI, T " = ", UI, T " ", TY, T " | " , UI] -- type Foo a = Bar <TYPE> | Baz
  ]
  |> map (\template -> [template, template ++ [T "\n\n", D]]) -- allow more than one definition
  |> concat

typeTemplates :: [[CodeChunk]]
typeTemplates =
  [ [UI] -- Foo
  , [LI] -- foo
  , [T "comparable"]
  , [T "appendable"]
  , [T "compappend"]
  , [T "number"]
  , [UI, T " ", UI] -- Foo Bar
  , [UI, T " ", LI] -- Foo bar
  , [T "()"]
  , [T "(", TY, T ")"] -- (<TYPE>)
  , [T "(", TY, T ", ", TY, T ")"] -- (<TYPE>, <TYPE>)
  , [TY, T " -> ", TY] -- <TYPE> -> <TYPE>
  -- TODO record types?
  ]

exprTemplates :: [[CodeChunk]]
exprTemplates =
  [ [T "(\\", LI, T " -> ", E, T ")"] -- (\foo -> [])
  , [T "()"]
  , [T "(", E, T ")"] -- ([])
  , [T "{}"]
  , [T "{ ", LI, T " = ", E, T " }"] -- { foo = [] }
  , [T "{ ", LI, T " | ", LI, T " = ", E, T " }"] -- { foo | bar = [] }
  , [E, T " :: ", E] -- [] :: []
  , [T "let ", LI, T " = ", E, T " in ", E] -- let foo = [] in []
  , [T "Int"]
  , [T "Float"]
  , [T "Bool"]
  , [T "Char"]
  , [T "String"]
  , [T "Never"]
  , [LI, T " ", E] -- foo []
  , [T "(", E, T ") ", E] -- ([]) []
  -- TODO as?
  , [E, T " + ", E] -- [] + []
  , [E, T " ++ ", E] -- [] ++ []
  , [E, T " == ", E] -- [] == []
  , [E, T " < ", E] -- [] < []
  , [E, T " <= ", E] -- [] <= []
  , [E, T " > ", E] -- [] > []
  , [E, T " >= ", E] -- [] >= []
  , [UI] -- Foo
  , [LI] -- foo
  , [UI, T ".", UI] -- Foo.Bar
  , [UI, T ".", LI] -- Foo.bar
  , [E, T " && ", E] -- [] && []
  , [E, T " || ", E] -- [] || []
  , [E, T " |> ", E] -- [] |> []
  , [E, T " <| ", E] -- [] <| []
  , [E, T " >> ", E] -- [] >> []
  , [E, T " << ", E] -- [] << []
  , [T "[ ", E, T " ]"] -- \[[]\]
  , [T "\"\"\"", LI, T "\"\"\""] -- """[]"""
  , [T "\"", LI, T "\""] -- "[]"
  , [T "'c'"]
  , [T "1"]
  , [T "True"]
  , [T "1.2"]
  , [T "\"\\x{002D}\""]
  ]

elmJsonContents :: String
elmJsonContents =
  [r|{
    "type": "application",
    "source-directories": [
        "."
    ],
    "elm-version": "0.19.0",
    "dependencies": {
        "direct": {
            "elm/core": "1.0.0",
            "elm/json": "1.0.0"
        },
        "indirect": {}
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}
|]
