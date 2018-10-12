{-# LANGUAGE QuasiQuotes #-}

module Templates
  ( startingTemplate
  , importTemplate
  , importTemplates
  , uppercaseTemplates
  , lowercaseTemplates
  , opTemplates
  , typeTemplates
  , definitionTemplates
  , exprTemplates
  , elmJsonContents
  ) where

import CodeChunk
import Helpers
import Text.RawString.QQ

startingTemplate :: String -> [CodeChunk]
startingTemplate moduleName =
  [T "module ", T moduleName , T " exposing (..)\n\n"] -- module Foo exposing (..)
    ++ [T "import Html\n"]
    ++ [IS, T "\n\n"] -- imports according to generated dependencies
    ++ [T "main = Html.text \"dummy\"\n\n"] -- eliminate "BAD MAIN TYPE" error
    ++ [D] -- definition(s). surprise me!

importTemplate :: String -> [CodeChunk]
importTemplate dependency =
  [T "\nimport ", T dependency, I] -- import Foo <IMPORT>

importTemplates :: [[CodeChunk]]
importTemplates =
  [ []
  , [T " as ", UI] -- as Foo
  , [T " exposing (..)"] -- exposing (..)
  ]

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

  --, [LI, T " : ", TY] -- foo : <TYPE>

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
  , [T "Int"]
  , [T "Float"]
  , [T "Bool"]
  , [T "Char"]
  , [T "String"]
  , [T "Never"]
  , [UI, T " ", UI] -- Foo Bar
  , [UI, T " ", LI] -- Foo bar
  , [T "()"]
  , [T "(", TY, T ")"] -- (<TYPE>)
  , [T "(", TY, T ", ", TY, T ")"] -- (<TYPE>, <TYPE>)
  , [T "(", TY, T " -> ", TY, T ")"] -- <TYPE> -> <TYPE>
  -- TODO record types?
  ]

opTemplates :: [String]
opTemplates =
  [ "&&"
  , "||"
  , "|>"
  , "<|"
  , ">>"
  , "<<"
  , "+"
  , "++"
  , "=="
  , "<"
  , "<="
  , ">"
  , ">="
  , "::"
  ]

exprTemplates :: [[CodeChunk]]
exprTemplates =
  [ [T "(\\", LI, T " -> ", E, T ")"] -- (\foo -> [])
  , [T "()"]
  , [T "(", E, T ")"] -- ([])
  , [T "{}"]
  , [T "{ ", LI, T " = ", E, T " }"] -- { foo = [] }
  , [T "{ ", LI, T " | ", LI, T " = ", E, T " }"] -- { foo | bar = [] }
  , [T "let ", LI, T " = ", E, T " in ", E] -- let foo = [] in []
  , [LI, T " ", E] -- foo []
  , [T "(", E, T ") ", E] -- ([]) []
  -- TODO as?
  , [UI] -- Foo
  , [LI] -- foo
  --, [UI, T ".", UI] -- Foo.Bar
  --, [UI, T ".", LI] -- Foo.bar
  , [E, OP, E] -- [] <OP> []
  , [T "[ ", E, T " ]"] -- \[[]\]
  , [T "\"\"\"", LI, T "\"\"\""] -- """[]"""
  , [T "\"", LI, T "\""] -- "[]"
  , [T "'c'"]
  , [T "1"]
  , [T "True"]
  , [T "1.2"]
  , [T "\"\\u{002D}\""]
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
            "elm/browser": "1.0.0",
            "elm/core": "1.0.0",
            "elm/html": "1.0.0"
        },
        "indirect": {
            "elm/json": "1.0.0",
            "elm/time": "1.0.0",
            "elm/url": "1.0.0",
            "elm/virtual-dom": "1.0.2"
        }
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}
|]
