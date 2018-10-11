module Helpers ((<|), (|>), (<<), (>>)) where

import Prelude hiding ((>>))

infixr 0 <|
infixl 0 |>
infixl 9 <<
infixr 9 >>

(|>) :: a -> (a -> b) -> b
x |> f = f x

(<|) :: (a -> b) -> a -> b
f <| x = f x

(<<) :: (b -> c) -> (a -> b) -> (a -> c)
g << f = g . f

(>>) :: (a -> b) -> (b -> c) -> (a -> c)
f >> g = g . f
