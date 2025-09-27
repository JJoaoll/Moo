module Utils where

import Data.Text

-- type Name = String
type Name = Text

(|>) :: t1 -> (t1 -> t2) -> t2
x |> f = f x

(||>) :: (a -> b) -> (b -> c) -> a -> c
f ||> g = g . f