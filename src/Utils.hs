module Utils where

type Name = String

(|>) :: t1 -> (t1 -> t2) -> t2
x |> f = f x

(||>) :: (a -> b) -> (b -> c) -> a -> c
f ||> g = g . f