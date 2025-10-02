module Utils where

import Data.Text

-- type Name = String
type Name = Text

infixl 1 |>

(|>) :: α -> (α-> β) -> β
x |> f = f x

(||>) :: (α -> β) -> (β -> γ) -> α -> γ
f ||> g = g . f