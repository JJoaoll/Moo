module Utils where

import Data.Text

-- type Name = String
type Name = Text

-- infixl 1 |>

-- use & instead
-- (|>) :: α -> (α-> β) -> β
-- x |> f = f x

-- use . instead
-- (||>) :: (α -> β) -> (β -> γ) -> α -> γ
-- f ||> g = g . f