module Utils where

{- operators -}
(|>) :: a -> (a -> b) -> b
(|>) =
  flip ($)
