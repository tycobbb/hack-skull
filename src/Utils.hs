module Utils where

{- impls -}
{- impls/operators -}
(|>) :: a -> (a -> b) -> b
(|>) =
  flip ($)
