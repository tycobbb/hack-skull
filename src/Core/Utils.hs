module Core.Utils where

{- impls -}
{- impls/operators -}
(|>) :: a -> (a -> b) -> b
(|>) =
  flip ($)

(#) :: a -> (a -> b) -> b
(#) =
  flip ($)
