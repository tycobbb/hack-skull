module Core.Utils where

{- impls -}
{- impls/operators -}
(|>) :: a -> (a -> b) -> b
(|>) =
  flip ($)

(#) :: a -> (a -> b) -> b
(#) =
  flip ($)

{- impls/tuple -}
mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst fn (fst, snd) =
  (fn fst, snd)
