module Core.Extra where

{- impls -}
{- impls/operators -}
(|>) :: a -> (a -> b) -> b
(|>) =
  flip ($)

(||>) :: a -> (a -> b) -> b
(||>) =
  flip ($)

(#) :: a -> (a -> b) -> b
(#) =
  flip ($)

{- impls/tuples -}
dupe :: a -> (a, a)
dupe value =
  (value, value)

{- impls/monads -}
whenM :: Applicative f => Bool -> (a -> f a) -> (a -> f a)
whenM condition continuation =
  if condition then continuation else pure
