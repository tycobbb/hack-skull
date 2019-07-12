module Vec where

-- internal
import Utils

{- types -}
-- A 2D coordinate
data Vec2 = Vec2 {
  x :: Int,
  y :: Int
} deriving (Eq)

{- values -}
-- The unit x-vector
ux :: Vec2
ux = Vec2 1 0

-- The unit y-vector
uy :: Vec2
uy = Vec2 0 1

{- impls -}
-- Produces a new vector assigning the x-value
--
-- @param x The x-value to set on the vec
setX :: Int -> Vec2 -> Vec2
setX x vec =
  vec { x = x }

-- Produces a new vector assigning the y-value
--
-- @param y The y-value to set on the vec
setY :: Int -> Vec2 -> Vec2
setY y vec =
  vec { y = y }

-- Transform both values of the vector
-- 
-- @param fn The function to call on both components of the vec
transform :: (Int -> Int) -> Vec2 -> Vec2
transform fn vec =
  Vec2 (fn (x vec)) (fn (y vec))

{- impls/operators -}
instance Num Vec2 where
  left + right  = Vec2 ((x left) + (x right)) ((y left) + (y right))
  left - right  = Vec2 ((x left) - (x right)) ((y left) - (y right))
  left * right  = Vec2 ((x left) * (x right)) ((y left) * (y right))
  negate        = transform negate
  abs           = transform abs
  signum        = transform signum
  fromInteger i = Vec2 (fromInteger i) (fromInteger i)
