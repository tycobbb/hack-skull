module Game.Vec where

-- internal
import Utils

{- types -}
-- A 2D coordinate
data Vec2 = Vec2 {
  x :: Int,
  y :: Int
} deriving (Eq, Show)

{- values -}
-- The unit x-vector
ux :: Vec2
ux = Vec2 1 0

-- The unit y-vector
uy :: Vec2
uy = Vec2 0 1

{- impls -}
{- impls/commands -}
-- Transform both values of the vector
--
-- @param fn The function to call on both components of the vec
transform :: (Int -> Int) -> Vec2 -> Vec2
transform fn vec =
  Vec2 (fn (vec#x)) (fn (vec#y))

{- impls/queries -}
-- Calculates the magnitude of the vector.
mag :: Vec2 -> Int
mag vec =
  (vec#x) * (vec#y)

-- Determines if the position vector is within the size.
contains :: Vec2 -> Vec2 -> Bool
contains pos size =
  (pos#x) >= 0 &&
  (pos#x) < (size#x) &&
  (pos#y) >= 0 &&
  (pos#y) < (size#y)

{- impls/operators -}
instance Num Vec2 where
  left + right  = Vec2 ((left#x) + (right#x)) ((left#y) + (right#y))
  left - right  = Vec2 ((left#x) - (right#x)) ((left#y) - (right#y))
  left * right  = Vec2 ((left#x) * (right#x)) ((left#y) * (right#y))
  negate        = transform negate
  abs           = transform abs
  signum        = transform signum
  fromInteger i = Vec2 (fromInteger i) (fromInteger i)
