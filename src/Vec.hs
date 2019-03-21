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
-- the unit x-vector
ux :: Vec2
ux = Vec2 1 0

-- the unit y-vector
uy :: Vec2
uy = Vec2 0 1

{- impls -}
-- produces a new vector assigning the x value
setX :: Int -> Vec2 -> Vec2
setX x vec=
  Vec2 x (y vec)

-- produces a new vector assigning the y value
setY :: Int -> Vec2 -> Vec2
setY y vec =
  Vec2 (x vec) y

-- transform both values of the vector
transform :: (Int -> Int) -> Vec2 -> Vec2
transform fn vec =
  Vec2 (fn (x vec)) (fn (y vec))

-- numeric operators
instance Num Vec2 where
  left + right  = Vec2 ((x left) + (x right)) ((y left) + (y right))
  left - right  = Vec2 ((x left) - (x right)) ((y left) - (y right))
  left * right  = Vec2 ((x left) * (x right)) ((y left) * (y right))
  abs           = transform abs
  signum        = transform signum
  fromInteger i = Vec2 (fromInteger i) (fromInteger i)
