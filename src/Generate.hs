module Generate where

-- internal
import qualified Vec as V
import qualified World as W

{- impls -}
-- generates the world
world :: W.World
world = W.World {
  W.room   = W.Room  (V.Vec2 4 4),
  W.player = W.Actor (V.Vec2 1 1)
}
