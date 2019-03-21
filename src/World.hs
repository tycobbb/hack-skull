module World where

-- interal
import qualified Vec as V

{- types -}
-- The game world.
data World = World {
  room   :: Room,
  player :: Actor
}

-- A room
data Room = Room {
  bounds :: V.Vec2
}

-- A movable actor
data Actor = Actor {
  pos :: V.Vec2
}
