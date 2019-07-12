module World where

-- internal
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

{- impls -}
-- Constructs a world with a valid initial state.
init :: World
init = 
  World {
    room   = Room  (V.Vec2 4 4),
    player = Actor (V.Vec2 1 1)
  }

{- impls/commands -}
-- Moves the player by the specified offset.
-- 
-- @param offset The delta to move the player by
movePlayer :: V.Vec2 -> World -> World
movePlayer offset world =
  world {
    player = moveActor offset (player world)
  }

moveActor :: V.Vec2 -> Actor -> Actor
moveActor offset actor =
  actor {
    pos = (pos actor) + offset
  }

