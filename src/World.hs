module World where

-- internal
import Utils
import qualified Vec as V

{- types -}
-- The game world.
data World = World {
  ground :: Ground,
  player :: Actor
}

-- A ground
data Ground = Ground {
  size :: V.Vec2
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
    ground = Ground (V.Vec2 4 4),
    player = Actor (V.Vec2 1 1)
  }

{- impls/commands -}
-- Moves the player by the specified offset.
--
-- @param offset The delta to move the player by
movePlayer :: V.Vec2 -> World -> World
movePlayer offset world =
  let
    moved = moveActor offset (world#player)
  in
    if V.contains (moved#pos) (world#ground#size) then
      world { player = moved }
    else
      world

moveActor :: V.Vec2 -> Actor -> Actor
moveActor offset actor =
  actor {
    pos = (actor#pos) + offset
  }

