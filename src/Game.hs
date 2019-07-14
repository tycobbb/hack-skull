module Game where

-- internal
import Utils
import qualified Game.World as W
import qualified Game.Vec as V

{- types -}
-- The game world.
data Game = Game {
  world :: W.World
}

data Action
  = MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight

{- impls -}
-- Initializes the game
--
-- @returns The initial game state.
init :: Game
init =
  Game {
    world = W.init
  }

-- Updates the game for a specific action.
--
-- @param action The action to apply to the game
-- @param game   The current game state
--
-- @returns The next game state
update :: Action -> Game -> Game
update action game =
  case action of
    MoveUp ->
      game
        |> updateWorld (W.movePlayer (-V.uy))
    MoveDown ->
      game
        |> updateWorld (W.movePlayer V.uy)
    MoveLeft ->
      game
        |> updateWorld (W.movePlayer (-V.ux))
    MoveRight ->
      game
        |> updateWorld (W.movePlayer V.ux)

updateWorld :: (W.World -> W.World) -> Game -> Game
updateWorld fn game =
  game {
    world = fn (game#world)
  }
