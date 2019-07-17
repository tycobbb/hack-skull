module Game where

-- internal
import Core.Utils
import qualified Core.Rand as R
import Core.Rand (Rand, RandGen)
import qualified Game.World as W
import Game.World (World)
import qualified Game.Vec as V

{- types -}
-- The game world.
data Game = Game
  { world :: W.World
  }

data Action
  = MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight

{- impls -}
-- Initializes the game.
--
-- @param gen A random generator
--
-- @returns The initial game state
init :: RandGen -> Rand Game
init gen =
  W.init gen
    |> R.map Game

-- Updates the game for a specific action.
--
-- @param action The action to apply to the game
-- @param game   The current game state
--
-- @returns The next game state
update :: Action -> Rand Game -> Rand Game
update action =
  case action of
    MoveUp ->
      updateWorld (W.movePlayer (-V.uy))
    MoveDown ->
      updateWorld (W.movePlayer V.uy)
    MoveLeft ->
      updateWorld (W.movePlayer (-V.ux))
    MoveRight ->
      updateWorld (W.movePlayer V.ux)

updateWorld :: (World -> World) -> Rand Game -> Rand Game
updateWorld fn =
  R.map (\game -> game { world = fn (game#world) })
