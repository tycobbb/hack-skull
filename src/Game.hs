module Game where

-- internal
import Utils
import qualified Generate
import qualified Render

{- impls -}
-- The game root
--
-- @returns The state as a string.
root :: String
root =
  Generate.world
    |> Render.render
