module Cli where

-- system
import qualified System.IO as IO

-- internal
import Utils
import qualified Cli.View as V
import qualified Cli.Action as A
import qualified Game

{- impls -}
start :: IO ()
start = do
  IO.hSetBuffering IO.stdin IO.NoBuffering
  IO.hSetEcho IO.stdin False
  loop Game.init

loop :: Game.Game -> IO ()
loop game = do
  -- show view
  V.render game

  -- get action from input
  action <- IO.getChar
    |> fmap A.decode

  -- resolve action
  case action of
    A.Game action ->
      game
        |> Game.update action
        |> loop
    A.Unknown ->
      game
        |> loop
    A.Quit ->
      return ()
