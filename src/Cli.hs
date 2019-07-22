module Cli where

-- system
import qualified System.IO as IO
import qualified System.Random as Random

-- internal
import Core.Utils
import qualified Cli.View as V
import qualified Cli.Action as A
import qualified Game as G
import Game (Game)

{- impls -}
start :: IO ()
start = do
  -- enable raw input
  IO.hSetBuffering IO.stdin IO.NoBuffering
  IO.hSetEcho IO.stdin False

  -- start game loop
  -- Random.setStdGen (Random.mkStdGen 5)
  gen <- Random.getStdGen
  G.init gen
    |> loop

loop :: (Game.Game, Random.StdGen) -> IO ()
loop game = do
  -- show view
  V.render (game#fst)

  -- get action from input
  action <- IO.getChar
    |> fmap A.decode

  -- resolve action
  case action of
    A.Game action ->
      game
        |> G.update action
        |> loop
    A.Unknown ->
      game
        |> loop
    A.Quit -> do
      V.reset
      return ()
