module Cli where

-- system
import qualified System.IO as IO
import qualified System.Random as Random
import qualified Data.Tuple as Tuple

-- internal
import Core.Utils
import qualified Core.Rand as R
import Core.Rand (Rand)
import qualified Game as G
import Game (Game)
import qualified Cli.View as V
import qualified Cli.Action as A

{- impls -}
start :: IO ()
start = do
  IO.hSetEcho IO.stdin False
  IO.hSetBuffering IO.stdin IO.NoBuffering

  gen <- Random.getStdGen
  G.init
    |> loop gen

loop :: Random.StdGen -> Rand Game -> IO ()
loop gen rand = do
  let
    (gen', game) =
      R.call rand gen

  -- show view
  game
    |> V.render

  -- get action from input
  action <-
    IO.getChar
      |> fmap A.decode

  -- resolve action
  case action of
    A.Game action' ->
      game
        |> G.update action'
        |> loop gen'
    A.Unknown ->
      game
        |> pure
        |> loop gen'
    A.Quit -> do
      V.reset
