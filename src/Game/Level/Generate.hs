module Game.Level.Generate where

-- external
import Data.Functor
import qualified Data.Maybe as Maybe
import qualified Data.Tuple as Tuple
import qualified Data.List as List
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Control.Monad

-- internal
import Core.Extra
import qualified Core.Rand as R
import Core.Rand (Rand)
import qualified Game.Vec as V
import Game.Vec (Vec2)
import qualified Game.Level.Grid as G
import Game.Level.Grid (Grid(..))
import qualified Game.Level.Cell as C
import Game.Level.Cell (Cell(..), Room(..))
import qualified Game.Level.Neighbors as N

{- impls -}
resolve :: Vec2 -> Rand Grid
resolve size =
  seed size
    >>= stepN 15

{- impls/seed -}
seed :: Vec2 -> Rand Grid
seed size =
  R.sampleN options (V.mag size)
    <&> seedRooms
    <&> Vector.fromList
    <&> Grid size
  where
    options =
      [ (1,  Floor . Room)
      , (99, const Empty)
      ]

seedRooms :: [Int -> Cell] -> [Cell]
seedRooms =
  Tuple.snd . foldr seedRoom (0, [])

seedRoom :: (Int -> Cell) -> (Int, [Cell]) -> (Int, [Cell])
seedRoom addCell (roomId, cells) =
  let
    cell =
      addCell roomId
    nextId =
      if C.isEmpty cell then roomId else roomId + 1
  in
    (nextId, cell : cells)

{- impls/gen/step -}
stepN :: Int -> Grid -> Rand Grid
stepN n =
  step
    >=> stepN (n - 1)
    ||> whenM (n /= 0)

step :: Grid -> Rand Grid
step grid =
  grid
    ||> G.imap (stepCell grid)
    ||> sequenceA
    <&> Vector.fromList
    <&> G.setCells grid

stepCell :: Grid -> Int -> Cell -> Rand Cell
stepCell grid i cell =
  cell
    |> C.thenA (R.sample options)
  where
    neighbors =
      G.pos grid i
        |> N.findAll grid
    floorRoom =
      neighbors
        |> N.mostCommonRoom
    floorChance =
      neighbors
        |> N.count
        |> (*15)
    options =
      [ (floorChance,       Floor (Maybe.fromJust floorRoom))
      , (100 - floorChance, Empty)
      ]
