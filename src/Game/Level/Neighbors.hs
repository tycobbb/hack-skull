module Game.Level.Neighbors where

-- external
import Prelude hiding (Left, Right)
import Data.Functor
import qualified Data.Maybe as Maybe
import qualified Data.Tuple as Tuple
import qualified Data.List as List
import qualified Data.Vector as Vector
import Data.Vector (Vector)

-- internal
import Core.Extra
import qualified Core.Rand as R
import Core.Rand (Rand)
import qualified Game.Vec as V
import Game.Vec (Vec2)
import qualified Game.Level.Grid as G
import Game.Level.Grid (Grid)
import qualified Game.Level.Cell as C
import Game.Level.Cell (Cell(..), Room(..))

{- types -}
data Neighbor = Neighbor
  { cell :: Cell
  , dir  :: Direction
  }
  deriving (Show)

data Direction
  = Up
  | Down
  | Left
  | Right
  deriving (Show)

{- impls -}
{- impls/factories -}
findAll :: Grid -> Vec2 -> [Neighbor]
findAll grid pos =
  let
    findOne' =
      findOne grid pos
  in
    Maybe.catMaybes
      [ findOne' Up
      , findOne' Down
      , findOne' Left
      , findOne' Right
      ]

findOne :: Grid -> Vec2 -> Direction -> Maybe Neighbor
findOne grid origin dir =
  let
    pos =
      calcPos origin dir
  in
    if not (G.contains grid pos) then
      Nothing
    else
      Just Neighbor
        { cell = (G.cellv grid pos)
        , dir  = dir
        }

calcPos :: Vec2 -> Direction -> Vec2
calcPos origin dir =
  case dir of
    Up    -> origin + V.uy
    Down  -> origin - V.uy
    Left  -> origin - V.ux
    Right -> origin + V.ux

{- impls/queries -}
count :: [Neighbor] -> Int
count neighbors =
  neighbors
    |> filter (not . C.isEmpty . cell)
    |> List.length

mostCommonRoom :: [Neighbor] -> Maybe Room
mostCommonRoom neighbors =
  neighbors
    ||> fmap (C.room . cell)
    ||> Maybe.catMaybes
    ||> List.sort
    ||> foldr mostCommonRoom' (Nothing, Nothing)
    ||> Tuple.snd
    <&> Tuple.fst

mostCommonRoom' :: Room -> (Maybe (Room, Int), Maybe (Room, Int)) -> (Maybe (Room, Int), Maybe (Room, Int))
mostCommonRoom' room (Nothing, _) =
  ( Just (room, 1)
  , Just (room, 1)
  )
mostCommonRoom' room (Just (curr, currF), Just (max, maxF))
  | room /= curr, currF > maxF = (Just (room, 1), Just (curr, currF))
  | room /= curr               = (Just (room, 1), Just (max, maxF))
  | otherwise                  = (Just (curr, currF + 1), Just (max, maxF))
