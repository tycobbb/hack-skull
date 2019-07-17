module Core.Rand where

-- external
import qualified System.Random as Random
import System.Random (Random)

-- internal
import Core.Utils

{- types -}
-- A random generator
type RandGen = Random.StdGen
-- A value and random generator pair
type Rand a = (a, RandGen)

{- impls -}
-- Creates a new Rand by populating a list `size` values.
--
-- @param size The size of the list
-- @param gen  The generator to use to populate the list
--
-- @return A list populated with random values
generate :: Random a => Int -> RandGen -> Rand [a]
generate size gen =
  let
    consNext =
      next (\list next -> next : list)
  in
    [0..size]
      |> foldr (\_ list -> consNext list) ([], gen)

-- Transforms the value in the Rand.
--
-- @param fn A function to transform the sequence value
map :: (a -> b) -> Rand a -> Rand b
map fn (value, gen) =
  (fn value, gen)

-- Generates the next value to add to the sequence
--
-- @param fn A function to update the sequence with the next value
next :: Random b => (a -> b -> a) -> Rand a -> Rand a
next fn (value, gen) =
  Random.random gen
    |> Core.Rand.map (\next -> fn value next)
