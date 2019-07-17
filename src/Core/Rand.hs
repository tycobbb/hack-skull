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
-- @param gen  The generator to use
-- @param size The size of the list
--
-- @return A list populated with random values
generate :: (Random a, Bounded a) => RandGen -> Int -> Rand [a]
generate =
  generateR (minBound, maxBound)

-- Creates a new Rand by populating a list with `size` values bounded
-- by the range.
--
-- @param range The range to generate within
-- @param gen   The generator to use
-- @param size  The size of the list
--
-- @return A list populated with random values
generateR :: Random a => (a, a) -> RandGen -> Int -> Rand [a]
generateR range gen size =
  let
    consNext (list, gen) =
      Random.randomR range gen
        |> Core.Rand.map (\next -> next : list)
  in
    [0..size]
      |> foldr (\_ memo -> consNext memo) ([], gen)

-- Transforms the value in the Rand.
--
-- @param fn A function to transform the sequence value
map :: (a -> b) -> Rand a -> Rand b
map fn (value, gen) =
  (fn value, gen)
