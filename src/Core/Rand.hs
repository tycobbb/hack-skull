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
-- Creates a new Rand by populating a list with `size` values bounded
-- by the range.
--
-- @param range The range to generate within
-- @param gen   The generator to use
-- @param size  The size of the list
--
-- @return A list populated with random values
generate :: Random a => (a, a) -> RandGen -> Int -> Rand [a]
generate range gen size =
  let
    consNext (list, gen) =
      Random.randomR range gen
        |> Core.Rand.map (\next -> next : list)
  in
    [0..size]
      |> foldr (\_ memo -> consNext memo) ([], gen)

{- impls/commands -}
-- Transforms the value in the Rand.
--
-- @param transform A function to transform the sequence value
map :: (a -> b) -> Rand a -> Rand b
map transform (value, gen) =
  (transform value, gen)

-- Transforms a property of the Rand using the generator.
--
-- @param get        The getter for the property
-- @param set        The setter for the property
-- @param transform  The updater for the property
update :: (a -> b) -> (b -> a -> a) -> (Rand b -> Rand b) -> Rand a -> Rand a
update get set transform initial =
  initial
    |> Core.Rand.map get
    |> transform
    |> Core.Rand.map ((flip set) (fst initial))

join :: b -> (b -> a -> a) -> (Rand b -> Rand b) -> Rand a -> Rand a
join next =
  update (const next)

-- Alias for System.Random.randomR
random :: Random.Random a => (a, a) -> RandGen -> Rand a
random =
  Random.randomR
