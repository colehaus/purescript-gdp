module Test.Main where

import GDP.Named
import Prelude

import Data.List (List(..))
import Data.List as List
import Data.Newtype (class Newtype, un)
import Data.Ord.Down (Down(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Test.Merge (mergeBy, sortBy, unSortedBy)
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

mergedGood :: forall a. Ord a => List a -> List a -> List a
mergedGood xs ys =
  name (comparing identity) (\comp ->
    unSortedBy (mergeBy comp (sortBy comp xs) (sortBy comp ys)))

-- Doesn't type check
-- mergedBad :: forall a. Ord a => List a -> List a -> List a
-- mergedBad xs ys =
--   name (comparing identity) (\comp1 ->
--     name (comparing Down) (\comp2 ->
--       un MkSortedBy (mergeBy comp1 (sortBy comp1 xs) (sortBy comp2 ys))))

main :: Effect Unit
main = run [consoleReporter] do
  describe "Basic naming" do
    it "works" do
      let xs = List.fromFoldable [ 1, 4, 3 ]
          ys = List.fromFoldable [ 8, 3, 2 ]
      liftEffect <<< log <<< show $ mergedGood xs ys
