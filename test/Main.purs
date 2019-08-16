module Test.Main where

import GDP.Named
import Prelude

import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Ord.Down (Down(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import GDP.Proof ((...))
import Test.Ends (ListCase(..))
import Test.Ends as Ends
import Test.Merge (mergeBy, sortBy, unSortedBy)
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

mergedGood :: forall a. Ord a => List a -> List a -> List a
mergedGood xs ys =
  name (comparing Down) (\comp ->
    unSortedBy (mergeBy comp (sortBy comp xs) (sortBy comp ys)))

-- Doesn't type check
-- mergedBad :: forall a. Ord a => List a -> List a -> List a
-- mergedBad xs ys =
--   name (comparing identity) (\comp1 ->
--     name (comparing Down) (\comp2 ->
--       un MkSortedBy (mergeBy comp1 (sortBy comp1 xs) (sortBy comp2 ys))))

endsGood :: forall a. List a -> Maybe (Tuple a a)
endsGood as =
  name as (\xs ->
    case Ends.classify xs of
      MkIsCons proof ->
        Just (Tuple (Ends.head $ xs ... proof) (Ends.head $ Ends.reverse xs ... Ends.rev_cons proof))
      MkIsNil _ -> Nothing)

main :: Effect Unit
main = run [consoleReporter] do
  describe "Basic naming" do
    it "works" do
      let xs = List.fromFoldable [ 1, 4, 3 ]
          ys = List.fromFoldable [ 8, 3, 2 ]
      liftEffect <<< log <<< show $ mergedGood xs ys
  describe "SuchThat and defn" do
    it "work" do
      let xs = List.fromFoldable  [ 1, 4, 5 ]
      liftEffect <<< log <<< show $ endsGood xs
