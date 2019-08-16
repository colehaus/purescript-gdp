module Test.Merge (SortedBy, unSortedBy, sortBy, mergeBy) where

import Prelude

import Data.List (List(..))
import Data.List as List

import GDP.Named (Named, unName)

newtype SortedBy comp a = MkSortedBy a

unSortedBy :: forall comp a. SortedBy comp a -> a
unSortedBy (MkSortedBy a) = a

sortBy :: forall comp a. (Named comp (a -> a -> Ordering)) -> List a -> SortedBy comp (List a)
sortBy comp xs = MkSortedBy (List.sortBy (unName comp) xs)

mergeBy ::
  forall comp a.
  (Named comp (a -> a -> Ordering)) ->
  SortedBy comp (List a) ->
  SortedBy comp (List a) ->
  SortedBy comp (List a)
mergeBy comp xs ys = MkSortedBy (mergeBy' (unName comp) (unSortedBy xs) (unSortedBy ys))
  where
    mergeBy' comp' xs'' ys'' = go xs'' ys''
      where
        go Nil ys' = ys'
        go xs' Nil = xs'
        go (Cons x xs') (Cons y ys') =
          case comp' x y of
            GT -> Cons y (go (Cons x xs') ys')
            _ -> Cons x (go xs' (Cons y ys'))
