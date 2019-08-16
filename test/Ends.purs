module Test.Ends (IsCons, IsNil, Length, ListCase(..), Rev, classify, head, length, reverse, rev_cons) where

import Prelude

import Data.List (List(..))
import Data.List as List
import Data.Maybe (fromJust)
import GDP.Named (Defn, Named, defn, unName)
import GDP.Proof (type (:::), Proof, axiom, unSuchThat)
import Partial.Unsafe (unsafePartial)

newtype Rev x = MkRev Defn
newtype Length x = MkLength Defn

reverse :: forall xs a. (Named xs (List a)) -> (Named (Rev xs) (List a))
reverse xs = defn MkRev (List.reverse <<< unName $ xs)

length :: forall xs a. (Named xs (List a) -> Named (Length xs) Int)
length xs = defn MkLength (List.length <<< unName $ xs)

head :: forall xs a. Named xs (List a) ::: IsCons xs -> a
head xs = unsafePartial $ fromJust <<< List.head <<< unName <<< unSuchThat $ xs

rev_cons :: forall xs. Proof (IsCons xs) -> Proof (IsCons (Rev xs))
rev_cons _ = axiom

data IsCons x
data IsNil x
data ListCase a xs
  = MkIsCons (Proof (IsCons xs))
  | MkIsNil (Proof (IsNil xs))

classify :: forall xs a. Named xs (List a) -> ListCase a xs
classify xs =
  case unName xs of
    Cons _ _ -> MkIsCons axiom
    Nil -> MkIsNil axiom
