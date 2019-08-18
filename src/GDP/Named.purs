module GDP.Named (Defn, Named, defn, name, name2, name3, unName) where

import Prelude

newtype Named name a = MkNamed a
instance showNamed :: Show a => Show (Named name a) where
  show (MkNamed a) = "(MkNamed " <> show a <> ")"
derive newtype instance eqNamed :: Eq a => Eq (Named name a)
derive newtype instance ordNamed :: Ord a => Ord (Named name a)

name :: forall a t. a -> (forall name. Named name a -> t) -> t
name a k = k (MkNamed a)

name2 :: forall a b t. a -> b -> (forall name1 name2. Named name1 a -> Named name2 b -> t) -> t
name2 a b k = k (MkNamed a) (MkNamed b)

name3 ::
  forall a b c t.
  a -> b -> c ->
  (forall name1 name2 name3. Named name1 a -> Named name2 b -> Named name3 c -> t) -> t
name3 a b c k = k (MkNamed a) (MkNamed b) (MkNamed c)

unName :: forall name a. Named name a -> a
unName (MkNamed a) = a

data Defn = MkDefn

defn :: forall f a. (Defn -> f) -> a -> Named f a
defn mk a = MkNamed a
