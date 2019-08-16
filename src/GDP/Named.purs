module GDP.Named (Named, name, unname) where

newtype Named name a = MkNamed a

name :: forall a t. a -> (forall name. (Named name a) -> t) -> t
name a k = k (MkNamed a)

unname :: forall name a. Named name a -> a
unname (MkNamed a) = a
