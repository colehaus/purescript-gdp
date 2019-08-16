module GDP.Named (Defn, Named, defn, name, unName) where

newtype Named name a = MkNamed a

name :: forall a t. a -> (forall name. (Named name a) -> t) -> t
name a k = k (MkNamed a)

unName :: forall name a. Named name a -> a
unName (MkNamed a) = a

data Defn = MkDefn

defn :: forall f a. (Defn -> f) -> a -> Named f a
defn mk a = MkNamed a
