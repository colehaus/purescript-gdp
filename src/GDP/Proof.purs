module GDP.Proof (type (:::), Proof, SuchThat, (...), attach, axiom, unSuchThat) where

data Proof p = QED

newtype SuchThat a p = SuchThat a
-- TODO: Figure out fixity
infix 4 type SuchThat as :::

unSuchThat :: forall a p. SuchThat a p -> a
unSuchThat (SuchThat a) = a

attach :: forall a p. a -> Proof p -> SuchThat a p
attach a proof = SuchThat a
infix 4 attach as ...

axiom :: forall p. Proof p
axiom = QED
