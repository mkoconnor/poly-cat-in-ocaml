{-# LANGUAGE GADTs #-}

module StateMachine where

class Polynomial p where
  pmap :: (a -> b) -> p a -> p b

newtype CartesianProduct p q a = CartesianProduct (p a, q a)

instance (Polynomial p, Polynomial q) => Polynomial (CartesianProduct p q) where
  pmap f (CartesianProduct (p, q)) = CartesianProduct (pmap f p, pmap f q)

data DirichletProduct p q c where
  T :: p a -> q b -> (a -> b -> c) -> DirichletProduct p q c

instance (Polynomial p, Polynomial q) => Polynomial (DirichletProduct p q) where
  pmap f (T p q f') = T p q (\a b -> f (f' a b))
