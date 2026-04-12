{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module StateMachine where

class Polynomial p where
  pmap :: (a -> b) -> p a -> p b

-- p × q
newtype CartesianProduct p q a = CartesianProduct (p a, q a)

instance (Polynomial p, Polynomial q) => Polynomial (CartesianProduct p q) where
  pmap f (CartesianProduct (p, q)) = CartesianProduct (pmap f p, pmap f q)

-- p ⊗ q
data DirichletProduct p q c where
  T :: p a -> q b -> (a -> b -> c) -> DirichletProduct p q c

instance (Polynomial p, Polynomial q) => Polynomial (DirichletProduct p q) where
  pmap f (T p q f') = T p q (\a b -> f (f' a b))

-- p ⊳ q
newtype SubstitutionProduct p q a = SubstitutionProduct (p (q a))

instance (Polynomial p, Polynomial q) => Polynomial (SubstitutionProduct p q) where
  pmap f (SubstitutionProduct p) = SubstitutionProduct (pmap (pmap f) p)

-- p^q
newtype CartesianClosure p q a = CartesianClosure (forall b. q b -> p (Either a b))

instance (Polynomial p) => Polynomial (CartesianClosure p q) where
  pmap f (CartesianClosure g) = CartesianClosure (\q -> pmap (either (Left . f) Right) (g q))

-- [p,q]
newtype DirichletClosure p q a = DirichletClosure (forall b. q b -> p (a, b))

instance (Polynomial p) => Polynomial (DirichletClosure p q) where
  pmap f (DirichletClosure g) = DirichletClosure (\q -> pmap (\(a, b) -> (f a, b)) (g q))
