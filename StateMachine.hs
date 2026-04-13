{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module StateMachine where

-- The same as Functor, but renamed to make it clear that we're
-- assuming it's polynomial
class Poly p where
  pmap :: (a -> b) -> p a -> p b

-- p × q
newtype CartesianProduct p q a = CartesianProduct (p a, q a)

instance (Poly p, Poly q) => Poly (CartesianProduct p q) where
  pmap f (CartesianProduct (p, q)) = CartesianProduct (pmap f p, pmap f q)

-- p ⊗ q
data DirichletProduct p q c where
  T :: p a -> q b -> (a -> b -> c) -> DirichletProduct p q c

instance (Poly p, Poly q) => Poly (DirichletProduct p q) where
  pmap f (T p q f') = T p q (\a b -> f (f' a b))

-- p ⊳ q
newtype SubstitutionProduct p q a = SubstitutionProduct (p (q a))

instance (Poly p, Poly q) => Poly (SubstitutionProduct p q) where
  pmap f (SubstitutionProduct p) = SubstitutionProduct (pmap (pmap f) p)

-- p^q
newtype CartesianClosure p q a = CartesianClosure (forall b. q b -> p (Either a b))

instance (Poly p) => Poly (CartesianClosure p q) where
  pmap f (CartesianClosure g) = CartesianClosure (\q -> pmap (either (Left . f) Right) (g q))

-- [p,q]
newtype DirichletClosure p q a = DirichletClosure (forall b. q b -> p (a, b))

instance (Poly p) => Poly (DirichletClosure p q) where
  pmap f (DirichletClosure g) = DirichletClosure (\q -> pmap (\(a, b) -> (f a, b)) (g q))

-- p × q^p → q
cartesianEval :: (Poly q) => CartesianProduct p (CartesianClosure q p) a -> q a
cartesianEval (CartesianProduct (p, CartesianClosure f)) = pmap (either id id) (f p)

-- p ⊗ [p,q] → q
dirichletEval :: (Poly q) => DirichletProduct p (DirichletClosure q p) a -> q a
dirichletEval (T p (DirichletClosure f) combine) = pmap (\(y, x) -> combine x y) (f p)
