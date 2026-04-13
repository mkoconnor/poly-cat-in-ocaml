module type Poly = sig
  type 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t
end

(* p × q *)
module Cartesian_product (P : Poly) (Q : Poly) :
  Poly with type 'a t = 'a P.t * 'a Q.t = struct
  type 'a t = 'a P.t * 'a Q.t

  let map (p, q) ~f = (P.map p ~f, Q.map q ~f)
end

(* p ⊗ q *)
module Dirichlet_product (P : Poly) (Q : Poly) : sig
  type _ t = T : 'a P.t * 'b Q.t * ('a -> 'b -> 'c) -> 'c t

  include Poly with type 'a t := 'a t
end = struct
  type _ t = T : 'a P.t * 'b Q.t * ('a -> 'b -> 'c) -> 'c t

  let map (T (p, q, f')) ~f = T (p, q, fun a b -> f (f' a b))
end

(* p ⊳ q *)
module Substitution_product (P : Poly) (Q : Poly) :
  Poly with type 'a t = 'a Q.t P.t = struct
  type 'a t = 'a Q.t P.t

  let map p ~f = P.map p ~f:(fun q -> Q.map q ~f)
end

type ('a, 'b) either = Left of 'a | Right of 'b

(* p^q *)
module Cartesian_closure (P : Poly) (Q : Poly) : sig
  type 'a t = { f : 'b. 'b Q.t -> ('a, 'b) either P.t }

  include Poly with type 'a t := 'a t
end = struct
  type 'a t = { f : 'b. 'b Q.t -> ('a, 'b) either P.t }

  let map { f } ~f:g =
    {
      f =
        (fun q ->
          P.map (f q) ~f:(function Left a -> Left (g a) | Right b -> Right b));
    }
end

(* P × Q^P → Q *)
module Cartesian_eval (P : Poly) (Q : Poly) = struct
  module PQ = Cartesian_closure (Q) (P)
  module Prod = Cartesian_product (P) (PQ)

  let eval : type a. a Prod.t -> a Q.t =
   fun (p, { f }) -> Q.map (f p) ~f:(function Left a | Right a -> a)
end

(* [p,q] *)
module Dirichlet_closure (P : Poly) (Q : Poly) : sig
  type 'a t = { f : 'b. 'b Q.t -> ('a * 'b) P.t }

  include Poly with type 'a t := 'a t
end = struct
  type 'a t = { f : 'b. 'b Q.t -> ('a * 'b) P.t }

  let map { f } ~f:g =
    { f = (fun q -> P.map (f q) ~f:(fun (a, b) -> (g a, b))) }
end

(* P ⊗ [P,Q] → Q *)
module Dirichlet_eval (P : Poly) (Q : Poly) = struct
  module PQ = Dirichlet_closure (Q) (P)
  module Prod = Dirichlet_product (P) (PQ)

  let eval : type a. a Prod.t -> a Q.t =
   fun (T (p, { f }, combine)) -> Q.map (f p) ~f:(fun (y, x) -> combine x y)
end
