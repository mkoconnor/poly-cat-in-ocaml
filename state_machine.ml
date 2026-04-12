module type Polynomial = sig
  type 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t
end

(* p × q *)
module Cartesian_product (P : Polynomial) (Q : Polynomial) :
  Polynomial with type 'a t = 'a P.t * 'a Q.t = struct
  type 'a t = 'a P.t * 'a Q.t

  let map (p, q) ~f = (P.map p ~f, Q.map q ~f)
end

(* p ⊗ q *)
module Dirichlet_product (P : Polynomial) (Q : Polynomial) : sig
  type _ t = T : 'a P.t * 'b Q.t * ('a -> 'b -> 'c) -> 'c t

  include Polynomial with type 'a t := 'a t
end = struct
  type _ t = T : 'a P.t * 'b Q.t * ('a -> 'b -> 'c) -> 'c t

  let map (T (p, q, f')) ~f = T (p, q, fun a b -> f (f' a b))
end

(* p ⊳ q *)
module Substitution_product (P : Polynomial) (Q : Polynomial) :
  Polynomial with type 'a t = 'a Q.t P.t = struct
  type 'a t = 'a Q.t P.t

  let map p ~f = P.map p ~f:(fun q -> Q.map q ~f)
end

type ('a, 'b) either = Left of 'a | Right of 'b

(* p^q *)
module Cartesian_closure (P : Polynomial) (Q : Polynomial) : sig
  type 'a t = { f : 'b. 'b Q.t -> ('a, 'b) either P.t }

  include Polynomial with type 'a t := 'a t
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
module Cartesian_eval (P : Polynomial) (Q : Polynomial) = struct
  module PQ = Cartesian_closure (Q) (P)
  module Prod = Cartesian_product (P) (PQ)

  let eval ((p, { PQ.f }) : 'a Prod.t) : 'a Q.t =
    Q.map (f p) ~f:(function Left a -> a | Right a -> a)
end

(* [p,q] *)
module Dirichlet_closure (P : Polynomial) (Q : Polynomial) : sig
  type 'a t = { f : 'b. 'b Q.t -> ('a * 'b) P.t }

  include Polynomial with type 'a t := 'a t
end = struct
  type 'a t = { f : 'b. 'b Q.t -> ('a * 'b) P.t }

  let map { f } ~f:g =
    { f = (fun q -> P.map (f q) ~f:(fun (a, b) -> (g a, b))) }
end

(* P ⊗ [P,Q] → Q *)
module Dirichlet_eval (P : Polynomial) (Q : Polynomial) = struct
  module PQ = Dirichlet_closure (Q) (P)
  module Prod = Dirichlet_product (P) (PQ)

  let eval : type a. a Prod.t -> a Q.t = fun (Prod.T (p, { PQ.f }, combine)) ->
    Q.map (f p) ~f:(fun (y, x) -> combine x y)
end
