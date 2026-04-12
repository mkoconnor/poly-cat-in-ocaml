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
end = struct end
