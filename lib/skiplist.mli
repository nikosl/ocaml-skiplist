(*
 * Copyright (c) 2021 Nikos Leivadaris
 * 
 * This software is released under the MIT License.
 * https://opensource.org/licenses/MIT
 *)

(** A SkipList behaves as a sorted list with, typically, O(log(n)) cost for
    insertion, look-up and removal *)

type ('k, 'v) t
(** The type of the [skiplist]*)

type ('k, 'v) pair = 'k * 'v
(** [skiplist] data.*)

(** Input signature of the functor {!Make}. *)
module type OrderedType = sig
  type t
  (** The type of the skiplist elements. *)

  val compare : t -> t -> int
  (** A total ordering function over the skiplist elements. This is a
      two-argument function [f] such that [f e1 e2] is zero if the elements [e1]
      and [e2] are equal, [f e1 e2] is strictly negative if [e1] is smaller than
      [e2], and [f e1 e2] is strictly positive if [e1] is greater than [e2].
      Example: a suitable ordering function is the generic structural comparison
      function {!Stdlib.compare}. *)

  val to_string : t -> string
end

module type S = sig
  type key
  (** The type of the skiplist elements. *)

  type 'a t
  (** The type of skiplists. *)

  val create : ?max_level:int -> unit -> 'a t
  (** [create] returns an empty skiplist. *)

  val is_empty : 'a t -> bool
  (** Test whether a skiplist is empty or not. *)

  val length : 'a t -> int
  (** Return the length of the skiplist. *)

  val min : 'a t -> (key, 'a) pair option
  (** Return the min element of the skiplist. *)

  val max : 'a t -> (key, 'a) pair option
  (** Return the max element of the skiplist. *)

  val find : key -> 'a t -> (key, 'a) pair option

  val find_finger : key -> 'a t -> (key * 'a) option

  val find_nearest :
    key ->
    'a t ->
    [ `Gt of (key, 'a) pair
    | `Lt of (key, 'a) pair
    | `Eq of (key, 'a) pair
    | `Empty ]

  val find_range : start:key -> stop:key -> 'a t -> (key, 'a) pair list

  val add : key:key -> value:'a -> 'a t -> unit

  val remove : key -> 'a t -> unit

  val mem : key -> 'a t -> bool

  val iter : (key -> 'a -> unit) -> 'a t -> unit

  val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit

  val fold : 'a t -> init:'b -> f:(key:key -> value:'a -> 'b -> 'b) -> 'b

  val flip : int -> int

  val clear : 'a t -> unit

  val copy : ?max_level:int -> 'a t -> 'a t
  (** Return a copy of the skiplist. *)

  val of_alist : (key, 'a) pair list -> 'a t
  (** Create a skiplist from a list of elements. *)

  val to_alist : 'a t -> (key, 'a) pair list
  (** Return a list of elements. *)

  val to_string : 'a t -> string

  val pp : Format.formatter -> 'a t -> unit [@@ocaml.toplevel_printer]

  val to_seq : 'a t -> (key * 'a) Seq.t

  val to_seq_keys : _ t -> key Seq.t

  val to_seq_values : 'a t -> 'a Seq.t

  val add_seq : 'a t -> (key * 'a) Seq.t -> unit

  val of_seq : (key * 'a) Seq.t -> 'a t
end

(** Functor building an implementation of the skiplist structure given a totally
    ordered type. *)
module Make (Ord : OrderedType) : S with type key = Ord.t
