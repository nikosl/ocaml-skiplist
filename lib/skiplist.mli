(*
 * Copyright (c) 2021 Nikos Leivadaris
 * 
 * This software is released under the MIT License.
 * https://opensource.org/licenses/MIT
 *)

(** A SkipList behaves as a sorted list with, typically, O(log(n)) cost for insertion, look-up and removal *)

type ('k, 'v) t
(** The type of the [skiplist]*)

type ('k, 'v) pair = 'k * 'v option
(** [skiplist] data.*)

(** Input signature of the functor {!Make}. *)
module type OrderedType = sig
  type t
  (** The type of the skiplist elements. *)

  val compare : t -> t -> int
  (** A total ordering function over the skiplist elements.
          This is a two-argument function [f] such that
          [f e1 e2] is zero if the elements [e1] and [e2] are equal,
          [f e1 e2] is strictly negative if [e1] is smaller than [e2],
          and [f e1 e2] is strictly positive if [e1] is greater than [e2].
          Example: a suitable ordering function is the generic structural
          comparison function {!Stdlib.compare}. *)
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

  val copy : 'a t -> 'a t
  (** Return a copy of the skiplist. *)

  val length : 'a t -> int
  (** Return the length of the skiplist. *)

  val first : 'a t -> (key, 'a) pair option
  (** Return the first element of the skiplist. *)

  val last : 'a t -> (key, 'a) pair option
  (** Return the last element of the skiplist. *)

  val find : 'a t -> key -> (key, 'a) pair option

  val insert : 'a t -> ?value:'a -> key -> unit

  val remove : 'a t -> key -> unit

  val mem : 'a t -> key -> bool

  val flip : unit -> [ `Head | `Tail ]

  val of_list : (key, 'a) pair list -> 'a t
  (** Create a skiplist from a list of elements. *)

  val to_list : 'a t -> (key, 'a) pair list
  (** Return a list of elements. *)
end

(** Functor building an implementation of the skiplist structure
   given a totally ordered type. *)
module Make (Ord : OrderedType) : S with type key = Ord.t
