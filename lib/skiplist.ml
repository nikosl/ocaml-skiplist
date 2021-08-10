(*
 * Copyright (c) 2021 Nikos Leivadaris
 * 
 * This software is released under the MIT License.
 * https://opensource.org/licenses/MIT
 *)

type ('k, 'v) pair = 'k * 'v option

type ('k, 'v) node =
  | Nil
  | NInf of {
      level_id : int;
      mutable above : ('k, 'v) node option;
      mutable below : ('k, 'v) node option;
      mutable next : ('k, 'v) node;
    }
  | PInf of {
      level_id : int;
      mutable above : ('k, 'v) node option;
      mutable below : ('k, 'v) node option;
      mutable prev : ('k, 'v) node;
    }
  | Node of {
      key : 'k;
      mutable value : 'v option;
      level_id : int;
      mutable above : ('k, 'v) node option;
      mutable below : ('k, 'v) node option;
      mutable next : ('k, 'v) node;
      mutable prev : ('k, 'v) node;
    }

type ('k, 'v) t = {
  mutable top : ('k, 'v) level;
  mutable bottom : ('k, 'v) level;
  mutable levels_num : int;
  levels : ('k, 'v) level Array.t;
  max_level : int;
}

and ('k, 'v) level =
  | Empty
  | Cons of {
      id : int;
      mutable head : ('k, 'v) node;
      mutable tail : ('k, 'v) node;
      mutable above : ('k, 'v) level;
      mutable below : ('k, 'v) level;
      mutable length : int;
    }

module SNode : sig
   val create : 'k -> 'v option -> int ->('k, 'v) node
  val prev: ('k, 'v) node -> ('k, 'v) node
  val get: ('k, 'v) node -> ('k, 'v) pair option
val next: ('k, 'v) node -> ('k, 'v) node
val link_nodes_hor: ('k, 'v) node -> ('k, 'v) node -> ('k, 'v) node -> unit


val 
  end = struct

    let create k v i =
      Node
        {
          level_id = i;
          key = k;
          value = v;
          next = Nil;
          prev = Nil;
          below = None;
          above = None;
        }

            let prev = function
      | NInf _ | Nil -> Nil
      | PInf n -> n.prev
      | Node n -> n.prev

    let next = function
      | PInf _ | Nil -> Nil
      | NInf n -> n.next
      | Node n -> n.next

      let get (nn : ('k, 'v) node) : ('k, 'v) pair option =
    match nn with
    | Node n -> Some (n.key, n.value)
    | Nil | NInf _ | PInf _ -> None


  let link_nodes_hor a b c : unit =
    match (a, b, c) with
    | NInf n1, Node n2, PInf n3 ->
        n1.next <- b;
        n2.prev <- a;
        n2.next <- c;
        n3.prev <- b
    | NInf n1, Node n2, Node n3 ->
        n1.next <- b;
        n2.prev <- a;
        n2.next <- c;
        n3.prev <- b
    | Node n1, Node n2, PInf n3 ->
        n1.next <- b;
        n2.prev <- a;
        n2.next <- c;
        n3.prev <- b
    | Node n1, Node n2, Node n3 ->
        n1.next <- b;
        n2.prev <- a;
        n2.next <- c;
        n3.prev <- b
    | Nil, _, _ -> assert false
    | PInf _, _, _ -> assert false
    | NInf _, _, _ -> assert false
    | Node _, _, _ -> assert false

    let link_nodes_vert bn an =
      match (bn, an) with
      | PInf bn', PInf an' ->
          bn'.above <- Some an;
          an'.below <- Some bn
      | NInf bn', NInf an' ->
          bn'.above <- Some an;
          an'.below <- Some bn
      | Node bn', Node an' ->
          bn'.above <- Some an;
          an'.below <- Some bn
      | NInf _, _ -> assert false
      | Node _, _ -> assert false
      | PInf _, _ -> assert false
      | Nil, _ -> assert false

    end

let prng = lazy (Random.State.make_self_init ())

let flip () : [ `Head | `Tail ] =
  match Random.State.int (Lazy.force prng) 2 with
  | 0 -> `Head
  | 1 -> `Tail
  | _ -> `Tail

let create_level sl i =
      (*
         in *)
      let ni = NInf { level_id = i; next = Nil; below = None; above = None } in
      let pi = PInf { level_id = i; prev = Nil; below = None; above = None } in
      let lvl =
        Cons
          {
            id = i;
            below = Empty;
            above = Empty;
            head = ni;
            tail = pi;
            length = 1;
          }
      in
      sl.levels_num <- sl.levels_num + 1;
      sl.levels.(i) <- lvl;
      (ni, pi)

module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module type S = sig
  type key

  type 'a t

  val create : ?max_level:int -> unit -> 'a t

  val is_empty : 'a t -> bool

  val copy : 'a t -> 'a t

  val length : 'a t -> int

  val first : 'a t -> (key, 'a) pair option

  val last : 'a t -> (key, 'a) pair option

  val find : 'a t -> key -> (key, 'a) pair option

  val insert : 'a t -> ?value:'a -> key -> unit

  val remove : 'a t -> key -> unit

  val mem : 'a t -> key -> bool

  val flip : unit -> [ `Head | `Tail ]

  val of_list : (key, 'a) pair list -> 'a t

  val to_list : 'a t -> (key, 'a) pair list
end

module Make (Ord : OrderedType) : S with type key = Ord.t = struct
  type key = Ord.t

  type 'a skiplist = (key, 'a) t

  type 'a t = 'a skiplist

  let default_max_level = 5

  let create ?(max_level = default_max_level) () : 'a t =
    let arr = Array.init max_level (fun _ -> Empty) in
    {
      top = arr.(max_level - 1);
      bottom = arr.(0);
      levels_num = 0;
      levels = arr;
      max_level;
    }

  let is_empty (sl : 'a t) =
    match sl.bottom with Empty -> true | Cons l -> l.length = 0

  let length (sl : 'a t) : int =
    match sl.bottom with Empty -> 0 | Cons l -> l.length

  let first (sl : 'a t) : (key, 'a) pair option =
    match sl.bottom with
    | Empty -> None
    | Cons l -> (
        match l.head with
        | Nil -> None
        | NInf i -> SNode.get i.next
        | Node _ | PInf _ -> assert false)

  let last (sl : 'a t) : (key, 'a) pair option =
    match sl.bottom with
    | Empty -> None
    | Cons l -> (
        match l.tail with
        | Nil -> None
        | PInf i -> SNode.get i.prev
        | Node _ | NInf _ -> assert false)

  let find_nearest_node (sl : 'a t) (key' : key) :
      [ `Gt of (key, 'a) node
      | `Lt of (key, 'a) node
      | `Eq of (key, 'a) node
      | `Navail ] =
    let rec below pn =
      match pn with
      | Nil -> Nil
      | PInf n -> below n.prev
      | NInf n -> ( match n.below with None -> Nil | Some n -> n)
      | Node n -> ( match n.below with None -> below n.prev | Some n -> n)
    in
    let rec aux_fnd key' nn ln =
      match nn with
      | Nil -> ln
      | NInf n -> aux_fnd key' n.next ln
      | PInf n -> aux_fnd key' (below n.prev) ln
      | Node n -> (
          match Ord.compare key' n.key with
          | -1 -> aux_fnd key' n.next (`Lt nn)
          | 0 -> `Eq nn
          | 1 -> aux_fnd key' (below n.prev) (`Gt nn)
          | _ -> assert false)
    in
    let rec find_lvl sl key i =
      match sl.levels.(i) with
      | Empty -> if i = 0 then `Navail else find_lvl sl key (i - 1)
      | Cons l -> aux_fnd key' l.head (`Gt l.head)
    in
    find_lvl sl key' (sl.max_level - 1)

  let find (sl : 'a t) (key' : key) : (key, 'a) pair option =
    let f = find_nearest_node sl key' in
    match f with `Eq n -> SNode.get n | `Navail | `Lt _ | `Gt _ -> None

  let copy (sl : 'a t) : 'a t = sl

  let insert (sl : 'a t) ?value (key' : key) : unit =
    let rec above = function
      | Nil -> Nil
      | PInf n -> above n.prev
      | NInf n -> ( match n.above with None -> Nil | Some n -> n)
      | Node n -> ( match n.above with None -> above n.prev | Some n -> n)
    in
    let rec update_values v = function
      | Node n ->
          n.value <- v;
          update_values v (Option.value ~default:Nil n.below)
      | Nil | NInf _ | PInf _ -> ()
    in
    let rec should_add_above i pn nn =
      match flip () with
      | `Head ->
          if i = sl.max_level then ()
          else
            let i = i + 1 in
            let pn, nn = insert_node_above pn nn i in
            should_add_above i pn nn
      | `Tail -> ()
    in
    let add_new_node nn =
      let ni, pi = create_level sl 0 in
      link_nodes_vert ni nn pi;
      should_add_above 0 ni nn
    in
   let rec insert_aux idx pn nn =
      if idx = sl.max_level then () else
      match flip () with
      | `Head ->
    in
    let f = find_nearest_node sl key' in
    match f with
    | `Eq n -> update_values value n
    | `Lt n -> link_nodes_vert (SNode.prev n) (create_node key' value 0) n
    | `Gt n -> link_nodes_vert n (create_node key' value 0) (SNode.next n)
    | `Navail -> add_new_node (create_node key' value 0)

  let remove (sl : 'a t) (key' : key) : unit =
    let decr_lvl lvl =
      match lvl with
      | Empty -> assert false
      | Cons l -> l.length <- l.length - 1
    in
    let below nn = match nn with None -> Nil | Some n -> n in
    let link a b =
      match (a, b) with
      | NInf n1, Node n2 ->
          n1.next <- b;
          n2.prev <- a
      | Node n1, PInf n2 ->
          n1.next <- b;
          n2.prev <- a
      | Node n1, Node n2 ->
          n1.next <- b;
          n2.prev <- a
      | NInf n1, PInf n2 ->
          n1.next <- b;
          n2.prev <- a
      | Nil, _ -> assert false
      | PInf _, _ -> assert false
      | NInf _, _ -> assert false
      | Node _, _ -> assert false
    in
    let rec aux_remove nn =
      match nn with
      | Nil | NInf _ | PInf _ -> ()
      | Node n ->
          let lvl = n.level_id in
          link n.prev n.next;
          decr_lvl sl.levels.(lvl);
          let b = below n.below in
          n.above <- None;
          n.below <- None;
          aux_remove b
    in
    let f = find_nearest_node sl key' in
    match f with `Eq n -> aux_remove n | `Navail | `Lt _ | `Gt _ -> ()

  let mem (sl : 'a t) (key' : key) : bool =
    let f = find sl key' in
    match f with None -> false | Some _ -> true

  let flip = flip

  let of_list (_el : (key, 'a) pair list) : 'a t = failwith "not implemented"

  let to_list (sl : 'a t) : (key, 'a) pair list =
    let rec aux_to_list nn l =
      match nn with
      | Nil -> l
      | NInf _ -> l
      | PInf n -> aux_to_list n.prev l
      | Node n -> aux_to_list n.prev ((n.key, n.value) :: l)
    in
    match sl.bottom with Empty -> [] | Cons l -> aux_to_list l.tail []

  (* let hd (sl : 'a t) : node =
     match sl.bottom with Empty -> Nil | Cons l -> match l.head with *)

  (* let map (sl:'a t ) ~f : (fun elt -> 'b) : 'b list *)
end
