(*
 * Copyright (c) 2021 Nikos Leivadaris
 * 
 * This software is released under the MIT License.
 * https://opensource.org/licenses/MIT
 *)

type ('k, 'v) pair = 'k * 'v

type ('k, 'v) node =
  | Nil
  | NInf of ('k, 'v) node Array.t
  | Node of {
      key : 'k;
      mutable value : 'v;
      mutable next : ('k, 'v) node Array.t;
    }

type ('k, 'v) t = {
  mutable head : ('k, 'v) node;
  mutable tail : ('k, 'v) node;
  mutable length : int;
  mutable cur_level : int;
  max_level : int;
}

module SNode : sig
  val create : 'k -> 'v -> int -> ('k, 'v) node

  val value : ('k, 'v) node -> ('k, 'v) pair option

  val update : ('k, 'v) node -> 'v -> unit

  val has_next : int -> ('k, 'v) node -> bool

  val link : int -> ('k, 'v) node -> ('k, 'v) node -> unit

  val unlink : int -> ('k, 'v) node -> ('k, 'v) node -> unit
end = struct
  let create k v l =
    Node { key = k; value = v; next = Array.init l (fun _ -> Nil) }

  let value = function Nil | NInf _ -> None | Node n -> Some (n.key, n.value)

  let update c v =
    match c with NInf _ | Nil -> assert false | Node n -> n.value <- v

  let next n i =
    match n with Nil -> Nil | NInf l -> l.(i) | Node l -> l.next.(i)

  let has_next i n =
    match next n i with Nil -> false | Node _ -> true | NInf _ -> assert false

  let link i n1 n2 =
    let n3 = next n1 i in
    match (n1, n2, n3) with
    | NInf l1, Node l2, Nil ->
        l1.(i) <- n2;
        l2.next.(i) <- n3
    | NInf l1, Node l2, Node _ ->
        l1.(i) <- n2;
        l2.next.(i) <- n3
    | Node l1, Node l2, Nil ->
        l1.next.(i) <- n2;
        l2.next.(i) <- n3
    | Node l1, Node l2, Node _ ->
        l1.next.(i) <- n2;
        l2.next.(i) <- n3
    | Nil, _, _ -> assert false
    | NInf _, _, _ -> assert false
    | Node _, _, _ -> assert false

  let unlink i n1 n2 =
    let n3 = next n2 i in
    match (n1, n3) with
    | NInf l1, Nil -> l1.(i) <- n3
    | NInf l1, Node _ -> l1.(i) <- n3
    | Node l1, Nil -> l1.next.(i) <- n3
    | Node l1, Node _ -> l1.next.(i) <- n3
    | Nil, _ -> assert false
    | NInf _, _ -> assert false
    | Node _, _ -> assert false
end

let prng = lazy (Random.State.make_self_init ())

let flip (p : int) : int = Random.State.int (Lazy.force prng) p

module type OrderedType = sig
  type t

  val compare : t -> t -> int

  val to_string : t -> string
end

module type S = sig
  type key

  type !'a t

  val create : ?max_level:int -> unit -> 'a t

  val is_empty : 'a t -> bool

  val length : 'a t -> int

  val min : 'a t -> (key, 'a) pair option

  val max : 'a t -> (key, 'a) pair option

  val find : key -> 'a t -> (key, 'a) pair option

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

  val of_alist : (key, 'a) pair list -> 'a t

  val to_alist : 'a t -> (key, 'a) pair list

  val to_string : 'a t -> string

  val pp : Format.formatter -> 'a t -> unit [@@ocaml.toplevel_printer]

  val to_seq : 'a t -> (key * 'a) Seq.t

  val to_seq_keys : _ t -> key Seq.t

  val to_seq_values : 'a t -> 'a Seq.t

  val add_seq : 'a t -> (key * 'a) Seq.t -> unit

  val of_seq : (key * 'a) Seq.t -> 'a t
end

module Make (Ord : OrderedType) : S with type key = Ord.t = struct
  type key = Ord.t

  type 'a skiplist = (key, 'a) t

  type 'a t = 'a skiplist

  type 'a ranged = [ `Gt of 'a | `Lt of 'a | `Eq of 'a | `Empty ]

  let default_max_level = 15

  let flip = flip

  let create ?(max_level = default_max_level) () : 'a t =
    let head = NInf (Array.init max_level (fun _ -> Nil)) in
    { length = 0; tail = Nil; cur_level = 0; head; max_level }

  let is_empty (sl : 'a t) = sl.length = 0

  let length (sl : 'a t) : int = sl.length

  (* let level (sl : 'a t) : int = sl.max_level *)

  let min (sl : 'a t) : (key, 'a) pair option =
    match sl.head with
    | NInf l -> SNode.value l.(0)
    | Nil -> assert false
    | Node _ -> assert false

  let max (sl : 'a t) : (key, 'a) pair option =
    match sl.tail with
    | Nil -> None
    | Node n -> Some (n.key, n.value)
    | NInf _ -> assert false

  let search_level c key i =
    let rec aux_search c i (ln, m) =
      match c with
      | NInf n -> aux_search n.(i) i (c, `Empty)
      | Nil -> (ln, m)
      | Node n -> (
          match Ord.compare key n.key with
          | -1 -> (ln, `Lt c)
          | 0 -> (ln, `Eq c)
          | 1 -> aux_search n.next.(i) i (c, `Gt c)
          | _ -> assert false)
    in
    aux_search c i (Nil, `Empty)

  let rec find_node_eq key c pn i =
    if i >= 0 then
      match c with
      | NInf n -> find_node_eq key n.(i) c i
      | Nil -> find_node_eq key pn c (i - 1)
      | Node { key = k; value; next } -> (
          match Ord.compare key k with
          | -1 -> find_node_eq key pn c (i - 1)
          | 0 -> Some (k, value)
          | 1 -> find_node_eq key next.(i) c i
          | _ -> assert false)
    else None

  let find_nearest_nodes (sl : 'a t) (key : key) lvl :
      (int * (key, 'a) node) list * (key, 'a) node ranged =
    let rec aux_find c i (v, m) =
      if i < 0 then (v, m)
      else
        let n, m = search_level c key i in
        match m with
        | `Eq _ -> ((i, n) :: v, m)
        | _ -> aux_find n (i - 1) ((i, n) :: v, m)
    in
    aux_find sl.head lvl ([], `Empty)

  let rec find_node_nearest k c pn i d =
    if i >= 0 then
      match c with
      | NInf n -> find_node_nearest k n.(i) c i d
      | Nil -> find_node_nearest k pn c (i - 1) d
      | Node { key; value; next } -> (
          match Ord.compare k key with
          | -1 -> find_node_nearest k pn c (i - 1) (`Lt (key, value))
          | 0 -> `Eq (key, value)
          | 1 -> find_node_nearest k next.(i) c i (`Gt (key, value))
          | _ -> assert false)
    else d

  let find_linked (sl : 'a t) (key : key) :
      (int * (key, 'a) node) list * (key, 'a) node ranged =
    let rec aux_find c i (v, m) =
      if i < 0 then (v, m)
      else
        let n, m = search_level c key i in
        match m with
        | `Eq _ -> aux_find n (i - 1) ((i, n) :: v, m)
        | _ -> aux_find n (i - 1) (v, m)
    in
    aux_find sl.head sl.cur_level ([], `Empty)

  let find (key : key) (sl : 'a t) : (key, 'a) pair option =
    find_node_eq key sl.head Nil sl.cur_level

  let find_nearest (key : key) (sl : 'a t) :
      [ `Gt of (key, 'a) pair
      | `Lt of (key, 'a) pair
      | `Eq of (key, 'a) pair
      | `Empty ] =
    find_node_nearest key sl.head Nil sl.cur_level `Empty

  let find_range ~(start : key) ~(stop : key) (sl : 'a t) : (key, 'a) pair list
      =
    if start > stop then [] else
    let rec until f l acc =
      match f with
      | NInf _ | Nil -> acc
      | Node n -> (
          match Ord.compare n.key l with
          | -1 -> until n.next.(0) l ((n.key, n.value) :: acc)
          | 0 | 1 -> acc
          | _ -> assert false)
    in
    let _, s = search_level sl.head start 0 in
    match s with
    | `Lt n -> until n stop []
    | `Eq n -> until n stop []
    | `Empty | `Gt _ -> []

  let add ~(key : key) ~(value : 'a) (sl : 'a t) : unit =
    let rec aux_add (p : (int * (key, 'a) node) list) n (i : int) =
      match p with
      | [] -> ()
      | h :: t ->
          let l, pn = h in
          if i >= l then (
            SNode.link l pn n;
            aux_add t n i)
          else ()
    in
    let lvl = flip sl.max_level in
    let path, f = find_nearest_nodes sl key lvl in
    match f with
    | `Eq n -> SNode.update n value
    | _ ->
        if sl.cur_level < lvl then sl.cur_level <- lvl;
        let n = SNode.create key value sl.max_level in
        sl.length <- sl.length + 1;
        aux_add path n lvl;
        if SNode.has_next 0 n then () else sl.tail <- n

  let remove (key : key) (sl : 'a t) : unit =
    let rec aux_remove path n =
      match path with
      | [] -> if sl.length = 0 then sl.tail <- Nil
      | h :: t ->
          let l, pn = h in
          SNode.unlink l pn n;
          if l = 0 && not (SNode.has_next 0 pn) then sl.tail <- pn;
          aux_remove t n
    in
    let path, f = find_linked sl key in
    match f with
    | `Eq n ->
        sl.length <- sl.length - 1;
        aux_remove path n
    | _ -> ()

  let mem (key : key) (sl : 'a t) : bool =
    let f = find key sl in
    match f with None -> false | Some _ -> true

  let filter_map_inplace f sl =
    let rec aux_do c =
      match c with
      | Nil -> ()
      | NInf n -> aux_do n.(0)
      | Node n -> (
          let next = n.next.(0) in
          match f n.key n.value with
          | None -> remove n.key sl
          | Some v ->
              n.value <- v;
              aux_do next)
    in
    aux_do sl.head

  let fold (sl : 'a t) ~init ~f =
    let rec aux_fold c acc =
      match c with
      | Nil -> acc
      | NInf n -> aux_fold n.(0) acc
      | Node n -> aux_fold n.next.(0) (f ~key:n.key ~value:n.value acc)
    in
    aux_fold sl.head init

  let iter f sl = fold sl ~init:() ~f:(fun ~key ~value _ -> f key value)

  let clear sl =
    sl.length <- 0;
    sl.cur_level <- 0;
    sl.tail <- Nil;
    sl.head <- NInf (Array.init sl.max_level (fun _ -> Nil))

  let copy ?max_level (sl : 'a t) : 'a t =
    let ml = match max_level with None -> sl.max_level | Some n -> n in
    let sl' = create ~max_level:ml () in
    fold sl ~init:sl' ~f:(fun ~key ~value acc ->
        add acc ~key ~value;
        acc)

  let of_alist (el : (key, 'a) pair list) : 'a t =
    let sl = create () in
    List.iter (fun (key, value) -> add ~key ~value sl) el;
    sl

  let to_alist (sl : 'a t) : (key, 'a) pair list =
    fold sl ~init:[] ~f:(fun ~key ~value acc -> (key, value) :: acc)

  let to_string (sl : 'a t) : string =
    let rec aux_lvl c i str =
      match c with
      | Nil -> str ^ "]"
      | NInf n -> aux_lvl n.(i) i "[ "
      | Node n -> aux_lvl n.next.(i) i (str ^ Ord.to_string n.key ^ "; ")
    in
    let rec aux_str c i str =
      let i = i - 1 in
      if i < 0 then str
      else
        let lvl = aux_lvl c i "" in
        aux_str c i (Printf.sprintf "%slevel: %d = %s\n" str i lvl)
    in
    let key_str k =
      match k with None -> "None" | Some (k', _) -> Ord.to_string k'
    in
    let hdr =
      Printf.sprintf "Info: max_level: %d, level: %d, len: %#d\n" sl.max_level
        sl.cur_level sl.length
    in
    let info =
      Printf.sprintf "%s Elements: min: %s, max: %s\n" hdr
        (key_str (min sl))
        (key_str (max sl))
    in
    aux_str sl.head sl.max_level info

  let pp ppf sl = Format.pp_print_string ppf (to_string sl)

  let to_seq sl : (key * 'a) Seq.t =
    let rec aux_seq c () =
      match c with
      | Nil -> Seq.Nil
      | NInf n -> aux_seq n.(0) ()
      | Node n -> Seq.Cons ((n.key, n.value), aux_seq n.next.(0))
    in
    aux_seq sl.head

  let to_seq_keys sl : key Seq.t = Seq.map fst (to_seq sl)

  let to_seq_values sl : 'a Seq.t = Seq.map snd (to_seq sl)

  let add_seq sl i = Seq.iter (fun (key, value) -> add ~key ~value sl) i

  let of_seq i =
    let sl = create () in
    add_seq sl i;
    sl
end
