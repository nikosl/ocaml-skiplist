(*
 * Copyright (c) 2021 Nikos Leivadaris
 * 
 * This software is released under the MIT License.
 * https://opensource.org/licenses/MIT
 *)
open QCheck

module SLConf = struct
  module Sut = Skiplist.Make (Int)

  type state = (int * int) list

  type sut = int Sut.t

  type cmd =
    | Clear
    | Add of int * int
    | Remove of int
    | Find of int
    | Find_finger of int
    | Find_nearest of int
    | Find_range of int * int
    | Find_all of int
    | Replace of int * int
    | Mem of int
    | Length
    | Min
    | Max
  [@@deriving show { with_path = false }]

  let gen_cmd s =
    let int_gen = Gen.small_nat in
    let k_int_gen =
      match s with
      | [] -> Gen.oneof [ Gen.small_nat; Gen.int ]
      | _ ->
          let keys = List.map fst s in
          Gen.oneof [ Gen.oneofl keys; Gen.small_nat; Gen.int ]
    in
    Gen.oneof
      [
        Gen.return Clear;
        Gen.map2 (fun k v -> Add (k, v)) k_int_gen int_gen;
        Gen.map (fun k -> Remove k) k_int_gen;
        Gen.map (fun k -> Find k) k_int_gen;
        Gen.map (fun k -> Find_finger k) k_int_gen;
        Gen.map (fun k -> Find_nearest k) k_int_gen;
        Gen.map2 (fun k1 k2 -> Find_range (k1, k2)) k_int_gen k_int_gen;
        Gen.map (fun k -> Find_all k) k_int_gen;
        Gen.map2 (fun k v -> Replace (k, v)) k_int_gen int_gen;
        Gen.map (fun k -> Mem k) k_int_gen;
        Gen.return Length;
      ]

  let shrink c =
    let open Iter in
    match c with
    | Clear -> Iter.empty
    | Add (k, v) ->
        Iter.map (fun k' -> Add (k', v)) (Shrink.int k)
        <+> Iter.map (fun v' -> Add (k, v')) (Shrink.int v)
    | Remove k -> Iter.map (fun k' -> Remove k') (Shrink.int k)
    | Find k -> Iter.map (fun k' -> Find k') (Shrink.int k)
    | Find_finger k -> Iter.map (fun k' -> Find_finger k') (Shrink.int k)
    | Find_nearest k -> Iter.map (fun k' -> Find_nearest k') (Shrink.int k)
    | Find_range (k1, k2) ->
        Iter.map (fun k1' -> Find_range (k1', k2)) (Shrink.int k1)
        <+> Iter.map (fun k2' -> Find_range (k1, k2')) (Shrink.int k2)
    | Find_all k -> Iter.map (fun k' -> Find_all k') (Shrink.int k)
    | Replace (k, v) ->
        Iter.map (fun k' -> Replace (k', v)) (Shrink.int k)
        <+> Iter.map (fun v' -> Replace (k, v')) (Shrink.int v)
    | Mem k -> Iter.map (fun k' -> Mem k') (Shrink.int k)
    | Length | Min | Max -> Iter.empty

  let arb_cmd s = QCheck.make ~print:show_cmd ~shrink (gen_cmd s)

  let init_state = []

  let state_add k v s = (k, v) :: List.remove_assoc k s

  let state_sort s = List.sort (fun (k, _) (k', _) -> Int.compare k k') s

  let state_find_nearest k s =
    let rec gte k s acc =
      match s with
      | [] -> acc
      | h :: t -> (
          match h with
          | k', _ when k' > k -> gte k t (`Lt h)
          | k', _ when k' = k -> `Eq h
          | k', _ when k' < k -> acc
          | _ -> assert false)
    in
    let nearest k s =
      match s with
      | [] -> `Empty
      | (k', v) :: _ -> if k' < k then `Gt (k', v) else gte k s `Empty
    in
    s |> state_sort |> List.rev |> nearest k

  let state_find_range k1 k2 s =
    s |> state_sort |> List.filter (fun (k, _) -> k >= k1 && k < k2)

  let next_state c s =
    match c with
    | Clear -> []
    | Add (k, v) -> state_add k v s
    | Remove k -> List.remove_assoc k s
    | Replace (k, v) -> state_add k v s
    | Find _ | Find_finger _ | Find_nearest _ | Find_range _ | Find_all _
    | Mem _ | Length | Min | Max ->
        s

  let init_sut () = Sut.create ()

  let cleanup _ = ()

  let run_cmd c s sl =
    match c with
    | Clear ->
        Sut.clear sl;
        true
    | Add (k, v) ->
        Sut.add sl ~key:k ~value:v;
        true
    | Remove k ->
        Sut.remove sl k;
        true
    | Find k -> List.assoc_opt k s = Sut.find sl k
    | Find_finger k -> List.assoc_opt k s = Sut.find_finger sl k
    | Find_nearest k -> (
        let r1 = state_find_nearest k s in
        let r2 = Sut.find_nearest sl k in
        match (r1, r2) with
        | `Empty, `Empty -> true
        | `Lt (k1, v1), `Lt (k2, v2) -> k1 = k2 && v1 = v2
        | `Eq (k1, v1), `Eq (k2, v2) -> k1 = k2 && v1 = v2
        | `Gt (k1, v1), `Gt (k2, v2) -> k1 = k2 && v1 = v2
        | _, _ -> false)
    | Find_range (k1, k2) ->
        (let r1 = state_find_range k1 k2 s in
         let r2 = Sut.find_range ~start:k1 ~stop:k2 sl |> List.rev in
         List.compare (fun (k, _v) (k', _v') -> Int.compare k k') r1 r2)
        = 0
    | Find_all _k -> true
    | Replace (k, v) ->
        Sut.add sl ~key:k ~value:v;
        true
    | Mem k -> List.mem_assoc k s = Sut.mem sl k
    | Length -> List.length s = Sut.length sl
    | Min ->
        let asc = state_sort s in
        let min = List.nth_opt asc 0 in
        min = Sut.min sl
    | Max ->
        let desc = s |> state_sort |> List.rev in
        let max = List.nth_opt desc 0 in
        max = Sut.max sl

  let precond _c _s = true
end

module SL = QCSTM.Make (SLConf)

let tc = SL.agree_test ~count:10_000 ~name:"Skiplist-model agreement"

let () =
  let suite = List.map QCheck_alcotest.to_alcotest [ tc ] in
  Alcotest.run "Skiplist" [ ("suite", suite) ]
