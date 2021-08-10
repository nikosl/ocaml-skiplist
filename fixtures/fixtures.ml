(*
 * Copyright (c) 2021 Nikos Leivadaris
 * 
 * This software is released under the MIT License.
 * https://opensource.org/licenses/MIT
 *)

open StdLabels

type fixture = { len : int; sorted : int list; shuffled : int list }
[@@deriving yojson]

type t = (int * fixture) list

let samples_id = [ 1; 10; 100; 1000; 10000 ]

let path id =
  let i = Int.to_string id in
  "/tmp/sample/n_" ^ i ^ ".json"

let unwrap = function Ok f -> f | Error e -> failwith e

let load_data_exn () : t =
  samples_id
  |> List.fold_right ~init:[] ~f:(fun id acc ->
         let s = path id in
         let json = Yojson.Safe.from_file s in
         let fixture = unwrap (fixture_of_yojson json) in
         (fixture.len, fixture) :: acc)

let rand_int n t =
  let fixture = List.assoc n t in
  fixture.shuffled

let incr_int n t =
  let fixture = List.assoc n t in
  fixture.sorted

let fixture_to_string fixture =
  Yojson.Safe.to_string (fixture_to_yojson fixture)
