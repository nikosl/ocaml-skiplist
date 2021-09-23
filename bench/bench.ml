(* * Copyright (c) 2021 Nikos Leivadaris * * This software is released under the
   MIT License. * https://opensource.org/licenses/MIT *)

open Core_bench
module M = Skiplist.Make (Int)
module IMap = Map.Make (Int)

let default_args = Fixtures.samples_id

let fixtures = Fixtures.load_data_exn ()

let incr_int n = Fixtures.incr_int n fixtures

let rec shuffle = function
  | [] -> []
  | [ single ] -> [ single ]
  | list ->
      let before, after = List.partition (fun _ -> Random.bool ()) list in
      List.rev_append (shuffle before) (shuffle after)

let rand_int n = Fixtures.rand_int n fixtures

let add_bench sl xs () = List.iter (fun key -> M.add sl ~key ~value:key) xs

let find_bench sl xs () =
  List.iter
    (fun k ->
      match M.find sl k with None -> failwith "unexpected" | Some _ -> ())
    xs

let hm_add_bench sl xs () = List.iter (fun k -> Hashtbl.add sl k k) xs

let hm_find_bench sl xs () =
  List.iter
    (fun k ->
      match Hashtbl.find_opt sl k with
      | None -> failwith "unexpected"
      | Some _ -> ())
    xs

let m_find_bench sl xs () =
  List.iter
    (fun k ->
      match IMap.find_opt k sl with
      | None -> failwith "unexpected"
      | Some _ -> ())
    xs

type benchmark = {
  name : string;
  gen : int -> int list;
  bench : int list -> unit -> unit;
}

let benchmarks =
  [
    {
      name = "SLAddRandInt";
      gen = rand_int;
      bench =
        (fun elems ->
          let sut = M.create () in
          add_bench sut elems);
    };
    {
      name = "SLAddIncrInt";
      gen = rand_int;
      bench =
        (fun elems ->
          let sut = M.create () in
          add_bench sut elems);
    };
    {
      name = "HMAddIncrInt";
      gen = rand_int;
      bench =
        (fun elems ->
          let sut = Hashtbl.create @@ List.length elems in

          hm_add_bench sut elems);
    };
    {
      name = "SLFindRandInt";
      gen = rand_int;
      bench =
        (fun elems ->
          let sut =
            elems |> List.to_seq |> Seq.map (fun k -> (k, k)) |> M.of_seq
          in
          find_bench sut elems);
    };
    {
      name = "HMFindRandInt";
      gen = rand_int;
      bench =
        (fun elems ->
          let sut =
            elems |> List.to_seq |> Seq.map (fun k -> (k, k)) |> Hashtbl.of_seq
          in
          hm_find_bench sut elems);
    };
    {
      name = "MFindRandInt";
      gen = rand_int;
      bench =
        (fun elems ->
          let sut =
            elems |> List.to_seq |> Seq.map (fun k -> (k, k)) |> IMap.of_seq
          in
          m_find_bench sut elems);
    };
  ]

let make_bench b =
  let { name; gen; bench } = b in
  Bench.Test.create_indexed ~name ~args:default_args (fun elems ->
      let v = gen elems in
      let f = bench v in
      Core.Staged.stage f)

let main () =
  Core.Command.run (Bench.make_command @@ List.map make_bench benchmarks)

let () = main ()
