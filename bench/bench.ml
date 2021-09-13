(* * Copyright (c) 2021 Nikos Leivadaris * * This software is released under the
   MIT License. * https://opensource.org/licenses/MIT *)

open Core_bench
module M = Skiplist.Make (Int)
module IMap = Map.Make (Int)

let incr_int n =
  let rec aux n acc = if n = 0 then acc else aux (n - 1) ((n, n + 1) :: acc) in
  aux n []

let rec shuffle = function
  | [] -> []
  | [ single ] -> [ single ]
  | list ->
      let before, after = List.partition (fun _ -> Random.bool ()) list in
      List.rev_append (shuffle before) (shuffle after)

let rand_int n = [ (n, n + 1) ]

let add_bench sl xs () = List.iter (fun (key, value) -> M.add sl ~key ~value) xs

let find_bench sl xs () =
  List.iter
    (fun (k, _) ->
      match M.find k sl with None -> failwith "unexpected" | Some _ -> ())
    xs

let hm_add_bench sl xs () = List.iter (fun (k, v) -> Hashtbl.add sl k v) xs

let hm_find_bench sl xs () =
  List.iter
    (fun (k, _) ->
      match Hashtbl.find_opt sl k with
      | None -> failwith "unexpected"
      | Some _ -> ())
    xs

let m_find_bench sl xs () =
  List.iter
    (fun (k, _) ->
      match IMap.find_opt k sl with
      | None -> failwith "unexpected"
      | Some _ -> ())
    xs

let default_args = [ 1; 100; 1000; 10000 ]

type benchmark = {
  name : string;
  gen : int -> (int * int) list;
  bench : (int * int) list -> unit -> unit;
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
          let sut = M.of_alist elems in
          find_bench sut elems);
    };
    {
      name = "HMFindRandInt";
      gen = rand_int;
      bench =
        (fun elems ->
          let sut = elems |> List.to_seq |> Hashtbl.of_seq in
          hm_find_bench sut elems);
    };
    {
      name = "MFindRandInt";
      gen = rand_int;
      bench =
        (fun elems ->
          let sut = elems |> List.to_seq |> IMap.of_seq in
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
