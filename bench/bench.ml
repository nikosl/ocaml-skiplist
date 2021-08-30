(* * Copyright (c) 2021 Nikos Leivadaris * * This software is released under the
   MIT License. * https://opensource.org/licenses/MIT *)

open Core_bench
module M = Skiplist.Make (Int)

let rand_int n =
  let rec aux n acc =
    if n = 0 then acc else aux (n - 1) ((Random.int n, 0) :: acc)
  in
  Random.self_init ();
  aux n []

let incr_int n =
  let rec aux n acc = if n = 0 then acc else aux (n - 1) ((n, n + 1) :: acc) in
  aux n []

let add_bench sl xs () =
  let rec aux xs =
    match xs with
    | [] -> ()
    | hd :: tl ->
        let key, value = hd in
        M.add sl ~key ~value;
        aux tl
  in
  aux xs

let find_bench sl xs () =
  let rec aux xs =
    match xs with
    | [] -> ()
    | hd :: tl -> (
        let key, _ = hd in
        match M.find key sl with None -> () | Some _ -> aux tl)
  in
  aux xs

let hm_add_bench sl xs () =
  let rec aux xs =
    match xs with
    | [] -> ()
    | hd :: tl ->
        let key, value = hd in
        Hashtbl.add sl key value;
        aux tl
  in
  aux xs

let hm_find_bench sl xs () =
  let rec aux xs =
    match xs with
    | [] -> ()
    | hd :: tl -> (
        let key, _ = hd in
        try
          ignore (Hashtbl.find sl key);
          aux tl
        with Not_found -> ())
  in
  aux xs

let default_args = [ 1; 100; 1000; 10000 ]

let benchmarks = []

let main () =
  Core.Command.run
    (Bench.make_command
       [
         Bench.Test.create_indexed ~name:"SLAddRandInt" ~args:[ 1; 100 ]
           (fun elems ->
             let v = rand_int elems in
             let sl = M.create () in
             Core.Staged.stage (add_bench sl v));
         Bench.Test.create_indexed ~name:"SLAddIncrInt" ~args:[ 1; 100 ]
           (fun elems ->
             let v = rand_int elems in
             let sl = M.create () in
             Core.Staged.stage (add_bench sl v));
         Bench.Test.create_indexed ~name:"HMAddIncrInt" ~args:[ 1; 100 ]
           (fun elems ->
             let v = rand_int elems in
             let sl = Hashtbl.create elems in
             Core.Staged.stage (hm_add_bench sl v));
         Bench.Test.create_indexed ~name:"SLFindRandInt" ~args:[ 1; 100 ]
           (fun elems ->
             let v = rand_int elems in
             let sl = M.of_alist v in
             Core.Staged.stage (find_bench sl v));
         Bench.Test.create_indexed ~name:"HMFindRandInt" ~args:[ 1; 100 ]
           (fun elems ->
             let v = rand_int elems in
             let sl = List.to_seq v |> Hashtbl.of_seq in
             Core.Staged.stage (hm_find_bench sl v));
       ])

let () = main ()
