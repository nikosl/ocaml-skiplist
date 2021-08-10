(* * Copyright (c) 2021 Nikos Leivadaris * * This software is released under the
   MIT License. * https://opensource.org/licenses/MIT *)

module M = Skiplist.Make (Int)

let default_args = Fixtures.samples_id

let fixtures = Fixtures.load_data_exn ()

let rand_int n = Fixtures.rand_int n fixtures

let incr_int n = Fixtures.incr_int n fixtures

let add_bench sl xs () =
  let rec aux xs =
    match xs with
    | [] -> ()
    | hd :: tl ->
        M.add sl ~key:hd ~value:hd;
        aux tl
  in
  aux xs

let find_bench sl xs () =
  let rec aux xs =
    match xs with
    | [] -> ()
    | hd :: tl -> ( match M.find sl hd with None -> () | Some _ -> aux tl)
  in
  aux xs

let rm_bench sl xs () =
  let rec aux xs =
    match xs with
    | [] -> ()
    | hd :: tl ->
        M.remove sl hd;
        aux tl
  in
  aux xs

let benchmarks = []

let main () =
  let elems = 10_000 in
  let v = rand_int elems in
  Memtrace.trace_if_requested ();
  let sl = M.create () in
  add_bench sl v ();
  Printf.printf "find\n";
  find_bench sl v ();
  Printf.printf "rm\n";
  rm_bench sl v ();
  ()

let () = main ()
