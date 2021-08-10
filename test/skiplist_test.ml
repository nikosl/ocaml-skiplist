open Alcotest
module M = Skiplist

(* let test_empty () =
  let module IntSList = M.Make (Int) in
  let sl = IntSList.empty in
  let actual = IntSList.is_empty sl in
  let expected = true in
  check bool "empty" actual expected

let test_insertion () =
  let module IntSList = M.Make (Int) in
  let sl = IntSList.empty in
  let n = 13 in
  ignore (IntSList.insert sl n);
  let actual = IntSList.find sl n in
  let expected = Some n in
  check (option int) "same number" actual expected *)

let suite =
  [
    (* ("can insert element", `Quick, test_insertion);
    ("can greet John", `Quick, test_empty); *)
  ]

let () = Alcotest.run "skiplist" [ ("Skiplist", suite) ]
