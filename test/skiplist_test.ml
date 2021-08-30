open Alcotest
module M = Skiplist

let test_empty () =
  let module IntSList = M.Make (Int) in
  let sl = IntSList.create () in
  let actual = IntSList.is_empty sl in
  let expected = true in
  check bool "empty" actual expected

let test_len_empty () =
  let module IntSList = M.Make (Int) in
  let sl = IntSList.create () in
  let actual = IntSList.length sl in
  let expected = 0 in
  check int "empty" actual expected

let test_len_insert () =
  let module IntSList = M.Make (Int) in
  let sl = IntSList.create () in
  let n = 13 in
  IntSList.add sl ~key:(n + 2) ~value:n;
  IntSList.add sl ~key:n ~value:n;
  IntSList.add sl ~key:(n + 1) ~value:n;
  IntSList.remove (n + 2) sl;
  IntSList.add sl ~key:(n + 2) ~value:n;
  let actual = IntSList.length sl in
  let expected = 3 in
  check int "three" actual expected

let test_len_remove () =
  let module IntSList = M.Make (Int) in
  let sl = IntSList.create () in
  let n = 13 in
  IntSList.add sl ~key:(n + 2) ~value:n;
  IntSList.add sl ~key:n ~value:n;
  IntSList.add sl ~key:(n + 1) ~value:n;
  IntSList.remove (n + 2) sl;
  IntSList.remove n sl;
  let actual = IntSList.length sl in
  let expected = 1 in
  check int "one" actual expected

let test_len_remove_all () =
  let module IntSList = M.Make (Int) in
  let sl = IntSList.create () in
  let n = 13 in
  IntSList.add sl ~key:(n + 2) ~value:n;
  IntSList.add sl ~key:n ~value:n;
  IntSList.add sl ~key:(n + 1) ~value:n;
  IntSList.remove (n + 2) sl;
  IntSList.remove n sl;
  IntSList.remove (n + 1) sl;
  let actual = IntSList.length sl in
  let expected = 0 in
  check int "empty" actual expected

let test_insertion () =
  let module IntSList = M.Make (Int) in
  let sl = IntSList.create () in
  let n = 13 in
  IntSList.add sl ~key:n ~value:n;
  let actual = IntSList.find n sl in
  let expected = Some (n, n) in
  check (option (pair int int)) "same number" actual expected

let test_insertion_update () =
  let module IntSList = M.Make (Int) in
  let sl = IntSList.create () in
  let n = 13 in
  IntSList.add sl ~key:n ~value:n;
  IntSList.add sl ~key:n ~value:(n + 1);
  let actual = IntSList.find n sl in
  let expected = Some (n, n + 1) in
  check (option (pair int int)) "same number" actual expected

let test_remove () =
  let module IntSList = M.Make (Int) in
  let sl = IntSList.create () in
  let n = 13 in
  IntSList.add sl ~key:n ~value:n;
  IntSList.remove n sl;
  let actual = IntSList.find n sl in
  let expected = None in
  check (option (pair int int)) "none" actual expected

let test_find_none () =
  let module IntSList = M.Make (Int) in
  let sl = IntSList.create () in
  let n = 13 in
  let actual = IntSList.find n sl in
  let expected = None in
  check (option (pair int int)) "none" actual expected

let test_find_range_empty () =
  let module IntSList = M.Make (Int) in
  let sl = IntSList.create () in
  let n = 13 in
  let actual = IntSList.find_range ~start:0 ~stop:n sl in
  let expected = [] in
  check (list (pair int int)) "empty" actual expected

let test_find_range () =
  let module IntSList = M.Make (Int) in
  let sl = IntSList.create () in
  let n = 6 in
  for i = 0 to n do
    IntSList.add sl ~key:i ~value:None
  done;
  let actual = IntSList.find_range ~start:1 ~stop:4 sl in
  let expected = [ (3, None); (2, None); (1, None) ] in
  check (list (pair int (option int))) "range" actual expected

let test_find_nearest_none () =
  let module IntSList = M.Make (Int) in
  let sl = IntSList.create () in
  let n = 13 in
  let actual = IntSList.find_nearest n sl in
  let expected = `Empty in
  let empty' =
    match (actual, expected) with `Empty, `Empty -> true | _, _ -> false
  in
  check bool "none" empty' true

let test_find_nearest_eq () =
  let module IntSList = M.Make (Int) in
  let sl = IntSList.create () in
  let n = 13 in
  for i = 0 to n do
    IntSList.add sl ~key:i ~value:None
  done;
  let actual = IntSList.find_nearest 2 sl in
  let eq' = match actual with `Eq (2, _) -> true | _ -> false in
  check bool "eq" eq' true

let test_find_nearest_gt () =
  let module IntSList = M.Make (Int) in
  let sl = IntSList.create () in
  let n = 13 in
  for i = 0 to n do
    IntSList.add sl ~key:i ~value:None
  done;
  let actual = IntSList.find_nearest 22 sl in
  let expected = 13 in
  let gt' = match actual with `Gt (x, _) -> x | _ -> -1 in
  check int "gt" gt' expected

let test_find_nearest_lt () =
  let module IntSList = M.Make (Int) in
  let sl = IntSList.create () in
  let n = 13 in
  for i = 4 to n do
    IntSList.add sl ~key:i ~value:None
  done;
  let actual = IntSList.find_nearest 2 sl in
  let expected = 4 in
  let lt' = match actual with `Lt (x, _) -> x | _ -> -1 in
  check int "lt" lt' expected

let test_find_nearest_lt_in_range () =
  let module IntSList = M.Make (Int) in
  let sl = IntSList.create () in
  let n = 32 in
  for i = 0 to n do
    IntSList.add sl ~key:(i * 2) ~value:None
  done;
  let actual = IntSList.find_nearest 3 sl in
  let expected = 4 in
  let lt' = match actual with `Lt (x, _) -> x | _ -> -1 in
  check int "lt range" expected lt'

let test_mem_exists () =
  let module IntSList = M.Make (Int) in
  let sl = IntSList.create () in
  let n = 13 in
  IntSList.add sl ~key:n ~value:n;
  let actual = IntSList.mem n sl in
  let expected = true in
  check bool "exist" actual expected

let test_non_mem () =
  let module IntSList = M.Make (Int) in
  let sl = IntSList.create () in
  let n = 13 in
  let actual = IntSList.mem n sl in
  let expected = false in
  check bool "missing" actual expected

let test_list_transform () =
  let module IntSList = M.Make (Int) in
  let l = [ (7, None); (1, None); (9, None); (3, None); (5, None) ] in
  let sl = IntSList.of_alist l in
  let actual = IntSList.to_alist sl in
  let expected = [ (9, None); (7, None); (5, None); (3, None); (1, None) ] in
  check (list (pair int (option int))) "same list" actual expected

let test_list_transform_empty () =
  let module IntSList = M.Make (Int) in
  let l = [] in
  let sl = IntSList.of_alist l in
  let actual = IntSList.to_alist sl in
  let expected = [] in
  check (list (pair int int)) "empty list" actual expected

let test_min () =
  let module IntSList = M.Make (Int) in
  let sl = IntSList.create () in
  let n = 13 in
  IntSList.add sl ~key:(n + 1) ~value:n;
  IntSList.add sl ~key:n ~value:n;
  let actual = IntSList.min sl in
  let expected = Some (n, n) in
  check (option (pair int int)) "min number" actual expected

let test_max () =
  let module IntSList = M.Make (Int) in
  let sl = IntSList.create () in
  let n = 13 in
  IntSList.add sl ~key:n ~value:n;
  IntSList.add sl ~key:(n - 1) ~value:n;
  let actual = IntSList.max sl in
  let expected = Some (n, n) in
  check (option (pair int int)) "max number" actual expected

let test_min_empty () =
  let module IntSList = M.Make (Int) in
  let sl = IntSList.create () in
  let n = 13 in
  IntSList.add sl ~key:n ~value:n;
  IntSList.remove n sl;
  let actual = IntSList.min sl in
  let expected = None in
  check (option (pair int int)) "none" actual expected

let test_max_empty () =
  let module IntSList = M.Make (Int) in
  let sl = IntSList.create () in
  let n = 13 in
  IntSList.add sl ~key:n ~value:n;
  IntSList.remove n sl;
  IntSList.remove n sl;
  let actual = IntSList.max sl in
  let expected = None in
  check (option (pair int int)) "none" actual expected

let test_copy_empty () =
  let module IntSList = M.Make (Int) in
  let sl = IntSList.create () in
  let sl' = IntSList.copy sl in
  let actual = IntSList.to_alist sl in
  let expected = IntSList.to_alist sl' in
  check (list (pair int int)) "empty list" actual expected

let test_copy () =
  let module IntSList = M.Make (Int) in
  let sl = IntSList.create () in
  let n = 13 in
  IntSList.add sl ~key:(n + 2) ~value:n;
  IntSList.add sl ~key:n ~value:n;
  IntSList.add sl ~key:(n + 1) ~value:n;
  let sl' = IntSList.copy sl in
  let actual = IntSList.to_alist sl in
  let expected = IntSList.to_alist sl' in
  check (list (pair int int)) "same list" actual expected

let test_seq_transform () =
  let module IntSList = M.Make (Int) in
  let l = [ (7, None); (1, None); (9, None); (3, None); (5, None) ] in
  let actual =
    List.to_seq l |> IntSList.of_seq |> IntSList.to_seq |> List.of_seq
  in
  let expected = [ (1, None); (3, None); (5, None); (7, None); (9, None) ] in
  check (list (pair int (option int))) "same list" actual expected

let suite =
  [
    ("empty list", `Quick, test_empty);
    ("empty list with length", `Quick, test_len_empty);
    ("length of list", `Quick, test_len_insert);
    ("length of list remove", `Quick, test_len_remove);
    ("length of list remove all", `Quick, test_len_remove_all);
    ("can insert element", `Quick, test_insertion);
    ("can update element", `Quick, test_insertion_update);
    ("can remove element", `Quick, test_remove);
    ("find on empty", `Quick, test_find_none);
    ("find range on empty", `Quick, test_find_range_empty);
    ("find range", `Quick, test_find_range);
    ("find nearest on empty", `Quick, test_find_nearest_none);
    ("find nearest exist", `Quick, test_find_nearest_eq);
    ("find nearest lt", `Quick, test_find_nearest_lt);
    ("find nearest gt", `Quick, test_find_nearest_gt);
    ("find nearest in range returns gt", `Quick, test_find_nearest_lt_in_range);
    ("mem exists", `Quick, test_mem_exists);
    ("not a member", `Quick, test_non_mem);
    ("list transformations", `Quick, test_list_transform);
    ("list transformations on empty", `Quick, test_list_transform_empty);
    ("min element", `Quick, test_min);
    ("max element", `Quick, test_max);
    ("min element empty", `Quick, test_min_empty);
    ("max element empty", `Quick, test_max_empty);
    ("can copy empty", `Quick, test_copy_empty);
    ("can copy", `Quick, test_copy);
    ("sequence transformations", `Quick, test_seq_transform);
  ]

let () = Alcotest.run "skiplist" [ ("Skiplist", suite) ]
