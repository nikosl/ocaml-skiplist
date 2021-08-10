(*
 * Copyright (c) 2021 Nikos Leivadaris
 * 
 * This software is released under the MIT License.
 * https://opensource.org/licenses/MIT
 *)

type fixture = { len : int; sorted : int list; shuffled : int list }

type t = (int * fixture) list

val samples_id : int list

val load_data_exn : unit -> t

val rand_int : 'a -> ('a * fixture) list -> int list

val incr_int : 'a -> ('a * fixture) list -> int list

val fixture_to_string : fixture -> string
