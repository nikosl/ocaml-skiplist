(lang dune 2.7)

(name skiplist)

(license MIT)

(authors "Nikos Leivadaris")

(maintainers "Nikos Leivadaris")

(generate_opam_files true)

(package
 (name skiplist)
 (synopsis
  "A SkipList behaves as a sorted list with, typically, O(log(n)) cost for insertion, look-up and removal")
 (description
  "A SkipList behaves as a sorted list with, typically, O(log(n)) cost for insertion, look-up and removal")
 (depends
  (ocaml
   (>= 4.08.0))
  (bisect_ppx
   (and
    :with-test
    (>= 2.5.0)))
  (dune
   (>= 2.7.0))
  (qcstm :with-test)
  (core_bench (and (>= "v0.14") :dev))
  (alcotest :with-test)
  (odoc :with-doc)))
