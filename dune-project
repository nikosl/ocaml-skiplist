(lang dune 2.9)

(name skiplist)

(license MIT)

(authors "Nikos Leivadaris")

(maintainers "Nikos Leivadaris")

(source
 (github nikosl/ocaml-skiplist))

(homepage "https://github.com/nikosl/ocaml-skiplist")

(bug_reports "https://github.com/nikosl/ocaml-skiplist")

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
  (dune
   (>= 2.9))
  (core_bench
   (and
    (>= v0.14)
    :dev))
  (memtrace
   (and
    (>= 0.2)
    :dev))
  (qcstm
   (and
    (>= 0.1.1)
    :with-test))
  (qcheck-alcotest
   (and
    (>= 0.18)
    :with-test))
  (bisect_ppx
   (and
    :with-test
    (>= 2.5.0)))
  (alcotest
   (and
    (>= 1.4.0)
    :with-test))
  (odoc :with-doc)))
