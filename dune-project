(lang dune 2.0)

(name skiplist)





(license MIT)

(authors
  "Nikos Leivadaris")

(maintainers
  "Nikos Leivadaris")

(generate_opam_files true)

(package
 (name skiplist)
 (synopsis "A SkipList behaves as a sorted list with, typically, O(log(n)) cost for insertion, look-up and removal")
 (description "A SkipList behaves as a sorted list with, typically, O(log(n)) cost for insertion, look-up and removal")
 (depends
  (ocaml (>= 4.08.0))
  dune
  (alcotest :with-test)
  (odoc :with-doc)))
