(lang dune 2.0)
(name arbogen)
(version 1.0c)

(generate_opam_files true)

(source (github fredokun/arbogen))
(license GPL-2.0-only)
(authors
 "Alexis Darasse"
 "Matthieu Dien"
 "Antoine Genitrini"
 "Marwan Ghanem"
 "Martin Pépin"
 "Frederic Peschanski"
 "Xuming Zhan")
(maintainers "Martin Pépin <kerl@wkerl.me>")

(package
 (name arbogen)
 (synopsis "A fast uniform random generator of tree structures")
 (depends
  (ocaml (>= 4.03))
  (dune (>= 2.0))
  (alcotest :with-test)
  (benchmark :with-test)
  (odoc :with-doc)))
