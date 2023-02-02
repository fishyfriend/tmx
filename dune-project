(lang dune 3.5)

(name tmx)

(homepage "https://github.com/fishyfriend/tmx")

(bug_reports "https://github.com/fishyfriend/tmx/issues")

(maintainers "Jacob First <jacob.first@member.fsf.org>")

(authors "Jacob First <jacob.first@member.fsf.org>")

(package
 (name tmx)
 (synopsis "Tiled XML loader for OCaml")
 (depends
  (ocaml
   (and
    (>= 4.14)))
  ppx_deriving
  (alcotest :with-test)))

(generate_opam_files true)
