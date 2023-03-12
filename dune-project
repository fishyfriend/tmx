(lang dune 3.7)

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
  base64
  bigstringaf
  ezgzip
  ezjsonm
  ezxmlm
  ppx_deriving
  ppx_import
  (alcotest :with-test)))

(generate_opam_files true)
