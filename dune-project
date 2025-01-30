(lang dune 3.16)

(name lc3)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name lc3)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune notty nottui)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
