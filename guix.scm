(use-modules (guix packages)
             (guix build-system asdf)
             (gnu packages lisp-xyz)
             (gnu packages guile)
             (guix gexp)
             ((guix licenses) #:prefix licenses:))

(package
 (name "sbcl-cl-guile")
 (version "0.0.1")
 (source (local-file (dirname (current-filename)) #:recursive? #t))
 (build-system asdf-build-system/sbcl)
 (inputs
  (list sbcl-cffi
        guile-3.0))
 (synopsis "")
 (description "")
 (home-page "")
 (license licenses:gpl3+))