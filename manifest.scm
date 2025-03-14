(use-modules (guix packages)
             (gnu packages lisp)
             (gnu packages lisp-xyz))

(define cl-guile (load "guix.scm"))

(packages->manifest (append (filter package?
                                    (map cadr
                                         (package-development-inputs cl-guile)))
                            (list
                             sbcl
                             sbcl-micros)))