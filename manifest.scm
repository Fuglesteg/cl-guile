(use-modules (guix packages)
             (gnu packages lisp)
             (gnu packages lisp-xyz))

(packages->manifest (append (map cadr 
                                 (package-inputs (load "guix.scm"))) ; cl-guile
                    (list
                     sbcl
                     sbcl-micros)))