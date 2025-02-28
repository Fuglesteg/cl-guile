(uiop:define-package :guile
  (:use :cl)
  (:local-nicknames (:api :guile/api))
  (:import-from :cffi
                :defcallback))

(in-package :guile)

(defun scm->string (scm)
  (cffi:foreign-string-to-lisp (api:scm->string scm)))

(defun scm->lisp (scm)
  (cond
    ((api:scm-null-pointer-p scm) nil)
    ((api:scm->bool (api:scm-null-p scm)) nil)
    ((api:boolp scm) (api:scm->bool scm))
    ((api:exact-integer-p scm) (api:scm->int32 scm))
    ((api:realp scm) (api:scm->double scm))
    ((api:stringp scm) (scm->string scm))
    ((api:pairp scm) (cons 
                      (scm->lisp (api:car scm))
                      (scm->lisp (api:cdr scm))))
    (t (error "Could not convert"))))

;; TODO: Implement this
(defun lisp->scm (lisp-object)
  (api:string-eval (format nil "~a" lisp-object)))

(defun eval-string (string)
  (scm->lisp (api:eval-string string)))

(defmacro guile (&body body)
  `(let ((*print-case* :downcase))
     (eval-string (format nil "~S" ',@body))))

(defmacro define-scheme-procedure (name lambda-list &body body)
  `(progn
     (defun ,name ,lambda-list
       ,@body)
     (defcallback ,name :pointer ,(mapcar (lambda (parameter)
                                                (list parameter :pointer))
                                              lambda-list)
       (lisp->scm (apply #',name (mapcar #'scm->lisp (list ,@lambda-list)))))
     (cffi:with-foreign-string (scheme-procedure-name (string-downcase (symbol-name ',name)))
       (api:scm-c-define-gsubr scheme-procedure-name
                               ,(length lambda-list)
                               0
                               0
                               (cffi:callback ,name)))))

;; Example make procedure callable from scheme
#+nil
(define-scheme-procedure hello-from-scheme ()
  (format t "Hello from Scheme!!!!"))
