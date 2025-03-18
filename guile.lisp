(uiop:define-package :guile
  (:use :cl)
  (:local-nicknames (:api :guile/api))
  (:import-from :cffi
                :defcallback))

(in-package :guile)

;; TODO: Guile record conversion
;; TODO: Error handling (with continuation restart)
;; TODO: Preserve case (readtable?)
;; TODO: Fix/Look into guile init in all threads
;; Figure out parsing, inspecting lisp lambda list

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *initialized* nil))

(defun init ()
  (unless *initialized*
    (cffi:use-foreign-library api:libguile)
    (api:init)
    (api:eval-string "(use-modules (ice-9 exceptions))")
    (eval-on-init)
    (setf *initialized* t)))

(defun scm->string (scm)
  (cffi:foreign-string-to-lisp (api:scm->string scm)))

;; TODO: Symbols
;; TODO: Keywords
;; TODO: Records
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
    (t (error "Could not convert scheme object"))))

;; TODO: Implement this
(defun lisp->scm (lisp-object)
  (cond
    ((null lisp-object) (cffi:make-pointer api::+scm-null+))
    (t (api:eval-string (format nil "~S" lisp-object)))))

(defun eval-string (string)
  (scm->lisp (api:eval-string string)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *eval-on-init* nil)
  (defmacro delay-evaluation (&body body)
    (if *initialized*
        `(progn ,@body)
        `(push
          (lambda ()
            ,@body)
          *eval-on-init*))))

(defmacro guile (&body body)
  `(let ((*print-case* :downcase))
     (delay-evaluation
       (eval-string
        (format nil "(with-exception-handler (lambda (exception) (exception-message exception)) (lambda () (eval '(begin ~S) (current-module))) #:unwind? #t)" ',@body)))))

(defun eval-on-init ()
  (loop for function in *eval-on-init*
        do (funcall function))
  (setf *eval-on-init* nil))

(defmacro define-scheme-procedure (name lambda-list &body body)
  `(progn
     (defun ,name ,lambda-list
       ,@body)
     (defcallback ,name :pointer ,(mapcar (lambda (parameter)
                                            (list parameter :pointer))
                                          lambda-list)
       (lisp->scm (apply #',name (mapcar #'scm->lisp (list ,@lambda-list)))))
     (delay-evaluation
       (cffi:with-foreign-string (scheme-procedure-name (string-downcase (symbol-name ',name)))
         (api:scm-c-define-gsubr scheme-procedure-name
                                 ,(length lambda-list)
                                 0
                                 0
                                 (cffi:callback ,name))))))

(define-condition scheme-error (error) ())

#+nil(define-scheme-procedure handle-scheme-exception (exception)
  ; 1. Get scheme restarts
  ; 2. Restart-bind
  ; 3. signal scheme-error
  (with-condition-restarts 'scheme-error
      (loop for handler in (exception-handlers exception)
            do 'something)
    (signal 'scheme-error (exception-message exception))))
   
;; Example make procedure callable from scheme
(define-scheme-procedure hello-from-scheme ()
  (format t "Hello from Scheme!!!!"))

(guile
  (define (record-details scm-record)
    (let* ((record-type-descriptor (record-type-descriptor scm-record))
           (record-type-fields (record-type-fields record-type-descriptor))
           (record-type-name (record-type-name record-type-descriptor)))
      (list (symbol->string record-type-name) (map-in-order (let ((i -1))
                                                              (lambda (field)
                                                                (set! i (+ i 1))
                                                                (cons (symbol->string field)
                                                                      (struct-ref scm-record i))))
                                                            record-type-fields)))))