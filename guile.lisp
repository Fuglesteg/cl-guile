(uiop:define-package :guile
  (:use :cl)
  (:local-nicknames (:api :guile/api))
  (:import-from :cffi
                :defcallback))

(in-package :guile)

;; TODO: Error handling (with continuation restart)
;; TODO: Preserve case / #t, #f ... (readtable?)
;; TODO: Fix/Look into guile init in all threads
;; TODO: Optimize `scheme` macro by evaluation to function and calling function
;; TODO: guile stdout = *standard-output*
;; Figure out parsing, inspecting lisp lambda list

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *initialized* nil))

(defun init ()
  (unless *initialized*
    (cffi:use-foreign-library api:libguile)
    (api:init)
    (api:eval-string "(use-modules (ice-9 exceptions))")
    (api:eval-string
     "(define (safe-eval exp)
       (with-exception-handler
           (lambda (exception)
             (call-with-output-string
              (lambda (port)
                (print-exception port
                                 #f
                                 (exception-kind exception)
                                 (exception-args exception)))))
         (lambda () (eval exp (current-module)))
         #:unwind? #t))")
    (eval-on-init)
    (setf *initialized* t)))

(defun scm->string (scm)
      (values (cffi:foreign-string-to-lisp (api:scm->string scm))))

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
    ((api:scm->bool (api:scm-call-1 (api:eval-string "procedure?") scm))
     (lambda (&rest args)
       (let* ((scm-args (cffi:foreign-alloc :pointer :initial-contents (mapcar #'lisp->scm args)))
              (result (api:scm-call-n scm scm-args (length args))))
         (cffi:foreign-free scm-args)
         (scm->lisp result))))
    ((api:keywordp scm)
     (intern
      (string-upcase
       (scm->string
        (api:scm-call-1 (api:eval-string "symbol->string")
                        (api:scm-call-1 (api:eval-string "keyword->symbol") scm))))
      'keyword))
    ((api:scm->bool (api:symbolp scm))
     (intern
      (string-upcase
       (scm->string
        (api:scm-call-1 (api:eval-string "symbol->string") scm)))))
    ((scm->lisp (api:scm-call-1 (api:eval-string "record?") scm))
     (convert-record (scm->lisp (api:scm-call-1 (api:eval-string "record-details") scm))))
    (t (error "Could not convert scheme object"))))

(defun convert-record (record-details)
  (destructuring-bind (name slots) record-details
    (let ((class-name (cdr (assoc name *scheme-record-types*))))
      (if class-name
          (apply #'make-instance class-name
                 (mapcan (lambda (pair)
                           (list (car pair) (cdr pair)))
                         slots))
          record-details))))

(defclass scm-record () ())

(defvar *scheme-record-types* nil)

(defmacro define-scheme-record (name superclasses slots)
  (destructuring-bind (class-name record-name)
      (if (consp name)
          name
          (list name name))
  `(progn
     (defclass ,class-name (,@superclasses scm-record)
       ,slots)
     (setf *scheme-record-types* (append (list (cons ',record-name ',class-name))
                                         *scheme-record-types*)))))

(define-scheme-record (scm-compound-exception &compound-exception) ()
  ((components
    :initarg :components
    :initform nil
    :accessor scm-compound-exception-components
    :type list)))

(define-scheme-record (scm-exception &exception) () ())

(define-scheme-record (scm-exception-with-message &message) (scm-exception)
  ((message
    :initarg :message
    :initform ""
    :accessor scm-exception-message
    :type string)))

(defun lisp->scm (lisp-object)
  (labels ((as-is () (api:eval-string (format nil "~S" lisp-object))))
    (if (eq t lisp-object)
        (api:eval-string "#t")
        (etypecase lisp-object
          (null (api:eval-string "#f"))
          (cons (api:cons
                 (lisp->scm (car lisp-object))
                 (lisp->scm (cdr lisp-object))))
          (keyword (api:string->scm-keyword (string-downcase (string lisp-object))))
          (symbol (api:string->scm-symbol (string-downcase (string lisp-object))))
          (number (as-is))
          (string (as-is))))))

(defun eval-string (string)
  (scm->lisp (api:eval-string string)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *eval-on-init* nil)
  (defmacro delay-evaluation (&body body)
    (if *initialized*
        `(progn ,@body)
        `(setf *eval-on-init*
               (append
                (list
                 (lambda ()
                  ,@body))
                *eval-on-init*))))
  (defmacro scheme (&body body)
    `(eval-string
      ,(let ((*print-case* :downcase))
         (format nil "(safe-eval '(begin ~{~S~}))" body))))
  (defmacro scheme-toplevel (&body body)
    `(delay-evaluation
       (scheme ,@body))))

(defun eval-on-init ()
  (loop for function in *eval-on-init*
        do (funcall function))
  (setf *eval-on-init* nil))

(scheme-toplevel
  (define (record-details scm-record)
    (let* ((record-type-descriptor (record-type-descriptor scm-record))
           (record-type-fields (record-type-fields record-type-descriptor))
           (record-type-name (record-type-name record-type-descriptor)))
      (list record-type-name
            (map-in-order (let ((i -1))
                            (lambda (field)
                              (set! i (+ i 1))
                              (cons (symbol->keyword field)
                                    (struct-ref scm-record i))))
                          record-type-fields)))))

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

(define-scheme-procedure handle-scheme-exception (exception)
  ; 1. Get scheme restarts
  ; 2. Restart-bind
  ; 3. signal scheme-error
  (break "~S" exception)
  #+nil(with-condition-restarts 'scheme-error
      (loop for handler in (exception-handlers exception)
            do 'something)
    (signal 'scheme-error (exception-message exception))))
   
;; Example make procedure callable from scheme
(define-scheme-procedure hello-from-scheme ()
  (format t "Hello from Scheme!!!!"))

(define-scheme-procedure say-name (name)
  (format t "Hello ~a" name))
