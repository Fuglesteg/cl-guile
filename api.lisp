(uiop:define-package :guile/api
  (:use)
  (:import-from :cl
                :in-package
                :defconstant
                :defun
                :=)
  (:import-from :cffi
                :define-foreign-library
                :use-foreign-library
                :defcfun)
  (:export :init
           :eval-string
           :scm-null-pointer-p
           :stringp
           :scm->string
           :integer-p
           :exact-integer-p
           :scm->int
           :scm->int32
           :scm->float
           :boolp
           :scm->bool
           :realp
           :scm->double
           :charp
           :scm-char->integer
           :symbolp
           :scm-symbol->string
           :keywordp
           :scm-keyword->scm-symbol
           :pairp
           :car
           :cdr
           :listp
           :vectorp
           :nullp
           :scm-null-p
           :scm-c-define-gsubr))

(in-package :guile/api)

(define-foreign-library libguile
  (:unix "/gnu/store/ylwk2vn18dkzkj0nxq2h4vjzhz17bm7c-guile-3.0.9/lib/libguile-3.0.so.1"))

(use-foreign-library libguile)

(defconstant +scm-null+ #X00000804)

(defun scm-null-pointer-p (pointer)
  (= (cffi:pointer-address pointer) +scm-null+))

(defcfun ("scm_init_guile" init) :void)

(defcfun ("scm_c_eval_string" eval-string) :pointer
  (scheme-code :string))

(defcfun ("scm_is_string" stringp) :boolean
  (scheme-object :pointer))

(defcfun ("scm_to_locale_string" scm->string) :pointer
  (scheme-object :pointer))

(defcfun ("scm_is_integer" integer-p) :boolean
  (scheme-object :pointer))

(defcfun ("scm_is_exact_integer" exact-integer-p) :boolean
  (scheme-object :pointer))

(defcfun ("scm_to_int" scm->int) :int
  (scheme-object :pointer))

(defcfun ("scm_to_int32" scm->int32) :int32
  (scheme-object :pointer))

(defcfun ("scm_to_float" scm->float) :float
  (scheme-object :pointer))

(defcfun ("scm_is_bool" boolp) :boolean
  (scheme-object :pointer))

(defcfun ("scm_to_bool" scm->bool) :boolean
  (scheme-object :pointer))

(defcfun ("scm_is_real" realp) :boolean
  (scheme-object :pointer))

(defcfun ("scm_to_double" scm->double) :double
  (scheme-object :pointer))

(defcfun ("scm_char_p" charp) :pointer
  (scheme-object :pointer))

(defcfun ("scm_char_to_integer" scm-char->integer) :int
  (scheme-object :pointer))

(defcfun ("scm_symbol_p" symbolp) :pointer
  (scheme-object :pointer))

(defcfun ("scm_symbol_to_string" scm-symbol->string) :int
  (scheme-object :pointer))

(defcfun ("scm_is_keyword" keywordp) :boolean
  (scheme-object :pointer))

(defcfun ("scm_keyword_to_symbol" scm-keyword->scm-symbol) :int
  (scheme-object :pointer))

(defcfun ("scm_is_pair" pairp) :boolean
  (scheme-object :pointer))

(defcfun ("scm_car" car) :pointer
  (scheme-object :pointer))

(defcfun ("scm_cdr" cdr) :pointer
  (scheme-object :pointer))

(defcfun ("scm_list_p" listp) :pointer
  (scheme-object :pointer))

(defcfun ("scm_is_vector" vectorp) :boolean
  (scheme-object :pointer))

(defcfun ("scm_is_null" nullp) :boolean
  (scheme-object :pointer))

(defcfun ("scm_null_p" scm-null-p) :pointer
  (scheme-object :pointer))

; SCM scm_c_define_gsubr (const char *name, int req, int opt, int rst, fcn)
(defcfun ("scm_c_define_gsubr" scm-c-define-gsubr) :pointer
  (name :pointer) (req :int) (opt :int) (rst :int) (fcn :pointer))
