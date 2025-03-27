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
           :libguile
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
           :bool->scm-bool
           :realp
           :scm->double
           :charp
           :scm-char->integer
           :symbolp
           :scm-symbol->string
           :string->scm-symbol
           :keywordp
           :scm-keyword->scm-symbol
           :string->scm-keyword
           :pairp
           :car
           :cdr
           :cons
           :listp
           :vectorp
           :nullp
           :scm-null-p
           :scm-c-define-gsubr
           :scm-call-n
           :scm-call-0
           :scm-call-1))

(in-package :guile/api)

(define-foreign-library libguile
  (:unix "libguile-3.0.so"))

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

(defcfun ("scm_is_real" realp) :boolean
  (scheme-object :pointer))

(defcfun ("scm_to_double" scm->double) :double
  (scheme-object :pointer))

(defcfun ("scm_char_p" charp) :pointer
  (scheme-object :pointer))

(defcfun ("scm_char_to_integer" scm-char->integer) :int
  (scheme-object :pointer))

;;; Booleans

(defcfun ("scm_is_bool" boolp) :boolean
  (scheme-object :pointer))

(defcfun ("scm_to_bool" scm->bool) :boolean
  (scheme-object :pointer))

(defcfun ("scm_from_bool" bool->scm-bool) :pointer
  (scheme-object :boolean))

;;; Symbols

(defcfun ("scm_symbol_p" symbolp) :pointer
  (scheme-object :pointer))

(defcfun ("scm_symbol_to_string" scm-symbol->string) :int
  (scheme-object :pointer))

(defcfun ("scm_from_locale_symbol" string->scm-symbol) :pointer
  (string :string))

;;; Keywords

(defcfun ("scm_is_keyword" keywordp) :boolean
  (scheme-object :pointer))

(defcfun ("scm_keyword_to_symbol" scm-keyword->scm-symbol) :int
  (scheme-object :pointer))

(defcfun ("scm_from_locale_keyword" string->scm-keyword) :pointer
  (string :string))

;; Pairs

(defcfun ("scm_is_pair" pairp) :boolean
  (scheme-object :pointer))

(defcfun ("scm_car" car) :pointer
  (scheme-object :pointer))

(defcfun ("scm_cdr" cdr) :pointer
  (scheme-object :pointer))

(defcfun ("scm_cons" cons) :pointer
  (scheme-object-car :pointer) (scheme-object-cdr :pointer))

;; Sequences

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

(defcfun ("scm_call_n" scm-call-n) :pointer
  (scheme-object-function :pointer) (argv (:pointer :pointer)) (nargs :int))

(defcfun ("scm_call_0" scm-call-0) :pointer
  (scheme-object-function :pointer))

(defcfun ("scm_call_1" scm-call-1) :pointer
  (scheme-object-function :pointer) (scheme-object :pointer))
