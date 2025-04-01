# cl-guile

Interoparability library for using Guile Scheme in Common Lisp. Instantiate a
real guile session and write code in both languages calling to each other when
needed.

**Depends on `libguile`**

**\*This library is still experimental, APIs are NOT stable\***

# Motivation

The inspiration for this library came from using [Guix](https://guix.gnu.org)
with the Common Lisp ecosystem and programs. I wanted deeper integration
between the two and wanted to for example use Guix as my package manager from
inside a live image, similar to quicklisp. This is already partly implemented
in [cl-guix-utils](https://git.sr.ht/~charje/cl-guix-utils), but I wished for
deeper integrations and to hopefully not have to depend on separate channels. I
want integrations to define profiles, manifests and packages and integrate them
with tools like [Lem](https://lem-project.github.io/) and
[StumpWM](https://stumpwm.github.io/). This library is the first step towards
this.

I was also very inspired by the idea of writing bilingual programs, I think
it's something that is very much worth exploring.
Guile Scheme has so many interesting features and libraries,
the ability to write a program using both languages
allows us to bring in the strength of each language and ecosystem for what
they each excel at.

# Features

- Write Scheme expressions as part of your Lisp code
- Define Scheme code scheduled to be evaluated when library initializes
- Define Lisp functions callable from Scheme
- Define Scheme functions callable from Lisp
- Define record to CLOS object conversion
- Use define* style syntax
- Mix Scheme and Lisp expressions
- Readsyntax for `#t` and `#f`

# Examples

## IMPORTANT: `init` must be called before any Scheme code will run

Before you run off and try the examples yourself be aware that the library
needs to initialize `libguile` before any Scheme code can run. You can still
define Scheme code, as you will get a glimpse of in the following examples, but
it will not be evaluated before `init` is called.

```lisp
(asdf:load-system :guile)
(use-package :guile)

(scheme-toplevel
  (define (add x y)
    (+ x y)))

(scheme (add 1 1)) => Uninitialized Guile environment [Condition of type SIMPLE-ERROR]

(init) ; Initializes libguile and compiles Scheme code that has been set for delayed-evaluation

(scheme (+ 1 1)) => 2
(scheme (add 1 1)) => 2
```

## Embed Scheme code in Lisp functions:

```lisp
(scheme-toplevel
  (use-modules (web client)))

(defun current-ip ()
  (format nil "Current IP address: ~a~%"
          (scheme-expression
            (call-with-values
             (lambda ()
               (http-get "https://api.ipify.org"))
             (lambda (request ip)
               ip)))))

(current-ip) => "Current IP address: XXX.XXX.XXX.XXX"
```

`scheme-expression` will compile the expression, at compile time or library
initialization time, into a lambda. At runtime it will simply get the pointer
and then just call the lambda using scm-call-0. Should be a quite efficient way
to run self-contained scheme code from inside lisp.

## Define Scheme code scheduled to be evaluated when library initializes

There are two main ways to do this, we already saw one `scheme-expression` for
embedding a scheme-expression in lisp code. There is another, mainly intended
to be used for declaring procedures and values in the global scope:
`scheme-toplevel`.

```lisp
(scheme-toplevel
  (define user (make-record-type "user" '(username password)))
  (define make-user (record-constructor user))
  (define %very-secure-database
    (list (make-user "Bob" "Secret_dont_tell_anyone")
          (make-user "Bert" "OpenSesamy"))))

```

Internally both `scheme-expression` and `scheme-toplevel` use a macro:
`delay-evaluation` which is also exported by the package. It collects all
expressions passed to it as lambdas and calls them when `init` is called. If
called after the library is already initialized the code will run right away.

## Define Lisp functions that are callable from Scheme

```lisp
(define-scheme-procedure say-hello (name)
  (format nil "Hello ~a" name))

(scheme (say-hello "Bob")) => "Hello Bob"
```

The parameters will be translated from scheme objects `scm` to lisp-object
before they reach the function body.

## Define scheme functions callable from lisp

```lisp
(setf (symbol-function 'say-hello)
      (scheme (lambda (name)
                (string-append "Hello " name))))

(say-hello "Bob") => "Hello Bob"
```

Currently lacking a more intentful way of expressing this.

## Define record to CLOS conversion

```lisp
(scheme-toplevel
  (define burger (make-record-type "burger" '(cheese tomato patty)))
  (define make-burger (record-constructor burger)))

; Record details object is returned if no conversion target found
(scheme (make-burger #t 3 1)) => (BURGER ((:CHEESE . T) (:TOMATO . 3) (:PATTY . 1)))

(define-scheme-record burger ()
  ((cheese
    :initarg :cheese)
   (tomato
    :initarg :tomato)
   (patty
    :initarg :patty)))
  
; Will now lookup type and instantiate a class filling fields using the initargs
(scheme (make-burger #t 3 1)) => #<BURGER {...}>

(let ((burger *))
  (with-slots (cheese tomato patty) burger
    (list cheese tomato patty))) => (T 3 1)

; You can alternatively give the CLOS class a different name than the scheme record type
(define-scheme-record (scheme-burger burger) ()
  ((cheese
    :initarg :cheese)
   (tomato
    :initarg :tomato)
   (patty
    :initarg :patty)))

(scheme (make-burger #t 3 1)) => #<SCHEME-BURGER {...}>
```

## Use lambda* and define*

```lisp
(scheme
  (lambda* (:key name age city) ; #:keyword style keywords will be read as uninterned symbols and will not work here.
      (string-append name " is "
                     age " years old and lives in "
                     city "."))) => #<FUNCTION (LAMBDA (&REST ARGS) :IN SCM->LISP) {...}>

(funcall *
         :name "Bob"
         :age "35"
         :city "Amsterdam") => "Bob is 35 years old and lives in Amsterdam."
```

Note that `#:keyword` style syntax is read by the Lisp reader as an uninterned
symbol, they will not work as keywords here.

## Mix scheme and lisp expressions

```lisp
(defvar *user-name* "Bob")
(safe-eval `(say-name ,*user-name*)) => "Hello Bob"
```

`safe-eval` evaluates an expression in the Guile Scheme environment.

## Read syntax

`#t` and `#f` will usually be read by the Lisp reader as a read time macro
dispatch character. This means that if you want to use them in an expression
you will need to change the readtable to read them as symbols. This library
provides the function `enable-scheme-syntax` to do exactly that.

```lisp
#t => no dispatch function defined for #\t
(enable-scheme-syntax)
#t => |#T|
#f => |#F|
```

These weird-looking symbols are translated to the appropriate Scheme symbols
when sent for evaluation.

This function is quite naive at the moment and will hopefully be more fleshed
out soon.
