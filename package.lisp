(defpackage dict
  (:use :common-lisp)
  (:shadow :common-lisp get set remove count map)
  (:export dict
           test
           
           make
           count
           get
           remove
           clear
           each
           map

           generate-test
           define-test
           undef-test
           find-test
           list-all-tests
           test-name))
(in-package :dict)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *fastest* '(optimize (speed 3) (safety 0) (debug 0)))
  (defparameter *interface* '(optimize (speed 3) (safety 2) (debug 1)))
  (defparameter *normal* '(optimize (speed 1) (safety 3) (debug 2)))
  (defparameter *muffle-note* #-SBCL '(ignore)
                              #+SBCL '(sb-ext:muffle-conditions sb-ext:compiler-note))
  (defconstant +HASHCODE_WIDTH+ #+X86-64 30 #-X86-64 29))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +MAX_HASHCODE+ #.(1- (ash 1 +HASHCODE_WIDTH+))))

(deftype positive-fixnum () '(integer 0 #.most-positive-fixnum))
(deftype array-index () '(unsigned-byte #.+HASHCODE_WIDTH+))
(deftype hashcode () '(unsigned-byte #.+HASHCODE_WIDTH+))
(deftype hashcode-width () '(mod #.(1+ +HASHCODE_WIDTH+)))
(deftype set-fn () '(function (t t dict) t))
(deftype get-fn () '(function (t dict t) (values t boolean)))
(deftype rem-fn () '(function (t dict) boolean))

(defparameter *functor-repository* (make-hash-table :test #'eq))
