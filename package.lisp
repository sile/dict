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

(deftype positive-fixnum () '(integer 0 #.most-positive-fixnum))
(deftype array-index () 'positive-fixnum)
(deftype hashcode () 'positive-fixnum)
(deftype hashcode-width () '(integer 1 #.(integer-length most-positive-fixnum)))
(deftype set-fn () '(function (t t dict) t))
(deftype get-fn () '(function (t dict t) (values t boolean)))
(deftype rem-fn () '(function (t dict) boolean))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *fastest* '(optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))
  (defparameter *interface* '(optimize (speed 3) (safety 2) (debug 1)))
  (defparameter *normal* '(optimize (speed 1) (safety 3) (debug 2)))
  (defparameter *muffle-note* #-SBCL '(ignore)
                              #+SBCL '(sb-ext:muffle-conditions sb-ext:compiler-note))
  (defconstant +HASHCODE_WIDTH+ #.(integer-length most-positive-fixnum))
  (defconstant +MAX_HASHCODE+ most-positive-fixnum))

(defparameter *functor-repository* (make-hash-table :test #'eq))
