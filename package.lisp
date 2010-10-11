(defpackage dict
  (:use :common-lisp)
  (:shadow :common-lisp get set remove)
  (:export make
           get
           remove
           each))
(in-package :dict)

(deftype positive-fixnum () `(integer 0 ,most-positive-fixnum))

(defvar *fastest* '(optimize (speed 3) (safety 0) (debug 0)))
(defvar *interface* '(optimize (speed 3) (safety 1) (debug 1)))
