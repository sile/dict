(in-package :asdf)

(defsystem dict
  :name "dict"
  :version "0.1.0"
  :author "Takeru Ohta"
  :description "A hash table"
  
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "functor")
               (:file "dict")
               (:file "predefine-test")))