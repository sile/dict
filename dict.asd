(in-package :asdf)

(defsystem dict
  :name "dict"
  :version "0.2.1"
  :author "Takeru Ohta"
  :description "A hash table"
  
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "functor")
               #+X86-64
               (:file "node-allocator-64bit")
               #+X86-32
               (:file "node-allocator-32bit")
               (:file "dict")
               (:file "predefine-test")))