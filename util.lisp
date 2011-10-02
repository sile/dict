(in-package :dict)

(defmacro with-gensyms (symbols &body body)
  `(let ,(mapcar (lambda (s) `(,s (gensym))) symbols)
     ,@body))

(defmacro defconst (name value &optional documentation)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when documentation (list documentation))))
