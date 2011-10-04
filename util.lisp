(in-package :dict)

(defmacro with-gensyms (symbols &body body)
  `(let ,(mapcar (lambda (s) `(,s (gensym))) symbols)
     ,@body))
