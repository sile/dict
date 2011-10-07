(in-package :dict)

(declaim (inline make-functor functor-set functor-get functor-rem
                 find-test get-test))
(declaim #.*fastest*)

(defstruct functor
  (name t :type symbol :read-only t)
  (set  t :type set-fn :read-only t)
  (get  t :type get-fn :read-only t)
  (rem  t :type rem-fn :read-only t))

(defmethod print-object ((o functor) stream)
  (print-unreadable-object (o stream :identity t)
    (format (the stream stream) "~s ~s ~s"
            'test :name (functor-name o))))

(defmacro generate-test (hash test &key (name :anonymous))
  (with-gensyms (x y)
    `(make-functor :name ,name
                   :set (generate-set-fn (lambda (,x) (,hash ,x)) (lambda (,x ,y) (,test ,x ,y)))
                   :get (generate-get-fn (lambda (,x) (,hash ,x)) (lambda (,x ,y) (,test ,x ,y)))
                   :rem (generate-rem-fn (lambda (,x) (,hash ,x)) (lambda (,x ,y) (,test ,x ,y))))))

(defmacro define-test (name hash test)
  `(progn (setf (gethash ',name *functor-repository*)
                (generate-test ,hash ,test :name ',name))
          (values t)))

(defun undef-test (name)
  (remhash name *functor-repository*))

(defun find-test (name)
  (values (gethash name *functor-repository*)))

(defun list-all-tests (&aux list)
  (maphash (lambda (k v) 
             (declare (ignore v))
             (push k list))
           *functor-repository*)
  (sort list #'string<))

(defun get-test (x)
  (declare ((or symbol functor) x))
  (if (typep x 'functor)
      x
    (or (find-test x)
        (find-test 'eql))))
