(in-package :dict)

(declaim (inline make-functor functor-set functor-get functor-rem
                 find-test get-test))
(declaim #.*fastest*)

(defstruct functor
  (name t :type symbol)
  (set  t :type set-fn)
  (get  t :type get-fn)
  (rem  t :type rem-fn))

(defmacro generate-test (hash test &key (name :anonymous))
  `(make-functor :name ,name
                 :set (generate-set-fn ,hash ,test)
                 :get (generate-get-fn ,hash ,test)
                 :rem (generate-rem-fn ,hash ,test)))

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
