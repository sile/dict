(in-package :dict)

(defstruct hashtrie
  (count               0 :type positive-fixnum)
  (next-resize-trigger 0 :type positive-fixnum)
  (root-depth          0 :type positive-fixnum)
  (root              #() :type simple-vector))

(declaim (inline hash empty-table index nth-index next make make-hashtrie
                 list-insert relocate-entries))

(defun hash (x)
  (sxhash x))

(defun empty-table ()
  (make-array 16 :initial-element '()))

(defun index (hash-code)
  (ldb (byte 4 0) hash-code))

(defun nth-index (nth hash-code)
  (ldb (byte 4 (* nth 4)) hash-code))

(defun next (hash-code)
  (ash hash-code -4))

(defun make ()
  (make-hashtrie :next-resize-trigger (* 16 4)
                 :root (empty-table)))

(defun candidates (hash table depth)
  (declare #.*fastest* 
           (positive-fixnum hash depth)
           (simple-vector table))
  (if (zerop depth)
      #1=(aref table (index hash))
    (candidates (next hash) #1# (1- depth))))

(defun get (key hashtrie)
  (declare #.*interface*
           (hashtrie hashtrie))
  (with-slots (root root-depth) hashtrie
    (let ((rlt (assoc key (candidates (hash key) root root-depth) :test #'equal)))
      (if rlt
          (values (cdr rlt) t)
        (values nil nil)))))

(defun relocate-entries (entries n)
  (let ((new-table (empty-table)))
    (loop FOR e IN entries
          FOR (key . value) = e
      DO
      (push e (aref new-table (nth-index n (hash key)))))
    new-table))

(defun resize-impl (table depth i n)
  (declare #.*fastest*
           (simple-vector table)
           (positive-fixnum depth i))
  (if (zerop depth)
      (setf #1=(aref table i) (relocate-entries #1# n))
    (loop FOR j FROM 0 BELOW 16
      DO
      (resize-impl #1# (1- depth) j n))))

(defun resize (hashtrie)
  (declare #.*fastest*
           (hashtrie hashtrie))
  (with-slots (root root-depth next-resize-trigger) hashtrie
    (loop FOR i FROM 0 BELOW 16
      DO
      (resize-impl root root-depth i (1+ root-depth)))
    (incf root-depth)
    (setf next-resize-trigger (* next-resize-trigger 16))))

(defun list-insert (key value list)
  (loop FOR x IN list
        WHEN (equal key (car x))
    DO
      (setf (cdr x) value)
      (return (values list nil))
    FINALLY
      (return (values `(,(cons key value) . ,list) t))))

(defun set-impl (key value hash table depth hashtrie)
  (declare #.*fastest*
           (hashtrie hashtrie)
           (simple-vector table)
           (positive-fixnum depth))
  (if (zerop depth)
      (multiple-value-bind (new-list added?) (list-insert key value #1=(aref table (index hash)))
        (setf #1# new-list)
        (when added?
          (incf (hashtrie-count hashtrie)))
        hashtrie)
    (set-impl key value (next hash) #1# (1- depth) hashtrie)))

(defun set (key value hashtrie)
  (declare #.*interface*
           (hashtrie hashtrie))
  (with-slots (root root-depth count next-resize-trigger) hashtrie
    (when (= count next-resize-trigger)
      (resize hashtrie))
    (set-impl key value (hash key) root root-depth hashtrie)))

(defsetf get (key hashtrie) (new-value)
  `(progn (set ,key ,new-value ,hashtrie)
          ,new-value))