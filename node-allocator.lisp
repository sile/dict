(in-package :dict)

(declaim (inline make-node-allocator
                 node-allocator-hashs
                 node-allocator-nexts
                 node-allocator-keys
                 node-allocator-values
                 node-allocator-position
                 check-capacity

                 make-node
                 make-allocator))
(declaim #.*fastest*)

(defstruct node-allocator 
  (hashs  #() :type (simple-array (unsigned-byte 60)))
  (keys   #() :type (simple-array t))
  (position 0 :type array-index))

(defun encode (hash next)
  (declare ((unsigned-byte 30) hash next))
  (logior (ash hash 30) next))
           
(defun make-allocator (initial-size)
  (declare (array-index initial-size))
  (macrolet ((array (type &optional size)
               (if (null size)
                   `(make-array initial-size :element-type ,type)
                 `(make-array ,size :element-type ,type))))
    (let ((o (make-node-allocator 
              :hashs  (array '(unsigned-byte 60))
              :keys   (array t (* initial-size 2)))))
      (make-node o :hash #.(1- (ash 1 30))
                   :next 0
                   :key t
                   :value t)
      o)))

(defun check-capacity (allocator)
  (with-slots (hashs position) (the node-allocator allocator)
    (< position (length hashs))))

(defun enlarge-allocator (allocator)
  (with-slots (hashs keys position) (the node-allocator allocator)
    (let ((new-size (* 2 (length hashs))))
      (declare (array-index new-size))
      (flet ((array (base &optional size)
               (if (null size)
                   (adjust-array base new-size)
                 (adjust-array base size))))
        (setf hashs (array hashs)
              keys (array keys (* 2 new-size)))))))
              
(defun make-node (allocator &key hash next key value)
  (with-slots (hashs keys position) (the node-allocator allocator)
    (unless (check-capacity allocator)
      (enlarge-allocator allocator))
    (setf (aref hashs position) (encode hash next)
          (aref keys (* 2 position)) key
          (aref keys (1+ (* 2 position))) value)
    (post-incf position)))

(defmacro node-hash (node-index allocator)
  `(ldb (byte 30 30) (aref (node-allocator-hashs ,allocator) ,node-index)))

(defmacro node-next (node-index allocator)
  `(ldb (byte 30 0) (aref (node-allocator-hashs ,allocator) ,node-index)))

(defmacro node-key (node-index allocator)
  `(aref (node-allocator-keys ,allocator) (* 2 ,node-index)))

(defmacro node-value (node-index allocator)
  `(aref (node-allocator-keys ,allocator) (1+ (* 2 ,node-index))))

#+C
(defun delete-node (node-index allocator)
  (with-slots (position) (the node-allocator allocator)
    (setf (node-key node-index allocator) t
          (node-value node-index allocator) t
          
          ;; XXX; out-of-range
          (node-hash position allocator) (- node-index))))


