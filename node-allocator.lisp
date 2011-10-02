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
  (values #() :type (simple-array t))
  (position 0 :type array-index))

(defun encode (hash next)
  (declare ((unsigned-byte 30) hash next))
  (logior (ash hash 30) next))
           
(defun make-allocator (initial-size)
  (declare (array-index initial-size))
  (macrolet ((array (type &optional initial-element)
               (if (null initial-element)
                   `(make-array initial-size :element-type ,type)
                 `(make-array initial-size :element-type ,type :initial-element ,initial-element))))
    (let ((o (make-node-allocator 
              :hashs  (array '(unsigned-byte 60) 0)
              :keys   (array t)
              :values (array t))))
      (make-node o :hash #.(1- (ash 1 30))
                   :next 0
                   :key t
                   :value t)
      o)))

(defun check-capacity (allocator)
  (with-slots (hashs position) (the node-allocator allocator)
    (< position (length hashs))))

(defun enlarge-allocator (allocator)
  (with-slots (hashs keys values position) (the node-allocator allocator)
    (let ((new-size (* 2 (length hashs))))
      (declare (array-index new-size))
      (flet ((array (base &optional initial-element)
               (if (null initial-element)
                   (adjust-array base new-size)
                 (adjust-array base new-size :initial-element initial-element))))
        (setf hashs (array hashs 0)
              keys (array keys)
              values (array values))))))
              
(defun make-node (allocator &key hash next key value)
  (with-slots (hashs keys values position) (the node-allocator allocator)
    (unless (check-capacity allocator)
      (enlarge-allocator allocator))
    (setf (aref hashs position) (encode hash next)
          (aref keys position) key
          (aref values position) value)
    (post-incf position)))

(defmacro node-hash (node-index allocator)
  `(ldb (byte 30 30) (aref (node-allocator-hashs ,allocator) ,node-index)))

(defmacro node-next (node-index allocator)
  `(ldb (byte 30 0) (aref (node-allocator-hashs ,allocator) ,node-index)))

(defmacro node-key (node-index allocator)
  `(aref (node-allocator-keys ,allocator) ,node-index))

(defmacro node-value (node-index allocator)
  `(aref (node-allocator-values ,allocator) ,node-index))

#+C
(defun delete-node (node-index allocator)
  (with-slots (position) (the node-allocator allocator)
    (setf (node-key node-index allocator) t
          (node-value node-index allocator) t
          
          ;; XXX; out-of-range
          (node-hash position allocator) (- node-index))))


