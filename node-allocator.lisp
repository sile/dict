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

(defun make-allocator (initial-size)
  (declare (array-index initial-size))
  (macrolet ((array (type &optional size)
               (if (null size)
                   `(make-array initial-size :element-type ,type :initial-element 0)
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
    (< position (1- (length hashs)))))

(defun enlarge-allocator (allocator)
  (with-slots (hashs keys position) (the node-allocator allocator)
    (let* ((new-size (* 2 (length hashs)))
           (new-hashs (make-array new-size :initial-element 0 :element-type '(unsigned-byte 60)))
           (new-keys (make-array (* 2 new-size))))
      (declare (array-index new-size))
      (dotimes (i (length hashs))
        (setf (aref new-hashs i) (aref hashs i)))
      (dotimes (i (length keys))
        (setf (aref new-keys i) (aref keys i)))

      (setf hashs new-hashs
            keys new-keys))))

#+C
(defun enlarge-allocator (allocator)
  (with-slots (hashs keys position) (the node-allocator allocator)
    (let ((new-size (* 2 (length hashs))))
      (declare (array-index new-size))
      (flet ((array (base &optional size)
               (if (null size)
                   (adjust-array base new-size :initial-element 0)
                 (adjust-array base size))))
        (setf hashs (array hashs)
              keys (array keys (* 2 new-size)))))))
              
(defmacro node-hash (node-index allocator)
  `(ldb (byte 30 30) (aref (node-allocator-hashs ,allocator) ,node-index)))

(defmacro node-next (node-index allocator)
  `(ldb (byte 29 1) (aref (node-allocator-hashs ,allocator) ,node-index)))

(defmacro node-key (node-index allocator)
  `(aref (node-allocator-keys ,allocator) (* 2 ,node-index)))

(defmacro node-value (node-index allocator)
  `(aref (node-allocator-keys ,allocator) (1+ (* 2 ,node-index))))

(defmacro node-flag (node-index allocator)
  `(ldb (byte 1 0) (aref (node-allocator-hashs ,allocator) ,node-index)))

(defun make-node (allocator &key hash next key value)
  (with-slots (hashs keys position) (the node-allocator allocator)
    (unless (check-capacity allocator)
      (enlarge-allocator allocator))
    (if (zerop (node-flag position allocator))
        (progn
          (setf (node-hash position allocator) hash
                (node-next position allocator) next
                (node-flag position allocator) 0
                (node-key position allocator) key
                (node-value position allocator) value)
          (post-incf position))
      (let ((pos (node-next position allocator)))
        (setf (node-flag position allocator) (node-flag pos allocator)
              (node-next position allocator) (node-flag pos allocator))
        
        (setf (node-hash pos allocator) hash
              (node-next pos allocator) next
              (node-flag pos allocator) 0
              (node-key pos allocator) key
              (node-value pos allocator) value)
        pos))))

;; TODO: test
(defun delete-node (node-index allocator)
  (with-slots (position) (the node-allocator allocator)
    (setf (node-key node-index allocator) t
          (node-value node-index allocator) t
          
          (node-next node-index allocator) (node-next position allocator)
          (node-flag node-index allocator) (node-flag position allocator)
          
          (node-next position allocator) node-index
          (node-flag position allocator) 1)))

(defun clear-nodes (allocator)
  (with-slots (hashs keys position) (the node-allocator allocator)
    (fill hashs 0 :start 1)
    (fill keys t :start 2)
    (setf position 1)))
