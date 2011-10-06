(in-package :dict)

(declaim (inline make-node-allocator
                 node-allocator-hashs
                 node-allocator-entries
                 node-allocator-position
                 check-capacity
                 delete-node
                 make-node
                 make-allocator))
(declaim #.*fastest*)

(defstruct node-allocator 
  (hashs #() :type (simple-array hashcode))
  (nexts #() :type (simple-array array-index))
  (entries        #() :type simple-vector)
  (position         0 :type array-index))

(defun make-allocator (initial-size)
  (declare (array-index initial-size))
  (let ((o (make-node-allocator 
            :hashs 
            (make-array initial-size :element-type 'hashcode)
            :nexts
            (make-array initial-size :element-type 'array-index :initial-element 0)              
            :entries
            (make-array (* 2 initial-size)))))
    (make-node o :hash +MAX_HASHCODE+
                 :next 0)
    o))

(defun check-capacity (allocator)
  (with-slots (hashs position) (the node-allocator allocator)
    (< position (1- (length hashs)))))

(defun enlarge-allocator (allocator)
  (with-slots (hashs nexts entries position) (the node-allocator allocator)
    (let* ((new-size (* 2 (length hashs)))
           (new-hashs
            (make-array new-size :element-type 'hashcode))
           (new-nexts
            (make-array new-size :element-type 'array-index))
           (new-entries (make-array (* 2 new-size))))
      (declare (array-index new-size))
      (flet ((copy (old-ary new-ary)
               (dotimes (i (length old-ary) new-ary)
                 (setf (aref new-ary i) (aref old-ary i)))))
            (declare (inline copy))
        (setf hashs (copy hashs new-hashs)
              nexts (copy nexts new-nexts)
              entries (copy entries new-entries))))))
              
(defmacro node-hash (node-index allocator)
  `(aref (node-allocator-hashs ,allocator) ,node-index))

(defmacro node-next (node-index allocator)
  `(ldb (byte #.(1- +HASHCODE_WIDTH+) 0) (aref (node-allocator-nexts ,allocator) ,node-index)))

(defmacro node-key (node-index allocator)
  `(aref (node-allocator-entries ,allocator) (* 2 ,node-index)))

(defmacro node-value (node-index allocator)
  `(aref (node-allocator-entries ,allocator) (1+ (* 2 ,node-index))))

(defmacro node-flag (node-index allocator)
  `(ldb (byte 1 #.(1- +HASHCODE_WIDTH+))
        (aref (node-allocator-nexts ,allocator) ,node-index)))

(defun make-node (allocator &key hash next key value)
  (declare (hashcode hash)
           (array-index next))

  (with-slots (position) (the node-allocator allocator)
    (let ((pos 
           (if (zerop (node-flag position allocator))
               (prog1 position
                 (incf position)
                 (unless (check-capacity allocator)
                   (enlarge-allocator allocator)))
             (let ((reuse-pos (node-next position allocator)))
               (setf (node-flag position allocator) (node-flag reuse-pos allocator)
                     (node-next position allocator) (node-next reuse-pos allocator)
                     
                     (node-flag reuse-pos allocator) 0)
               reuse-pos))))
      (setf (node-hash pos allocator) hash
            (node-next pos allocator) next
            (node-key pos allocator) key
            (node-value pos allocator) value)
      pos)))

(defun delete-node (node-index allocator)
  (declare (array-index node-index))
  (with-slots (position) (the node-allocator allocator)
    (setf (node-key node-index allocator) t
          (node-value node-index allocator) t
          
          (node-next node-index allocator) (node-next position allocator)
          (node-flag node-index allocator) (node-flag position allocator)
          
          (node-next position allocator) node-index
          (node-flag position allocator) 1)))

(defun clear-nodes (allocator)
  (with-slots (hashs nexts entries position) (the node-allocator allocator)
    (fill nexts 0 :start 1)
    (fill entries t :start 2)
    (setf position 1)))
