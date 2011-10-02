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
  (hashs  #() :type (simple-array hashcode))
  (nexts  #() :type (simple-array fixnum))
  (keys   #() :type (simple-array t))
  (values #() :type (simple-array t))
  (position 0 :type array-index))

(defun make-allocator (initial-size)
  (declare (array-index initial-size))
  (macrolet ((array (type &optional initial-element)
               (if (null initial-element)
                   `(make-array initial-size :element-type ,type)
                 `(make-array initial-size :element-type ,type :initial-element ,initial-element))))
    (let ((o (make-node-allocator 
              :hashs  (array 'hashcode 0)
              :nexts  (array 'fixnum -1)
              :keys   (array t)
              :values (array t))))
      (make-node o :hash +MAX_HASHCODE+  ; sentinel node
                   :next 0
                   :key t
                   :value t)
      o)))

(defun check-capacity (allocator)
  (with-slots (hashs position) (the node-allocator allocator)
    (< position (length hashs))))

(defun enlarge-allocator (allocator)
  (with-slots (hashs nexts keys values position) (the node-allocator allocator)
    (let ((new-size (* 2 (length hashs))))
      (declare (array-index new-size))
      (flet ((array (base &optional initial-element)
               (if (null initial-element)
                   (adjust-array base new-size)
                 (adjust-array base new-size :initial-element initial-element))))
        (setf hashs (array hashs 0)
              nexts (array nexts -1)
              keys (array keys)
              values (array values))))))
              
(defun make-node (allocator &key hash next key value)
  (with-slots (hashs nexts keys values position) (the node-allocator allocator)
    (unless (check-capacity allocator)
      (enlarge-allocator allocator))
    (setf (aref hashs position) hash
          (aref nexts position) next
          (aref keys position) key
          (aref values position) value)
    (post-incf position)))

(declaim (inline node-swap))
(defun node-swap (node1 node2 alloc)
  (rotatef (node-hash node1 alloc) (node-hash node2 alloc))
  (rotatef (node-next node1 alloc) (node-next node2 alloc))
  (rotatef (node-key node1 alloc) (node-key node2 alloc))
  (rotatef (node-value node1 alloc) (node-value node2 alloc)))

(defmacro node-hash (node-index allocator)
  `(aref (node-allocator-hashs ,allocator) ,node-index))

(defmacro node-next (node-index allocator)
  `(aref (node-allocator-nexts ,allocator) ,node-index))

(defmacro node-key (node-index allocator)
  `(aref (node-allocator-keys ,allocator) ,node-index))

(defmacro node-value (node-index allocator)
  `(aref (node-allocator-values ,allocator) ,node-index))

(defun delete-node (node-index allocator)
  (setf (node-hash node-index allocator) 0
        (node-next node-index allocator) -1
        (node-key node-index allocator) t
        (node-value node-index allocator) t))
