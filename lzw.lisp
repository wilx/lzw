(defstruct node
  num
  edges)

(defun null-node ()
  (make-node :num 0 :edges (make-array 256)))

(defun new-node (n)
  (make-node :num n
             :edges (make-hash-table)))
