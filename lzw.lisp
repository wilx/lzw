;; Tree.
(defclass node ()
  ((num :reader node-num :initarg :num)))

(defclass root-node (node)
  ((edges :initform (make-array 256))))

(defclass inner-node (node)
  ((edges :initform (make-hash-table))))


;; next-node
(defmethod next-node ((node node) letter)
  (error "pure virtual"))

(defmethod next-node ((node root-node) letter)
  (aref (slot-value node 'edges) letter))

(defmethod next-node ((node inner-node) letter)
  (gethash letter (slot-value node 'edges)))


;; insert-node
(defmethod insert-node ((node node) inserted-node letter)
  (error "pure virtual"))

(defmethod insert-node ((node root-node) inserted-node letter)
  (error "cannot insert new nodes into a root-node"))

(defmethod insert-node ((node inner-node) (inserted-node inner-node) letter)
  (let ((table (slot-value node 'edges)))
    (setf (gethash letter table) inserted-node)))


(defun new-root-node ()
  (make-instance 'root-node :num -1))

(defun new-node (n)
  (assert (>= n 0))
  (make-instance 'inner-node :num n))

(defun fresh-tree ()
  (let* ((root (new-root-node))
         (edges (slot-value root 'edges)))
    (loop for i from 0 to 255
          do (setf (aref edges i) (new-node i)))
    root))


;; LZW context.
;;(defclass lzw-enc-context ()
;;  ((tree :reader lzw-enc-ctx-tree :initarg :tree :initform (fresh-tree))
;;   (nodes :reader lzw-enc-ctx-nodes :initarg :nodes :initform 256)
;;   (inserted :reader lzw-enc-ctx-inserted :initarg :inserted :initform nil))

(defstruct lzw-enc-ctx
  tree
  (node-count 256 :type fixnum)
  ;inserted
  current
  ;last-char
)

(defun fresh-lzw-enc-ctx ()
  (let ((ctx (make-lzw-enc-ctx)))
    (setf (lzw-enc-ctx-tree ctx) (fresh-tree))
    ctx))


;; Tries to walk from node to a next node using a letter.
;; Returns list of (moved-to-node inserted-node output-code)
;; The two possible outputs are: 
;;   (moved-to-node nil nil): It was possible to move to a next node.
;;   (nil inserted-node output-code): It was not possible to move to a next 
;;     node.
(defun lzw-enc-letter (letter ctx)
  (let* ((node (or (lzw-enc-ctx-current ctx) (lzw-enc-ctx-tree ctx)))
         (next (next-node node letter)))
    (if (null next)
	(block true
	  (assert (not (eq node (lzw-enc-ctx-tree ctx))))
	  (let* ((output (node-num node)) ;; Output
		 (num (lzw-enc-ctx-node-count ctx)) ;; Number of the new node.
		 (root (lzw-enc-ctx-tree ctx)) ;; Root of the tree.
		 (new-current (next-node root letter))) ;; New current node.
	    ;; Insert new node.
	    (insert-node node (new-node num) letter)
	    ;; Increment node-count.
	    (incf (lzw-enc-ctx-node-count ctx))
	    ;; Reset current node.
	    (setf (lzw-enc-ctx-current ctx) new-current)
	    ;; Result is the output symbol
	    output))
	(block false
          ;; Change current node.
          (setf (lzw-enc-ctx-current ctx) next)
          ;; Return nil as we have nothing to output yet.
          nil))))

(defun test ()
  (let ((ss (make-string-input-stream "abaaacaabacdbaaaaaaa"))
        (ctx (fresh-lzw-enc-ctx)))
    (loop 
        (let ((ch (read-char ss)))
          (if (not ch) (return))
          (let* ((retval (lzw-enc-letter (- (char-code ch) (char-code #\a)) ctx)))
            (print retval)
            ;(new-line)
            t)))))
