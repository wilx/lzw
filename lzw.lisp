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
(defstruct lzw-enc-ctx
  ;; Root of LZW dictionary.
  tree
  ;; Node count in the tree.
  (node-count 256 :type fixnum)
  ;; Current position in the tree during encoding.
  current)

;; Returns fresh LZW context that is ready for use with lzw-enc-letter.
(defun fresh-lzw-enc-ctx ()
  (let ((ctx (make-lzw-enc-ctx)))
    (setf (lzw-enc-ctx-tree ctx) (fresh-tree))
    ctx))

;; Tries to walk from node to a next node using a letter.
;; Returns either NIL or integer 
;; The two possible outputs are: 
;;   (moved-to-node nil nil): It was possible to move to a next node.
;;   (nil inserted-node output-code): It was not possible to move to a next 
;;     node.
(defun lzw-enc-letter (letter ctx)
  (let* ((node (or (lzw-enc-ctx-current ctx) (lzw-enc-ctx-tree ctx)))
         (next (next-node node letter)))
    (if (null next)
	(progn
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
	(progn
          ;; Change current node.
          (setf (lzw-enc-ctx-current ctx) next)
          ;; Return nil as we have nothing to output yet.
          nil))))

;; Returns the last output character at the end of encoding.
(defun lzw-enc-finish (ctx)
  (assert (not (eq (lzw-enc-ctx-current ctx) (lzw-enc-ctx-tree ctx))))
  (node-num (lzw-enc-ctx-current ctx)))

;; Queue
(defstruct queue 
  front
  back)

(defun queue-put (q x)
  (assert (queue-p q))
  (if (null (queue-front q))
      (setf (queue-front q) (setf (queue-back q) (cons x nil)))
      (setf (cdr (queue-back q)) (cons x nil)
            (queue-back q) (cdr (queue-back q)))))

(defun queue-empty? (q)
  (if (null (queue-front q))
      t
      nil))

(defun queue-get (q)
  (assert (queue-p q))
  (if (queue-empty? q)
      (error "Queue is empty."))
  (let ((retval (pop (queue-front q))))
    (if (null (queue-front q))
        (setf (queue-back q) nil))
    retval))

;; A simple test.
(defvar *input-string* "abaaacaabacdbaaaaaaa")
(defvar *encoded-seq* nil)
(defun test-enc ()
  (setf *encoded-seq* nil)
  (let ((ss (make-string-input-stream *input-string*))
        (ctx (fresh-lzw-enc-ctx)))
    ;; Loop through all of the letters.
    (loop for ch = (read-char ss nil nil)
          while ch
          do (let ((retval (lzw-enc-letter (char-code ch) ctx)))
               (print retval)
               (setf *encoded-seq* (cons retval *encoded-seq*))))
    ;; Finish encoding by outputing the last encoded number.
    (let ((retval (lzw-enc-finish ctx)))
      (assert (not (null retval)))
      (print retval)
      (setf *encoded-seq* (cons retval *encoded-seq*)))))

;; Decoding.

(defstruct dec-table-entry
  ;; Points to dec-table-entry that makes prefix of this element.
  prefix
  ;; String that this table element adds to its prefix.
  rest)

(defstruct lzw-dec-ctx
  ;; Table mapping encoded numbers to strings that they represent.
  (table (make-hash-table))
  ;; Points to dec-table-entry that was last inserted.
  last-inserted
  ;; Points to dec-table-entry that was last decoded.
  last-decoded
  ;; Number of nodes in the table.
  node-count)

(defun fresh-lzw-dec-ctx ()
  (let ((ctx (make-lzw-dec-ctx :node-count 256)))
    (loop for i from 0 to 255
          do (setf (gethash i (lzw-dec-ctx-table ctx))
                   (make-dec-table-entry :prefix nil :rest i)))
    ctx))

