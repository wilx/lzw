(in-package #:cl-user)

;; Tree.
(defclass node ()
  ((num :reader node-num :initarg :num)))

(defclass root-node (node)
  ((edges :initform (make-array 256))))

(defclass inner-node (node)
  ((edges :initform (make-hash-table))))


;; next-node
(defgeneric next-node (node letter))
(defmethod next-node ((node node) letter)
  (error "pure virtual"))

(defmethod next-node ((node root-node) letter)
  (aref (slot-value node 'edges) letter))

(defmethod next-node ((node inner-node) letter)
  (gethash letter (slot-value node 'edges)))


;; insert-node
(defgeneric insert-node (node inserted-node letter))
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
(defun lzw-enc-letter (letter ctx)
  (check-type letter (unsigned-byte 8))
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


;; Returns the final code, or NIL when no input has been encoded.
(defun lzw-enc-finish (ctx)
  (let ((current (lzw-enc-ctx-current ctx)))
    (when current
      (node-num current))))


;; Input test string.
(defvar *input-string* "abaaacaabacdbaaaaaaa")
;; List of LZW indicies.
(defvar *encoded-seq* nil)
;; Simple encoding test.
(defun test-enc ()
  (setf *encoded-seq* nil)
  (let ((ss (make-string-input-stream *input-string*))
        (ctx (fresh-lzw-enc-ctx)))
    ;; Loop through all of the letters.
    (loop for ch = (read-char ss nil nil)
          while ch
          do (let ((retval (lzw-enc-letter (char-code ch) ctx)))
               (setf *encoded-seq* (cons retval *encoded-seq*))))
    ;; Finish encoding by outputing the last encoded number.
    (let ((retval (lzw-enc-finish ctx)))
      (setf *encoded-seq* 
            (remove-if (lambda (x) (null x))
                       (reverse (cons retval *encoded-seq*)))))))


;; Queue

(defstruct queue 
  front
  back)

;; Puts x into queue q.
(defun queue-put (q x)
  (assert (queue-p q))
  (if (null (queue-front q))
      (setf (queue-front q) (setf (queue-back q) (cons x nil)))
      (setf (cdr (queue-back q)) (cons x nil)
            (queue-back q) (cdr (queue-back q)))))

;; Predicate.
(defun queue-empty? (q)
  (if (null (queue-front q))
      t
      nil))

;; Returns first element of queue.
(defun queue-get (q)
  (assert (queue-p q))
  (if (queue-empty? q)
      (error "Queue is empty."))
  (let ((retval (pop (queue-front q))))
    (if (null (queue-front q))
        (setf (queue-back q) nil))
    retval))


;; Decoding.

(defstruct dec-table-entry
  ;; Points to dec-table-entry that makes prefix of this element.
  prefix
  ;; List of chars that this table element adds to its prefix.
  rest)

(defstruct lzw-dec-ctx
  ;; Table mapping encoded numbers to strings that they represent.
  (table (make-array 256 :initial-element nil :adjustable t 
                     :fill-pointer t))
  ;; Points to dec-table-entry that will be inserted once its
  ;; rest field is known.
  new-entry
  ;; Points to dec-table-entry that was last decoded.
  last-decoded
  ;; Number of nodes in the table.
  node-count)

(defun fresh-lzw-dec-ctx ()
  (let ((ctx (make-lzw-dec-ctx :node-count 256)))
    (loop for i from 0 to 255
          do (setf (aref (lzw-dec-ctx-table ctx) i)
                   (make-dec-table-entry :prefix nil :rest i)))
    ctx))

;; Returns the first letter of dec-table-entry by traversing all
;; entries chaned by prefix field up to the first one.
(defun first-letter (entry)
  ;(assert (dec-table-entry-p entry))
  (if (null entry) 
      nil
      (let ((prefix (dec-table-entry-prefix entry))
            (rest (dec-table-entry-rest entry)))
        (if (not (null prefix))
            (first-letter prefix)
            rest))))
  
;; Traverses entry and all of its prefixes, collects them and returns
;; them in a list.
(defun splice-output (entry)
  (labels ((splice-into-queue (q entry)
             (if (not (null entry))
                 (progn
                   (splice-into-queue q (dec-table-entry-prefix entry))
                   (queue-put q (dec-table-entry-rest entry))))))
    (let ((q (make-queue)))
      (splice-into-queue q entry)
      (queue-front q))))

;; Testing string:
;; a b a a a c a a b a c d b a a a a a a a
;; (97 98 97 258 99 258 257 99 100 257 258 266 97)

(defun lzw-dec-code (ctx code)
  (check-type code (integer 0 *))
  (assert (= (lzw-dec-ctx-node-count ctx) (length (lzw-dec-ctx-table ctx))))
  (let* ((table (lzw-dec-ctx-table ctx))
         (count (lzw-dec-ctx-node-count ctx))
         (pending (lzw-dec-ctx-new-entry ctx)))
    ;; An unknown code is valid only at the next unused index, after a
    ;; previous phrase. Reject invalid codes before changing the context.
    (unless (or (< code count)
                (and (= code count) pending))
      (error "Invalid LZW code ~S; next dictionary code is ~D." code count))
    (let* ((entry (if (< code count) (aref table code) pending))
           (new-entry (make-dec-table-entry :prefix entry)))
      ;; In the KwKwK case ENTRY is PENDING: previous phrase + its first
      ;; letter. Complete and insert exactly one entry in either case.
      (when pending
        (setf (dec-table-entry-rest pending) (first-letter entry))
        (vector-push-extend pending table)
        (incf (lzw-dec-ctx-node-count ctx)))
      (setf (lzw-dec-ctx-last-decoded ctx) entry
            (lzw-dec-ctx-new-entry ctx) new-entry)
      (splice-output entry))))

(defvar *decoded-seq* nil)
(defvar *decoded-string* nil)
(defun test-dec ()
  (let* ((ctx (fresh-lzw-dec-ctx)))
    (setf *decoded-seq* 
          (mapcar (lambda (code) 
                    ;;(print (lzw-dec-code ctx code)))
                    (mapcar (lambda (c) (code-char c))
                            (lzw-dec-code ctx code)))
                  *encoded-seq*))
    (setf *decoded-string* 
          (reduce (lambda (a b) (concatenate 'string a b))
                  (mapcar (lambda (l) (coerce l 'string)) *decoded-seq*)
                  :initial-value ""))))
