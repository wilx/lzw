(in-package #:lzw/tests)

(def-suite lzw-suite :description "Byte-oriented, unbounded LZW code streams")
(in-suite lzw-suite)

(defun encode-bytes (bytes)
  (let ((ctx (fresh-lzw-enc-ctx)))
    (append (loop for byte in bytes
                  for code = (lzw-enc-letter byte ctx)
                  when code collect code)
            (let ((code (lzw-enc-finish ctx)))
              (when code (list code))))))

(defun decode-codes (codes)
  (let ((ctx (fresh-lzw-dec-ctx)))
    (loop for code in codes append (lzw-dec-code ctx code))))

(defun string-bytes (string)
  (map 'list #'char-code string))

(defun reference-encode (bytes)
  "Independent LZW oracle using whole phrases, rather than the production trie."
  (let ((dictionary (make-hash-table :test #'equal))
        (next-code 256)
        (phrase nil)
        (codes nil))
    (dotimes (byte 256)
      (setf (gethash (list byte) dictionary) byte))
    (dolist (byte bytes)
      (let ((candidate (append phrase (list byte))))
        (multiple-value-bind (code present-p) (gethash candidate dictionary)
          (declare (ignore code))
          (if present-p
              (setf phrase candidate)
              (progn
                (push (gethash phrase dictionary) codes)
                (setf (gethash candidate dictionary) next-code
                      phrase (list byte))
                (incf next-code))))))
    (when phrase
      (push (gethash phrase dictionary) codes))
    (nreverse codes)))

(defun check-input (bytes)
  ;; Compare encoding with an independent oracle, then decode oracle codes.
  ;; Merely round-tripping could hide corresponding encoder/decoder errors.
  (let ((expected-codes (reference-encode bytes)))
    (is (equal expected-codes (encode-bytes bytes)))
    (is (equal bytes (decode-codes expected-codes)))))

(defun generated-bytes (length alphabet-size seed)
  "Reproducible corpus independent of the implementation's RANDOM algorithm."
  (loop repeat length
        do (setf seed (mod (+ (* seed 1664525) 1013904223) (expt 2 32)))
        collect (mod (ldb (byte 8 16) seed) alphabet-size)))

(test empty-input
  (is (null (encode-bytes nil)))
  (is (null (decode-codes nil))))

(test every-single-byte
  (dotimes (byte 256)
    (is (equal (list byte) (encode-bytes (list byte))))
    (is (equal (list byte) (decode-codes (list byte))))))

(test zero-is-a-code
  (is (equal '(0 256 257) (encode-bytes '(0 0 0 0 0 0))))
  (is (equal '(0 0 0 0 0 0) (decode-codes '(0 256 257)))))

(test published-example
  (let ((bytes (string-bytes "TOBEORNOTTOBEORTOBEORNOT"))
        (codes '(84 79 66 69 79 82 78 79 84 256 258 260 265 259 261 263)))
    (is (equal codes (reference-encode bytes)))
    (is (equal codes (encode-bytes bytes)))
    (is (equal bytes (decode-codes codes)))))

(test original-example
  (let ((bytes (string-bytes "abaaacaabacdbaaaaaaa"))
        (codes '(97 98 97 258 99 258 257 99 100 257 258 266 97)))
    (is (equal codes (encode-bytes bytes)))
    (is (equal bytes (decode-codes codes)))))

(test incremental-encoding
  (let ((ctx (fresh-lzw-enc-ctx)))
    (is (null (lzw-enc-letter 65 ctx)))
    (is (= 65 (lzw-enc-letter 66 ctx)))
    (is (= 66 (lzw-enc-letter 65 ctx)))
    (is (null (lzw-enc-letter 66 ctx)))
    (is (= 256 (lzw-enc-finish ctx)))
    (is (= 258 (lzw-enc-ctx-node-count ctx)))))

(test consecutive-new-codes
  ;; The next unused code denotes previous-phrase + its first byte (KwKwK).
  ;; Each code after the first must add exactly one dictionary entry.
  (let ((ctx (fresh-lzw-dec-ctx)))
    (is (equal '(65) (lzw-dec-code ctx 65)))
    (is (= 256 (lzw-dec-ctx-node-count ctx)))
    (is (equal '(65 65) (lzw-dec-code ctx 256)))
    (is (= 257 (lzw-dec-ctx-node-count ctx)))
    (is (equal '(65 65 65) (lzw-dec-code ctx 257)))
    (is (= 258 (lzw-dec-ctx-node-count ctx)))
    (is (equal '(65 65 65)
               (splice-output (aref (lzw-dec-ctx-table ctx) 257))))))

(test existing-code-after-new-code
  (is (equal '(65 65 65 66 65 65 66) (decode-codes '(65 256 66 257)))))

(test decoder-dictionary-growth
  (let* ((bytes (generated-bytes 8192 256 42))
         (codes (reference-encode bytes))
         (ctx (fresh-lzw-dec-ctx)))
    (loop for code in codes
          for consumed from 1
          do (lzw-dec-code ctx code)
             (is (= (+ 255 consumed) (lzw-dec-ctx-node-count ctx)))
             (is (= (lzw-dec-ctx-node-count ctx)
                    (length (lzw-dec-ctx-table ctx)))))
    (is (> (lzw-dec-ctx-node-count ctx) 4096))))

(test invalid-first-code
  (dolist (code '(256 257 999))
    (let ((ctx (fresh-lzw-dec-ctx)))
      (signals error (lzw-dec-code ctx code))
      (is (= 256 (lzw-dec-ctx-node-count ctx)))
      (is (null (lzw-dec-ctx-new-entry ctx)))
      (is (null (lzw-dec-ctx-last-decoded ctx)))
      (is (equal '(65) (lzw-dec-code ctx 65))))))

(test invalid-code-gap
  (let ((ctx (fresh-lzw-dec-ctx)))
    (lzw-dec-code ctx 65)
    (let ((pending (lzw-dec-ctx-new-entry ctx))
          (previous (lzw-dec-ctx-last-decoded ctx)))
      (dolist (code '(257 999))
        (signals error (lzw-dec-code ctx code))
        (is (= 256 (lzw-dec-ctx-node-count ctx)))
        (is (= 256 (length (lzw-dec-ctx-table ctx))))
        (is (eq pending (lzw-dec-ctx-new-entry ctx)))
        (is (eq previous (lzw-dec-ctx-last-decoded ctx)))
        (is (null (dec-table-entry-rest pending)))))
    (is (equal '(65 65) (lzw-dec-code ctx 256)))))

(test invalid-code-types
  (dolist (code '(-1 1/2 65.0 999.0 #\A "65" nil))
    (let ((ctx (fresh-lzw-dec-ctx)))
      (signals type-error (lzw-dec-code ctx code))
      (is (= 256 (lzw-dec-ctx-node-count ctx)))
      (is (equal '(0) (lzw-dec-code ctx 0))))))

(test invalid-input-bytes
  (dolist (byte '(-1 256 1/2 65.0 #\A "65" nil))
    (let ((ctx (fresh-lzw-enc-ctx)))
      (lzw-enc-letter 65 ctx)
      (let ((current (lzw-enc-ctx-current ctx)))
        (signals type-error (lzw-enc-letter byte ctx))
        (is (eq current (lzw-enc-ctx-current ctx)))
        (is (= 256 (lzw-enc-ctx-node-count ctx)))
        (is (= 65 (lzw-enc-finish ctx)))))))

(test fresh-trees
  (let ((first (fresh-tree)) (second (fresh-tree)))
    (dotimes (byte 256)
      (is (= byte (node-num (next-node first byte))))
      (is (null (next-node (next-node first byte) 0)))
      (is (not (eq (next-node first byte) (next-node second byte)))))
    (let ((entry (new-node 256)))
      (insert-node (next-node first 0) entry 255)
      (is (eq entry (next-node (next-node first 0) 255)))
      (is (null (next-node (next-node second 0) 255))))))

(test fresh-decoder
  (let ((ctx (fresh-lzw-dec-ctx)))
    (is (= 256 (lzw-dec-ctx-node-count ctx)))
    (is (= 256 (length (lzw-dec-ctx-table ctx))))
    (is (null (lzw-dec-ctx-new-entry ctx)))
    (is (null (lzw-dec-ctx-last-decoded ctx)))
    (dotimes (byte 256)
      (let ((entry (aref (lzw-dec-ctx-table ctx) byte)))
        (is (null (dec-table-entry-prefix entry)))
        (is (= byte (dec-table-entry-rest entry)))))))

(test interleaved-contexts
  (let ((first-encoder (fresh-lzw-enc-ctx))
        (second-encoder (fresh-lzw-enc-ctx))
        (first-input (string-bytes "AAAAAABAAAAAAB"))
        (second-input (string-bytes "TOBEORNOTTOBEORTOBEORNOT"))
        (first-codes nil) (second-codes nil))
    (loop for index below (max (length first-input) (length second-input))
          do (when (< index (length first-input))
               (let ((code (lzw-enc-letter (nth index first-input) first-encoder)))
                 (when code (push code first-codes))))
             (when (< index (length second-input))
               (let ((code (lzw-enc-letter (nth index second-input) second-encoder)))
                 (when code (push code second-codes)))))
    (push (lzw-enc-finish first-encoder) first-codes)
    (push (lzw-enc-finish second-encoder) second-codes)
    (setf first-codes (nreverse first-codes)
          second-codes (nreverse second-codes))
    (is (equal (reference-encode first-input) first-codes))
    (is (equal (reference-encode second-input) second-codes))
    (let ((first-decoder (fresh-lzw-dec-ctx))
          (second-decoder (fresh-lzw-dec-ctx))
          (first-output nil) (second-output nil))
      (loop for index below (max (length first-codes) (length second-codes))
            do (when (< index (length first-codes))
                 (setf first-output
                       (nconc first-output
                              (lzw-dec-code first-decoder (nth index first-codes)))))
               (when (< index (length second-codes))
                 (setf second-output
                       (nconc second-output
                              (lzw-dec-code second-decoder (nth index second-codes))))))
      (is (equal first-input first-output))
      (is (equal second-input second-output)))))

(test queue-fifo-and-reuse
  (let ((queue (make-queue)))
    (is (queue-empty? queue))
    (signals error (queue-get queue))
    (dolist (value '(nil 0 255)) (queue-put queue value))
    (is (not (queue-empty? queue)))
    (is (null (queue-get queue)))
    (is (= 0 (queue-get queue)))
    (is (= 255 (queue-get queue)))
    (is (queue-empty? queue))
    (is (null (queue-front queue)))
    (is (null (queue-back queue)))
    (queue-put queue 42)
    (is (= 42 (queue-get queue)))
    (signals error (queue-get queue))))

(test prefix-output
  (let* ((first (make-dec-table-entry :rest 0))
         (second (make-dec-table-entry :prefix first :rest 255))
         (third (make-dec-table-entry :prefix second :rest 0)))
    (is (= 0 (first-letter third)))
    (is (equal '(0 255 0) (splice-output third)))
    (is (null (first-letter nil)))
    (is (null (splice-output nil)))))

(test exhaustive-small-inputs
  (labels ((visit (prefix remaining)
             (check-input prefix)
             (when (plusp remaining)
               (dolist (byte '(0 1 255))
                 (visit (cons byte prefix) (1- remaining))))))
    (visit nil 6)))

(test reproducible-generated-inputs
  (dolist (seed '(1 42 123456789 4294967295))
    (dolist (alphabet-size '(1 2 3 16 256))
      (dolist (length '(0 1 2 3 6 17 128 1024))
        (check-input (generated-bytes length alphabet-size seed))))))

(test larger-inputs
  (check-input (loop for byte below 256 collect byte))
  (check-input (loop repeat 16 append (loop for byte below 256 collect byte)))
  (check-input (make-list 32768 :initial-element 65))
  (check-input (generated-bytes 32768 256 42)))

(test demonstration-helpers
  (dolist (input '("" "A" "AAAAAA" "abaaacaabacdbaaaaaaa"))
    (let ((*input-string* input)
          (*encoded-seq* nil) (*decoded-seq* nil) (*decoded-string* nil))
      (test-enc)
      (is (equal (reference-encode (string-bytes input)) *encoded-seq*))
      (test-dec)
      (is (string= input *decoded-string*)))))

(defun run-tests ()
  (run! 'lzw-suite))
