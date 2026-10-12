(defpackage #:lzw/tests
  (:use #:cl #:fiveam)
  ;; Keep the demonstration's existing CL-USER API intact.
  (:import-from #:cl-user
                #:fresh-tree #:new-node #:next-node #:insert-node #:node-num
                #:fresh-lzw-enc-ctx #:lzw-enc-letter #:lzw-enc-finish
                #:lzw-enc-ctx-tree #:lzw-enc-ctx-current #:lzw-enc-ctx-node-count
                #:fresh-lzw-dec-ctx #:lzw-dec-code
                #:lzw-dec-ctx-table #:lzw-dec-ctx-node-count
                #:lzw-dec-ctx-new-entry #:lzw-dec-ctx-last-decoded
                #:make-dec-table-entry #:dec-table-entry-prefix
                #:dec-table-entry-rest #:first-letter #:splice-output
                #:make-queue #:queue-put #:queue-get #:queue-empty?
                #:queue-front #:queue-back
                #:test-enc #:test-dec #:*input-string* #:*encoded-seq*
                #:*decoded-seq* #:*decoded-string*)
  (:export #:run-tests))
