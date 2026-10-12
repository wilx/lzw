(asdf:defsystem "lzw"
  :description "LZW compression demonstration in Common Lisp"
  :author "Vaclav Zeman"
  :license "BSD-2-Clause"
  :components ((:file "lzw"))
  :in-order-to ((asdf:test-op (asdf:test-op "lzw/tests"))))

(asdf:defsystem "lzw/tests"
  :description "Regression and property tests for LZW"
  :depends-on ("lzw" "fiveam")
  :serial t
  :components ((:file "tests/package")
               (:file "tests/lzw-tests"))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call :lzw/tests :run-tests)
               (error "LZW tests failed."))))
