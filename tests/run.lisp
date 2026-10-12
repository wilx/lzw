;; Run from any working directory: sbcl --script /path/to/lzw/tests/run.lisp
(require "asdf")
(asdf:load-asd (merge-pathnames "../lzw.asd" *load-truename*))
(asdf:test-system "lzw")
