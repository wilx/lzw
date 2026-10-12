# lzw

LZW compression demonstration in Common Lisp.

The encoder consumes bytes (integers from 0 to 255) and produces integer
codes. The decoder consumes those codes and returns lists of bytes. The
dictionary starts with the 256 byte values and assigns codes from 256 to new
phrases. It grows without a configured size limit. This demonstration does
not pack codes into bits or implement the headers, clear codes, or end codes
of GIF, TIFF, or Unix `.Z` files.

## Setup on Ubuntu

```sh
sudo apt update
sudo apt install sbcl cl-fiveam
```

SBCL includes ASDF. The `cl-fiveam` package supplies the test framework and
installs its dependencies. Loading the demonstration itself does not require
FiveAM. There is no need for Quicklisp to run the tests with these packages.

## Running tests

From the checkout:

```sh
sbcl --script tests/run.lisp
```

The script also works when invoked by its full path from another directory.
A failed test signals an error, giving batch execution a nonzero exit status.

In a REPL, load the system definition and use ASDF's standard test operation:

```lisp
(require "asdf")
(asdf:load-asd (truename "lzw.asd"))
(asdf:test-system "lzw")
```

The separate `lzw/tests` system keeps FiveAM out of the demonstration's
runtime dependencies. Tests are in the `lzw/tests` package. After loading
that system, `(lzw/tests:run-tests)` runs the full suite and returns true on
success. Individual tests can be run interactively, for example:

```lisp
(fiveam:run! 'lzw/tests::consecutive-new-codes)
```

The suite checks known code sequences, empty input, all 256 byte values,
incremental operation, dictionary growth, the LZW "KwKwK" case, invalid
inputs, independent contexts, queues, and prefix output. Property tests
enumerate every input up to length six over `(0 1 255)` and use a reproducible
generated corpus, including longer binary and repeated inputs. An independent
encoder based on whole-phrase hash keys supplies expected codes; tests check
both production encoding and decoding those expected codes.

For a second implementation, Ubuntu's CLISP includes ASDF too:

```sh
sudo apt install clisp
clisp -q -norc tests/run.lisp
```

GitHub Actions runs the suite on SBCL and CLISP on Ubuntu.

## Using the demonstration

The original functions remain in `CL-USER`. They can be loaded directly with
`(load "lzw.lisp")` or through `(asdf:load-system "lzw")` after loading the
system definition. The original `test-enc` and `test-dec` helpers use
`*input-string*`, `*encoded-seq*`, and `*decoded-string*` for a REPL example.

For incremental encoding, create a context with `(fresh-lzw-enc-ctx)` and
call `(lzw-enc-letter byte context)` for each byte. Collect each non-`nil`
return value, including zero. Finally, collect `(lzw-enc-finish context)` if
it is non-`nil`; empty input has no final code. Finish emits the pending code
without resetting the context, so call it once at the end of a stream.

For decoding, create a context with `(fresh-lzw-dec-ctx)` and concatenate the
byte lists returned by `(lzw-dec-code context code)`. The first code must be
a byte value. Later codes must already exist in the dictionary or equal its
next unused index. Invalid codes signal an error before changing the context.
