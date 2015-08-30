#!/usr/bin/env guile
!#

(use-modules
 (srfi srfi-11)
 (srfi srfi-64)
 ((system foreign) #:prefix ffi:)
 (bytestructures bytevectors)
 (bytestructures guile)
 (bytestructures guile utils)
 (bytestructures guile explicit-endianness))

(include-from-path "bytestructures/run-tests.body.scm")
