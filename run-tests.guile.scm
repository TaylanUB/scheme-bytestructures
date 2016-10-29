#!/usr/bin/env guile
!#

(use-modules
 (srfi srfi-11)
 (srfi srfi-64)
 ((rnrs exceptions) #:select (guard))
 ((system foreign) #:prefix ffi:)
 (bytestructures r6 bytevectors)
 (bytestructures guile utils)
 (bytestructures guile)
 (bytestructures guile numeric-metadata))

(define inexact exact->inexact)

(include-from-path "run-tests.body.scm")
