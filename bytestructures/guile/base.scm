(define-module (bytestructures guile base))
(import
 (srfi :9)
 (srfi :11)
 (ice-9 format)
 (bytestructures guile bytevectors)
 (bytestructures guile utils))
(include-from-path "bytestructures/body/base.scm")
(include-from-path "bytestructures/r7/base.exports.sld")

(import (srfi srfi-9 gnu))

(set-record-type-printer!
 <bytestructure-descriptor>
 (lambda (record port)
   (format port "#<bytestructure-descriptor 0x~x>" (object-address record))))

(set-record-type-printer!
 <bytestructure>
 (lambda (record port)
   (format port "#<bytestructure 0x~x>" (object-address record))))
