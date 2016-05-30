(define-module (bytestructures guile bitfields))
(import
 (srfi :9)
 (srfi :60)
 (bytestructures guile utils)
 (bytestructures guile base)
 (bytestructures guile numeric-metadata))
(include-from-path "bytestructures/body/bitfields.scm")
(include-from-path "bytestructures/r7/bitfields.exports.sld")
