(define-module (bytestructures guile bitfields))
(export bitfield-descriptor)
(import
 (bytestructures guile base)
 (bytestructures guile numeric-metadata)
 (bytestructures guile utils)
 (srfi :60))
(include-from-path "bytestructures/body/bitfields.scm")
