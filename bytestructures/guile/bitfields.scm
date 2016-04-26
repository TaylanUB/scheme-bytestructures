(define-module (bytestructures guile bitfields))
(import
 (srfi :60)
 (bytestructures guile utils)
 (bytestructures guile base)
 (bytestructures guile numeric-metadata))
(include-from-path "bytestructures/body/bitfields.scm")
(export bitfield-descriptor)
