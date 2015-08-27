(define-module (bytestructures guile union))
(export bs:union)
(import
 (bytestructures guile base)
 (bytestructures guile utils)
 (bytestructures bytevectors)
 (srfi :9)
 (srfi :11))
(include-from-path "bytestructures/body/union.scm")
