(define-module (bytestructures guile vector))
(export bs:vector)
(import
 (bytestructures guile base)
 (bytestructures guile utils)
 (bytestructures bytevectors)
 (srfi :9))
(include-from-path "bytestructures/body/vector.scm")
