(define-module (bytestructures guile struct))
(export bs:struct)
(import
 (bytestructures guile base)
 (bytestructures bytevectors)
 (srfi :9)
 (srfi :11))
(include-from-path "bytestructures/body/struct.scm")
