(define-module (bytestructures guile union))
(import
 (srfi :9)
 (srfi :11)
 (bytestructures r6 bytevectors)
 (bytestructures guile utils)
 (bytestructures guile base))
(include-from-path "bytestructures/body/union.scm")
(include-from-path "bytestructures/r7/union.exports.sld")
