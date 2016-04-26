(define-library (bytestructures r7 explicit-endianness)
  (import
   (scheme base)
   (bytestructures r7 utils)
   (bytestructures r7 bytevectors))
  (include-library-declarations "explicit-endianness.exports.sld")
  (include "body/explicit-endianness.scm"))
