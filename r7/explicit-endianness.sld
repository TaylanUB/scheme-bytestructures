(define-library (bytestructures r7 explicit-endianness)
  (import
   (scheme base)
   (bytestructures r7 utils)
   (r6rs bytevectors))
  (include-library-declarations "explicit-endianness.exports.sld")
  (include "../body/explicit-endianness.scm"))
