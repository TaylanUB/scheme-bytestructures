(define-library (bytestructures r7 vector)
  (import
   (scheme base)
   (bytestructures r7 utils)
   (bytestructures r7 base))
  (include-library-declarations "vector.exports.sld")
  (include "body/vector.scm"))
