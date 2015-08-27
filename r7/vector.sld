(define-library (bytestructures r7 vector)
  (import
   (scheme base)
   (bytestructures r7 base)
   (bytestructures r7 utils))
  (include-library-declarations "vector.exports.scm")
  (include "../body/vector.scm"))
