(define-library (bytestructures r7 union)
  (import
   (scheme base)
   (bytestructures r7 base)
   (bytestructures r7 utils))
  (include-library-declarations "union.exports.scm")
  (include "../body/union.scm"))
