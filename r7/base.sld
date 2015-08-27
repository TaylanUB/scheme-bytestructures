(define-library (bytestructures r7 base)
  (import
   (scheme base)
   (scheme case-lambda)
   (bytestructures r7 utils))
  (include-library-declarations "base.exports.scm")
  (include "../body/base.scm"))
