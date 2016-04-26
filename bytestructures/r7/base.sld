(define-library (bytestructures r7 base)
  (import
   (scheme base)
   (scheme case-lambda)
   (bytestructures r7 utils))
  (include-library-declarations "base.exports.sld")
  (include "body/base.scm"))
