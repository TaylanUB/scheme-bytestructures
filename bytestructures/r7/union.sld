(define-library (bytestructures r7 union)
  (import
   (scheme base)
   (bytestructures r7 utils)
   (bytestructures r7 base))
  (include-library-declarations "union.exports.sld")
  (include "body/align.scm")
  (include "body/union.scm"))
