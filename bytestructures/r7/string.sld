(define-library (bytestructures r7 string)
  (import
   (scheme base)
   (bytestructures r7 utils)
   (bytestructures r7 base))
  (include-library-declarations "string.exports.sld")
  (include "body/string.scm"))
