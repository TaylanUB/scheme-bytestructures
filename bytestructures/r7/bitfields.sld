(define-library (bytestructures r7 bitfields)
  (import
   (scheme base)
   (srfi 60)
   (bytestructures r7 utils)
   (bytestructures r7 base)
   (bytestructures r7 numeric-metadata))
  (include-library-declarations "bitfields.exports.sld")
  (include "body/bitfields.scm"))
