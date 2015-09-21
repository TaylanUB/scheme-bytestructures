(define-library (bytestructures r7 bitfields)
  (import
   (scheme base)
   (srfi 60)
   (bytestructures r7 base)
   (bytestructures r7 numeric-metadata)
   (bytestructures r7 utils))
  (export bitfield-descriptor)
  (include "../body/bitfields.scm"))
