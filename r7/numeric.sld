(define-library (bytestructures r7 numeric)
  (import
   (scheme base)
   (bytestructures r7 base)
   (bytestructures r7 utils))
  (cond-expand
   ((library (r6rs bytevectors))
    (import (r6rs bytevectors)
            (bytestructures r7 explicit-endianness)))
   (else))
  (include-library-declarations "numeric.exports.sld")
  (include "../body/numeric.scm"))
