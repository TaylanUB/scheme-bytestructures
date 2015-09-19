(define-library (bytestructures r7 numeric)
  (import
   (scheme base)
   (bytestructures r7 base)
   (bytestructures r7 explicit-endianness)
   (bytestructures r7 utils))
  (cond-expand
   ((library (rnrs bytevectors))
    (import (rnrs bytevectors)))
   (else
    (import (r6rs bytevectors))))
  (include-library-declarations "numeric.exports.sld")
  (include "../body/numeric.scm"))
