(define-library (bytestructures r7 numeric)
  (import
   (scheme base)
   (bytestructures r7 base)
   (bytestructures r7 simple)
   (bytestructures r7 utils))
  (cond-expand
   ((library (r6rs bytevectors))
    (import (r6rs bytevectors)))
   (else))
  (include-library-declarations "numeric.exports.sld")
  (include "../body/numeric.scm"))
