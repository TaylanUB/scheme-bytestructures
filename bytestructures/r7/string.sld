(define-library (bytestructures r7 string)
  (import
   (scheme base)
   (bytestructures r7 bytevectors)
   (bytestructures r7 utils)
   (bytestructures r7 base))
  (cond-expand
   ((library (rnrs syntax-case))
    (import (rnrs syntax-case)))
   (else))
  (include-library-declarations "string.exports.sld")
  (include "body/string.scm"))
