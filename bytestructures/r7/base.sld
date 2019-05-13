(define-library (bytestructures r7 base)
  (import
   (scheme base)
   (scheme case-lambda)
   (bytestructures r7 utils))
  (cond-expand
   ((library (rnrs syntax-case))
    (import (rnrs syntax-case)))
   (else))
  (include-library-declarations "base.exports.sld")
  (include "body/base.scm"))
