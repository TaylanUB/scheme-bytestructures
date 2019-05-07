(define-library (bytestructures r7 utils)
  (import (scheme base))
  (cond-expand
   ((library (rnrs syntax-case))
    (import (rnrs syntax-case)))
   (else))
  (export
   define-syntax-rule
   if-syntax-case
   define-syntax-case-stubs
   quasisyntax
   unsyntax
   unsyntax-splicing
   syntax->datum
   datum->syntax
   )
  (include "body/utils.scm"))
