(define-library (bytestructures r7 utils)
  (import
   (scheme base))
  (export
   define-syntax-rule
   if-syntax-case
   define/sc
   define-syntax/sc
   define-syntax-rule/sc)
  (include "../body/utils.scm"))
