(define-library (bytestructures r7 simple)
  (import
   (scheme base)
   (bytestructures r7 base)
   (bytestructures r7 utils))
  (include-library-declarations "simple.exports.scm")
  (include "../body/simple.scm"))
