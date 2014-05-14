(define-library (bytestructures r7 uint8)
  (import (scheme base)
          (bytestructures r7 base)
          (bytestructures r7 simple))
  (include-library-declarations "uint8.exports.scm")
  (begin
    (define uint8
      (make-bytestructure-descriptor
       (list bs:simple 1 bytevector-u8-ref bytevector-u8-set!)))))
