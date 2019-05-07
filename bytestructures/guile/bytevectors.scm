;;; Compatibility shim for Guile, because its implementation of utf16->string
;;; and utf32->string doesn't conform to R6RS.
(define-module (bytestructures guile bytevectors))

(import
 (rnrs base)
 (rnrs control)
 (bytestructures r6 bytevectors))

(re-export
 endianness native-endianness bytevector?
 make-bytevector bytevector-length bytevector=? bytevector-fill!
 bytevector-copy!
 bytevector-copy

 bytevector-u8-ref bytevector-s8-ref
 bytevector-u8-set! bytevector-s8-set! bytevector->u8-list
 u8-list->bytevector
 bytevector-uint-ref bytevector-uint-set!
 bytevector-sint-ref bytevector-sint-set!
 bytevector->sint-list bytevector->uint-list
 uint-list->bytevector sint-list->bytevector

 bytevector-u16-ref bytevector-s16-ref
 bytevector-u16-set! bytevector-s16-set!
 bytevector-u16-native-ref bytevector-s16-native-ref
 bytevector-u16-native-set! bytevector-s16-native-set!

 bytevector-u32-ref bytevector-s32-ref
 bytevector-u32-set! bytevector-s32-set!
 bytevector-u32-native-ref bytevector-s32-native-ref
 bytevector-u32-native-set! bytevector-s32-native-set!

 bytevector-u64-ref bytevector-s64-ref
 bytevector-u64-set! bytevector-s64-set!
 bytevector-u64-native-ref bytevector-s64-native-ref
 bytevector-u64-native-set! bytevector-s64-native-set!

 bytevector-ieee-single-ref
 bytevector-ieee-single-set!
 bytevector-ieee-single-native-ref
 bytevector-ieee-single-native-set!

 bytevector-ieee-double-ref
 bytevector-ieee-double-set!
 bytevector-ieee-double-native-ref
 bytevector-ieee-double-native-set!

 string->utf8
 utf8->string
 string->utf16 string->utf32)

(export
 (r6rs-utf16->string . utf16->string)
 (r6rs-utf32->string . utf32->string))

(define (read-bom16 bv)
  (let ((c0 (bytevector-u8-ref bv 0))
        (c1 (bytevector-u8-ref bv 1)))
    (cond
     ((and (= c0 #xFE) (= c1 #xFF))
      'big)
     ((and (= c0 #xFF) (= c1 #xFE))
      'little)
     (else
      #f))))

(define r6rs-utf16->string
  (case-lambda
    ((bv default-endianness)
     (let ((bom-endianness (read-bom16 bv)))
       (if (not bom-endianness)
           (utf16->string bv default-endianness)
           (substring/shared (utf16->string bv bom-endianness) 1))))
    ((bv endianness endianness-mandatory?)
     (if endianness-mandatory?
         (utf16->string bv endianness)
         (r6rs-utf16->string bv endianness)))))

(define (read-bom32 bv)
  (let ((c0 (bytevector-u8-ref bv 0))
        (c1 (bytevector-u8-ref bv 1))
        (c2 (bytevector-u8-ref bv 2))
        (c3 (bytevector-u8-ref bv 3)))
    (cond
     ((and (= c0 #x00) (= c1 #x00) (= c2 #xFE) (= c3 #xFF))
      'big)
     ((and (= c0 #xFF) (= c1 #xFE) (= c2 #x00) (= c3 #x00))
      'little)
     (else
      #f))))

(define r6rs-utf32->string
  (case-lambda
    ((bv default-endianness)
     (let ((bom-endianness (read-bom32 bv)))
       (if (not bom-endianness)
           (utf32->string bv default-endianness)
           (substring/shared (utf32->string bv bom-endianness) 1))))
    ((bv endianness endianness-mandatory?)
     (if endianness-mandatory?
         (utf32->string bv endianness)
         (r6rs-utf32->string bv endianness)))))
