;;; Compatibility shim for R6RS systems, because R6RS and R7RS have different
;;; argument order for `bytevector-copy!'.
(library (bytestructures bytevectors)
  (export
   endianness native-endianness bytevector?
   make-bytevector bytevector-length bytevector=? bytevector-fill!
   bytevector-copy! bytevector-copy
   uniform-array->bytevector
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

   string->utf8 string->utf16 string->utf32
   utf8->string utf16->string utf32->string
   )
  (import
   (rnrs base)
   (rnrs control)
   (rename (rnrs bytevectors)
           (bytevector-copy! %bytevector-copy!)))
  (define bytevector-copy!
    (case-lambda
      ((to at from)
       (%bytevector-copy! from 0 to at))
      ((to at from start)
       (%bytevector-copy! from start to at))
      ((to at from start end)
       (%bytevector-copy! from start to at (- end start))))))
