;;; Compatibility shim for R6RS systems, because R6RS and R7RS have different
;;; semantics for some procedures of the same name.  We use R7RS semantics
;;; everywhere, so implement them in terms of R6RS.
(library (bytestructures r6 bytevectors)
  (export
   endianness native-endianness bytevector?
   make-bytevector bytevector-length bytevector=? bytevector-fill!
   (rename (r7rs-bytevector-copy! bytevector-copy!))
   (rename (r7rs-bytevector-copy bytevector-copy))

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

   (rename (r7rs-string->utf8 string->utf8))
   (rename (r7rs-utf8->string utf8->string))
   string->utf16 string->utf32
   utf16->string utf32->string
   )
  (import
   (rnrs base)
   (rnrs control)
   (rnrs bytevectors))
  (define r7rs-bytevector-copy!
    (case-lambda
      ((to at from)
       (bytevector-copy! from 0 to at (bytevector-length from)))
      ((to at from start)
       (bytevector-copy! from start to at (- (bytevector-length from) start)))
      ((to at from start end)
       (bytevector-copy! from start to at (- end start)))))
  (define r7rs-bytevector-copy
    (case-lambda
      ((bytevector)
       (bytevector-copy bytevector))
      ((bytevector start)
       (r7rs-bytevector-copy bytevector start (bytevector-length bytevector)))
      ((bytevector start end)
       (let* ((size (- end start))
              (bytevector* (make-bytevector size)))
         (bytevector-copy! bytevector start bytevector* 0 size)
         bytevector*))))
  (define r7rs-string->utf8
    (case-lambda
      ((string)
       (string->utf8 string))
      ((string start)
       (string->utf8 (substring string start (string-length string))))
      ((string start end)
       (string->utf8 (substring string start end)))))
  (define r7rs-utf8->string
    (case-lambda
      ((bytevector)
       (utf8->string bytevector))
      ((bytevector start)
       (utf8->string (r7rs-bytevector-copy bytevector start)))
      ((bytevector start end)
       (utf8->string (r7rs-bytevector-copy bytevector start end))))))
