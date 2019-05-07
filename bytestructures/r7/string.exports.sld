(export bs:string)
(cond-expand
 (r6rs
  (export bytevector->string string->bytevector
          ascii utf8 utf16le utf16be utf32le utf32be
          bytevector-zero!))
 (else))
