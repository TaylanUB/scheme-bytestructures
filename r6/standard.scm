(library (bytestructures r6 standard (1 3 1))
  (export
   make-bytestructure-descriptor-type
   bytestructure-descriptor-type?
   make-bytestructure-descriptor
   bytestructure-descriptor?
   bytestructure-descriptor-size
   make-bytestructure
   bytestructure?
   bytestructure-bytevector
   bytestructure-offset
   bytestructure-descriptor
   bytestructure
   bytestructure-ref-helper
   bytestructure-ref-helper*
   bytestructure-ref
   bytestructure-ref*
   bytestructure-set!
   bytestructure-set!*

   bs:simple bs:vector bs:struct bs:union

   float double int8 uint8 int16 uint16 int32 uint32 int64 uint64
   floatle doublele
   int16le uint16le int32le uint32le int64le uint64le
   floatbe doublebe
   int16be uint16be int32be uint32be int64be uint64be
   )
  (import
   (bytestructures r7 standard)
   (bytestructures r6 numeric)))
