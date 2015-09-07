(define-module (bytestructures guile numeric))
(export
 int8 uint8 int16 uint16 int32 uint32 int64 uint64
 int16le uint16le int32le uint32le int64le uint64le
 int16be uint16be int32be uint32be int64be uint64be
 float32 double64 float32le double64le float32be double64be
 )
(import
 (bytestructures guile base)
 (bytestructures guile utils)
 (bytestructures bytevectors)
 (bytestructures guile explicit-endianness))
(include-from-path "bytestructures/body/numeric.scm")
