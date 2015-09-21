(define-module (bytestructures guile))
(import
 (bytestructures guile base)
 (bytestructures guile numeric)
 (bytestructures guile vector)
 (bytestructures guile struct)
 (bytestructures guile union)
 (bytestructures guile pointer)
 (bytestructures guile numeric-native))
(re-export
 make-bytestructure-descriptor
 bytestructure-descriptor?
 bytestructure-descriptor-size
 bytestructure-descriptor-size/syntax
 bytestructure-descriptor-alignment
 bytestructure-descriptor-ref-helper
 bytestructure-descriptor-getter
 bytestructure-descriptor-setter
 make-bytestructure
 bytestructure?
 bytestructure-bytevector
 bytestructure-offset
 bytestructure-descriptor
 bytestructure-size
 bytestructure
 bytestructure-ref-helper
 bytestructure-ref-helper*
 bytestructure-ref
 bytestructure-ref*
 bytestructure-set!
 bytestructure-set!*
 bytestructure-ref-helper/syntax
 bytestructure-ref/syntax
 bytestructure-set!/syntax
 define-bytestructure-accessors

 bs:vector bs:struct bs:union bs:pointer

 int8 int16 int32 int64
 int16le int32le int64le
 int16be int32be int64be
 uint8 uint16 uint32 uint64
 uint16le uint32le uint64le
 uint16be uint32be uint64be
 float32 double64 float32le
 double64le float32be double64be

 short unsigned-short int unsigned-int long unsigned-long
 size_t ssize_t ptrdiff_t
 )
