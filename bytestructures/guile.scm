(define-module (bytestructures guile))

(import
 (bytestructures guile base)
 (bytestructures guile vector)
 (bytestructures guile struct)
 (bytestructures guile union)
 (bytestructures guile pointer)
 (bytestructures guile numeric)
 (bytestructures guile string))
(re-export
 make-bytestructure-descriptor
 bytestructure-descriptor?
 bytestructure-descriptor-size
 bytestructure-descriptor-size/syntax
 bytestructure-descriptor-alignment
 bytestructure-descriptor-unwrapper
 bytestructure-descriptor-getter
 bytestructure-descriptor-setter
 bytestructure-descriptor-metadata
 make-bytestructure
 bytestructure?
 bytestructure-bytevector
 bytestructure-offset
 bytestructure-descriptor
 bytestructure-size
 bytestructure
 bytestructure-unwrap
 bytestructure-unwrap*
 bytestructure-ref
 bytestructure-ref*
 bytestructure-set!
 bytestructure-set!*
 bytestructure-ref/dynamic
 bytestructure-set!/dynamic
 bytestructure-unwrap/syntax
 bytestructure-ref/syntax
 bytestructure-set!/syntax
 define-bytestructure-accessors

 bs:vector
 vector-metadata? vector-metadata-length vector-metadata-element-descriptor

 bs:struct
 struct-metadata? struct-metadata-field-alist

 bs:union
 union-metadata? union-metadata-field-alist

 bs:pointer
 pointer-metadata? pointer-metadata-content-descriptor

 int8 int16 int32 int64
 int16le int32le int64le
 int16be int32be int64be
 uint8 uint16 uint32 uint64
 uint16le uint32le uint64le
 uint16be uint32be uint64be
 float32 float64
 float32le float64le
 float32be float64be

 short unsigned-short
 int unsigned-int
 long unsigned-long
 long-long unsigned-long-long
 intptr_t uintptr_t
 size_t ssize_t ptrdiff_t
 float double

 complex64 complex128
 complex64le complex128le
 complex64be complex128be

 bs:string
 )
