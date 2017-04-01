(define-module (bytestructures guile numeric))
(import (bytestructures guile numeric-all))
(re-export

 int8 uint8 int16 uint16 int32 uint32 int64 uint64
 int16le uint16le int32le uint32le int64le uint64le
 int16be uint16be int32be uint32be int64be uint64be
 float32 float64 float32le float64le float32be float64be

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
 )
