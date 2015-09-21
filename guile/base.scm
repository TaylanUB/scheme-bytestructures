(define-module (bytestructures guile base))
(import
 (srfi :9)
 (srfi :11)
 (bytestructures r6 bytevectors)
 (bytestructures guile utils))
(include-from-path "bytestructures/body/base.scm")
(define bytestructure-descriptor-alignment bd-alignment)
(define bytestructure-descriptor-ref-helper bd-ref-helper)
(define bytestructure-descriptor-getter bd-getter)
(define bytestructure-descriptor-setter bd-setter)
(export
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
 )
