;;; run-tests.body.scm --- Bytestructures test suite.

;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A relatively simple SRFI-64 test suite.


;;; Code:

(define-syntax-rule (test-= name expected expr)
  (test-assert name (= expected expr)))

(define (maybe-skip-syntax)
  (test-skip (if-syntax-case 0 1)))

(test-begin "bytestructures")

(cond-expand
 ((or guile (library (r6rs bytevectors)) (library (rnrs bytevectors)))
  (values))
 (else
  (test-skip "numeric")))
(test-group "numeric"
  (define-syntax test-numeric-descriptors
    (syntax-rules ()
      ((_ (descriptor signed? size ref-proc set-proc) ...)
       (let ()
         (define (max s? sz)
           ;; Not necessarily a "maximal" value; it's minimal for signed types.
           ;; It's also nonsensical for floating-point, but doesn't hurt; we
           ;; don't try to test their limits.
           (if s?
               (- (- (expt 256 sz) 1) (expt 256 sz))
               (- (expt 256 sz) 1)))
         (test-group "procedural"
           (test-group (symbol->string 'descriptor)
             (let ((max (max signed? size)))
               (define bs (bytestructure descriptor))
               (test-eqv "size" size (bytevector-length
                                      (bytestructure-bytevector bs)))
               (test-= "ref" 2
                       (begin
                         (set-proc (bytestructure-bytevector bs) 0 2)
                         (bytestructure-ref bs)))
               (test-= "set" 1
                       (begin
                         (bytestructure-set! bs 1)
                         (ref-proc (bytestructure-bytevector bs) 0)))
               (test-= "max" max
                       (begin
                         (bytestructure-set! bs max)
                         (bytestructure-ref bs)))))
           ...)
         (maybe-skip-syntax)
         (test-group "syntactic"
           (test-group (symbol->string 'descriptor)
             (let ((max (max signed? size)))
               (define-bytestructure-accessors descriptor
                 ref-helper getter setter)
               (define bv (make-bytevector size))
               (test-= "ref" 2
                       (begin
                         (set-proc bv 0 2)
                         (getter bv)))
               (test-= "set" 1
                       (begin
                         (setter bv 1)
                         (ref-proc bv 0)))
               (test-= "max" max
                       (begin
                         (setter bv max)
                         (getter bv)))))
           ...)))))
  (test-numeric-descriptors
   (float32 #t 4
            bytevector-ieee-single-native-ref
            bytevector-ieee-single-native-set!)
   (float32le #t 4
              bytevector-ieee-single-le-ref
              bytevector-ieee-single-le-set!)
   (float32be #t 4
              bytevector-ieee-single-be-ref
              bytevector-ieee-single-be-set!)
   (double64 #t 8
             bytevector-ieee-double-native-ref
             bytevector-ieee-double-native-set!)
   (double64le #t 8
               bytevector-ieee-double-le-ref
               bytevector-ieee-double-le-set!)
   (double64be #t 8
               bytevector-ieee-double-be-ref
               bytevector-ieee-double-be-set!)
   (int8     #t 1 bytevector-s8-ref         bytevector-s8-set!)
   (uint8    #f 1 bytevector-u8-ref         bytevector-u8-set!)
   (int16    #t 2 bytevector-s16-native-ref bytevector-s16-native-set!)
   (int16le  #t 2 bytevector-s16le-ref      bytevector-s16le-set!)
   (int16be  #t 2 bytevector-s16be-ref      bytevector-s16be-set!)
   (uint16   #f 2 bytevector-u16-native-ref bytevector-u16-native-set!)
   (uint16le #f 2 bytevector-u16le-ref      bytevector-u16le-set!)
   (uint16be #f 2 bytevector-u16be-ref      bytevector-u16be-set!)
   (int32    #t 4 bytevector-s32-native-ref bytevector-s32-native-set!)
   (int32le  #t 4 bytevector-s32le-ref      bytevector-s32le-set!)
   (int32be  #t 4 bytevector-s32be-ref      bytevector-s32be-set!)
   (uint32   #f 4 bytevector-u32-native-ref bytevector-u32-native-set!)
   (uint32le #f 4 bytevector-u32le-ref      bytevector-u32le-set!)
   (uint32be #f 4 bytevector-u32be-ref      bytevector-u32be-set!)
   (int64    #t 8 bytevector-s64-native-ref bytevector-s64-native-set!)
   (int64le  #t 8 bytevector-s64le-ref      bytevector-s64le-set!)
   (int64be  #t 8 bytevector-s64be-ref      bytevector-s64be-set!)
   (uint64   #f 8 bytevector-u64-native-ref bytevector-u64-native-set!)
   (uint64le #f 8 bytevector-u64le-ref      bytevector-u64le-set!)
   (uint64be #f 8 bytevector-u64be-ref      bytevector-u64be-set!)))

(test-group "vector"
  (test-assert "create" (bs:vector 3 uint16))
  (test-group "procedural"
    (define bs (bytestructure (bs:vector 3 uint16) #(321 123 321)))
    (bytevector-u16-native-set! (bytestructure-bytevector bs) 2 321)
    (test-eqv "ref" 321 (bytestructure-ref bs 1))
    (test-eqv "set" 456 (begin (bytestructure-set! bs 1 456)
                               (bytestructure-ref bs 1))))
  (maybe-skip-syntax)
  (test-group "syntactic"
    (define-bytestructure-accessors (bs:vector 3 uint16)
      ref-helper getter setter)
    (define bv (make-bytevector 6))
    (bytevector-u16-native-set! bv 2 321)
    (test-eqv "ref" 321 (getter bv 1))
    (test-eqv "set" 456 (begin (setter bv 1 456)
                               (getter bv 1)))))

(test-group "struct"
  (test-group "aligned"
    (test-assert "create" (bs:struct `((x ,uint8) (y ,uint16))))
    (test-group "procedural"
      (define bs (bytestructure (bs:struct `((x ,uint8) (y ,uint16)))
                                #(123 321)))
      (bytevector-u16-native-set! (bytestructure-bytevector bs) 2 321)
      (test-eqv "ref" 321 (bytestructure-ref bs 'y))
      (test-eqv "set" 456 (begin (bytestructure-set! bs 'y 456)
                                 (bytestructure-ref bs 'y))))
    (maybe-skip-syntax)
    (test-group "syntactic"
      (define-bytestructure-accessors (bs:struct `((x ,uint8) (y ,uint16)))
        ref-helper getter setter)
      (define bv (make-bytevector 4))
      (bytevector-u16-native-set! bv 2 321)
      (test-eqv "ref" 321 (getter bv y))
      (test-eqv "set" 456 (begin (setter bv y 456)
                                 (getter bv y)))))
  (test-group "packed"
    (test-assert "create" (bs:struct #t `((x ,uint8) (y ,uint16))))
    (test-group "procedural"
      (define bs (bytestructure (bs:struct #t `((x ,uint8) (y ,uint16)))
                                #(123 321)))
      (bytevector-u16-native-set! (bytestructure-bytevector bs) 1 321)
      (test-eqv "ref" 321 (bytestructure-ref bs 'y))
      (test-eqv "set" 456 (begin (bytestructure-set! bs 'y 456)
                                 (bytestructure-ref bs 'y))))
    (maybe-skip-syntax)
    (test-group "syntactic"
      (define-bytestructure-accessors (bs:struct #t `((x ,uint8) (y ,uint16)))
        ref-helper getter setter)
      (define bv (make-bytevector 4))
      (bytevector-u16-native-set! bv 1 321)
      (test-eqv "ref" 321 (getter bv y))
      (test-eqv "set" 456 (begin (setter bv y 456)
                                 (getter bv y))))))

(test-group "union"
  (test-assert "create" (bs:union `((x ,uint8) (y ,uint16))))
  (test-group "procedural"
    (define bs (bytestructure (bs:union `((x ,uint8) (y ,uint16)))
                              '(y 321)))
    (bytevector-u16-native-set! (bytestructure-bytevector bs) 0 321)
    (test-eqv "ref" 321 (bytestructure-ref bs 'y))
    (test-eqv "set" 456 (begin (bytestructure-set! bs 'y 456)
                               (bytestructure-ref bs 'y))))
  (maybe-skip-syntax)
  (test-group "syntactic"
    (define-bytestructure-accessors (bs:union `((x ,uint8) (y ,uint16)))
      ref-helper getter setter)
    (define bv (make-bytevector 2))
    (bytevector-u16-native-set! bv 0 321)
    (test-eqv "ref" 321 (getter bv y))
    (test-eqv "set" 456 (begin (setter bv y 456)
                               (getter bv y)))))

(test-skip (cond-expand (guile 0) (else 1)))
(test-group "pointer"
  (define (protect-from-gc-upto-here obj)
    (with-output-to-file *null-device*
      (lambda ()
        (display (eq? #f obj)))))
  (define pointer-size (ffi:sizeof '*))
  (define bytevector-address-set!
    (case pointer-size
      ((1) bytevector-u8-set!)
      ((2) bytevector-u16-native-set!)
      ((4) bytevector-u32-native-set!)
      ((8) bytevector-u64-native-set!)))
  (test-assert "create" (bs:pointer uint16))
  (test-group "procedural"
    (define bs (bytestructure (bs:pointer uint16)))
    (define bv1 (make-bytevector 2))
    (define address (ffi:pointer-address (ffi:bytevector->pointer bv1)))
    (bytevector-address-set! (bytestructure-bytevector bs) 0 address)
    (bytevector-u16-native-set! bv1 0 321)
    (test-eqv "ref" 321 (bytestructure-ref bs '*))
    (test-eqv "set" 456 (begin (bytestructure-set! bs '* 456)
                               (bytestructure-ref bs '*)))
    (test-eqv "ref2" address (bytestructure-ref bs))
    (protect-from-gc-upto-here bv1)
    (let* ((bv2 (make-bytevector 2 123))
           (address (ffi:pointer-address (ffi:bytevector->pointer bv2))))
      (test-eqv "set2" address (begin (bytestructure-set! bs address)
                                      (bytestructure-ref bs)))
      (protect-from-gc-upto-here bv2)))
  (test-group "syntactic"
    (define-bytestructure-accessors (bs:pointer uint16)
      ref-helper getter setter)
    (define bv (make-bytevector pointer-size))
    (define bv1 (make-bytevector 2))
    (define address (ffi:pointer-address (ffi:bytevector->pointer bv1)))
    (bytevector-address-set! bv 0 address)
    (bytevector-u16-native-set! bv1 0 321)
    (test-eqv "ref" 321 (getter bv *))
    (test-eqv "set" 456 (begin (setter bv * 456)
                               (getter bv *)))
    (test-eqv "ref2" address (getter bv))
    (protect-from-gc-upto-here bv1)
    (let* ((bv2 (make-bytevector 2 123))
           (address (ffi:pointer-address (ffi:bytevector->pointer bv2))))
      (test-eqv "set2" address (begin (setter bv address)
                                      (getter bv)))
      (protect-from-gc-upto-here bv2))))

(test-end "bytestructures")
