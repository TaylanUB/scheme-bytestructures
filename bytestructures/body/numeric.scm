;;; numeric.scm --- Numeric types as supported by (rnrs bytevectors).

;; Copyright © 2015, 2016 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>

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

;; This module defines descriptors for numeric types of specific size, and
;; native or specific endianness, as made possible by the bytevector referencing
;; and assigning procedures in the (rnrs bytevectors) module.


;;; Code:

(define word-size
  (cond-expand
   (lp32  4)
   (ilp32 4)
   (else  8)))

(define-syntax-rule (make-numeric-descriptor <size> <getter> <setter>)
  (let ()
    (define size <size>)
    (define alignment (min <size> word-size))
    (define (getter syntax? bytevector offset)
      (if syntax?
          (quasisyntax
           (<getter> (unsyntax bytevector) (unsyntax offset)))
          (<getter> bytevector offset)))
    (define (setter syntax? bytevector offset value)
      (if syntax?
          (quasisyntax
           (<setter> (unsyntax bytevector) (unsyntax offset) (unsyntax value)))
          (<setter> bytevector offset value)))
    (make-bytestructure-descriptor size alignment #f getter setter)))

(define-syntax-rule (define-numeric-descriptors <list>
                      (<name> <size> <getter> <setter>)
                      ...)
  (begin
    (define <name>
      (make-numeric-descriptor <size> <getter> <setter>))
    ...
    (define <list> (list (list <name> '<name> <getter> <setter>) ...))))

(define-numeric-descriptors
  signed-integer-native-descriptors
  (int8   1 bytevector-s8-ref bytevector-s8-set!)
  (int16  2 bytevector-s16-native-ref bytevector-s16-native-set!)
  (int32  4 bytevector-s32-native-ref bytevector-s32-native-set!)
  (int64  8 bytevector-s64-native-ref bytevector-s64-native-set!))

(define-numeric-descriptors
  unsigned-integer-native-descriptors
  (uint8  1 bytevector-u8-ref bytevector-u8-set!)
  (uint16 2 bytevector-u16-native-ref bytevector-u16-native-set!)
  (uint32 4 bytevector-u32-native-ref bytevector-u32-native-set!)
  (uint64 8 bytevector-u64-native-ref bytevector-u64-native-set!))

(define-numeric-descriptors
  float-native-descriptors
  (float32
   4 bytevector-ieee-single-native-ref bytevector-ieee-single-native-set!)
  (float64
   8 bytevector-ieee-double-native-ref bytevector-ieee-double-native-set!))

(define-syntax-rule (define-with-endianness <list> <endianness>
                      (<name> <size> <native-name> <getter> <setter>)
                      ...)
  (begin
    (define <name>
      (if (equal? <endianness> (native-endianness))
          <native-name>
          (make-numeric-descriptor <size> <getter> <setter>)))
    ...
    (define <list> (list (list <name> '<name> <getter> <setter>) ...))))

(define-with-endianness
  signed-integer-le-descriptors (endianness little)
  (int16le 2 int16 bytevector-s16le-ref bytevector-s16le-set!)
  (int32le 4 int32 bytevector-s32le-ref bytevector-s32le-set!)
  (int64le 8 int64 bytevector-s64le-ref bytevector-s64le-set!))

(define-with-endianness
  signed-integer-be-descriptors (endianness big)
  (int16be 2 int16 bytevector-s16be-ref bytevector-s16be-set!)
  (int32be 4 int32 bytevector-s32be-ref bytevector-s32be-set!)
  (int64be 8 int64 bytevector-s64be-ref bytevector-s64be-set!))

(define-with-endianness
  unsigned-integer-le-descriptors (endianness little)
  (uint16le 2 uint16 bytevector-u16le-ref bytevector-u16le-set!)
  (uint32le 4 uint32 bytevector-u32le-ref bytevector-u32le-set!)
  (uint64le 8 uint64 bytevector-u64le-ref bytevector-u64le-set!))

(define-with-endianness
  unsigned-integer-be-descriptors (endianness big)
  (uint16be 2 uint16 bytevector-u16be-ref bytevector-u16be-set!)
  (uint32be 4 uint32 bytevector-u32be-ref bytevector-u32be-set!)
  (uint64be 8 uint64 bytevector-u64be-ref bytevector-u64be-set!))

(define-with-endianness
  float-le-descriptors (endianness little)
  (float32le
   4 float32 bytevector-ieee-single-le-ref bytevector-ieee-single-le-set!)
  (float64le
   8 float64 bytevector-ieee-double-le-ref bytevector-ieee-double-le-set!))

(define-with-endianness
  float-be-descriptors (endianness big)
  (float32be
   4 float32 bytevector-ieee-single-be-ref bytevector-ieee-single-be-set!)
  (float64be
   8 float64 bytevector-ieee-double-be-ref bytevector-ieee-double-be-set!))

(define-syntax-rule (make-complex-descriptor
                     <float-size> <float-getter> <float-setter>)
  (let ()
    (define size (* 2 <float-size>))
    (define alignment <float-size>)
    (define (getter syntax? bytevector offset)
      (if syntax?
          (quasisyntax
           (let ((real (<float-getter> (unsyntax bytevector)
                                       (unsyntax offset)))
                 (imag (<float-getter> (unsyntax bytevector)
                                       (+ (unsyntax offset) <float-size>))))
             (make-rectangular real imag)))
          (let ((real (<float-getter> bytevector offset))
                (imag (<float-getter> bytevector (+ offset <float-size>))))
            (make-rectangular real imag))))
    (define (setter syntax? bytevector offset value)
      (if syntax?
          (quasisyntax
           (let ((val (unsyntax value)))
             (let ((real (real-part val))
                   (imag (imag-part val)))
               (<float-setter> (unsyntax bytevector)
                               (unsyntax offset)
                               real)
               (<float-setter> (unsyntax bytevector)
                               (+ (unsyntax offset) <float-size>)
                               imag))))
          (let ((real (real-part value))
                (imag (imag-part value)))
            (<float-setter> bytevector offset real)
            (<float-setter> bytevector (+ offset <float-size>) imag))))
    (make-bytestructure-descriptor size alignment #f getter setter)))

(define-syntax-rule (define-complex-descriptors <list>
                      (<name> <float-size> <float-getter> <float-setter>)
                      ...)
  (begin
    (define <name>
      (make-complex-descriptor <float-size> <float-getter> <float-setter>))
    ...
    (define <list> (list (list <name> '<name> <float-getter> <float-setter>)
                         ...))))

(define-complex-descriptors
  complex-native-descriptors
  (complex64
   4 bytevector-ieee-single-native-ref bytevector-ieee-single-native-set!)
  (complex128
   8 bytevector-ieee-double-native-ref bytevector-ieee-double-native-set!))

(define-syntax-rule (define-complex-with-endianness <list> <endianness>
                      (<name> <float-size> <native-name>
                              <float-getter> <float-setter>)
                      ...)
  (begin
    (define <name>
      (if (equal? <endianness> (native-endianness))
          <native-name>
          (make-complex-descriptor <float-size> <float-getter> <float-setter>)))
    ...
    (define <list> (list (list <name> '<name> <float-getter> <float-setter>)
                         ...))))

(define-complex-with-endianness
  complex-le-descriptors (endianness little)
  (complex64le
   4 complex64  bytevector-ieee-single-le-ref bytevector-ieee-single-le-set!)
  (complex128le
   8 complex128 bytevector-ieee-double-le-ref bytevector-ieee-double-le-set!))

(define-complex-with-endianness
  complex-be-descriptors (endianness big)
  (complex64be
   4 complex64  bytevector-ieee-single-be-ref bytevector-ieee-single-be-set!)
  (complex128be
   8 complex128 bytevector-ieee-double-be-ref bytevector-ieee-double-be-set!))

(define signed-integer-descriptors
  (append signed-integer-native-descriptors
          signed-integer-le-descriptors
          signed-integer-be-descriptors))

(define unsigned-integer-descriptors
  (append unsigned-integer-native-descriptors
          unsigned-integer-le-descriptors
          unsigned-integer-be-descriptors))

(define integer-descriptors
  (append signed-integer-descriptors unsigned-integer-descriptors))

(define float-descriptors
  (append float-native-descriptors
          float-le-descriptors
          float-be-descriptors))

(define complex-descriptors
  (append complex-native-descriptors
          complex-le-descriptors
          complex-be-descriptors))

(define numeric-descriptors
  (append integer-descriptors float-descriptors complex-descriptors))

(define short int16)
(define unsigned-short uint16)

(define int (cond-expand
             (lp32  int16)
             (ilp64 int64)
             (else  int32)))

(define unsigned-int (cond-expand
                      (lp32  uint16)
                      (ilp64 uint64)
                      (else  uint32)))

(define long (cond-expand
              (ilp64 int64)
              (lp64  int64)
              (else  int32)))

(define unsigned-long (cond-expand
                       (ilp64 uint64)
                       (lp64  uint64)
                       (else  uint32)))

(define long-long int64)
(define unsigned-long-long uint64)

(define arch32bit? (cond-expand
                    (lp32  #t)
                    (ilp32 #t)
                    (else  #f)))

(define intptr_t (if arch32bit?
                     int32
                     int64))

(define uintptr_t (if arch32bit?
                      uint32
                      uint64))

(define size_t uintptr_t)

(define ssize_t intptr_t)

(define ptrdiff_t intptr_t)

(define float float32)
(define double float64)

;;; numeric.scm ends here
