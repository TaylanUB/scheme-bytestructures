;;; numeric.scm --- Numeric types as supported by (rnrs bytevectors).

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

;; This module defines descriptors for numeric types of specific size, and
;; native or specific endianness, as made possible by the bytevector referencing
;; and assigning procedures in the (rnrs bytevectors) module.


;;; Code:

(define-syntax-rule (make-numeric-descriptor <size> <getter> <setter>)
  (let ()
    (define size <size>)
    (define alignment <size>)
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
  (double64
   8 bytevector-ieee-double-native-ref bytevector-ieee-double-native-set!))

(define-syntax-rule (define-with-endianness <list> <endianness>
                      (<name> <size> <native-name> <getter> <setter>)
                      ...)
  (begin
    (define <name>
      (if (equal? <endianness> native-endianness)
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
  (double64le
   8 double64 bytevector-ieee-double-le-ref bytevector-ieee-double-le-set!))

(define-with-endianness
  float-be-descriptors (endianness big)
  (float32be
   4 float32 bytevector-ieee-single-be-ref bytevector-ieee-single-be-set!)
  (double64be
   8 double64 bytevector-ieee-double-be-ref bytevector-ieee-double-be-set!))

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

(define numeric-descriptors
  (append integer-descriptors float-descriptors))

;;; numeric.scm ends here
