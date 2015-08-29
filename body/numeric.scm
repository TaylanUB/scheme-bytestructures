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
;;
;; If the R6RS bytevector API is supported, all the types are defined; otherwise
;; we can only define uint8.


;;; Code:

(define-syntax define-numeric-types
  (syntax-rules ()
    ((_ (name size ref-proc set-proc) ...)
     (begin
       (define name
         (make-bytestructure-descriptor
          size
          #f
          (lambda (syntax? bytevector offset)
            (if syntax?
                (quasisyntax
                 (ref-proc (unsyntax bytevector) (unsyntax offset)))
                (ref-proc bytevector offset)))
          (lambda (syntax? bytevector offset value)
            (if syntax?
                (quasisyntax
                 (set-proc (unsyntax bytevector) (unsyntax offset)
                           (unsyntax value)))
                (set-proc bytevector offset value)))))
       ...))))

(define-numeric-types
  (uint8 1 bytevector-u8-ref bytevector-u8-set!))

(cond-expand
 ((or guile
      (library (rnrs bytevectors))
      (library (r6rs bytevectors)))

  (define-numeric-types
    (float
     4 bytevector-ieee-single-native-ref bytevector-ieee-single-native-set!)
    (double
     8 bytevector-ieee-double-native-ref bytevector-ieee-double-native-set!)
    (int8   1 bytevector-s8-ref bytevector-s8-set!)
    (int16  2 bytevector-s16-native-ref bytevector-s16-native-set!)
    (uint16 2 bytevector-u16-native-ref bytevector-u16-native-set!)
    (int32  4 bytevector-s32-native-ref bytevector-s32-native-set!)
    (uint32 4 bytevector-u32-native-ref bytevector-u32-native-set!)
    (int64  8 bytevector-s64-native-ref bytevector-s64-native-set!)
    (uint64 8 bytevector-u64-native-ref bytevector-u64-native-set!))

  (define-syntax define-with-endianness
    (syntax-rules ()
      ((_ (name native-name size ref-proc set-proc endianness) ...)
       (begin
         (define name
           (if (equal? endianness native-endianness)
               native-name
               (make-bytestructure-descriptor
                size
                #f
                (lambda (syntax? bytevector offset)
                  (if syntax?
                      (quasisyntax
                       (ref-proc (unsyntax bytevector) (unsyntax offset)))
                      (ref-proc bytevector offset)))
                (lambda (syntax? bytevector offset value)
                  (if syntax?
                      (quasisyntax
                       (set-proc (unsyntax bytevector) (unsyntax offset)
                                 (unsyntax value)))
                      (set-proc bytevector offset value))))))
         ...))))

  (define-syntax define-with-endianness*
    (syntax-rules ()
      ((_ (native-name size
                       le-name le-ref-proc le-set-proc
                       be-name be-ref-proc be-set-proc) ...)
       (begin
         (define-with-endianness
           (le-name native-name size
                    le-ref-proc le-set-proc (endianness little))
           (be-name native-name size
                    be-ref-proc be-set-proc (endianness big)))
         ...))))

  (define-with-endianness*
    (float 4
           floatle bytevector-ieee-single-le-ref bytevector-ieee-single-le-set!
           floatbe bytevector-ieee-single-be-ref bytevector-ieee-single-be-set!)
    (double
     8
     doublele bytevector-ieee-double-le-ref bytevector-ieee-double-le-set!
     doublebe bytevector-ieee-double-be-ref bytevector-ieee-double-be-set!)
    (int16 2
           int16le bytevector-s16le-ref bytevector-s16le-set!
           int16be bytevector-s16be-ref bytevector-s16be-set!)
    (uint16 2
            uint16le bytevector-u16le-ref bytevector-u16le-set!
            uint16be bytevector-u16be-ref bytevector-u16be-set!)
    (int32 4
           int32le bytevector-s32le-ref bytevector-s32le-set!
           int32be bytevector-s32be-ref bytevector-s32be-set!)
    (uint32 4
            uint32le bytevector-u32le-ref bytevector-u32le-set!
            uint32be bytevector-u32be-ref bytevector-u32be-set!)
    (int64 8
           int64le bytevector-s64le-ref bytevector-s64le-set!
           int64be bytevector-s64be-ref bytevector-s64be-set!)
    (uint64 8
            uint64le bytevector-u64le-ref bytevector-u64le-set!
            uint64be bytevector-u64be-ref bytevector-u64be-set!))

  )
 (else
  ))

;;; numeric.scm ends here
