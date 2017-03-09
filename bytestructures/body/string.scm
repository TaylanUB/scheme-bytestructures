;;; string.scm --- Strings in encodings supported by (rnrs bytevectors).

;; Copyright © 2017 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>

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

;; This module defines descriptors for strings encoded in various encodings, as
;; supported by (rnrs bytevectors).


;;; Code:

(define (bytevector->string
         bytevector offset size encoding endianness endianness-mandatory?)
  (if (eq? encoding 'utf8)
      (utf8->string bytevector offset (+ offset size))
      (let ((bytevector (bytevector-copy bytevector offset (+ offset size))))
        (case encoding
          ((utf16) (utf16->string bytevector endianness endianness-mandatory?))
          ((utf32) (utf32->string bytevector endianness endianness-mandatory?))
          (else (error "Unknown string encoding." encoding))))))

(define (string->bytevector string encoding endianness)
  (case encoding
    ((utf8) (string->utf8 string))
    ((utf16) (string->utf16 string endianness))
    ((utf32) (string->utf32 string endianness))))

;;; Note: because macro output may not contain raw symbols, we cannot output
;;; (quote foo) for raw symbol foo either, so there's no way to inject symbol
;;; literals into macro output.  Hence we use symbol->string and inject the code
;;; (string->symbol "foo").
(define (bs:string size encoding endianness endianness-mandatory?)
  (let ((endianness (or endianness 'big)))
    (define alignment 1)
    (define encoding* (symbol->string encoding))
    (define endianness* (symbol->string endianness))
    (define (getter syntax? bytevector offset)
      (if syntax?
          (quasisyntax
           (bytevector->string (unsyntax bytevector)
                               (unsyntax offset)
                               (unsyntax size)
                               (string->symbol (unsyntax encoding*))
                               (string->symbol (unsyntax endianness*))
                               (unsyntax endianness-mandatory?)))
          (bytevector->string
           bytevector offset size encoding endianness endianness-mandatory?)))
    (define (setter syntax? bytevector offset string)
      (if syntax?
          (quasisyntax
           (bytevector-copy! (unsyntax bytevector)
                             (unsyntax offset)                 
                             (string->bytevector
                              (unsyntax string)
                              (string->symbol (unsyntax encoding*))
                              (string->symbol (unsyntax endianness*)))))
          (bytevector-copy!
           bytevector offset (string->bytevector string encoding endianness))))
    (make-bytestructure-descriptor size alignment #f getter setter)))

;;; string.scm ends here
