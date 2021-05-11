;;; string.scm --- Strings in encodings supported by (rnrs bytevectors).

;; Copyright © 2017 Taylan Kammer <taylan.kammer@gmail.com>

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

(define (ascii->string bytevector start end)
  (let ((string (utf8->string bytevector start end)))
    (when (not (= (string-length string) (bytevector-length bytevector)))
      (error "Bytevector contains non-ASCII characters." bytevector))
    string))

(define (string->ascii string)
  (let ((bytevector (string->utf8 string)))
    (when (not (= (string-length string) (bytevector-length bytevector)))
      (error "String contains non-ASCII characters." string))
    bytevector))

(define (bytevector->string bytevector offset size encoding)
  (case encoding
    ((ascii) (ascii->string bytevector offset (+ offset size)))
    ((utf8) (utf8->string bytevector offset (+ offset size)))
    (else
     (let ((bytevector (bytevector-copy bytevector offset (+ offset size))))
       (case encoding
         ((utf16le) (utf16->string bytevector 'little #t))
         ((utf16be) (utf16->string bytevector 'big #t))
         ((utf32le) (utf32->string bytevector 'little #t))
         ((utf32be) (utf32->string bytevector 'big #t))
         (else (error "Unknown string encoding." encoding)))))))

(define (string->bytevector string encoding)
  (case encoding
    ((ascii) (string->ascii string))
    ((utf8) (string->utf8 string))
    ((utf16le) (string->utf16 string 'little))
    ((utf16be) (string->utf16 string 'big))
    ((utf32le) (string->utf32 string 'little))
    ((utf32be) (string->utf32 string 'big))))

;;; Note: because macro output may not contain raw symbols, we cannot output
;;; (quote foo) for raw symbol foo either, so there's no way to inject symbol
;;; literals into macro output.  Hence we inject references to the following
;;; variables instead.

(define ascii 'ascii)
(define utf8 'utf8)
(define utf16le 'utf16le)
(define utf16be 'utf16be)
(define utf32le 'utf32le)
(define utf32be 'utf32be)

;;; Make sure this returns a boolean and not any other type of value, as the
;;; output will be part of macro output.
(define (fixed-width-encoding? encoding)
  (not (not (memq encoding '(ascii utf32le utf32be)))))

(define (bytevector-zero! bv start end)
  (do ((i start (+ i 1)))
      ((= i end))
    (bytevector-u8-set! bv i #x00)))

(define (bs:string size encoding)
  (define alignment 1)
  (define (getter syntax? bytevector offset)
    (if syntax?
        (quasisyntax
         (bytevector->string (unsyntax bytevector)
                             (unsyntax offset)
                             (unsyntax size)
                             (unsyntax
                              (datum->syntax (syntax utf8) encoding))))
        (bytevector->string bytevector offset size encoding)))
  (define (setter syntax? bytevector offset string)
    (if syntax?
        (quasisyntax
         (let* ((bv (string->bytevector
                     (unsyntax string)
                     (unsyntax
                      (datum->syntax (syntax utf8) encoding))))
                (length (bytevector-length bv)))
           (when (> length (unsyntax size))
             (error "String too long." (unsyntax string)))
           (when (and (unsyntax (fixed-width-encoding? encoding))
                      (< length (unsyntax size)))
             (error "String too short." (unsyntax string)))
           (bytevector-copy! (unsyntax bytevector)
                             (unsyntax offset)
                             bv)
           (when (not (unsyntax (fixed-width-encoding? encoding)))
             (bytevector-zero! (unsyntax bytevector)
                               (+ (unsyntax offset) (bytevector-length bv))
                               (+ (unsyntax offset) (unsyntax size))))))
        (let* ((bv (string->bytevector string encoding))
               (length (bytevector-length bv)))
          (when (> length size)
            (error "String too long." string))
          (when (and (fixed-width-encoding? encoding) (< length size))
            (error "String too short." string))
          (bytevector-copy! bytevector offset bv)
          (when (not (fixed-width-encoding? encoding))
            (bytevector-zero! bytevector
                              (+ offset (bytevector-length bv))
                              (+ offset size))))))
  (make-bytestructure-descriptor size alignment #f getter setter))

;;; string.scm ends here
