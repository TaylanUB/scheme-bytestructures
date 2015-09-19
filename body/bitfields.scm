;;; bitfields.scm --- Struct bitfield constructor.

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

;; This module is complementary to the struct module.  It isn't used on its own.

;; This code partly uses rational numbers for byte counts and offsets, to
;; represent granularity down to bits.  I.e. 1/8 is a size or offset of one bit.


;;; Code:

;;; Returns the bit-offset of 'position' from the nearest smaller multiple of
;;; 'alignment'.
(define (offsetof position alignment)
  (remainder (* 8 position) (* 8 alignment)))

;;; Only a macro for efficiency reasons.
(define-syntax bit-field/signed
  (syntax-rules ()
    ((_ <num> <size> <start> <end> <signed?>)
     (let ((unsigned-value (bit-field <num> <start> <end>)))
       (if (not <signed?>)
           unsigned-value
           (let ((sign (bit-set? unsigned-value (- <size> 1))))
             (if sign
                 (- unsigned-value (expt 2 <size>))
                 unsigned-value)))))))

(define (validate-integer-descriptor descriptor)
  (when (not (memq descriptor (list int8 int16 int32 int64
                                    uint8 uint16 uint32 uint64
                                    int16le int32le int64le
                                    uint16le uint32le uint64le
                                    int16be int32be int64be
                                    uint16be uint32be uint64be)))
    (error "Invalid descriptor for bitfield." descriptor)))

(define (integer-descriptor-signed? descriptor)
  (memq descriptor (list int8 int16 int32 int64
                         int16le int32le int64le
                         int16be int32be int64be)))

(define (bitfield-descriptor size position integer-descriptor)
  (validate-integer-descriptor integer-descriptor)
  (let ((alignment (bytestructure-descriptor-alignment integer-descriptor))
        (num-getter (bytestructure-descriptor-getter integer-descriptor))
        (num-setter (bytestructure-descriptor-setter integer-descriptor))
        (signed? (integer-descriptor-signed? integer-descriptor)))
    (define bit-size (* 8 size))
    (define bit-offset (offsetof position alignment))
    (define start bit-offset)
    (define end (+ start size))
    (define (getter syntax? bytevector offset)
      (let ((num (num-getter syntax? bytevector offset)))
        (if syntax?
            (quasisyntax
             (bit-field/signed (unsyntax num) (unsyntax size)
                               (unsyntax start) (unsyntax end)
                               (unsyntax signed?)))
            (bit-field/signed num size start end signed?))))
    (define (setter syntax? bytevector offset value)
      (let* ((oldnum (num-getter syntax? bytevector offset))
             (newnum (if syntax?
                         (quasisyntax
                          (copy-bit-field (unsyntax oldnum) (unsyntax value)
                                          (unsyntax start) (unsyntax end)))
                         (copy-bit-field oldnum value start end))))
        (num-setter syntax? bytevector offset newnum)))
    (make-bytestructure-descriptor size alignment #f getter setter)))

;;; bitfields.scm ends here
