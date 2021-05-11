;;; bitfields.scm --- Struct bitfield constructor.

;; Copyright © 2015, 2016 Taylan Kammer <taylan.kammer@gmail.com>

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

;;; Only a macro for efficiency reasons.
(define-syntax bit-field/signed
  (syntax-rules ()
    ((_ <num> <width> <start> <end> <signed?>)
     (let ((unsigned-value (bit-field <num> <start> <end>)))
       (if (not <signed?>)
           unsigned-value
           (let ((sign (bit-set? (- <width> 1) unsigned-value)))
             (if sign
                 (- unsigned-value (expt 2 <width>))
                 unsigned-value)))))))

(define (validate-integer-descriptor descriptor)
  (when (not (assq descriptor integer-descriptors))
    (error "Invalid descriptor for bitfield." descriptor)))

(define (integer-descriptor-signed? descriptor)
  (assq descriptor signed-integer-descriptors))

(define integer-descriptor-signed->unsigned-mapping
  (map cons
       (map car signed-integer-descriptors)
       (map car unsigned-integer-descriptors)))

(define (integer-descriptor-signed->unsigned descriptor)
  (cdr (assq descriptor integer-descriptor-signed->unsigned-mapping)))

(define (unsigned-integer-descriptor integer-descriptor)
  (if (integer-descriptor-signed? integer-descriptor)
      (integer-descriptor-signed->unsigned integer-descriptor)
      integer-descriptor))

(define-record-type <bitfield-metadata>
  (make-bitfield-metadata int-descriptor width)
  bitfield-metadata?
  (int-descriptor bitfield-metadata-int-descriptor)
  (width          bitfield-metadata-width))

(define (bitfield-descriptor int-descriptor bit-offset width)
  (validate-integer-descriptor int-descriptor)
  (let ((signed? (integer-descriptor-signed? int-descriptor))
        (uint-descriptor (unsigned-integer-descriptor int-descriptor)))
    (let ((num-getter (bytestructure-descriptor-getter uint-descriptor))
          (num-setter (bytestructure-descriptor-setter uint-descriptor)))
      (define start bit-offset)
      (define end (+ start width))
      (define (getter syntax? bytevector offset)
        (let ((num (num-getter syntax? bytevector offset)))
          (if syntax?
              (quasisyntax
               (bit-field/signed (unsyntax num) (unsyntax width)
                                 (unsyntax start) (unsyntax end)
                                 (unsyntax signed?)))
              (bit-field/signed num width start end signed?))))
      (define (setter syntax? bytevector offset value)
        (let* ((oldnum (num-getter syntax? bytevector offset))
               (newnum (if syntax?
                           (quasisyntax
                            (copy-bit-field (unsyntax oldnum) (unsyntax value)
                                            (unsyntax start) (unsyntax end)))
                           (copy-bit-field oldnum value start end))))
          (num-setter syntax? bytevector offset newnum)))
      (define meta (make-bitfield-metadata int-descriptor width))
      (make-bytestructure-descriptor #f #f #f getter setter meta))))

;;; bitfields.scm ends here
