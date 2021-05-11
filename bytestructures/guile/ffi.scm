;;; ffi.scm --- Convert bytestructure descriptors to Guile/libffi types.

;; Copyright © 2016 Taylan Kammer <taylan.kammer@gmail.com>

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

;; This module offers a way to convert bytestructure descriptors to Guile/libffi
;; type objects.  For instance, the bytestructure descriptor created with
;; (bs:struct `((x ,uint8) (y ,uint16))) gets converted into a two-element list
;; containing the libffi codes for uint8 and uint16.


;;; Code:

(define-module (bytestructures guile ffi))
(import
 (ice-9 match)
 (prefix (system foreign) ffi:)
 (bytestructures guile base)
 (bytestructures guile numeric)
 (bytestructures guile vector)
 (bytestructures guile struct)
 (bytestructures guile union)
 (bytestructures guile pointer)
 (bytestructures guile bitfields))
(export
 bytestructure-descriptor->ffi-descriptor
 bs:pointer->proc
 )

(define numeric-type-mapping
  `((,int8 . ,ffi:int8)
    (,uint8 . ,ffi:uint8)
    (,int16 . ,ffi:int16)
    (,uint16 . ,ffi:uint16)
    (,int32 . ,ffi:int32)
    (,uint32 . ,ffi:uint32)
    (,int64 . ,ffi:int64)
    (,uint64 . ,ffi:uint64)
    (,float32 . ,ffi:float)
    (,float64 . ,ffi:double)))

(define (bytestructure-descriptor->ffi-descriptor descriptor)
  (define (convert descriptor)
    (cond
     ((assq descriptor numeric-type-mapping)
      => (match-lambda ((key . val) val)))
     (else
      (let ((metadata (bytestructure-descriptor-metadata descriptor)))
        (cond
         ((vector-metadata? metadata)
          (make-list
           (vector-metadata-length metadata)
           (convert (vector-metadata-element-descriptor metadata))))
         ((struct-metadata? metadata)
          (map convert (map cdr (struct-metadata-field-alist metadata))))
         ((union-metadata? metadata)
          ;; TODO: Add support once Guile/libffi supports this.
          (error "Unions not supported." descriptor))
         ((pointer-metadata? metadata)
          '*)
         ((bitfield-metadata? metadata)
          ;; TODO: Add support once Guile/libffi supports this.
          (error "Bitfields not supported." descriptor))
         (else
          (error "Unsupported bytestructure descriptor." descriptor)))))))
  (cond
   ((eq? descriptor 'void)
    ffi:void)
   ((vector-metadata? (bytestructure-descriptor-metadata descriptor))
    '*)
   (else
    (convert descriptor))))

(define (bs:pointer->proc ret-type func-ptr arg-types)
  (define (type->raw-type type)
    (if (bytestructure-descriptor? type)
        (bytestructure-descriptor->ffi-descriptor type)
        type))
  (define (value->raw-value value)
    (if (bytestructure? value)
        (ffi:bytevector->pointer (bytestructure-bytevector value))
        value))
  (define (raw-value->value raw-value type)
    (if (bytestructure-descriptor? type)
        (make-bytestructure (ffi:pointer->bytevector
                             raw-value
                             (bytestructure-descriptor-size type))
                            0
                            type)
        raw-value))
  (let* ((raw-ret-type (type->raw-type ret-type))
         (raw-arg-types (map type->raw-type arg-types))
         (raw-proc (ffi:pointer->procedure
                    raw-ret-type func-ptr raw-arg-types)))
    (lambda args
      (let* ((raw-args (map value->raw-value args))
             (raw-ret-val (apply raw-proc raw-args)))
        (raw-value->value raw-ret-val ret-type)))))
