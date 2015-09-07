;;; pointer.scm --- Pointer descriptor constructor.

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

;; This constructor allows the creation of pointer descriptors with a specific
;; pointed-to descriptor.


;;; Code:

(define-module (bytestructures guile pointer))
(export bs:pointer)
(import
 (bytestructures guile base)
 (srfi :9)
 (bytestructures guile utils)
 (bytestructures bytevectors)
 (prefix (system foreign) ffi:))

(define pointer-size (ffi:sizeof '*))

(define bytevector-address-ref
  (case pointer-size
    ((1) bytevector-u8-ref)
    ((2) bytevector-u16-native-ref)
    ((4) bytevector-u32-native-ref)
    ((8) bytevector-u64-native-ref)))

(define bytevector-address-set!
  (case pointer-size
    ((1) bytevector-u8-set!)
    ((2) bytevector-u16-native-set!)
    ((4) bytevector-u32-native-set!)
    ((8) bytevector-u64-native-set!)))

(define (%pointer-ref bytevector offset content-size)
  (let ((address (bytevector-address-ref bytevector offset)))
    (if (zero? address)
        (error "Tried to dereference null-pointer.")
        (ffi:pointer->bytevector (ffi:make-pointer address) content-size))))

(define (%pointer-set! bytevector offset value)
  (let ((address (cond
                  ((integer? value)
                   value)
                  ((ffi:pointer? value)
                   (ffi:pointer-address value))
                  ((bytevector? value)
                   (ffi:pointer-address (ffi:bytevector->pointer value)))
                  (else
                   (error "Pointer type failed to write:" value)))))
    (bytevector-address-set! bytevector offset address)))

(define (bs:pointer %descriptor)
  (define (get-descriptor)
    (if (promise? %descriptor)
        (force %descriptor)
        %descriptor))
  (define size pointer-size)
  (define alignment size)
  (define (ref-helper syntax? bytevector offset index)
    (define (syntax-list id . elements)
      (datum->syntax id (map syntax->datum elements)))
    (let ((descriptor (get-descriptor))
          (bytevector* (getter syntax? bytevector offset)))
      (if (eq? '* (if syntax? (syntax->datum index) index))
          (values bytevector* 0 descriptor)
          (if syntax?
              (bytestructure-ref-helper/syntax
               bytevector* 0 descriptor (syntax-list index))
              (bytestructure-ref-helper*
               bytevector* 0 descriptor index)))))
  (define (getter syntax? bytevector offset)
    (let ((size (bytestructure-descriptor-size (get-descriptor))))
      (if syntax?
          #`(%pointer-ref #,bytevector #,offset #,size)
          (%pointer-ref bytevector offset size))))
  (define (setter syntax? bytevector offset value)
    (define (syntax-car syntax)
      (datum->syntax syntax (car (syntax->datum syntax))))
    (let ((value-datum (if syntax? (syntax->datum value) value)))
      (if (and (pair? value-datum) (null? (cdr value-datum)))
          (let ((descriptor (get-descriptor))
                (bytevector* (getter syntax? bytevector offset)))
            (if syntax?
                (bytestructure-set!/syntax bytevector* 0 descriptor '()
                                           (syntax-car value))
                (bytestructure-set!* bytevector* 0 descriptor
                                     (car value))))
          (if syntax?
              #`(%pointer-set! #,bytevector #,offset #,value)
              (%pointer-set! bytevector offset value)))))
  (make-bytestructure-descriptor size alignment ref-helper getter setter))

;;; pointer.scm ends here
