;;; pointer.scm --- Pointer descriptor constructor.

;; Copyright © 2015 Taylan Kammer <taylan.kammer@gmail.com>

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
(import
 (srfi :9)
 (bytestructures guile bytevectors)
 (bytestructures guile utils)
 (bytestructures guile base)
 (prefix (system foreign) ffi:))
(export
 bs:pointer
 pointer-metadata? pointer-metadata-content-descriptor
 )

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

(define (pointer-ref bytevector offset index content-size)
  (let* ((base-address (bytevector-address-ref bytevector offset))
         (address (+ base-address (* index content-size))))
    (if (zero? base-address)
        (error "Tried to dereference null-pointer.")
        (ffi:pointer->bytevector (ffi:make-pointer address) content-size))))

(define (pointer-set! bytevector offset value)
  (cond
   ((exact-integer? value)
    (bytevector-address-set! bytevector offset value))
   ((bytevector? value)
    (bytevector-address-set! bytevector offset
                             (ffi:pointer-address
                              (ffi:bytevector->pointer value))))
   ((bytestructure? value)
    (bytevector-address-set! bytevector offset
                             (ffi:pointer-address
                              (ffi:bytevector->pointer
                               (bytestructure-bytevector value)))))))

(define-record-type <pointer-metadata>
  (make-pointer-metadata content-descriptor)
  pointer-metadata?
  (content-descriptor pointer-metadata-content-descriptor))

(define (bs:pointer %descriptor)
  (define (get-descriptor)
    (if (promise? %descriptor)
        (force %descriptor)
        %descriptor))
  (define size pointer-size)
  (define alignment size)
  (define (unwrapper syntax? bytevector offset index)
    (define (syntax-list id . elements)
      (datum->syntax id (map syntax->datum elements)))
    (let ((descriptor (get-descriptor)))
      (when (eq? 'void descriptor)
        (error "Tried to follow void pointer."))
      (let* ((size (bytestructure-descriptor-size descriptor))
             (index-datum (if syntax? (syntax->datum index) index))
             (index (if (eq? '* index-datum) 0 index-datum))
             (bytevector*
              (if syntax?
                  #`(pointer-ref #,bytevector #,offset #,index #,size)
                  (pointer-ref bytevector offset index size))))
        (values bytevector* 0 descriptor))))
  (define (getter syntax? bytevector offset)
    (if syntax?
        #`(bytevector-address-ref #,bytevector #,offset)
        (bytevector-address-ref bytevector offset)))
  (define (setter syntax? bytevector offset value)
    (if syntax?
        #`(pointer-set! #,bytevector #,offset #,value)
        (pointer-set! bytevector offset value)))
  (define meta (make-pointer-metadata %descriptor))
  (make-bytestructure-descriptor size alignment unwrapper getter setter meta))

;;; pointer.scm ends here
