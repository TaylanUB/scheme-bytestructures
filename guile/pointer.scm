;;; pointer.scm --- Pointer descriptor type.

;; Copyright (C) 2013  Taylan Ulrich Bayırlı/Kammer

;; Author: Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;; Keywords: ffi struct bytevector bytestructure pointer

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

;; This descriptor type allows the creation of pointer descriptors with a
;; specific pointed-to descriptor.


;;; Code:

(define-module (bytestructures guile pointer))
(export bs:pointer)
(import
 (bytestructures guile base)
 (srfi :9)
 (bytestructures guile utils)
 (bytestructures bytevectors)
 (prefix (system foreign) ffi:))

(define-record-type <pointer>
  (%make-pointer content)
  pointer?
  (content %pointer-content))

(define (pointer-content pointer)
  (let ((content (%pointer-content pointer)))
    (if (promise? content)
        (force content)
        content)))

(define (make-pointer content-description)
  (%make-pointer
   (if (promise? content-description)
       (delay (make-bytestructure-descriptor (force content-description)))
       (make-bytestructure-descriptor content-description))))

(define (pointer-ref-helper bytevector offset pointer index)
  (let ((content (pointer-content pointer))
        (bytevector* (pointer-ref bytevector offset pointer)))
    (if (eq? '* index)
        (values bytevector* 0 content)
        (bytestructure-ref-helper* bytevector* 0 content index))))

(define (syntax-list id . elements)
  (datum->syntax id (map syntax->datum elements)))

(define (pointer-ref-helper/syntax offset pointer index)
  (let ((content (pointer-content pointer)))
    (if (eq? '* (syntax->datum index))
        (values #'0 content)
        (bytestructure-ref-helper/syntax
         #'0 content (syntax-list index index)))))

(define bytevector-address-ref
  (case (ffi:sizeof '*)
    ((1) bytevector-u8-ref)
    ((2) bytevector-u16-native-ref)
    ((4) bytevector-u32-native-ref)
    ((8) bytevector-u64-native-ref)))

(define bytevector-address-set!
  (case (ffi:sizeof '*)
    ((1) bytevector-u8-set!)
    ((2) bytevector-u16-native-set!)
    ((4) bytevector-u32-native-set!)
    ((8) bytevector-u64-native-set!)))

(define (pointer-ref bytevector offset pointer)
  (let ((size (bytestructure-descriptor-size (pointer-content pointer))))
    (%pointer-ref bytevector offset size)))

(define (%pointer-ref bytevector offset content-size)
  (let ((address (bytevector-address-ref bytevector offset)))
    (if (zero? address)
        (error "Tried to dereference null-pointer.")
        (ffi:pointer->bytevector (ffi:make-pointer address) content-size))))

(define (pointer-set! bytevector offset pointer value)
  (if (and (pair? value) (null? (cdr value)))
      (let ((bytevector* (pointer-ref bytevector offset pointer))
            (content (pointer-content pointer)))
        (bytestructure-set!* bytevector* 0 content (car value)))
      (pointer-set-primitive! bytevector offset value)))

(define (pointer-set-primitive! bytevector offset value)
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

(cond-expand
 ((or guile syntax-case)

  (define (pointer-ref/syntax bytevector offset pointer)
    (let ((size (bytestructure-descriptor-size (pointer-content pointer))))
      #`(%pointer-ref bytevector offset #,size)))

  (define (pointer-set!/syntax bytevector offset pointer value)
    (let ((value-datum (syntax->datum value)))
      (if (and (pair? value-datum) (null? (cdr value-datum)))
          (let* ((content (pointer-content pointer))
                 (size (bytestructure-descriptor-size content))
                 (bytevector* #`(%pointer-ref #,bytevector #,offset #,size)))
            (bytestructure-set!/syntax bytevector* 0 content '()
                                       (datum->syntax value (car value-datum))))
          #`(pointer-set-primitive! #,bytevector #,offset #,value))))

  )
 (else

  (define (pointer-ref/syntax)
    (error "Not implemented.  You need syntax-case."))

  (define (pointer-set!/syntax)
    (error "Not implemented.  You need syntax-case."))

  ))

(define bs:pointer
  (make-bytestructure-descriptor-type
   make-pointer
   (ffi:sizeof '*) #f
   pointer-ref-helper pointer-ref pointer-set!
   pointer-ref-helper/syntax pointer-ref/syntax pointer-set!/syntax))

;;; pointer.scm ends here
