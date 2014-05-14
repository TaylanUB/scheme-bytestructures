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

(define-module (bytestructures guile pointer)
  #:version (1 3 1)
  #:export (bs:pointer))

(use-modules (bytestructures r7 base)
             (srfi srfi-9)
             (rnrs bytevectors)
             ((system foreign) #:renamer (symbol-prefix-proc 'ffi:)))

(define-record-type <pointer>
  (%make-pointer content)
  pointer?
  (content %pointer-content))

(define (pointer-content pointer)
  (let ((content (%pointer-content pointer)))
    (if (promise? content) (force content) content)))

(define (make-pointer content-description)
  (%make-pointer
   (if (promise? content-description)
       (delay (make-bytestructure-descriptor (force content-description)))
       (make-bytestructure-descriptor content-description))))

(define (pointer-ref-helper bytevector offset pointer index)
  (let ((content (pointer-content pointer)))
    (let ((bytevector* (pointer-ref-bv bytevector offset pointer)))
      (if (eq? index '*)
          (values bytevector* 0 content)
          (bytestructure-ref-helper* bytevector* 0 content index)))))

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

(define (pointer-ref-bv bytevector offset pointer)
  (let ((address (bytevector-address-ref bytevector offset)))
    (if (zero? address)
        (error "Tried to dereference null-pointer.")
        (let ((content-size (bytestructure-descriptor-size
                             (pointer-content pointer))))
          (ffi:pointer->bytevector (ffi:make-pointer address)
                                   content-size)))))

(define (pointer-set-bv! bytevector offset pointer value)
  (cond
   ((and (pair? value) (null? (cdr value)))
    (let ((bytevector* (pointer-ref-bv bytevector offset pointer))
          (content (pointer-content pointer)))
      (bytestructure-set!* bytevector* 0 content (car value))))
   (else
    (bytevector-address-set!
     bytevector
     offset
     (ffi:pointer-address
      (cond
       ((ffi:pointer? value) value)
       ((bytevector? value) (ffi:bytevector->pointer value))
       (else (error "Pointer type failed to write:" value))))))))

(define bs:pointer
  (make-bytestructure-descriptor-type
   make-pointer (ffi:sizeof '*)
   pointer-ref-helper pointer-ref-bv pointer-set-bv!))

;;; pointer.scm ends here
