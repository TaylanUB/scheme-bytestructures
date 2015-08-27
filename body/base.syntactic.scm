;;; bytestructures --- Structured access to bytevector contents.

;; Copyright (C) 2015  Taylan Ulrich Bayırlı/Kammer

;; Author: Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;; Keywords: ffi struct bytevector bytestructure

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

;; This is an extension to the base of the module which allows using the API
;; purely in the macro-expand phase, which puts some limitations on its use but
;; reduces run-time overhead to zero or nearly zero.


;;; Code:

(define-syntax-rule (syntax-case-lambda <stx> <pattern> <body>)
  (lambda (<stx>)
    (syntax-case <stx> ()
      ((_ . <pattern>)
       <body>))))

(define (syntax-car syntax)
  (datum->syntax syntax (car (syntax->datum syntax))))

(define (syntax-cdr syntax)
  (datum->syntax syntax (cdr (syntax->datum syntax))))

(define (syntax-null? syntax)
  (null? (syntax->datum syntax)))

(define (syntactic-ref-helper offset descriptor indices)
  (let loop ((offset offset) (descriptor descriptor) (indices indices))
    (if (not (syntax-null? indices))
        (let ((content (bytestructure-descriptor-content descriptor))
              (ref-helper (bytevector-ref-helper/syntax descriptor)))
          (when (not ref-helper)
            (error "Cannot index through this descriptor:" descriptor))
          (let-values (((offset* descriptor*)
                        (ref-helper offset content (syntax-car indices))))
            (loop offset* descriptor* (syntax-cdr indices))))
        (let ((ref-proc (bytevector-ref-proc/syntax descriptor))
              (set-proc (bytevector-set-proc/syntax descriptor)))
          (values offset descriptor ref-proc set-proc)))))

(define (bytestructure-ref-helper/syntax offset descriptor indices)
  (let-values (((offset _descriptor _ref-proc _set-proc)
                (syntactic-ref-helper offset descriptor indices)))
    offset))

(define (bytestructure-ref/syntax bytevector offset descriptor indices)
  (let-values (((offset descriptor ref-proc _set-proc)
                (syntactic-ref-helper offset descriptor indices)))
    (if ref-proc
        (let ((content (bytestructure-descriptor-content descriptor)))
          (ref-proc bytevector offset content))
        (let ((size (bytestructure-descriptor-size descriptor)))
          #`(let ((bv (make-bytevector #,size)))
              (bytevector-copy! bv 0 #,bytevector #,offset #,size)
              bv)))))

(define (bytestructure-set!/syntax bytevector offset descriptor indices value)
  (let-values (((offset descriptor _ref-proc set-proc)
                (syntactic-ref-helper offset descriptor indices)))
    (if set-proc
        (let ((content (bytestructure-descriptor-content descriptor)))
          (set-proc bytevector offset content value))
        (let ((size (bytestructure-descriptor-size descriptor)))
          #`(bytevector-copy! #,bytevector #,offset #,value 0 #,size)))))

(define-syntax-rule (define-bytestructure-ref-helper <name> <description>)
  (define-syntax <name>
    (let ((descriptor (make-bytestructure-descriptor <description>)))
      (syntax-case-lambda stx <indices>
        (bytestructure-ref-helper/syntax #'0 descriptor #'<indices>)))))

(define-syntax-rule (define-bytestructure-reffer <name> <description>)
  (define-syntax <name>
    (let ((descriptor (make-bytestructure-descriptor <description>)))
      (syntax-case-lambda stx (<bytevector> . <indices>)
        (bytestructure-ref/syntax #'<bytevector> #'0 descriptor #'<indices>)))))

(define-syntax-rule (define-bytestructure-setter <name> <description>)
  (define-syntax <name>
    (let ((descriptor (make-bytestructure-descriptor <description>)))
      (syntax-case-lambda stx (<bytevector> <index> (... ...) <value>)
        (bytestructure-set!/syntax
         #'<bytevector> #'0 descriptor #'(<index> (... ...)) #'<value>)))))

(define-syntax-rule (define-bytestructure-accessors
                      <ref-helper> <reffer> <setter> <description>)
  (define-bytestructure-ref-helper <ref-helper> <description>)
  (define-bytestructure-reffer <reffer> <description>)
  (define-bytestructure-setter <setter> <description>))

;;; base.syntactic.scm ends here
