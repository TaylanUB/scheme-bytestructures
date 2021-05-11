;;; bytestructures --- Structured access to bytevector contents.

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

;; This is the base of the module, defining the data types and procedures that
;; make up the bytestructures framework.


;;; Code:

;;; Descriptors

(define-record-type <bytestructure-descriptor>
  (%make-bytestructure-descriptor size alignment unwrapper getter setter meta)
  bytestructure-descriptor?
  (size       bd-size)
  (alignment  bd-alignment)
  (unwrapper  bd-unwrapper)
  (getter     bd-getter)
  (setter     bd-setter)
  (meta       bd-meta))

(define make-bytestructure-descriptor
  (case-lambda
    ((size alignment unwrapper getter setter)
     (%make-bytestructure-descriptor
      size alignment unwrapper getter setter #f))
    ((size alignment unwrapper getter setter meta)
     (%make-bytestructure-descriptor
      size alignment unwrapper getter setter meta))))

(define bytestructure-descriptor-size
  (case-lambda
    ((descriptor) (bytestructure-descriptor-size descriptor #f #f))
    ((descriptor bytevector offset)
     (let ((size (bd-size descriptor)))
       (if (procedure? size)
           (size #f bytevector offset)
           size)))))

(define (bytestructure-descriptor-size/syntax bytevector offset descriptor)
  (let ((size (bd-size descriptor)))
    (if (procedure? size)
        (size #t bytevector offset)
        size)))

(define bytestructure-descriptor-alignment bd-alignment)
(define bytestructure-descriptor-unwrapper bd-unwrapper)
(define bytestructure-descriptor-getter bd-getter)
(define bytestructure-descriptor-setter bd-setter)
(define bytestructure-descriptor-metadata bd-meta)


;;; Bytestructures

(define-record-type <bytestructure>
  (make-bytestructure bytevector offset descriptor)
  bytestructure?
  (bytevector bytestructure-bytevector)
  (offset     bytestructure-offset)
  (descriptor bytestructure-descriptor))

(define bytestructure
  (case-lambda ((descriptor)        (%bytestructure descriptor #f #f))
               ((descriptor values) (%bytestructure descriptor #t values))))

(define (%bytestructure descriptor init? values)
  (let ((bytevector (make-bytevector
                     (bytestructure-descriptor-size descriptor))))
    (when init?
      (bytestructure-primitive-set! bytevector 0 descriptor values))
    (make-bytestructure bytevector 0 descriptor)))

(define (bytestructure-size bytestructure)
  (bytestructure-descriptor-size (bytestructure-descriptor bytestructure)
                                 (bytestructure-bytevector bytestructure)
                                 (bytestructure-offset bytestructure)))

(define-syntax-rule (bytestructure-unwrap <bytestructure> <index> ...)
  (let ((bytestructure <bytestructure>))
    (let ((bytevector (bytestructure-bytevector bytestructure))
          (offset     (bytestructure-offset     bytestructure))
          (descriptor (bytestructure-descriptor bytestructure)))
      (bytestructure-unwrap* bytevector offset descriptor <index> ...))))

(define-syntax bytestructure-unwrap*
  (syntax-rules ()
    ((_ <bytevector> <offset> <descriptor>)
     (values <bytevector> <offset> <descriptor>))
    ((_ <bytevector> <offset> <descriptor> <index> <indices> ...)
     (let ((bytevector <bytevector>)
           (offset <offset>)
           (descriptor <descriptor>))
       (let ((unwrapper (bd-unwrapper descriptor)))
         (when (not unwrapper)
           (error "Cannot index through this descriptor." descriptor))
         (let-values (((bytevector* offset* descriptor*)
                       (unwrapper #f bytevector offset <index>)))
           (bytestructure-unwrap*
            bytevector* offset* descriptor* <indices> ...)))))))

(define-syntax-rule (bytestructure-ref <bytestructure> <index> ...)
  (let-values (((bytevector offset descriptor)
                (bytestructure-unwrap <bytestructure> <index> ...)))
    (bytestructure-primitive-ref bytevector offset descriptor)))

(define-syntax-rule (bytestructure-ref*
                     <bytevector> <offset> <descriptor> <index> ...)
  (let-values (((bytevector offset descriptor)
                (bytestructure-unwrap*
                 <bytevector> <offset> <descriptor> <index> ...)))
    (bytestructure-primitive-ref bytevector offset descriptor)))

(define (bytestructure-primitive-ref bytevector offset descriptor)
  (let ((getter (bd-getter descriptor)))
    (if getter
        (getter #f bytevector offset)
        (make-bytestructure bytevector offset descriptor))))

(define-syntax-rule (bytestructure-set! <bytestructure> <index> ... <value>)
  (let-values (((bytevector offset descriptor)
                (bytestructure-unwrap <bytestructure> <index> ...)))
    (bytestructure-primitive-set! bytevector offset descriptor <value>)))

(define-syntax-rule (bytestructure-set!*
                     <bytevector> <offset> <descriptor> <index> ... <value>)
  (let-values (((bytevector offset descriptor)
                (bytestructure-unwrap*
                 <bytevector> <offset> <descriptor> <index> ...)))
    (bytestructure-primitive-set! bytevector offset descriptor <value>)))

(define (bytestructure-primitive-set! bytevector offset descriptor value)
  (let ((setter (bd-setter descriptor)))
    (if setter
        (setter #f bytevector offset value)
        (if (bytevector? value)
            (bytevector-copy! bytevector offset value 0
                              (bytestructure-descriptor-size
                               descriptor bytevector offset))
            (error "Cannot write value with this bytestructure descriptor."
                   value descriptor)))))

(define (bytestructure-ref/dynamic bytestructure . indices)
  (let-values (((bytevector offset descriptor)
                (bytestructure-unwrap bytestructure)))
    (let loop ((bytevector bytevector)
               (offset offset)
               (descriptor descriptor)
               (indices indices))
      (if (null? indices)
          (bytestructure-primitive-ref bytevector offset descriptor)
          (let-values (((bytevector* offset* descriptor*)
                        (bytestructure-unwrap*
                         bytevector offset descriptor (car indices))))
            (loop bytevector*
                  offset*
                  descriptor*
                  (cdr indices)))))))

(define (bytestructure-set!/dynamic bytestructure . args)
  (let-values (((bytevector offset descriptor)
                (bytestructure-unwrap bytestructure)))
    (let loop ((bytevector bytevector)
               (offset offset)
               (descriptor descriptor)
               (args args))
      (if (null? (cdr args))
          (bytestructure-primitive-set! bytevector offset descriptor (car args))
          (let-values (((bytevector* offset* descriptor*)
                        (bytestructure-unwrap*
                         bytevector offset descriptor (car args))))
            (loop bytevector*
                  offset*
                  descriptor*
                  (cdr args)))))))

(define-syntax-case-stubs
  bytestructure-unwrap/syntax
  bytestructure-ref/syntax
  bytestructure-set!/syntax
  define-bytestructure-accessors)

(cond-expand
 (guile       (include-from-path "bytestructures/body/base.syntactic.scm"))
 (syntax-case (include "base.syntactic.scm"))
 (else))

;;; base.scm ends here
