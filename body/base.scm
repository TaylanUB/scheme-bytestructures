;;; bytestructures --- Structured access to bytevector contents.

;; Copyright (C) 2013 - 2015  Taylan Ulrich Bayırlı/Kammer

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

;; This is the base of the module, defining the data types and procedures that
;; make up the bytestructures framework.


;;; Code:

;;; Descriptors

(define-record-type <bytestructure-descriptor>
  (make-bytestructure-descriptor size ref-helper reffer setter)
  bytestructure-descriptor?
  (size       bd-size)
  (ref-helper bd-ref-helper)
  (reffer     bd-reffer)
  (setter     bd-setter))

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


;;; Bytestructures

(define-record-type <bytestructure>
  (make-bytestructure bytevector offset descriptor)
  bytestructure?
  (bytevector bytestructure-bytevector)
  (offset     bytestructure-offset)
  (descriptor bytestructure-descriptor))

(define bytestructure
  (case-lambda ((description)        (%bytestructure description #f #f))
               ((description values) (%bytestructure description #t values))))

(define (%bytestructure descriptor init? values)
  (let ((bytevector (make-bytevector
                     (bytestructure-descriptor-size descriptor))))
    (when init?
      (bytestructure-primitive-set! bytevector 0 descriptor values))
    (make-bytestructure bytevector 0 descriptor)))

(define-syntax-rule (bytestructure-ref-helper <bytestructure> <index> ...)
  (let ((bytestructure <bytestructure>))
    (let ((bytevector (bytestructure-bytevector bytestructure))
          (offset     (bytestructure-offset     bytestructure))
          (descriptor (bytestructure-descriptor bytestructure)))
      (bytestructure-ref-helper* bytevector offset descriptor <index> ...))))

(define-syntax bytestructure-ref-helper*
  (syntax-rules ()
    ((_ <bytevector> <offset> <descriptor>)
     (values <bytevector> <offset> <descriptor>))
    ((_ <bytevector> <offset> <descriptor> <index> <indices> ...)
     (let ((bytevector <bytevector>)
           (offset <offset>)
           (descriptor <descriptor>))
       (let ((ref-helper (bd-ref-helper descriptor)))
         (when (not ref-helper)
           (error "Cannot index through this descriptor." descriptor))
         (let-values (((bytevector* offset* descriptor*)
                       (ref-helper #f bytevector offset <index>)))
           (bytestructure-ref-helper*
            bytevector* offset* descriptor* <indices> ...)))))))

(define-syntax-rule (bytestructure-ref <bytestructure> <index> ...)
  (let-values (((bytevector offset descriptor)
                (bytestructure-ref-helper <bytestructure> <index> ...)))
    (bytestructure-primitive-ref bytevector offset descriptor)))

(define-syntax-rule (bytestructure-ref*
                     <bytevector> <offset> <descriptor> <index> ...)
  (let-values (((bytevector offset descriptor)
                (bytestructure-ref-helper*
                 <bytevector> <offset> <descriptor> <index> ...)))
    (bytestructure-primitive-ref bytevector offset descriptor)))

(define (bytestructure-primitive-ref bytevector offset descriptor)
  (let ((reffer (bd-reffer descriptor)))
    (if reffer
        (reffer #f bytevector offset)
        (make-bytestructure bytevector offset descriptor))))

(define-syntax-rule (bytestructure-set! <bytestructure> <index> ... <value>)
  (let-values (((bytevector offset descriptor)
                (bytestructure-ref-helper <bytestructure> <index> ...)))
    (bytestructure-primitive-set! bytevector offset descriptor <value>)))

(define-syntax-rule (bytestructure-set!*
                     <bytevector> <offset> <descriptor> <index> ... <value>)
  (let-values (((bytevector offset descriptor)
                (bytestructure-ref-helper*
                 <bytevector> <offset> <descriptor> <index> ...)))
    (bytestructure-primitive-set! bytevector offset descriptor <value>)))

(define (bytestructure-primitive-set! bytevector offset descriptor value)
  (let ((setter (bd-setter descriptor)))
    (if setter
        (setter #f bytevector offset value)
        (if (bytevector? value)
            (bytevector-copy! bytevector offset value 0
                              (bytestructure-descriptor-size
                               bytevector offset descriptor))
            (error "Cannot write value with this bytestructure descriptor."
                   value descriptor)))))

(cond-expand
 (guile

  (include-from-path "bytestructures/body/base.syntactic.scm")

  )
 ((or syntax-case)               ;redundant 'or' for proper indentation by Emacs

  (include "base.syntactic.scm")

  )
 (else

  (define-syntax-rule (define-syntax-case-stubs <name> ...)
    (define-syntax-rule (<name> . rest)
      (syntax-error "Not implemented.  You need syntax-case."))
    ...)

  (define-syntax-case-stubs
    bytestructure-ref-helper/syntax
    bytestructure-ref/syntax
    bytestructure-set!/syntax
    define-bytestructure-accessors)

  ))

;;; base.scm ends here
