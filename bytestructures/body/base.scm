;;; bytestructures --- Structured access to bytevector contents.

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

;; This is the base of the module, defining the data types and procedures that
;; make up the bytestructures framework.


;;; Code:

;;; Descriptors

(define-record-type <bytestructure-descriptor>
  (make-bytestructure-descriptor size alignment ref-helper getter setter)
  bytestructure-descriptor?
  (size       bd-size)
  (alignment  bd-alignment)
  (ref-helper bd-ref-helper)
  (getter     bd-getter)
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

(define bytestructure-descriptor-alignment bd-alignment)
(define bytestructure-descriptor-ref-helper bd-ref-helper)
(define bytestructure-descriptor-getter bd-getter)
(define bytestructure-descriptor-setter bd-setter)


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
  (let ((getter (bd-getter descriptor)))
    (if getter
        (getter #f bytevector offset)
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

(define-syntax-case-stubs
  bytestructure-ref-helper/syntax
  bytestructure-ref/syntax
  bytestructure-set!/syntax
  define-bytestructure-accessors)

(cond-expand
 (guile       (include-from-path "bytestructures/body/base.syntactic.scm"))
 (syntax-case (include "base.syntactic.scm"))
 (else))

;;; base.scm ends here
