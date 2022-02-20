;;; bytestructures --- Structured access to bytevector contents.

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

;; This is an extension to the base of the module which allows using the API
;; purely in the macro-expand phase, which puts some limitations on its use but
;; reduces run-time overhead to zero or nearly zero.


;;; Code:

(define-syntax-rule (syntax-case-lambda <pattern> <body>)
  (lambda (stx)
    (syntax-case stx ()
      (<pattern> <body>))))

(define syntax-car (syntax-case-lambda (car . cdr) #'car))
(define syntax-cdr (syntax-case-lambda (car . cdr) #'cdr))
(define syntax-null? (syntax-case-lambda stx (null? (syntax->datum #'stx))))

(define (syntactic-unwrap bytevector offset descriptor indices)
  (let loop ((bytevector bytevector)
             (offset offset)
             (descriptor descriptor)
             (indices indices))
    (if (not (syntax-null? indices))
        (let ((unwrapper (bd-unwrapper descriptor)))
          (when (not unwrapper)
            (error "Cannot index through this descriptor." descriptor))
          (let-values (((bytevector* offset* descriptor*)
                        (unwrapper #t bytevector offset (syntax-car indices))))
            (loop bytevector* offset* descriptor* (syntax-cdr indices))))
        (let ((getter (bd-getter descriptor))
              (setter (bd-setter descriptor)))
          (values bytevector offset descriptor getter setter)))))

(define (bytestructure-unwrap/syntax bytevector offset descriptor indices)
  (let-values (((bytevector* offset* _descriptor _getter _setter)
                (syntactic-unwrap bytevector offset descriptor indices)))
    #`(values #,bytevector* #,offset*)))

(define (bytestructure-ref/syntax bytevector offset descriptor indices)
  (let-values (((bytevector* offset* descriptor* getter _setter)
                (syntactic-unwrap bytevector offset descriptor indices)))
    (if getter
        (getter #t bytevector* offset*)
        (error "The indices given to bytestructure-ref/syntax do not lead to a
bytestructure descriptor that can decode values.  You must have used the wrong
getter macro, forgot to provide some of the indices, or meant to use the
unwrapper instead of the getter.  The given indices follow." indices))))

(define (bytestructure-set!/syntax bytevector offset descriptor indices value)
  (let-values (((bytevector* offset* descriptor* _getter setter)
                (syntactic-unwrap bytevector offset descriptor indices)))
    (if setter
        (setter #t bytevector* offset* value)
        (error "The indices given to bytestructure-set!/syntax do not lead to a
bytestructure descriptor that can encode values.  You must have used the wrong
setter macro, or forgot to provide some of the indices.  The given indices
follow." indices))))

(define-syntax-rule (define-bytestructure-unwrapper <name> <descriptor>)
  (define-syntax <name>
    (let ((descriptor <descriptor>))
      (syntax-case-lambda (_ <bytevector> <offset> . <indices>)
        (bytestructure-unwrap/syntax
         #'<bytevector> #'<offset> descriptor #'<indices>)))))

(define-syntax-rule (define-bytestructure-getter* <name> <descriptor>)
  (define-syntax <name>
    (let ((descriptor <descriptor>))
      (syntax-case-lambda (_ <bytevector> <offset> . <indices>)
        (bytestructure-ref/syntax
         #'<bytevector> #'<offset> descriptor #'<indices>)))))

(define-syntax-rule (define-bytestructure-setter* <name> <descriptor>)
  (define-syntax <name>
    (let ((descriptor <descriptor>))
      (syntax-case-lambda (_ <bytevector> <offset> <index> (... ...) <value>)
        (bytestructure-set!/syntax
         #'<bytevector> #'<offset> descriptor #'(<index> (... ...)) #'<value>)))))

(define-syntax-rule (define-bytestructure-getter <name> <descriptor>)
  (define-syntax <name>
    (let ((descriptor <descriptor>))
      (syntax-case-lambda (_ <bytevector> . <indices>)
        (bytestructure-ref/syntax #'<bytevector> 0 descriptor #'<indices>)))))

(define-syntax-rule (define-bytestructure-setter <name> <descriptor>)
  (define-syntax <name>
    (let ((descriptor <descriptor>))
      (syntax-case-lambda (_ <bytevector> <index> (... ...) <value>)
        (bytestructure-set!/syntax
         #'<bytevector> 0 descriptor #'(<index> (... ...)) #'<value>)))))

(define-syntax define-bytestructure-accessors
  (syntax-rules ()
    ((_ <descriptor> <unwrapper> <getter> <setter>)
     (begin
       (define-bytestructure-unwrapper <unwrapper> <descriptor>)
       (define-bytestructure-getter <getter> <descriptor>)
       (define-bytestructure-setter <setter> <descriptor>)))
    ((_ <descriptor> <unwrapper> <getter> <setter> <getter*> <setter*>)
     (begin
       (define-bytestructure-unwrapper <unwrapper> <descriptor>)
       (define-bytestructure-getter <getter> <descriptor>)
       (define-bytestructure-setter <setter> <descriptor>)
       (define-bytestructure-getter* <getter*> <descriptor>)
       (define-bytestructure-setter* <setter*> <descriptor>)))))

;; Local Variables:
;; eval: (put (quote syntax-case-lambda) (quote scheme-indent-function) 1)
;; End:

;;; base.syntactic.scm ends here
