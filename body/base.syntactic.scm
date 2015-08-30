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

;; This is an extension to the base of the module which allows using the API
;; purely in the macro-expand phase, which puts some limitations on its use but
;; reduces run-time overhead to zero or nearly zero.


;;; Code:

(define-syntax-rule (syntax-case-lambda <stx> <pattern> <body>)
  (lambda (<stx>)
    (syntax-case <stx> ()
      ((_ . <pattern>)
       <body>))))

(define (syntactic-ref-helper bytevector offset descriptor indices)
  (define (syntax-car stx)
    (syntax-case stx () ((car . cdr) #'car)))
  (define (syntax-cdr stx)
    (syntax-case stx () ((car . cdr) #'cdr)))
  (define (syntax-null? stx)
    (syntax-case stx () (() #t) (_ #f)))
  (let loop ((bytevector bytevector)
             (offset offset)
             (descriptor descriptor)
             (indices indices))
    (if (not (syntax-null? indices))
        (let ((ref-helper (bd-ref-helper descriptor)))
          (when (not ref-helper)
            (error "Cannot index through this descriptor." descriptor))
          (let-values (((bytevector* offset* descriptor*)
                        (ref-helper #t bytevector offset (syntax-car indices))))
            (loop bytevector* offset* descriptor* (syntax-cdr indices))))
        (let ((reffer (bd-reffer descriptor))
              (setter (bd-setter descriptor)))
          (values bytevector offset descriptor reffer setter)))))

(define (bytestructure-ref-helper/syntax bytevector offset descriptor indices)
  (let-values (((bytevector* offset* _descriptor _reffer _setter)
                (syntactic-ref-helper bytevector offset descriptor indices)))
    #`(values #,bytevector* #,offset*)))

(define (bytestructure-ref/syntax bytevector offset descriptor indices)
  (let-values (((bytevector* offset* descriptor* reffer _setter)
                (syntactic-ref-helper bytevector offset descriptor indices)))
    (if reffer
        (reffer #t bytevector* offset*)
        (let ((size (bytestructure-descriptor-size/syntax
                     bytevector* offset* descriptor*)))
          #`(let ((bv (make-bytevector #,size)))
              (bytevector-copy! bv 0 #,bytevector* #,offset* #,size)
              bv)))))

(define (bytestructure-set!/syntax bytevector offset descriptor indices value)
  (let-values (((bytevector* offset* descriptor* _reffer setter)
                (syntactic-ref-helper bytevector offset descriptor indices)))
    (if setter
        (setter #t bytevector* offset* value)
        (let ((size (bytestructure-descriptor-size/syntax
                     bytevector* offset* descriptor*)))
          #`(bytevector-copy! #,bytevector* #,offset* #,value 0 #,size)))))

(define-syntax-rule (define-bytestructure-ref-helper <name> <descriptor>)
  (define-syntax <name>
    (let ((descriptor <descriptor>))
      (syntax-case-lambda stx (<bytevector> <offset> . <indices>)
        (bytestructure-ref-helper/syntax
         #'<bytevector> #'<offset> descriptor #'<indices>)))))

(define-syntax-rule (define-bytestructure-reffer <name> <descriptor>)
  (define-syntax <name>
    (let ((descriptor <descriptor>))
      (syntax-case-lambda stx (<bytevector> . <indices>)
        (bytestructure-ref/syntax #'<bytevector> 0 descriptor #'<indices>)))))

(define-syntax-rule (define-bytestructure-setter <name> <descriptor>)
  (define-syntax <name>
    (let ((descriptor <descriptor>))
      (syntax-case-lambda stx (<bytevector> <index> (... ...) <value>)
        (bytestructure-set!/syntax
         #'<bytevector> 0 descriptor #'(<index> (... ...)) #'<value>)))))

(define-syntax-rule (define-bytestructure-accessors <descriptor>
                      <ref-helper> <reffer> <setter>)
  (define-bytestructure-ref-helper <ref-helper> <descriptor>)
  (define-bytestructure-reffer <reffer> <descriptor>)
  (define-bytestructure-setter <setter> <descriptor>))

;;; base.syntactic.scm ends here
