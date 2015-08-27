;;; union.scm --- Union descriptor type.

;; Copyright (C) 2013 - 2015  Taylan Ulrich Bayırlı/Kammer

;; Author: Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;; Keywords: ffi struct bytevector bytestructure union

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

;; This descriptor type allows the creation of union descriptors with named
;; fields with a specific content descriptor.


;;; Code:

(define field-name car)
(define field-content cdr)
(define field-find assq)

(define (construct-fields fields)
  (map (lambda (field)
         (cons (car field)
               (make-bytestructure-descriptor (cadr field))))
       fields))

(define-record-type <union>
  (%make-union fields size)
  union?
  (fields union-fields)
  (size   %union-size))

(define (make-union . fields)
  (let ((fields (construct-fields fields)))
    (%make-union fields (apply max (map (lambda (field)
                                          (bytestructure-descriptor-size
                                           (field-content field)))
                                        fields)))))

(define (union-size bytevector offset union)
  (%union-size union))

(define (union-ref-helper bytevector offset union key)
  (values bytevector
          offset
          (field-content (field-find key (union-fields union)))))

(define/sc (union-ref-helper/syntax offset union key)
  (let ((key (syntax->datum key)))
    (values offset
            (field-content (field-find key (union-fields union))))))

(define (union-set! bytevector offset union values)
  (cond
   ((and (list? values) (= 2 (length values)))
    (let-values (((bytevector* offset* descriptor)
                  (union-ref-helper bytevector offset union (car values))))
      (bytestructure-set!* bytevector* offset* descriptor (cadr values))))
   ((bytevector? values)
    (bytevector-copy! bytevector offset values 0 (%union-size union)))
   (else
    (error "Union type failed to write:" values))))

(define bs:union
  (make-bytestructure-descriptor-type
   make-union union-size
   union-ref-helper #f union-set!
   union-ref-helper/syntax #f #f))

;;; union.scm ends here
