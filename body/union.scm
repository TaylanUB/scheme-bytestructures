;;; union.scm --- Union descriptor constructor.

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

;; This constructor allows the creation of union descriptors with named fields
;; with a specific content descriptor.


;;; Code:

(define make-field cons)
(define field-name car)
(define field-content cdr)
(define find-field assq)

(define (construct-fields fields)
  (map (lambda (field)
         (make-field (car field) (cadr field)))
       fields))

(define (bs:union %fields)
  (define fields (construct-fields %fields))
  (define size (apply max (map (lambda (field)
                                 (bytestructure-descriptor-size
                                  (field-content field)))
                               fields)))
  (define alignment size)
  (define (ref-helper syntax? bytevector offset index)
    (let ((index (if syntax? (syntax->datum index) index)))
      (values bytevector
              offset
              (field-content (find-field index fields)))))
  (define (setter syntax? bytevector offset value)
    (cond
     ((and (list? value) (= 2 (length value)))
      (let-values (((bytevector* offset* descriptor)
                    (ref-helper #f bytevector offset (car value))))
        (bytestructure-set!* bytevector* offset* descriptor (cadr value))))
     ((bytevector? value)
      (bytevector-copy! bytevector offset value 0 size))
     (else
      (error "Invalid value for writing into union." value))))
  (make-bytestructure-descriptor size alignment ref-helper #f setter))

;;; union.scm ends here
