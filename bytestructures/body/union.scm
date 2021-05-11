;;; union.scm --- Union descriptor constructor.

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

(define-record-type <union-metadata>
  (make-union-metadata field-alist)
  union-metadata?
  (field-alist union-metadata-field-alist))

(define (bs:union %fields)
  (define fields (construct-fields %fields))
  (define alignment (apply max (map (lambda (field)
                                      (bytestructure-descriptor-alignment
                                       (field-content field)))
                                    fields)))
  (define size (let ((max-element
                      (apply max (map (lambda (field)
                                        (bytestructure-descriptor-size
                                         (field-content field)))
                                      fields))))
                 (let-values (((size . _) (next-boundary max-element alignment)))
                   size)))
  (define (unwrapper syntax? bytevector offset index)
    (let ((index (if syntax? (syntax->datum index) index)))
      (values bytevector
              offset
              (field-content (find-field index fields)))))
  (define (setter syntax? bytevector offset value)
    (when syntax?
      (error "Writing into union not supported with macro API."))
    (cond
     ((bytevector? value)
      (bytevector-copy! bytevector offset value 0 size))
     ((and (list? value) (= 2 (length value)))
      (let-values (((bytevector* offset* descriptor)
                    (unwrapper #f bytevector offset (car value))))
        (bytestructure-set!* bytevector* offset* descriptor (cadr value))))
     (else
      (error "Invalid value for writing into union." value))))
  (define meta (make-union-metadata fields))
  (make-bytestructure-descriptor size alignment unwrapper #f setter meta))

;;; union.scm ends here
