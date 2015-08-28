;;; struct.scm --- Struct descriptor type.

;; Copyright (C) 2013 - 2015  Taylan Ulrich Bayırlı/Kammer

;; Author: Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;; Keywords: ffi struct bytevector bytestructure struct

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

;; This descriptor type allows the creation of struct descriptors with named and
;; ordered fields with a specific content descriptor.


;;; Code:

(define (alist? list)
  (or (null? list)
      (and (pair? list)
           (pair? (car list))
           (symbol? (caar list))
           (alist? (cdr list)))))

(define make-field cons)
(define field-name car)
(define field-content cdr)

(define (construct-fields fields)
  (map (lambda (field)
         (make-field (car field) (cadr field)))
       fields))

(define (bs:struct %fields)
  (define fields (construct-fields %fields))
  (define size (apply + (map (lambda (field)
                               (bytestructure-descriptor-size
                                (field-content field)))
                             fields)))
  (define (ref-helper syntax? bytevector offset index)
    (let ((index (if syntax? (syntax->datum index) index)))
      (let loop ((fields fields)
                 (offset offset))
        (when (null? fields)
          (error "No such struct field." index))
        (let* ((field (car fields))
               (name (field-name field))
               (content (field-content field)))
          (if (eq? name index)
              (values bytevector offset content)
              (loop (cdr fields)
                    (if syntax?
                        (quasisyntax
                         (+ (unsyntax offset)
                            (unsyntax (bytestructure-descriptor-size content))))
                        (+ offset
                           (bytestructure-descriptor-size content)))))))))
  (define (setter bytevector offset value)
    (define (count-error fields values)
      (error "Mismatch between number of struct fields and given values."
             fields values))
    (cond
     ((vector? value)
      (let loop ((fields fields)
                 (values (vector->list value))
                 (offset offset))
        (if (null? values)
            (when (not (null? fields))
              (count-error fields value))
            (begin
              (when (null? fields)
                (count-error fields value))
              (let ((content (field-content (car fields))))
                (bytestructure-set!* bytevector offset content (car values))
                (loop (cdr fields)
                      (cdr values)
                      (+ offset (bytestructure-descriptor-size content))))))))
     ((alist? value)
      (for-each
       (lambda (pair)
         (let ((key (car pair))
               (value (cdr pair)))
           (let-values (((bytevector offset descriptor)
                         (ref-helper #f bytevector offset key)))
             (bytestructure-set!* bytevector offset descriptor value))))
       value))
     ((bytevector? value)
      (bytevector-copy! bytevector offset value 0 size))
     (else
      (error "Invalid value for writing into struct." value))))
  (make-bytestructure-descriptor size ref-helper #f setter))

;;; struct.scm ends here
