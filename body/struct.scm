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

(define field-name car)
(define field-content cdr)
(define field-find assq)

(define (construct-fields fields)
  (map (lambda (field)
         (cons (car field)
               (make-bytestructure-descriptor (cadr field))))
       fields))

(define-record-type <struct>
  (%make-struct fields size)
  struct?
  (fields struct-fields)
  (size   %struct-size))

(define (make-struct . fields)
  (let ((fields (construct-fields fields)))
    (%make-struct fields (apply + (map (lambda (field)
                                         (bytestructure-descriptor-size
                                          (field-content field)))
                                       fields)))))

(define (struct-size bytevector offset struct)
  (%struct-size struct))

(define (struct-ref-helper bytevector offset struct key)
  (let ((fields (struct-fields struct)))
    (let lp ((fields fields)
             (offset offset))
      (when (null? fields)
        (error "No such struct field:" key))
      (let ((field (car fields)))
        (if (eq? (field-name field) key)
            (values bytevector offset (field-content field))
            (lp (cdr fields)
                (+ offset
                   (bytestructure-descriptor-size
                    (field-content field)))))))))

(define/sc (struct-ref-helper/syntax offset struct key)
  (let ((fields (struct-fields struct))
        (key (syntax->datum key)))
    (let lp ((fields fields)
             (offset offset))
      (when (null? fields)
        (error "No such struct field:" key))
      (let ((field (car fields)))
        (if (eq? (field-name field) key)
            (values offset (field-content field))
            (lp (cdr fields)
                (quasisyntax
                 (+ (unsyntax offset)
                    (unsyntax
                     (bytestructure-descriptor-size
                      (field-content field)))))))))))

(define (alist? list)
  (or (null? list)
      (and (pair? list)
           (pair? (car list))
           (symbol? (caar list))
           (alist? (cdr list)))))

(define (struct-set! bytevector offset struct values)
  (cond
   ((vector? values)
    (let lp ((values (vector->list values))
             (fields (struct-fields struct))
             (offset offset)
             (index 0))
      (unless (null? values)
        (when (null? fields)
          (error "Struct field index out of bounds:" index))
        (let ((content (field-content (car fields))))
          (bytestructure-set!* bytevector offset content (car values))
          (lp (cdr values)
              (cdr fields)
              (+ offset (bytestructure-descriptor-size content))
              (+ 1 index))))))
   ((alist? values)
    (for-each
     (lambda (pair)
       (let ((key (car pair))
             (value (cdr pair)))
         (let-values (((bytevector offset descriptor)
                       (struct-ref-helper bytevector offset struct key)))
           (bytestructure-set!* bytevector offset descriptor value))))
     values))
   ((bytevector? values)
    (bytevector-copy! bytevector offset values 0 (%struct-size struct)))
   (else
    (error "Struct type failed to write:" values))))

(define bs:struct
  (make-bytestructure-descriptor-type
   make-struct struct-size
   struct-ref-helper #f struct-set!
   struct-ref-helper/syntax #f #f))

;;; struct.scm ends here
