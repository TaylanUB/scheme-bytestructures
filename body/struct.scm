;;; struct.scm --- Struct descriptor constructor.

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

;; This constructor allows the creation of struct descriptors with named and
;; ordered fields with a specific content descriptor.


;;; Code:

(define (alist? list)
  (or (null? list)
      (and (pair? list)
           (pair? (car list))
           (symbol? (caar list))
           (alist? (cdr list)))))

(define (align size alignment)
  (let-values (((q r) (floor/ size alignment)))
    (if (zero? r)
        size
        (* alignment (+ 1 q)))))

(define-record-type <field>
  (make-field name descriptor size position)
  field?
  (name field-name)
  (descriptor field-descriptor)
  (size field-size)
  (position field-position))

(define (construct-fields align? field-specs)
  (if (null? (cdr field-specs))
      (let* ((field-spec (car field-specs))
             (name (car field-spec))
             (descriptor (cadr field-spec))
             (size (bytestructure-descriptor-size descriptor)))
        (make-field name descriptor size 0))
      (let loop ((field-spec (car field-specs))
                 (next (cadr field-specs))
                 (field-specs (cddr field-specs))
                 (position 0)
                 (fields '()))
        (let* ((name (car field-spec))
               (descriptor (cadr field-spec))
               (size (bytestructure-descriptor-size descriptor))
               (nname (car next))
               (ndescriptor (cadr next))
               (nsize (bytestructure-descriptor-size ndescriptor))
               (nalignment (bytestructure-descriptor-alignment ndescriptor))
               (nposition (if align?
                              (align (+ position size) nalignment)
                              (+ position size))))
          (let* ((field (make-field name descriptor size position))
                 (fields (cons field fields)))
            (if (null? field-specs)
                (let* ((nfield (make-field nname ndescriptor nsize nposition))
                       (fields (cons nfield fields)))
                  (reverse fields))
                (loop next
                      (car field-specs)
                      (cdr field-specs)
                      nposition
                      fields)))))))

(define (bs:struct align? field-specs)
  (define fields (construct-fields align? field-specs))
  (define alignment (if align?
                        (apply max (map field-size fields))
                        1))
  (define size (align (apply + (map field-size fields)) alignment))
  (define (ref-helper syntax? bytevector offset index)
    (let* ((index (if syntax? (syntax->datum index) index))
           (field (find (lambda (field)
                          (eq? index (field-name field)))
                        fields)))
      (when (not field)
        (error "No such struct field." index))
      (let* ((descriptor (field-descriptor field))
             (position (field-position field))
             (offset (if syntax?
                         (quasisyntax (+ (unsyntax offset) (unsyntax position)))
                         (+ offset position))))
        (values bytevector offset descriptor))))
  (define (setter syntax? bytevector offset value)
    (define (count-error fields values)
      (error "Mismatch between number of struct fields and given values."
             fields values))
    (cond
     ((bytevector? value)
      (bytevector-copy! bytevector offset value 0 size))
     ((vector? value)
      (let loop ((fields fields)
                 (values (vector->list value)))
        (if (null? values)
            (when (not (null? fields))
              (count-error fields value))
            (begin
              (when (null? fields)
                (count-error fields value))
              (let* ((field (car fields))
                     (value (car values))
                     (descriptor (field-descriptor field))
                     (position (field-position field))
                     (offset (+ offset position)))
                (bytestructure-set!* bytevector offset descriptor value)
                (loop (cdr fields) (cdr values)))))))
     ((alist? value)
      (for-each
       (lambda (pair)
         (let ((key (car pair))
               (value (cdr pair)))
           (let-values (((bytevector offset descriptor)
                         (ref-helper #f bytevector offset key)))
             (bytestructure-set!* bytevector offset descriptor value))))
       value))
     (else
      (error "Invalid value for writing into struct." value))))
  (make-bytestructure-descriptor size alignment ref-helper #f setter))

;;; struct.scm ends here
