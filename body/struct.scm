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

;; This code partly uses rational numbers for byte counts and offsets, to
;; represent granularity down to bits.  I.e. 1/8 is a size or offset of one bit.


;;; Code:

;;; Rounds 'position' up to the next alignment boundary if necessary.
(define (align position size alignment)
  (let* ((prev-boundary (quotient (floor position) alignment))
         (next-boundary (+ prev-boundary alignment)))
    (if (< next-boundary (+ position (min size alignment)))
        next-boundary
        position)))

(define (pack-alignment pack alignment)
  (case pack
    ((#t) 1)
    ((#f) alignment)
    (else (min pack alignment))))

(define-record-type <field>
  (make-field name descriptor size alignment position)
  field?
  (name field-name)
  (descriptor field-descriptor)
  (size field-size)
  (alignment field-alignment)
  (position field-position))

(define (construct-fields pack field-specs)
  (let loop ((field-specs field-specs)
             (position 0)
             (fields '()))
    (if (null? field-specs)
        (reverse fields)
        (let* ((field-spec (car field-specs))
               (name (car field-spec))
               (descriptor (cadr field-spec))
               (bitfield? (not (null? (cddr field-spec))))
               (size (if bitfield?
                         (* 1/8 (car (cddr field-spec)))
                         (bytestructure-descriptor-size descriptor)))
               (alignment (bytestructure-descriptor-alignment descriptor))
               (alignment (pack-alignment pack alignment))
               (position (align position size alignment))
               (descriptor (if bitfield?
                               (bitfield-descriptor size position descriptor)
                               descriptor))
               (field (make-field name descriptor size alignment position)))
          (loop (cdr field-specs)
                (+ position size)
                (cons field fields))))))

(define bs:struct
  (case-lambda
    ((field-specs)
     (bs:struct #f field-specs))
    ((pack field-specs)
     (define fields (construct-fields pack field-specs))
     (define alignment (apply max (map field-alignment fields)))
     (define size (let* ((field (last fields))
                         (end (+ (field-position field) (field-size field))))
                    (align end +inf.0 alignment)))
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
                            (quasisyntax
                             (+ (unsyntax offset) (unsyntax position)))
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
        ((pair? value)
         ;; Assumed to be an alist.
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
     (make-bytestructure-descriptor size alignment ref-helper #f setter))))

(define (debug-alignment pack fields)
  (let* ((fields (construct-fields pack fields))
         (alignment (apply max (map field-alignment fields)))
         (size (let* ((field (last fields))
                      (end (+ (field-position field) (field-size field))))
                 (align end +inf.0 alignment))))
    (format #t "{\n")
    (for-each (lambda (field)
                (let ((name (field-name field))
                      (pos (field-position field))
                      (size (field-size field)))
                  (format #t "  ~a: ~a:~a\n" (* 8 pos) name (* 8 size))))
              fields)
    (format #t "} = ~a\n" (* 8 size))
    (values)))

;;; struct.scm ends here
