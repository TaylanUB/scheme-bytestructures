;;; struct.scm --- Struct descriptor constructor.

;; Copyright © 2015, 2016, 2021 Taylan Kammer <taylan.kammer@gmail.com>

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

(define (construct-normal-field pack position name descriptor)
  (let*-values
      (((size)
        (bytestructure-descriptor-size descriptor))
       ((alignment)
        (pack-alignment pack (bytestructure-descriptor-alignment descriptor)))
       ((position _boundary _bit-offset)
        (align position size alignment)))
    (values (make-field name descriptor size alignment position)
            (+ position size))))

(define (construct-bit-field pack position name descriptor width)
  (if (zero? width)
      (let* ((alignment (bytestructure-descriptor-alignment descriptor))
             (position (next-boundary position alignment)))
        (values (make-field #f descriptor 0 1 position)
                position))
      (let*-values
          (((int-size)
            (bytestructure-descriptor-size descriptor))
           ((size)
            (* 1/8 width))
           ((int-alignment)
            (bytestructure-descriptor-alignment descriptor))
           ((alignment)
            (pack-alignment pack int-alignment))
           ((position boundary offset)
            (align position size alignment))
           ((descriptor)
            (bitfield-descriptor descriptor offset width)))
        (values (make-field name descriptor int-size alignment boundary)
                (+ position size)))))

(define (construct-fields pack field-specs)
  (let loop ((field-specs field-specs)
             (position 0)
             (fields '()))
    (if (null? field-specs)
        (reverse fields)
        (let* ((field-spec (car field-specs))
               (field-specs (cdr field-specs))
               (name-or-type (car field-spec)))
          (if (and (eq? name-or-type 'union)
                   (pair? (cadr field-spec)))
              (let-values (((next-position fields)
                            (add-union-fields pack
                                              position
                                              (cadr field-spec)
                                              fields)))
                (loop field-specs
                      next-position
                      fields))
              (let-values (((field next-position)
                            (construct-field pack position field-spec)))
                (loop field-specs
                      next-position
                      (cons field fields))))))))

(define (add-union-fields pack position field-specs fields)
  (define (field-spec-alignment field-spec)
    (let ((descriptor (cadr field-spec)))
      (bytestructure-descriptor-alignment descriptor)))
  (define (field-spec-size field-spec)
    (let ((descriptor (cadr field-spec)))
      (bytestructure-descriptor-size descriptor)))
  (let* ((alignment (apply max (map field-spec-alignment field-specs)))
         (alignment (pack-alignment pack alignment))
         (size (apply max (map field-spec-size field-specs)))
         (position (align position size alignment)))
    (let loop ((field-specs field-specs)
               (next-position position)
               (fields fields))
      (if (null? field-specs)
          (values next-position fields)
          (let ((field-spec (car field-specs))
                (field-specs (cdr field-specs)))
            (let-values (((field next-position)
                          (construct-field pack position field-spec)))
              (loop field-specs
                    (max position next-position)
                    (cons field fields))))))))

(define (construct-field pack position field-spec)
  (let* ((name (car field-spec))
         (descriptor (cadr field-spec))
         (bitfield? (not (null? (cddr field-spec))))
         (width (if bitfield?
                    (car (cddr field-spec))
                    #f)))
    (if bitfield?
        (construct-bit-field pack position name descriptor width)
        (construct-normal-field pack position name descriptor))))

(define-record-type <struct-metadata>
  (make-struct-metadata field-alist)
  struct-metadata?
  (field-alist struct-metadata-field-alist))

(define bs:struct
  (case-lambda
    ((field-specs)
     (bs:struct #f field-specs))
    ((pack field-specs)
     (define %fields (construct-fields pack field-specs))
     (define fields (filter field-name %fields))
     (define field-alist (map (lambda (field)
                                (cons (field-name field) field))
                              fields))
     (define alignment (apply max (map field-alignment fields)))
     (define (field-end field)
       (+ (field-position field) (field-size field)))
     (define size (let ((end (apply max (map field-end %fields))))
                    (let-values (((size . _) (next-boundary end alignment)))
                      size)))
     (define (unwrapper syntax? bytevector offset index)
       (let* ((index (if syntax? (syntax->datum index) index))
              (field-entry (assq index field-alist))
              (field (if field-entry
                         (cdr field-entry)
                         (error "No such struct field." index))))
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
       (when syntax?
         (error "Writing into struct not supported with macro API."))
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
         ;; Assumed to be a pseudo-alist like ((k1 v1) (k2 v2) ...).
         (for-each
          (lambda (pair)
            (let ((key (car pair))
                  (value (cadr pair)))
              (let-values (((bytevector offset descriptor)
                            (unwrapper #f bytevector offset key)))
                (bytestructure-set!* bytevector offset descriptor value))))
          value))
        (else
         (error "Invalid value for writing into struct." value))))
     (define meta
       (let ((simple-field-alist (map (lambda (field)
                                        (cons (field-name field)
                                              (field-descriptor field)))
                                      fields)))
         (make-struct-metadata simple-field-alist)))
     (make-bytestructure-descriptor size alignment unwrapper #f setter meta))))

(define debug-alignment
  (case-lambda
   ((fields) (debug-alignment #f fields))
   ((pack fields)
    (let* ((fields (construct-fields pack fields))
         (alignment (apply max (map field-alignment fields)))
         (size (let* ((field (last fields))
                      (end (+ (field-position field) (field-size field))))
                 (let-values (((size . _) (next-boundary end alignment)))
                   size))))
    (format #t "{\n")
    (for-each (lambda (field)
                (let ((name (field-name field))
                      (pos (* 8 (field-position field)))
                      (size (* 8 (field-size field)))
                      (align (* 8 (field-alignment field))))
                  (format #t "  ~a - ~a: ~a (~a, ~a)\n"
                          pos (+ pos size) name size align)))
              fields)
    (format #t "} = ~a\n" (* 8 size))
    (values)))))

;;; struct.scm ends here
