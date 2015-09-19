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

;;; We need floor/ generalized to rationals.
;;; E.g. (floor/ 2.5 2) => 1, 0.5
(cond-expand
 (guile
  (define rat-floor/ floor/))
 (else
  (define (rat-floor/ x y)
    (if (integer? x)
        (floor/ x y)
        (let* ((integer (floor x))
               (fraction (- x integer)))
          (let-values (((q r) (floor/ integer y)))
            (values q (+ r fraction))))))))

;;; Makes sure that a given position is a multiple of 'alignment', rounding to
;;; the next multiple if it isn't.
(define (align position alignment)
  (let-values (((q r) (rat-floor/ position alignment)))
    (if (zero? r)
        position
        (* alignment (+ 1 q)))))

;;; Makes sure that a given position for a bitfield with a given size won't make
;;; the bitfield cross its alignment boundaries.
(define (lay-out position size alignment)
  (let* ((boundary-beginning (quotient (floor position) alignment))
         (boundary-end (+ boundary-beginning alignment)))
    (if (< boundary-end (+ position (min size alignment)))
        boundary-end
        position)))

;;; Returns the bit-offset of 'position' from the nearest smaller multiple of
;;; 'alignment'.
(define (offsetof position alignment)
  (remainder (* 8 position) (* 8 alignment)))

(define (pack-alignment pack alignment)
  (case pack
    ((#t) 1)
    ((#f) alignment)
    (else (min pack alignment))))

;;; Only a macro for efficiency reasons.
(define-syntax bit-field/signed
  (syntax-rules ()
    ((_ <num> <size> <start> <end> <signed?>)
     (let ((unsigned-value (bit-field <num> <start> <end>)))
       (if (not <signed?>)
           unsigned-value
           (let ((sign (bit-set? unsigned-value (- <size> 1))))
             (if sign
                 (- unsigned-value (expt 2 <size>))
                 unsigned-value)))))))

(define (bitfield-descriptor size position integer-descriptor)
  (validate-bitfield-descriptor integer-descriptor)
  (let ((alignment (bytestructure-descriptor-alignment integer-descriptor))
        (num-getter (bytestructure-descriptor-getter integer-descriptor))
        (num-setter (bytestructure-descriptor-setter integer-descriptor))
        (signed? (bitfield-descriptor-signed? integer-descriptor)))
    (define bit-size (* 8 size))
    (define bit-offset (offsetof position alignment))
    (define start bit-offset)
    (define end (+ start size))
    (define (getter syntax? bytevector offset)
      (let ((num (num-getter syntax? bytevector offset)))
        (if syntax?
            (quasisyntax
             (bit-field/signed (unsyntax num) (unsyntax size)
                               (unsyntax start) (unsyntax end)
                               (unsyntax signed?)))
            (bit-field/signed num size start end signed?))))
    (define (setter syntax? bytevector offset value)
      (let* ((oldnum (num-getter syntax? bytevector offset))
             (newnum (if syntax?
                         (quasisyntax
                          (copy-bit-field (unsyntax oldnum) (unsyntax value)
                                          (unsyntax start) (unsyntax end)))
                         (copy-bit-field oldnum value start end))))
        (num-setter syntax? bytevector offset newnum)))
    (make-bytestructure-descriptor size alignment #f getter setter)))

(define (validate-bitfield-descriptor descriptor)
  (when (not (memq descriptor (list int8 int16 int32 int64
                                    uint8 uint16 uint32 uint64
                                    int16le int32le int64le
                                    uint16le uint32le uint64le
                                    int16be int32be int64be
                                    uint16be uint32be uint64be)))
    (error "Invalid descriptor for bitfield." descriptor)))

(define (bitfield-descriptor-signed? descriptor)
  (memq descriptor (list int8 int16 int32 int64
                         int16le int32le int64le
                         int16be int32be int64be)))

(define-record-type <field>
  (make-field name descriptor size alignment position)
  field?
  (name field-name)
  (descriptor field-descriptor)
  (size field-size)
  (alignment field-alignment)
  (position field-position))

(define (construct-fields pack field-specs)
  (if (null? (cdr field-specs))
      (let* ((field-spec (car field-specs))
             (name (car field-spec))
             (descriptor (cadr field-spec))
             (bitfield? (not (null? (cddr field-spec))))
             (size (if bitfield?
                       (* 1/8 (car (cddr field-spec)))
                       (bytestructure-descriptor-size descriptor)))
             (alignment (pack-alignment
                         pack
                         (bytestructure-descriptor-alignment descriptor)))
             (descriptor (if bitfield?
                             (bitfield-descriptor size 0 descriptor)
                             descriptor)))
        (list (make-field name descriptor size alignment 0)))
      (let loop ((field-spec (car field-specs))
                 (next (cadr field-specs))
                 (field-specs (cddr field-specs))
                 (position 0)
                 (fields '()))
        (let* ((name (car field-spec))
               (descriptor (cadr field-spec))
               (bitfield? (not (null? (cddr field-spec))))
               (size (if bitfield?
                         (* 1/8 (car (cddr field-spec)))
                         (bytestructure-descriptor-size descriptor)))
               (alignment (pack-alignment
                           pack
                           (bytestructure-descriptor-alignment descriptor)))
               (descriptor (if bitfield?
                               (bitfield-descriptor size position descriptor)
                               descriptor))
               (nname (car next))
               (ndescriptor (cadr next))
               (nbitfield? (not (null? (cddr next))))
               (nsize (if nbitfield?
                          (* 1/8 (car (cddr next)))
                          (bytestructure-descriptor-size ndescriptor)))
               (nalignment (pack-alignment
                            pack
                            (bytestructure-descriptor-alignment ndescriptor)))
               (nposition (if nbitfield?
                              (lay-out (+ position size) nsize nalignment)
                              (align (+ position size) nalignment)))
               (ndescriptor (if nbitfield?
                                (bitfield-descriptor nsize nposition ndescriptor)
                                ndescriptor)))
          (let* ((field (make-field name descriptor size alignment position))
                 (fields (cons field fields)))
            (if (null? field-specs)
                (let* ((nfield (make-field
                                nname ndescriptor nsize nalignment nposition))
                       (fields (cons nfield fields)))
                  (reverse fields))
                (loop next
                      (car field-specs)
                      (cdr field-specs)
                      nposition
                      fields)))))))

(define bs:struct
  (case-lambda
    ((field-specs)
     (bs:struct #f field-specs))
    ((pack field-specs)
     (define fields (construct-fields pack field-specs))
     (define alignment (apply max (map field-alignment fields)))
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
         (size (align (apply + (map field-size fields)) alignment)))
    (format #t "{\n")
    (for-each (lambda (field)
                (let ((name (field-name field))
                      (pos (field-position field))
                      (size (field-size field)))
                  (format #t "  ~a: ~a:~a\n" (* 8 pos) name (* 8 size))))
              fields)
    (format #t "} = ~a\n" (* 8 (align size alignment)))
    (values)))

;;; struct.scm ends here
