;;; bytestructures --- Structured access to bytevector contents.

;; Copyright (C) 2013  Taylan Ulrich B.

;; Author: Taylan Ulrich B. <taylanbayirli@gmail.com>
;; Keywords: ffi struct bytevector

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

;; This is the procedural implementation, meaning new bytestructure-descriptors
;; can be defined at run-time, but performance is sub-optimal, because the
;; bytevector-offset to access a field is calculated at run-time.

;;; Version: 1.2.0.-1


;;; Code:

(define-module (bytestructures procedural)
  #:export (
            make-bytestructure-descriptor-type
            make-bytestructure-descriptor-compound-type
            bytestructure-descriptor-type?
            make-bytestructure-descriptor
            bytestructure-descriptor?
            bytestructure-descriptor-size
            make-bytestructure
            bytestructure?
            bytestructure-bytevector
            bytestructure-offset
            bytestructure-descriptor
            bytestructure
            bytestructure-fill!
            bytestructure-ref-helper
            bytestructure-ref-helper*
            bytestructure-ref
            bytestructure-ref*
            bytestructure-set!
            bytestructure-set!*
            bytestructure-pointer
            bs:simple bs:vector bs:struct bs:union bs:pointer
            float double int8 uint8 int16 uint16 int32 uint32 int64 uint64
            int16le uint16le int32le uint32le int64le uint64le
            int16be uint16be int32be uint32be int64be uint64be
            short unsigned-short int unsigned-int long unsigned-long
            size_t ssize_t ptrdiff_t
            ))

(use-modules (srfi srfi-1)
             (srfi srfi-9)
             (srfi srfi-11)
             (rnrs bytevectors)
             ((system foreign)
              #:renamer (symbol-prefix-proc 'ffi:)))


;;; Descriptor-types

(define-record-type :bytestructure-descriptor-type
  (%make-bytestructure-descriptor-type
   constructor size-or-size-accessor
   bytevector-constructor-helper bytevector-ref-helper
   bytevector-ref-proc bytevector-set-proc)
  bytestructure-descriptor-type?
  (constructor                   bytestructure-descriptor-constructor)
  (size-or-size-accessor         bytestructure-descriptor-type-size)
  (bytevector-constructor-helper bytevector-constructor-helper)
  (bytevector-ref-helper         bytevector-ref-helper)
  (bytevector-ref-proc           bytevector-ref-proc)
  (bytevector-set-proc           bytevector-set-proc))

(define (make-bytestructure-descriptor-type
         constructor size-or-size-accessor
         bytevector-ref-proc bytevector-set-proc)
  (%make-bytestructure-descriptor-type
   constructor size-or-size-accessor
   #f #f
   bytevector-ref-proc bytevector-set-proc))

(define make-bytestructure-descriptor-compound-type
  %make-bytestructure-descriptor-type)


;;; Descriptors

(define-record-type :bytestructure-descriptor
  (%make-bytestructure-descriptor type content)
  bytestructure-descriptor?
  (type    bytestructure-descriptor-type)
  (content bytestructure-descriptor-content))

(define (make-bytestructure-descriptor description)
  (cond
   ((bytestructure-descriptor-type? description)
    (%make-bytestructure-descriptor
     description
     ((bytestructure-descriptor-constructor description))))
   ((list? description)
    (let ((type (car description))
          (contents (cdr description)))
      (%make-bytestructure-descriptor
       type
       (apply (bytestructure-descriptor-constructor type) contents))))
   ((bytestructure-descriptor? description)
    description)
   (else (error "Invalid bytestructure-descriptor description." description))))

(define bytestructure-descriptor-size
  (case-lambda
    ((descriptor) (bytestructure-descriptor-size #f #f descriptor))
    ((bytevector offset descriptor)
     (let ((size (bytestructure-descriptor-type-size
                  (bytestructure-descriptor-type descriptor))))
       (if (procedure? size)
           (size
            bytevector offset (bytestructure-descriptor-content descriptor))
           size)))))


;;; Bytestructures

(define-record-type :bytestructure
  (make-bytestructure bytevector offset descriptor)
  bytestructure?
  (bytevector bytestructure-bytevector)
  (offset     bytestructure-offset)
  (descriptor bytestructure-descriptor))

(define bytestructure
  (case-lambda ((description)        (%bytestructure description #f #f))
               ((description values) (%bytestructure description #t values))))
(define (%bytestructure description init? values)
  (let* ((descriptor (make-bytestructure-descriptor description))
         (bytevector (make-bytevector
                      (bytestructure-descriptor-size descriptor))))
    (when init?
      (bytestructure-fill! bytevector 0 descriptor values))
    (make-bytestructure bytevector 0 descriptor)))

(define (bytestructure-fill! bytevector offset descriptor values)
  (cond
   ((pair? values)
    (let ((type (bytestructure-descriptor-type descriptor))
          (content (bytestructure-descriptor-content descriptor)))
      (let lp ((index 0)
               (values values))
        (unless (null? values)
          (let-values (((bytevector* offset* descriptor*)
                        ((bytevector-constructor-helper type)
                         bytevector offset content index)))
            (bytestructure-fill! bytevector* offset* descriptor* (car values)))
          (lp (+ 1 index) (cdr values))))))
   (else
    (bytestructure-primitive-set! bytevector offset descriptor values))))

(define-syntax bytestructure-ref-helper
  (syntax-rules ()
    ((_ bytestructure index ...)
     (let ((bytevector (bytestructure-bytevector bytestructure))
           (offset (bytestructure-offset bytestructure))
           (descriptor (bytestructure-descriptor bytestructure)))
       (bytestructure-ref-helper* bytevector offset descriptor index ...)))))

(define-syntax bytestructure-ref-helper*
  (syntax-rules ()
    ((_ bytevector offset descriptor)
     (values bytevector offset descriptor))
    ((_ bytevector offset descriptor index indices ...)
     (let ((type (bytestructure-descriptor-type descriptor))
           (content (bytestructure-descriptor-content descriptor)))
       (let-values (((bytevector* offset* descriptor*)
                     ((bytevector-ref-helper type)
                      bytevector offset content index)))
         (let ((descriptor* (if (eq? descriptor* content)
                                descriptor
                                descriptor*)))
           (bytestructure-ref-helper*
            bytevector* offset* descriptor* indices ...)))))))

(define-syntax bytestructure-ref
  (syntax-rules ()
    ((_ bytestructure index ...)
     (let-values (((bytevector offset descriptor)
                   (bytestructure-ref-helper bytestructure index ...)))
       (bytestructure-primitive-ref bytevector offset descriptor)))))

(define-syntax bytestructure-ref*
  (syntax-rules ()
    ((_ bytevector offset descriptor index ...)
     (let-values (((bytevector* offset* descriptor*)
                   (bytestructure-ref-helper*
                    bytevector offset descriptor index ...)))
       (bytestructure-primitive-ref bytevector* offset* descriptor*)))))

(define (bytestructure-primitive-ref bytevector offset descriptor)
  (let ((ref-proc (bytevector-ref-proc
                   (bytestructure-descriptor-type descriptor))))
    (if ref-proc
        (let ((content (bytestructure-descriptor-content descriptor)))
          (ref-proc bytevector offset content))
        (make-bytestructure bytevector offset descriptor))))

(define-syntax bytestructure-set!
  (syntax-rules ()
    ((_ bytestructure index ... value)
     (let-values (((bytevector offset descriptor)
                   (bytestructure-ref-helper bytestructure index ...)))
       (bytestructure-primitive-set! bytevector offset descriptor value)))))

(define-syntax bytestructure-set!*
  (syntax-rules ()
    ((_ bytevector offset descriptor index ... value)
     (let-values (((bytevector* offset* descriptor*)
                   (bytestructure-ref-helper*
                    bytevector offset descriptor index ...)))
       (bytestructure-primitive-set! bytevector* offset* descriptor* value)))))

(define (bytestructure-primitive-set! bytevector offset descriptor value)
  (let ((set-proc (bytevector-set-proc
                   (bytestructure-descriptor-type descriptor))))
    (if set-proc
        (let ((content (bytestructure-descriptor-content descriptor)))
          (set-proc bytevector offset content value))
        (bytevector-copy! value 0 bytevector offset
                          (bytestructure-descriptor-size
                           bytevector offset descriptor)))))


;;; "Simple" type

(define-record-type :simple
  (make-simple size ref-proc set-proc)
  simple?
  (size     %simple-size)
  (ref-proc simple-ref-proc)
  (set-proc simple-set-proc))

(define (simple-size bytevector offset simple)
  (%simple-size simple))

(define (simple-ref bytevector offset simple)
  ((simple-ref-proc simple) bytevector offset))

(define (simple-set! bytevector offset simple value)
  ((simple-set-proc simple) bytevector offset value))

(define bs:simple
  (make-bytestructure-descriptor-type
   make-simple simple-size
   simple-ref simple-set!))


;;; Numeric types

(let-syntax ((define-numeric-types
               (syntax-rules ()
                 ((_ (name size ref-proc set-proc) ...)
                  (begin
                    (define name
                      (make-bytestructure-descriptor
                       (list bs:simple size ref-proc set-proc)))
                    ...)))))
  (define-numeric-types
    (float
     4 bytevector-ieee-single-native-ref bytevector-ieee-single-native-set!)
    (double
     8 bytevector-ieee-double-native-ref bytevector-ieee-double-native-set!)
    (int8   1 bytevector-s8-ref bytevector-s8-set!)
    (uint8  1 bytevector-u8-ref bytevector-u8-set!)
    (int16  2 bytevector-s16-native-ref bytevector-s16-native-set!)
    (uint16 2 bytevector-u16-native-ref bytevector-u16-native-set!)
    (int32  4 bytevector-s32-native-ref bytevector-s32-native-set!)
    (uint32 4 bytevector-u32-native-ref bytevector-u32-native-set!)
    (int64  8 bytevector-s64-native-ref bytevector-s64-native-set!)
    (uint64 8 bytevector-u64-native-ref bytevector-u64-native-set!)))

(let-syntax ((define-signed-native-synonyms
               (syntax-rules ()
                 ((_ name ...)
                  (begin
                    (define name
                      (case (ffi:sizeof (@ (system foreign) name))
                        ((1) int8)
                        ((2) int16)
                        ((4) int32)
                        ((8) int64)))
                    ...)))))
  (define-signed-native-synonyms
    short int long ssize_t ptrdiff_t))

(let-syntax ((define-unsigned-native-synonyms
               (syntax-rules ()
                 ((_ name ...)
                  (begin
                    (define name
                      (case (ffi:sizeof (@ (system foreign) name))
                        ((1) uint8)
                        ((2) uint16)
                        ((4) uint32)
                        ((8) uint64)))
                    ...)))))
  (define-unsigned-native-synonyms
    unsigned-short unsigned-int unsigned-long size_t))

(letrec-syntax
    ((define-with-endianness
       (syntax-rules ()
         ((_ (name native-name size ref-proc set-proc endianness) ...)
          (begin
            (define name
              (if (equal? endianness native-endianness)
                  native-name
                  (make-bytestructure-descriptor
                   (list bs:simple size
                         (lambda (bytevector index)
                           (ref-proc bytevector index endianness))
                         (lambda (bytevector index value)
                           (set-proc
                            bytevector index value endianness))))))
            ...))))
     (define-with-endianness*
       (syntax-rules ()
         ((_ (le-name be-name native-name size ref-proc set-proc) ...)
          (begin
            (define-with-endianness
              (le-name native-name size ref-proc set-proc (endianness little))
              (be-name native-name size ref-proc set-proc (endianness big)))
            ...)))))
  (define-with-endianness*
    (int16le  int16be  int16  2 bytevector-s16-ref bytevector-s16-set!)
    (uint16le uint16be uint16 2 bytevector-u16-ref bytevector-u16-set!)
    (int32le  int32be  int32  4 bytevector-s32-ref bytevector-s32-set!)
    (uint32le uint32be uint32 4 bytevector-u32-ref bytevector-u32-set!)
    (int64le  int64be  int64  8 bytevector-s64-ref bytevector-s64-set!)
    (uint64le uint64be uint64 8 bytevector-u64-ref bytevector-u64-set!)))


;;; Vector

(define-record-type :vector
  (%make-vector length content size)
  vector?
  (length  vector-length)
  (content vector-content)
  (size    %vector-size))

(define (make-vector length content-description)
  (let ((content (make-bytestructure-descriptor content-description)))
    (%make-vector length content
                  (* length (bytestructure-descriptor-size content)))))

(define (vector-size bytevector offset vector)
  (%vector-size vector))

(define (vector-ref-helper bytevector offset vector index)
  (let ((content (vector-content vector)))
    (values bytevector
            (+ offset
               (* index (bytestructure-descriptor-size
                         bytevector offset content)))
            content)))

(define bs:vector
  (make-bytestructure-descriptor-compound-type
   make-vector vector-size
   vector-ref-helper vector-ref-helper
   #f #f))


;;; Helpers for Structs and Unions

(define field-name car)
(define field-content cdr)
(define field-find assq)

(define (construct-fields fields)
  (map (lambda (field)
         (cons (car field)
               (make-bytestructure-descriptor (cadr field))))
       fields))


;;; Struct

(define-record-type :struct
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

(define (struct-constructor-helper bytevector offset struct index)
  (let ((fields (struct-fields struct)))
    (let lp ((fields fields)
             (offset offset)
             (field-count index))
      (if (null? fields)
          (error "Struct field index out of bounds." index)
          (let ((field (car fields)))
            (if (= field-count 0)
                (values bytevector offset (field-content field))
                (lp (cdr fields)
                    (+ offset (bytestructure-descriptor-size
                               (field-content field)))
                    (- field-count 1))))))))

(define (struct-ref-helper bytevector offset struct key)
  (let ((fields (struct-fields struct)))
    (let lp ((fields fields)
             (offset offset))
      (if (null? fields)
          (error "No such struct field." key)
          (let ((field (car fields)))
            (if (eq? (field-name field) key)
                (values bytevector offset (field-content field))
                (lp (cdr fields)
                    (+ offset
                       (bytestructure-descriptor-size
                        bytevector
                        offset
                        (field-content field))))))))))

(define bs:struct
  (make-bytestructure-descriptor-compound-type
   make-struct struct-size
   struct-constructor-helper struct-ref-helper
   #f #f))


;;; Union

(define-record-type :union
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

(define (union-constructor-helper bytevector offset union index)
  (values bytevector
          offset
          (field-content (list-ref (union-fields union) index))))

(define (union-ref-helper bytevector offset union key)
  (values bytevector
          offset
          (field-content (field-find key (union-fields union)))))

(define bs:union
  (make-bytestructure-descriptor-compound-type
   make-union union-size
   union-constructor-helper union-ref-helper
   #f #f))


;;; Pointer

(define-record-type :pointer
  (%make-pointer content)
  pointer?
  (content pointer-content))

(define (make-pointer content-description)
  (%make-pointer (make-bytestructure-descriptor content-description)))

(define (pointer-constructor-helper bytevector offset pointer index)
  (if (not (zero? index))
      (error "Pointer can only hold one value.")
      (let* ((content (pointer-content pointer))
             (content-size (bytestructure-descriptor-size content)))
        (let ((bytevector* (make-bytevector content-size)))
          (pointer-set-bv! bytevector offset pointer bytevector*)
          (values bytevector* 0 content)))))

(define (pointer-ref-helper bytevector offset pointer index)
  (let ((content (pointer-content pointer)))
    (let ((bytevector* (pointer-ref-bv bytevector offset pointer)))
      (if (eq? index '*)
          (values bytevector* 0 content)
          (bytestructure-ref-helper* bytevector* 0 content index)))))

(define bytevector-address-ref
  (case (ffi:sizeof '*)
    ((1) bytevector-u8-ref)
    ((2) bytevector-u16-native-ref)
    ((4) bytevector-u32-native-ref)
    ((8) bytevector-u64-native-ref)))

(define bytevector-address-set!
  (case (ffi:sizeof '*)
    ((1) bytevector-u8-set!)
    ((2) bytevector-u16-native-set!)
    ((4) bytevector-u32-native-set!)
    ((8) bytevector-u64-native-set!)))

(define (pointer-ref-bv bytevector offset pointer)
  (let ((address (bytevector-address-ref bytevector offset))
        (content-size (bytestructure-descriptor-size
                       (pointer-content pointer))))
    (if (zero? address)
        (let ((bv (make-bytevector content-size)))
          (pointer-set-bv! bytevector offset pointer bv)
          bv)
        (ffi:pointer->bytevector (ffi:make-pointer address) content-size))))

(define (pointer-set-bv! bytevector offset pointer value)
  (bytevector-address-set!
   bytevector offset (ffi:pointer-address (if (bytevector? value)
                                              (ffi:bytevector->pointer value)
                                              value))))

(define bs:pointer
  (make-bytestructure-descriptor-compound-type
   make-pointer (ffi:sizeof '*)
   pointer-constructor-helper pointer-ref-helper
   pointer-ref-bv pointer-set-bv!))

;;; procedural.scm ends here
