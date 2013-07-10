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

;;; Version: 1.2.-1

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
            bytestructure*
            bytestructure-ref-helper
            bytestructure-ref-helper*
            bytestructure-ref
            bytestructure-ref*
            bytestructure-set!
            bytestructure-set!*
            bsd:simple bsd:vector bsd:struct bsd:union
            float double int8 uint8 int16 uint16 int32 uint32 int64 uint64
            ))

(use-modules (srfi srfi-1)
             (srfi srfi-9)
             (srfi srfi-11)
             (rnrs bytevectors))

(define-syntax assert
  (syntax-rules ()
    ((assert expression)
     (unless expression
       (error "Assertion not met." 'expression)))))

;;; Descriptor-types

(define-record-type :bytestructure-descriptor-type
  (make-bytestructure-descriptor-type*
   compound? constructor size-or-size-accessor
   bytevector-constructor-helper bytevector-ref-helper
   bytevector-ref-fn bytevector-set-fn)
  bytestructure-descriptor-type?
  (compound?                     bytestructure-descriptor-type-compound?)
  (constructor                   bytestructure-descriptor-constructor)
  (size-or-size-accessor         bytestructure-descriptor-type-size)
  (bytevector-constructor-helper bytevector-constructor-helper)
  (bytevector-ref-helper         bytevector-ref-helper)
  (bytevector-ref-fn             bytevector-ref-fn)
  (bytevector-set-fn             bytevector-set-fn))

(define (make-bytestructure-descriptor-type
         constructor size-or-size-accessor
         bytevector-ref-fn bytevector-set-fn)
  (assert (every procedure?
                 (list constructor bytevector-ref-fn bytevector-set-fn)))
  (assert (or (procedure? size-or-size-accessor)
              (and (integer? size-or-size-accessor)
                   (exact? size-or-size-accessor)
                   (<= 0 size-or-size-accessor))))
  (make-bytestructure-descriptor-type*
   #f constructor size-or-size-accessor
   #f #f
   bytevector-ref-fn bytevector-set-fn))

(define (make-bytestructure-descriptor-compound-type
         constructor size-accessor
         bytevector-constructor-helper bytevector-ref-helper)
  (assert (every procedure?
                 (list constructor size-accessor
                       bytevector-constructor-helper bytevector-ref-helper)))
  (make-bytestructure-descriptor-type*
   #t constructor size-accessor
   bytevector-constructor-helper bytevector-ref-helper
   #f #f))

;;; Descriptors

(define-record-type :bytestructure-descriptor
  (make-bytestructure-descriptor* type content)
  bytestructure-descriptor?
  (type    bytestructure-descriptor-type)
  (content bytestructure-descriptor-content))

(define (make-bytestructure-descriptor description)
  (cond
   ((bytestructure-descriptor-type? description)
    (make-bytestructure-descriptor*
     description
     ((bytestructure-descriptor-constructor description))))
   ((list? description)
    (let ((type (car description))
          (contents (cdr description)))
      (make-bytestructure-descriptor*
       type
       (apply (bytestructure-descriptor-constructor type) contents))))
   ;; See `construct-fields' for why this is useful.
   ((bytestructure-descriptor? description)
    description)
   (else (error "Invalid bytestructure-descriptor description." description))))

(define (bytestructure-descriptor-size descriptor)
  (let ((size (bytestructure-descriptor-type-size
               (bytestructure-descriptor-type descriptor))))
    (if (procedure? size)
        (size (bytestructure-descriptor-content descriptor))
        size)))

;;; Bytestructures

(define-record-type :bytestructure
  (make-bytestructure bytevector offset descriptor)
  bytestructure?
  (bytevector bytestructure-bytevector)
  (offset bytestructure-offset)
  (descriptor bytestructure-descriptor))

(define-syntax bytestructure
  (syntax-rules ()
    ((_ descriptor)
     (make-bytestructure
      (make-bytevector (bytestructure-descriptor-size descriptor))
      0
      descriptor))
    ((_ descriptor content)
     (let ((bytevector
            (make-bytevector (bytestructure-descriptor-size descriptor))))
       (bytestructure* bytevector 0 descriptor content)
       (make-bytestructure bytevector 0 descriptor)))))

(define-syntax bytestructure*
  (syntax-rules ()
    ((_ bytevector offset descriptor (value ...))
     (let ((type (bytestructure-descriptor-type descriptor))
           (content (bytestructure-descriptor-content descriptor))
           (index 0))
       (begin 
         (let-values (((offset* descriptor*)
                       ((bytevector-constructor-helper type) content index)))
           (bytestructure* bytevector (+ offset offset*) descriptor* value))
         (set! index (+ 1 index)))
       ...))
    ((_ bytevector offset descriptor value)
     (let ((type (bytestructure-descriptor-type descriptor))
           (content (bytestructure-descriptor-content descriptor)))
       ((bytevector-set-fn type)
        bytevector offset content value)))))

(define-syntax bytestructure-ref-helper
  (syntax-rules ()
    ((_ descriptor index ...)
     (bytestructure-ref-helper* 0 descriptor index ...))))

(define-syntax bytestructure-ref-helper*
  (syntax-rules ()
    ((_ offset descriptor)
     (values offset descriptor))
    ((_ offset descriptor index indices ...)
     (let ((type (bytestructure-descriptor-type descriptor))
           (content (bytestructure-descriptor-content descriptor)))
       (let-values (((offset* descriptor*)
                     ((bytevector-ref-helper type) content index)))
         (bytestructure-ref-helper*
          (+ offset offset*) descriptor* indices ...))))))

(define-syntax bytestructure-ref
  (syntax-rules ()
    ((_ bytestructure index ...)
     (let ((bytevector (bytestructure-bytevector bytestructure))
           (offset (bytestructure-offset bytestructure))
           (descriptor (bytestructure-descriptor bytestructure)))
       (bytestructure-ref* bytevector offset descriptor index ...)))))

(define-syntax bytestructure-ref*
  (syntax-rules ()
    ((_ bytevector offset descriptor index ...)
     (let-values (((offset* descriptor*)
                   (bytestructure-ref-helper* offset descriptor index ...)))
       (let ((type (bytestructure-descriptor-type descriptor*)))
         (if (bytestructure-descriptor-type-compound? type)
             (values offset* descriptor*)
             (let ((content (bytestructure-descriptor-content descriptor*)))
               ((bytevector-ref-fn type) bytevector offset* content))))))))

(define-syntax bytestructure-set!
  (syntax-rules ()
    ((_ bytestructure index ... value)
     (let ((bytevector (bytestructure-bytevector bytestructure))
           (offset (bytestructure-offset bytestructure))
           (descriptor (bytestructure-descriptor bytestructure)))
       (bytestructure-set!* bytevector offset descriptor index ... value)))))

(define-syntax bytestructure-set!*
  (syntax-rules ()
    ((_ bytevector offset descriptor index ... value)
     (let-values (((offset* descriptor*)
                   (bytestructure-ref-helper* offset descriptor index ...)))
       (let ((type (bytestructure-descriptor-type descriptor*)))
         (if (bytestructure-descriptor-type-compound? type)
             (bytevector-copy! value 0 bytevector offset*
                               (bytevector-length value))
             (let ((content (bytestructure-descriptor-content descriptor*)))
               ((bytevector-set-fn type)
                bytevector offset* content value))))))))


;;; Pre-provided types:

;;; "Simple" type

(define-record-type :simple-descriptor
  (simple-descriptor size ref-fn set-fn)
  simple-descriptor?
  (size   simple-descriptor-size)
  (ref-fn simple-descriptor-ref-fn)
  (set-fn simple-descriptor-set-fn))

(define (simple-descriptor-ref bytevector offset descriptor)
  ((simple-descriptor-ref-fn descriptor) bytevector offset))

(define (simple-descriptor-set! bytevector offset descriptor value)
  ((simple-descriptor-set-fn descriptor) bytevector offset value))

(define bsd:simple
  (make-bytestructure-descriptor-type
   simple-descriptor simple-descriptor-size
   simple-descriptor-ref simple-descriptor-set!))

;;; Numeric types

(let-syntax
    ((define-numeric-types
       (syntax-rules ()
         ((_ (name size ref-fn set-fn) ...)
          (begin
            (define name
              (make-bytestructure-descriptor
               (list bsd:simple size ref-fn set-fn)))
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

;;; Vector

(define-record-type :vector-descriptor
  (vector-descriptor* length content-descriptor size)
  vector-descriptor?
  (length             vector-descriptor-length)
  (content-descriptor vector-descriptor-content-descriptor)
  (size               vector-descriptor-size))

(define (vector-descriptor length content-description)
  (assert (and (integer? length) (<= 0 length)))
  (let ((content-descriptor
         (make-bytestructure-descriptor content-description)))
    (vector-descriptor*
     length content-descriptor
     (* length (bytestructure-descriptor-size content-descriptor)))))

(define (vector-constructor-helper descriptor index)
  (vector-ref-helper descriptor index))

(define (vector-ref-helper descriptor index)
  (let ((content-descriptor (vector-descriptor-content-descriptor descriptor)))
    (values (* index (bytestructure-descriptor-size content-descriptor))
            content-descriptor)))

(define bsd:vector
  (make-bytestructure-descriptor-compound-type
   vector-descriptor vector-descriptor-size
   vector-constructor-helper vector-ref-helper))

;;; Helpers for Structs and Unions

(define field-name car)
(define field-content-descriptor cdr)
(define field-find assq)

(define (construct-fields fields)
  (map (lambda (field)
         (assert (and (list? field)
                      (= 2 (length field))
                      (symbol? (car field))))
         (cons (car field)
               (make-bytestructure-descriptor (cadr field))))
       fields))

;;; Struct

(define-record-type :struct-descriptor
  (struct-descriptor* fields size)
  struct-descriptor?
  (fields struct-descriptor-fields)
  (size   struct-descriptor-size))

(define (struct-descriptor . fields)
  (let ((fields (construct-fields fields)))
    (struct-descriptor*
     fields (apply + (map (lambda (field)
                            (bytestructure-descriptor-size
                             (field-content-descriptor field)))
                          fields)))))

(define (struct-constructor-helper descriptor index)
  (let ((fields (struct-descriptor-fields descriptor)))
    (let try-next ((field (car fields))
                   (fields (cdr fields))
                   (offset 0)
                   (field-count index))
      (if (= field-count 0)
          (values offset (field-content-descriptor field))
          (if (null? fields)
              (error "Struct field index out of bounds." index)
              (try-next (car fields)
                        (cdr fields)
                        (+ offset (bytestructure-descriptor-size
                                   (field-content-descriptor field)))
                        (- field-count 1)))))))

(define (struct-ref-helper descriptor key)
  (let ((fields (struct-descriptor-fields descriptor)))
    (let try-next ((field (car fields))
                   (fields (cdr fields))
                   (offset 0))
      (if (eq? (field-name field) key)
          (values offset (field-content-descriptor field))
          (if (null? fields)
              (error "No such struct field." key)
              (try-next (car fields)
                        (cdr fields)
                        (+ offset (bytestructure-descriptor-size
                                   (field-content-descriptor field)))))))))

(define bsd:struct
  (make-bytestructure-descriptor-compound-type
   struct-descriptor struct-descriptor-size
   struct-constructor-helper struct-ref-helper))

;;; Union

(define-record-type :union-descriptor
  (union-descriptor* fields size)
  union-descriptor?
  (fields union-descriptor-fields)
  (size   union-descriptor-size))

(define (union-descriptor . fields)
  (let ((fields (construct-fields fields)))
    (union-descriptor*
     fields (apply max (map (lambda (field)
                              (bytestructure-descriptor-size
                               (field-content-descriptor field)))
                            fields)))))

(define (union-constructor-helper descriptor index)
  (values 0 (field-content-descriptor
             (list-ref (union-descriptor-fields descriptor) index))))

(define (union-ref-helper descriptor key)
  (values 0 (field-content-descriptor
             (field-find key (union-descriptor-fields descriptor)))))

(define bsd:union
  (make-bytestructure-descriptor-compound-type
   union-descriptor union-descriptor-size
   union-constructor-helper union-ref-helper))

;;; procedural.scm ends here
