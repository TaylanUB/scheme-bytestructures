;;; bytestructures --- Structured access to bytevector contents.

;; Copyright (C) 2013  Taylan Ulrich B.

;; Author: Taylan Ulrich B. <taylanbayirli@gmail.com>
;; Keywords: 

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

;;; Code:

(define-module (bytestructures procedural)
  #:export (
            define-bytestructure-descriptor-type
            define-bytestructure-descriptor-compound-type
            bytestructure-descriptor
            bytestructure-descriptor?
            bytestructure-descriptor-size
            bytestructure
            bytestructure-access
            bytestructure-access*
            bytestructure-set!
            bytestructure-set!*
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

;;; Types

(define-record-type :bytestructure-descriptor-type
  (bytestructure-descriptor-type compound? constructor predicate size
                                 bytevector-constructor-helper
                                 bytevector-accessor-helper
                                 bytevector-accessor bytevector-mutator)
  bytestructure-descriptor-type?
  (compound? bytestructure-descriptor-type-compound?)
  (constructor bytestructure-descriptor-constructor)
  (predicate bytestructure-descriptor-type-predicate)
  (size bytestructure-descriptor-type-size)
  (bytevector-constructor-helper bytevector-constructor-helper)
  (bytevector-accessor-helper bytevector-accessor-helper)
  (bytevector-accessor bytevector-accessor)
  (bytevector-mutator bytevector-mutator))

(define bytestructure-descriptor-types (make-parameter '()))

(define (define-bytestructure-descriptor-type name constructor predicate
          size-or-size-accessor bytevector-accessor bytevector-mutator)
  (assert (symbol? name))
  (assert (every procedure? (list constructor predicate bytevector-accessor
                                  bytevector-mutator)))
  (assert (or (procedure? size-or-size-accessor)
              (and (integer? size-or-size-accessor)
                   (exact? size-or-size-accessor)
                   (< 0 size-or-size-accessor))))
  (bytestructure-descriptor-types
   (alist-cons
    name (bytestructure-descriptor-type #f constructor predicate
                                        size-or-size-accessor #f #f
                                        bytevector-accessor bytevector-mutator)
    (bytestructure-descriptor-types)))
  *unspecified*)

(define (define-bytestructure-descriptor-compound-type name constructor
          predicate size-accessor bytevector-constructor-helper
          bytevector-accessor-helper)
  (assert (symbol? name))
  (assert (every procedure? (list constructor predicate size-accessor
                                  bytevector-constructor-helper
                                  bytevector-accessor-helper)))
  (bytestructure-descriptor-types
   (alist-cons
    name (bytestructure-descriptor-type #t constructor predicate size-accessor
                                        bytevector-constructor-helper
                                        bytevector-accessor-helper #f #f)
    (bytestructure-descriptor-types)))
  *unspecified*)

(define (bytestructure-descriptor-type-with-name name)
  (cdr (or (assoc name (bytestructure-descriptor-types))
           (error "Not a bytestructure-descriptor-type name." name))))

(define (bytestructure-descriptor-find-type descriptor)
  (cdr
   (or (find (lambda (entry)
               (let ((type (cdr entry)))
                 ((bytestructure-descriptor-type-predicate type) descriptor)))
             (bytestructure-descriptor-types))
       (error "Not a bytestructure-descriptor." descriptor))))

;;; Generals

(define (bytestructure-descriptor description)
  (cond
   ((symbol? description)
    ((bytestructure-descriptor-constructor
      (bytestructure-descriptor-type-with-name description))))
   ((list? description)
    (let ((name (car description))
          (contents (cdr description)))
      (apply (bytestructure-descriptor-constructor
              (bytestructure-descriptor-type-with-name name))
             contents)))
   ;; See `construct-fields' for why this is useful.
   ((bytestructure-descriptor? description)
    description)
   (else (error "Invalid bytestructure-descriptor description." description))))

(define (bytestructure-descriptor? obj)
  (not (not (bytestructure-descriptor-find-type obj))))

(define (bytestructure-descriptor-size descriptor)
  (let* ((type (bytestructure-descriptor-find-type descriptor))
         (size (bytestructure-descriptor-type-size type)))
    (if (procedure? size)
        (size descriptor)
        size)))

(define-syntax bytestructure
  (syntax-rules ()
    ((_ descriptor)
     (make-bytevector (bytestructure-descriptor-size descriptor)))
    ((_ descriptor (value ...))
     (let ((bytevector (bytestructure descriptor)))
       (bytestructure* descriptor bytevector 0 (value ...))
       bytevector))))

(define-syntax bytestructure*
  (syntax-rules ()
    ((_ descriptor bytevector offset (value ...))
     (let ((type (bytestructure-descriptor-find-type descriptor))
           (index 0))
       (begin 
         (let-values (((offset* descriptor*)
                       ((bytevector-constructor-helper type) descriptor index)))
           (bytestructure* descriptor* bytevector (+ offset offset*) value))
         (set! index (+ 1 index)))
       ...))
    ((_ descriptor bytevector offset value)
     ((bytevector-mutator (bytestructure-descriptor-find-type descriptor))
      bytevector descriptor offset value))))

(define-syntax bytestructure-access
  (syntax-rules ()
    ((_ bytevector descriptor accessor ...)
     (bytestructure-access* bytevector descriptor 0 accessor ...))))

(define-syntax bytestructure-access*
  (syntax-rules ()
    ((_ bytevector descriptor offset)
     (let ((type (bytestructure-descriptor-find-type descriptor)))
       (if (bytestructure-descriptor-type-compound? type)
           (values bytevector descriptor offset)
           ((bytevector-accessor type) bytevector descriptor offset))))
    ((_ bytevector descriptor offset accessor accessors ...)
     (let ((type (bytestructure-descriptor-find-type descriptor)))
       (let-values (((offset* descriptor*)
                     ((bytevector-accessor-helper type) descriptor accessor)))
         (bytestructure-access* bytevector descriptor* (+ offset offset*)
                                accessors ...))))))

(define-syntax bytestructure-set!
  (syntax-rules ()
    ((_ bytevector descriptor accessor ... value)
     (bytestructure-set!* bytevector descriptor 0 accessor ... value))))

(define-syntax bytestructure-set!*
  (syntax-rules ()
    ((_ bytevector descriptor offset accessor accessors ... value)
     (let ((type (bytestructure-descriptor-find-type descriptor)))
       (let-values (((offset* descriptor*)
                     ((bytevector-accessor-helper type) descriptor accessor)))
         (bytestructure-set!* bytevector descriptor* (+ offset offset*)
                              accessors ... value))))
    ((_ bytevector descriptor offset value)
     (let ((type (bytestructure-descriptor-find-type descriptor)))
       (if (bytestructure-descriptor-type-compound? type)
           (bytevector-copy! value 0 bytevector offset
                             (bytevector-length value))
           ((bytevector-mutator type) bytevector descriptor offset value))))))

;;; Vector

(define-record-type :vector-descriptor
  (vector-descriptor* length content-descriptor size)
  vector-descriptor?
  (length vector-descriptor-length)
  (content-descriptor vector-descriptor-content-descriptor)
  (size vector-descriptor-size))

(define (vector-descriptor length content-description)
  (assert (and (integer? length) (<= 0 length)))
  (let ((content-descriptor (bytestructure-descriptor content-description)))
    (vector-descriptor*
     length content-descriptor
     (* length (bytestructure-descriptor-size content-descriptor)))))

(define (vector-constructor-helper descriptor index)
  (vector-accessor-helper descriptor index))

(define (vector-accessor-helper descriptor index)
  (let ((content-descriptor (vector-descriptor-content-descriptor descriptor)))
    (values (* index (bytestructure-descriptor-size content-descriptor))
            content-descriptor)))

(define-bytestructure-descriptor-compound-type
  'vector
  vector-descriptor
  vector-descriptor?
  vector-descriptor-size
  vector-constructor-helper
  vector-accessor-helper)

;;; Helpers for Structures and Unions

(define field-name car)
(define field-content-descriptor cdr)
(define field-find assq)

(define (construct-fields fields)
  (map (lambda (field)
         (assert (and (list? field)
                      (= 2 (length field))
                      (symbol? (car field))))
         (cons (car field)
               (bytestructure-descriptor (cadr field))))
       fields))

;;; Struct

(define-record-type :struct-descriptor
  (struct-descriptor* fields size)
  struct-descriptor?
  (fields struct-descriptor-fields)
  (size struct-descriptor-size))

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
                   (index index))
      (if (= index 0)
          (values offset (field-content-descriptor field))
          (if (not (null? fields))
              (try-next (car fields)
                        (cdr fields)
                        (+ offset (bytestructure-descriptor-size
                                   (field-content-descriptor field)))
                        (- index 1))
              (error "Struct field index out of bounds." index))))))

(define (struct-accessor-helper descriptor key)
  (let ((fields (struct-descriptor-fields descriptor)))
    (let try-next ((field (car fields))
                   (fields (cdr fields))
                   (offset 0))
      (if (eq? (field-name field) key)
          (values offset (field-content-descriptor field))
          (if (not (null? fields))
              (try-next (car fields)
                        (cdr fields)
                        (+ offset (bytestructure-descriptor-size
                                   (field-content-descriptor field))))
              (error "No such struct field." key))))))

(define-bytestructure-descriptor-compound-type
  'struct
  struct-descriptor
  struct-descriptor?
  struct-descriptor-size
  struct-constructor-helper
  struct-accessor-helper)

;;; Union

(define-record-type :union-descriptor
  (union-descriptor* fields size)
  union-descriptor?
  (fields union-descriptor-fields)
  (size union-descriptor-size))

(define (union-descriptor . fields)
  (assert (list? fields))
  (let ((fields (construct-fields fields)))
    (union-descriptor*
     fields (apply max (map (lambda (field)
                              (bytestructure-descriptor-size
                               (field-content-descriptor field)))
                            fields)))))

(define (union-constructor-helper descriptor index)
  (values 0 (field-content-descriptor
             (list-ref (union-descriptor-fields descriptor) index))))

(define (union-accessor-helper descriptor key)
  (values 0 (field-content-descriptor
             (field-find key (union-descriptor-fields descriptor)))))

(define-bytestructure-descriptor-compound-type
  'union
  union-descriptor
  union-descriptor?
  union-descriptor-size
  union-constructor-helper
  union-accessor-helper)

;;; Numeric types

(let-syntax
    ((define-numeric-types
       (syntax-rules ()
         ((_ (name record-type constructor predicate size accessor mutator) ...)
          (begin
            (begin
              (define-record-type record-type
                (constructor)
                predicate)
              (define-bytestructure-descriptor-type
                'name
                constructor
                predicate
                size
                (lambda (bytevector descriptor offset)
                  (accessor bytevector offset))
                (lambda (bytevector descriptor offset value)
                  (mutator bytevector offset value))))
            ...)))))
  (define-numeric-types
    (float :float-descriptor float-descriptor float-descriptor? 4
           bytevector-ieee-single-native-ref
           bytevector-ieee-single-native-set!)
    (double :double-descriptor double-descriptor double-descriptor? 8
            bytevector-ieee-double-native-ref
            bytevector-ieee-double-native-set!)
    (int8 :int8-descriptor int8-descriptor int8-descriptor? 1
          bytevector-s8-ref
          bytevector-s8-set!)
    (uint8 :uint8-descriptor uint8-descriptor uint8-descriptor? 1
           bytevector-u8-ref
           bytevector-u8-set!)
    (int16 :int16-descriptor int16-descriptor int16-descriptor? 2
           bytevector-s16-native-ref
           bytevector-s16-native-set!)
    (uint16 :uint16-descriptor uint16-descriptor uint16-descriptor? 2
            bytevector-u16-native-ref
            bytevector-u16-native-set!)
    (int32 :int32-descriptor int32-descriptor int32-descriptor? 4
           bytevector-s32-native-ref
           bytevector-s32-native-set!)
    (uint32 :uint32-descriptor uint32-descriptor uint32-descriptor? 4
            bytevector-u32-native-ref
            bytevector-u32-native-set!)
    (int64 :int64-descriptor int64-descriptor int64-descriptor? 8
           bytevector-s64-native-ref
           bytevector-s64-native-set!)
    (uint64 :uint64-descriptor uint64-descriptor uint64-descriptor? 8
            bytevector-u64-native-ref
            bytevector-u64-native-set!)))
