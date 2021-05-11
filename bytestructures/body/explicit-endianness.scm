;;; explicit-endianness.scm --- Auxiliary bytevector operations.

;; Copyright © 2015 Taylan Kammer <taylan.kammer@gmail.com>

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

;; The numeric module requires top-level bindings to bytevector procedures with
;; an explicit endianness, instead of the ones that take an endianness
;; argument.  This library provides them.


;;; Code:

(define-syntax define-explicit-endianness-getters
  (syntax-rules ()
    ((_ (original le-name be-name) ...)
     (begin
       (begin
         (define (le-name bytevector index)
           (original bytevector index (endianness little)))
         (define (be-name bytevector index)
           (original bytevector index (endianness big))))
       ...))))

(define-explicit-endianness-getters
  (bytevector-ieee-single-ref bytevector-ieee-single-le-ref
                              bytevector-ieee-single-be-ref)
  (bytevector-ieee-double-ref bytevector-ieee-double-le-ref
                              bytevector-ieee-double-be-ref)
  (bytevector-s16-ref bytevector-s16le-ref
                      bytevector-s16be-ref)
  (bytevector-u16-ref bytevector-u16le-ref
                      bytevector-u16be-ref)
  (bytevector-s32-ref bytevector-s32le-ref
                      bytevector-s32be-ref)
  (bytevector-u32-ref bytevector-u32le-ref
                      bytevector-u32be-ref)
  (bytevector-s64-ref bytevector-s64le-ref
                      bytevector-s64be-ref)
  (bytevector-u64-ref bytevector-u64le-ref
                      bytevector-u64be-ref))

(define-syntax define-explicit-endianness-setters
  (syntax-rules ()
    ((_ (original le-name be-name) ...)
     (begin
       (begin
         (define (le-name bytevector index value)
           (original bytevector index value (endianness little)))
         (define (be-name bytevector index value)
           (original bytevector index value (endianness big))))
       ...))))

(define-explicit-endianness-setters
  (bytevector-ieee-single-set! bytevector-ieee-single-le-set!
                               bytevector-ieee-single-be-set!)
  (bytevector-ieee-double-set! bytevector-ieee-double-le-set!
                               bytevector-ieee-double-be-set!)
  (bytevector-s16-set! bytevector-s16le-set!
                       bytevector-s16be-set!)
  (bytevector-u16-set! bytevector-u16le-set!
                       bytevector-u16be-set!)
  (bytevector-s32-set! bytevector-s32le-set!
                       bytevector-s32be-set!)
  (bytevector-u32-set! bytevector-u32le-set!
                       bytevector-u32be-set!)
  (bytevector-s64-set! bytevector-s64le-set!
                       bytevector-s64be-set!)
  (bytevector-u64-set! bytevector-u64le-set!
                       bytevector-u64be-set!))

;;; explicit-endianness.scm ends here
