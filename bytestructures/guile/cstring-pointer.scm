;;; cstring-pointer.scm --- Pointers to null-terminated strings.

;; Copyright © 2017 Taylan Kammer <taylan.kammer@gmail.com>

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

;; The cstring-pointer descriptor represents a pointer to a null-terminated
;; string, and will return the string as a Scheme string upon a reference
;; operation.  Its setter however does not take Scheme strings, only addresses
;; to existing strings in memory.  The reason is: Guile's string->pointer
;; creates a new C string in memory, returning an FFI pointer object holding its
;; address; the string is freed when the pointer object is garbage collected.
;; We have no means of holding a reference to the FFI pointer object; we can
;; only write the address it holds into our bytevector, which won't protect the
;; pointer object from GC.


;;; Code:

(define-module (bytestructures guile cstring-pointer))
(import
 (bytestructures guile base)
 (bytestructures guile numeric)
 (prefix (system foreign) ffi:))
(export cstring-pointer)

(define (bytevector-address-ref bv offset)
  (bytestructure-ref* bv offset uintptr_t))

(define (bytevector-address-set! bv offset address)
  (bytestructure-set!* bv offset uintptr_t address))

(define cstring-pointer
  (let ()
    (define size (bytestructure-descriptor-size intptr_t))
    (define alignment (bytestructure-descriptor-alignment intptr_t))
    (define unwrapper #f)
    (define (getter syntax? bv offset)
      (if syntax?
          #`(let* ((address (bytevector-address-ref #,bv #,offset))
                   (pointer (ffi:make-pointer address)))
              (ffi:pointer->string pointer))
          (let* ((address (bytevector-address-ref bv offset))
                 (pointer (ffi:make-pointer address)))
            (ffi:pointer->string pointer))))
    (define (setter syntax? bv offset address)
      (if syntax?
          #`(bytevector-address-set! #,bv #,offset #,address)
          (bytevector-address-set! bv offset address)))
    (make-bytestructure-descriptor size alignment unwrapper getter setter)))
