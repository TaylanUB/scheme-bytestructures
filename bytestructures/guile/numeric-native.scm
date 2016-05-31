;;; numeric-native.scm --- Native numeric types, via Guile's FFI module.

;; Copyright © 2015, 2016 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>

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

;; This module defines descriptors for platform-specific numeric types, as made
;; possible by Guile's FFI module.


;;; Code:

(define-module (bytestructures guile numeric-native))
(import
 (bytestructures guile utils)
 (bytestructures guile numeric)
 (only (system foreign) sizeof))
(export
 short unsigned-short int unsigned-int long unsigned-long
 size_t ssize_t ptrdiff_t
 float double
 )

(define-syntax define-native-synonyms
  (syntax-rules ()
    ((_ (signed ...) (unsigned ...) (float ...))
     (begin
       (define signed
         (case (sizeof (@ (system foreign) signed))
           ((1) int8)
           ((2) int16)
           ((4) int32)
           ((8) int64)))
       ...
       (define unsigned
         (case (sizeof (@ (system foreign) unsigned))
           ((1) uint8)
           ((2) uint16)
           ((4) uint32)
           ((8) uint64)))
       ...
       (define float
         (case (sizeof (@ (system foreign) float))
           ((4) float32)
           ((8) float64)))
       ...))))

(define-native-synonyms
  (short int long ssize_t ptrdiff_t)
  (unsigned-short unsigned-int unsigned-long size_t)
  (float double))

;;; numeric-native.scm ends here
