;;; standard.scm --- Standard descriptors and descriptor types.

;; Copyright (C) 2013  Taylan Ulrich B.

;; Author: Taylan Ulrich B. <taylanbayirli@gmail.com>
;; Keywords: ffi struct bytevector bytestructure standard

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

;; This module is a shorthand for the base, and all descriptors and descriptor
;; types which come with the module.


;;; Code:

(define-module (bytestructures standard)
  #:use-module (bytestructures base)
  #:use-module (bytestructures simple)
  #:use-module (bytestructures vector)
  #:use-module (bytestructures struct)
  #:use-module (bytestructures union)
  #:use-module (bytestructures pointer)
  #:use-module (bytestructures numeric)
  #:use-module (bytestructures numeric-native)
  #:re-export
  (
   make-bytestructure-descriptor-type
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
   bytestructure-ref-helper
   bytestructure-ref-helper*
   bytestructure-ref
   bytestructure-ref*
   bytestructure-set!
   bytestructure-set!*
   bytestructure-pointer
            
   bs:simple bs:vector bs:struct bs:union bs:pointer
            
   float double int8 uint8 int16 uint16 int32 uint32 int64 uint64
   floatle doublele
   int16le uint16le int32le uint32le int64le uint64le
   floatbe doublebe
   int16be uint16be int32be uint32be int64be uint64be
            
   short unsigned-short int unsigned-int long unsigned-long
   size_t ssize_t ptrdiff_t
   ))

;;; standard.scm ends here
