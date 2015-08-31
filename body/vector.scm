;;; vector.scm --- Vector descriptor constructor.

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

;; This constructor allows the creation of vector descriptors with a specific
;; length and element descriptor.

;; Be careful with identifier names here; don't confuse vector descriptor and
;; Scheme vector APIs and variables.


;;; Code:

(define (bs:vector length descriptor)
  (define content-size (bytestructure-descriptor-size descriptor))
  (define size (* length content-size))
  (define alignment content-size)
  (define (ref-helper syntax? bytevector offset index)
    (values bytevector
            (if syntax?
                (quasisyntax
                 (+ (unsyntax offset)
                    (* (unsyntax index) (unsyntax content-size))))
                (+ offset (* index content-size)))
            descriptor))
  (define (setter syntax? bytevector offset value)
    (cond
     ((bytevector? value)
      (bytevector-copy! bytevector offset value 0 size))
     ((vector? value)
      (do ((i 0 (+ i 1))
           (offset offset (+ offset content-size)))
          ((= i (vector-length value)))
        (bytestructure-set!*
         bytevector offset descriptor (vector-ref value i))))
     (else
      (error "Invalid value for writing into vector." value))))
  (make-bytestructure-descriptor size alignment ref-helper #f setter))

;;; vector.scm ends here
