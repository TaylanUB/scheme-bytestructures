;;; vector.scm --- Vector descriptor constructor.

;; Copyright © 2015, 2016 Taylan Kammer <taylan.kammer@gmail.com>

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

(define-record-type <vector-metadata>
  (make-vector-metadata length element-descriptor)
  vector-metadata?
  (length             vector-metadata-length)
  (element-descriptor vector-metadata-element-descriptor))

(define (bs:vector length descriptor)
  (define element-size (bytestructure-descriptor-size descriptor))
  (define size (* length element-size))
  (define alignment (bytestructure-descriptor-alignment descriptor))
  (define (unwrapper syntax? bytevector offset index)
    (values bytevector
            (if syntax?
                (quasisyntax
                 (+ (unsyntax offset)
                    (* (unsyntax index) (unsyntax element-size))))
                (+ offset (* index element-size)))
            descriptor))
  (define (setter syntax? bytevector offset value)
    (when syntax?
      (error "Writing into vector not supported with macro API."))
    (cond
     ((bytevector? value)
      (bytevector-copy! bytevector offset value 0 size))
     ((vector? value)
      (do ((i 0 (+ i 1))
           (offset offset (+ offset element-size)))
          ((= i (vector-length value)))
        (bytestructure-set!*
         bytevector offset descriptor (vector-ref value i))))
     (else
      (error "Invalid value for writing into vector." value))))
  (define meta (make-vector-metadata length descriptor))
  (make-bytestructure-descriptor size alignment unwrapper #f setter meta))

;;; vector.scm ends here
