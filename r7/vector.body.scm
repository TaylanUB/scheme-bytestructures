;;; vector.scm --- Vector descriptor type.

;; Copyright (C) 2013  Taylan Ulrich Bayırlı/Kammer

;; Author: Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;; Keywords: ffi struct bytevector bytestructure vector

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

;; This descriptor type allows the creation of vector descriptors of a specific
;; element descriptor and length.

;; Be careful with identifier names here; don't confuse vector descriptor and
;; Scheme vector APIs and variables.


;;; Code:

(define-record-type <vector>
  (%make-vector length content size)
  %vector?
  (length  %vector-length)
  (content %vector-content)
  (size    %vector-size))

(define (make-vector length content-description)
  (let ((content (make-bytestructure-descriptor content-description)))
    (%make-vector length content
                  (* length (bytestructure-descriptor-size content)))))

(define (vector-size bytevector offset vector)
  (%vector-size vector))

(define (vector-ref-helper bytevector offset vector index)
  (let ((content (%vector-content vector)))
    (values bytevector
            (+ offset
               (* index (bytestructure-descriptor-size
                         bytevector offset content)))
            content)))

(define (vector-set! bytevector offset vector values)
  (cond
   ((vector? values)
    (let* ((content (%vector-content vector))
           (content-size (bytestructure-descriptor-size content)))
      (do ((i 0 (+ 1 i))
           (offset offset (+ offset content-size)))
          ((= i (vector-length values)))
        (bytestructure-set!* bytevector offset content (vector-ref values i)))))
   ((bytevector? values)
    (bytevector-copy! bytevector offset values 0 (%vector-size vector)))
   (else
    (error "Vector type failed to write:" values))))

(define bs:vector
  (make-bytestructure-descriptor-type
   make-vector vector-size
   vector-ref-helper #f vector-set!))

;;; vector.scm ends here
