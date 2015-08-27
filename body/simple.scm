;;; simple.scm --- "Simple" descriptor type

;; Copyright (C) 2013 - 2015  Taylan Ulrich Bayırlı/Kammer

;; Author: Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;; Keywords: ffi struct bytevector bytestructure simple

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

;; This descriptor type offers a way to create descriptors that convert
;; fixed-size, fixed-layout objects to binary and back; the constructor takes
;; only a size, a bytevector-ref procedure, and a bytevector-set procedure.


;;; Code:

(define-record-type <simple>
  (make-simple size ref-proc set-proc ref-id set-id)
  simple?
  (size     %simple-size)
  (ref-proc simple-ref-proc)
  (set-proc simple-set-proc)
  (ref-id   simple-ref-id)
  (set-id   simple-set-id))

(define (simple-size bytevector offset simple)
  (%simple-size simple))

(define (simple-ref bytevector offset simple)
  ((simple-ref-proc simple) bytevector offset))

(define (simple-set! bytevector offset simple value)
  ((simple-set-proc simple) bytevector offset value))

(define/sc (simple-ref/synax bytevector offset simple)
  (quasisyntax
   ((unsyntax (simple-ref-id simple)) (unsyntax bytevector) (unsyntax offset))))

(define/sc (simple-set!/syntax bytevector offset simple value)
  (quasisyntax
   ((unsyntax (simple-set-id simple))
    (unsyntax bytevector) (unsyntax offset) (value))))

(define bs:simple
  (make-bytestructure-descriptor-type
   make-simple simple-size
   #f simple-ref simple-set!
   #f simple-ref/synax simple-set!/syntax))

;;; simple.el ends here
