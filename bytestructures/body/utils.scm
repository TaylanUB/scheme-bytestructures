;;; utils.scm --- Utility library for bytestructures.

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

;; Just some utility procedures and macros.


;;; Code:

(define-syntax define-syntax-rule
  (syntax-rules ()
    ((_ (<name> . <args>) <expr>)
     (define-syntax <name>
       (syntax-rules ()
         ((_ . <args>)
          <expr>))))))

(cond-expand
 ((or guile syntax-case)
  (define-syntax-rule (if-syntax-case <then> <else>)
    <then>))
 (else
  (define-syntax-rule (if-syntax-case <then> <else>)
    <else>)))

(define-syntax-rule (define-syntax-case-stubs <name> ...)
  (if-syntax-case
   (begin)
   (begin
     (define (<name> . rest)
       (error "Not supported.  You need syntax-case."))
     ...)))

(define-syntax-case-stubs
  syntax
  quasisyntax
  unsyntax
  unsyntax-splicing
  syntax->datum
  datum->syntax)

;;; utils.scm ends here
