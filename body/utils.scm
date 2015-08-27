;;; utils.scm --- Utility library for bytestructures.

;; Copyright (C) 2015  Taylan Ulrich Bayırlı/Kammer

;; Author: Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;; Keywords: ffi struct bytevector bytestructure struct

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
    ((_ (<name> . <args>) . <body>)
     (define-syntax <name>
       (syntax-rules ()
         ((_ . <args>)
          (begin . <body>)))))))

(define-syntax-rule (if-syntax-case <then> <else>)
  (cond-expand
   ((or guile syntax-case)
    <then>)
   (else
    <else>)))

(define-syntax-rule (define/sc (<name> . <args>) . <rest>)
  (if-syntax-case
   (define (<name> . <args>)
     . <rest>)
   (define (<name> . <args>)
     (error "Not implemented.  You need syntax-case."))))

(define-syntax-rule (define-syntax/sc <name> <expr>)
  (if-syntax-case
   (define-syntax <name> <expr>)
   (define-syntax-rule (<name> . rest)
     (syntax-error "Not implemented.  You need syntax-case."))))

(define-syntax-rule (define-syntax-rule/sc <pattern> . <body>)
  (if-syntax-case
   (define-syntax-rule <pattern> . <body>)
   (define-syntax-rule <pattern>
     (syntax-error "Not implemented.  You need syntax-case."))))

;;; utils.scm ends here
