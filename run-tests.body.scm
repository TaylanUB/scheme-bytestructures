;;; run-tests.body.scm --- Bytestructures test suite.

;; Copyright © 2015, 2021 Taylan Kammer <taylan.kammer@gmail.com>

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

;; A relatively simple SRFI-64 test suite.


;;; Code:

(define-syntax-rule (test-= name expected expr)
  (test-approximate name expected expr 0))

(define-syntax-rule (maybe-skip-syntax . <body>)
  (if-syntax-case
   (begin . <body>)
   (begin)))

(test-begin "bytestructures")

(test-group "numeric"
  (define-syntax test-numeric-descriptors
    (syntax-rules ()
      ((_ <descriptor-id> ...)
       (let ()
         (define (destructure-numeric-descriptor-entry descriptor-entry proc)
           (define descriptor (list-ref descriptor-entry 0))
           (define name (list-ref descriptor-entry 1))
           (define getter (list-ref descriptor-entry 2))
           (define setter (list-ref descriptor-entry 3))
           (define size (bytestructure-descriptor-size descriptor))
           (define float? (assq descriptor float-descriptors))
           (define signed? (or float? (assq descriptor signed-integer-descriptors)))
           (proc descriptor name getter setter size float? signed?))
         (define (get-min/max float? signed? size)
           (cond
            (float?  (inexact (expt 2 (case size ((4) 24) ((8) 53)))))
            (signed? (- (expt 256 (- size 1))))
            (else    (- (expt 256 size) 1))))
         (destructure-numeric-descriptor-entry
          (assq <descriptor-id> numeric-descriptors)
          (lambda (descriptor name getter setter size float? signed?)
            (test-group (symbol->string name)
              (let ((test-value-1 (if float? 1.0 1))
                    (test-value-2 (if float? 2.0 1)))
                (test-group "procedural"
                  (define min/max (get-min/max float? signed? size))
                  (define bs (bytestructure descriptor))
                  (test-eqv "size" size (bytevector-length
                                         (bytestructure-bytevector bs)))
                  (test-= "ref" test-value-1
                    (begin
                      (setter (bytestructure-bytevector bs) 0 test-value-1)
                      (bytestructure-ref bs)))
                  (test-= "set" test-value-2
                    (begin
                      (bytestructure-set! bs test-value-2)
                      (getter (bytestructure-bytevector bs) 0)))
                  (test-= "min/max" min/max
                    (begin
                      (bytestructure-set! bs min/max)
                      (bytestructure-ref bs))))
                (maybe-skip-syntax
                 (test-group "syntactic"
                   (define min/max (get-min/max float? signed? size))
                   ;; Must insert the top-level reference <descriptor-id> here.
                   (define-bytestructure-accessors <descriptor-id>
                     bs-unwrapper bs-getter bs-setter)
                   (define bv (make-bytevector size))
                   (test-= "ref" test-value-1
                     (begin
                       (setter bv 0 test-value-1)
                       (bs-getter bv)))
                   (test-= "set" test-value-2
                     (begin
                       (bs-setter bv test-value-2)
                       (getter bv 0)))
                   (test-= "min/max" min/max
                     (begin
                       (bs-setter bv min/max)
                       (bs-getter bv)))))))))
         ...))))
  (test-numeric-descriptors
   float32 float32le float32be
   float64 float64le float64be
   int8 int16 int32 int64
   int16le int32le int64le
   int16be int32be int64be
   uint8 uint16 uint32 uint64
   uint16le uint32le uint64le
   uint16be uint32be uint64be))

(test-group "vector"
  (test-assert "create" (bs:vector 3 uint16))
  (test-group "procedural"
    (define bs (bytestructure (bs:vector 3 uint16)))
    (bytevector-u16-native-set! (bytestructure-bytevector bs) 2 321)
    (test-eqv "ref" 321 (bytestructure-ref bs 1))
    (test-eqv "set" 456 (begin (bytestructure-set! bs 1 456)
                               (bytestructure-ref bs 1)))
    (test-eqv "init" 321
      (let ((bs (bytestructure (bs:vector 3 uint16) '#(321 123 321))))
        (bytestructure-ref bs 2))))
  (maybe-skip-syntax
   (test-group "syntactic"
     (define-bytestructure-accessors (bs:vector 3 uint16)
       unwrapper getter setter)
     (define bv (make-bytevector 6))
     (bytevector-u16-native-set! bv 2 321)
     (test-eqv "ref" 321 (getter bv 1))
     (test-eqv "set" 456 (begin (setter bv 1 456)
                                (getter bv 1))))))

(test-group "struct"
  (test-group "aligned"
    (test-assert "create" (bs:struct `((x ,uint8) (y ,uint16))))
    (test-group "procedural"
      (define bs (bytestructure (bs:struct `((x ,uint8) (y ,uint16)))))
      (bytevector-u16-native-set! (bytestructure-bytevector bs) 2 321)
      (test-eqv "ref" 321 (bytestructure-ref bs 'y))
      (test-eqv "set" 456 (begin (bytestructure-set! bs 'y 456)
                                 (bytestructure-ref bs 'y)))
      (test-eqv "init" 321
        (let ((bs (bytestructure (bs:struct `((x ,uint8) (y ,uint16)))
                                 '#(123 321))))
          (bytestructure-ref bs 'y))))
    (maybe-skip-syntax
     (test-group "syntactic"
       (define-bytestructure-accessors (bs:struct `((x ,uint8) (y ,uint16)))
         unwrapper getter setter)
       (define bv (make-bytevector 4))
       (bytevector-u16-native-set! bv 2 321)
       (test-eqv "ref" 321 (getter bv y))
       (test-eqv "set" 456 (begin (setter bv y 456)
                                  (getter bv y))))))
  (test-group "packed"
    (test-assert "create" (bs:struct #t `((x ,uint8) (y ,uint16))))
    (test-group "procedural"
      (define bs (bytestructure (bs:struct #t `((x ,uint8) (y ,uint16)))))
      ;; u16-native-set! may error on non-aligned access.
      (guard (err (else (test-skip 3)))
        (bytevector-u16-native-set! (bytestructure-bytevector bs) 1 321))
      (test-eqv "ref" 321 (bytestructure-ref bs 'y))
      (test-eqv "set" 456 (begin (bytestructure-set! bs 'y 456)
                                 (bytestructure-ref bs 'y)))
      (test-eqv "init" 321
        (let ((bs (bytestructure (bs:struct #t `((x ,uint8) (y ,uint16)))
                                 '#(123 321))))
          (bytestructure-ref bs 'y))))
    (maybe-skip-syntax
     (test-group "syntactic"
       (define-bytestructure-accessors (bs:struct #t `((x ,uint8) (y ,uint16)))
         unwrapper getter setter)
       (define bv (make-bytevector 4))
       ;; u16-native-set! may error on non-aligned access.
       (guard (err (else (test-skip 2)))
         (bytevector-u16-native-set! bv 1 321))
       (test-eqv "ref" 321 (getter bv y))
       (test-eqv "set" 456 (begin (setter bv y 456)
                                  (getter bv y))))))

  (test-group "anonymous-union"
    (test-assert "create"
      (bs:struct
       `((x ,uint8)
         (union
          ((a ,uint16)
           (b ,uint32))))))
    ;; Don't use 64-bit ints; their alignment differs between platforms.
    (test-group "aligned"
      (define bs
        (bytestructure
         (bs:struct
          `((union
             ((x ,uint8)
              (y ,uint16)))
            (union
             ((a ,uint16)
              (b ,uint32)))))))
      (test-eqv "size" 8 (bytevector-length (bytestructure-bytevector bs)))
      (bytevector-u16-native-set! (bytestructure-bytevector bs) 4 321)
      (test-eqv "ref1" 321 (bytestructure-ref bs 'a))
      (bytevector-u32-native-set! (bytestructure-bytevector bs) 4 456)
      (test-eqv "ref2" 456 (bytestructure-ref bs 'b))
      (test-eqv "set1" 789 (begin (bytestructure-set! bs 'a 789)
                                  (bytestructure-ref bs 'a)))
      (test-eqv "set2" 987 (begin (bytestructure-set! bs 'b 987)
                                  (bytestructure-ref bs 'b))))
    (test-group "packed"
      (define bs
        (bytestructure
         (bs:struct
          #t
          `((union
             ((x ,uint8)
              (y ,uint16)))
            (union
             ((a ,uint16)
              (b ,uint32)))))))
      (test-eqv "size" 6 (bytevector-length (bytestructure-bytevector bs)))
      (bytevector-u16-native-set! (bytestructure-bytevector bs) 2 321)
      (test-eqv "ref1" 321 (bytestructure-ref bs 'a))
      (bytevector-u32-native-set! (bytestructure-bytevector bs) 2 456)
      (test-eqv "ref2" 456 (bytestructure-ref bs 'b))
      (test-eqv "set1" 789 (begin (bytestructure-set! bs 'a 789)
                                  (bytestructure-ref bs 'a)))
      (test-eqv "set2" 987 (begin (bytestructure-set! bs 'b 987)
                                  (bytestructure-ref bs 'b))))))

(test-group "union"
  (test-assert "create" (bs:union `((x ,uint8) (y ,uint16))))
  (test-group "procedural"
    (define bs (bytestructure (bs:union `((x ,uint8) (y ,uint16)))))
    (bytevector-u16-native-set! (bytestructure-bytevector bs) 0 321)
    (test-eqv "ref" 321 (bytestructure-ref bs 'y))
    (test-eqv "set" 456 (begin (bytestructure-set! bs 'y 456)
                               (bytestructure-ref bs 'y))))
  (maybe-skip-syntax
   (test-group "syntactic"
     (define-bytestructure-accessors (bs:union `((x ,uint8) (y ,uint16)))
       unwrapper getter setter)
     (define bv (make-bytevector 2))
     (bytevector-u16-native-set! bv 0 321)
     (test-eqv "ref" 321 (getter bv y))
     (test-eqv "set" 456 (begin (setter bv y 456)
                                (getter bv y))))))

(test-group "string"
  (test-group "ascii"
    (test-assert "create" (bs:string 4 'ascii))
    (test-group "procedural"
      (define bsd (bs:string 4 'ascii))
      (define bs (make-bytestructure (string->utf8 "1234") 0 bsd))
      (test-equal "ref" "1234" (bytestructure-ref bs))
      (test-equal "set" "4321" (begin
                                 (bytestructure-set! bs "4321")
                                 (bytestructure-ref bs)))
      (test-error "too-long" #t (bytestructure-set! bs "12345"))
      (test-error "too-short" #t (bytestructure-set! bs "123"))
      (set! bs (make-bytestructure (string->utf8 "äåãø") 0 bsd))
      (test-error "decoding-error" #t (bytestructure-ref bs))
      (test-error "encoding-error" #t (bytestructure-set! bs "øãåä")))
    (test-group "syntactic"
      (define-bytestructure-accessors (bs:string 4 'ascii)
        unwrapper getter setter)
      (define bv (string->utf8 "1234"))
      (test-equal "ref" "1234" (getter bv))
      (test-equal "set" "4321" (begin
                                 (setter bv "4321")
                                 (getter bv)))
      (test-error "too-long" #t (setter bv "12345"))
      (test-error "too-short" #t (setter bv "123"))
      (set! bv (string->utf8 "äåãø"))
      (test-error "ref-error" #t (getter bv))
      (test-error "set-error" #t (setter bv "øãåä"))))
  (test-group "utf8"
    (test-assert "create" (bs:string 4 'utf8))
    (test-group "procedural"
      (define bsd (bs:string 4 'utf8))
      (define bs (make-bytestructure (string->utf8 "1234") 0 bsd))
      (test-equal "ref" "1234" (bytestructure-ref bs))
      (test-equal "set" "4321" (begin
                                 (bytestructure-set! bs "4321")
                                 (bytestructure-ref bs)))
      (test-error "too-long" #t (bytestructure-set! bs "äåãø"))
      (test-equal (string-append "123" (string #\nul))
                  (begin
                    (bytestructure-set! bs "123")
                    (bytestructure-ref bs))))
    (test-group "syntactic"
      (define-bytestructure-accessors (bs:string 4 'utf8)
        unwrapper getter setter)
      (define bv (string->utf8 "1234"))
      (test-equal "ref" "1234" (getter bv))
      (test-equal "set" "4321" (begin
                                 (setter bv "4321")
                                 (getter bv)))
      (test-error "too-long" #t (setter bv "äåãø"))
      (test-equal (string-append "123" (string #\nul))
                  (begin
                    (setter bv "123")
                    (getter bv)))))
  (let ()
    (define-syntax-rule
      (test-string-encodings
       (<name> <encoding> <endianness> <size> <fixed-width?> <string->utf>)
       ...)
      (begin
        (test-group <name>
          (test-assert "create" (bs:string <size> '<encoding>))
          (test-group "procedural"
            (define bs (make-bytestructure (<string->utf> "1234" '<endianness>)
                                           0
                                           (bs:string <size> '<encoding>)))
            (test-equal "ref" "1234" (bytestructure-ref bs))
            (test-equal "set" "4321" (begin
                                       (bytestructure-set! bs "4321")
                                       (bytestructure-ref bs)))
            (test-error "too-long" #t (bytestructure-set! bs "12345"))
            (if <fixed-width?>
                (test-error "too-short" #t (bytestructure-set! bs "123"))
                (test-equal (string-append "123" (string #\nul))
                            (begin
                              (bytestructure-set! bs "123")
                              (bytestructure-ref bs)))))
          (test-group "syntactic"
            (define-bytestructure-accessors (bs:string <size> '<encoding>)
              unwrapper getter setter)
            (define bv (<string->utf> "1234" '<endianness>))
            (test-equal "ref" "1234" (getter bv))
            (test-equal "set" "4321" (begin
                                       (setter bv "4321")
                                       (getter bv)))
            (test-error "too-long" #t (setter bv "12345"))
            (if <fixed-width?>
                (test-error "too-short" #t (setter bv "123"))
                (test-equal (string-append "123" (string #\nul))
                            (begin
                              (setter bv "123")
                              (getter bv))))))
        ...))
    (test-string-encodings
     ("utf16le" utf16le little 8 #f string->utf16)
     ("utf16be" utf16be big 8 #f string->utf16)
     ("utf32le" utf32le little 16 #t string->utf32)
     ("utf32be" utf32be big 16 #t string->utf32))))

(cond-expand
 (guile
  (let ()

    (define (protect-from-gc-upto-here obj)
      (with-output-to-file *null-device*
        (lambda ()
          (display (eq? #f obj)))))

    (define pointer-size (ffi:sizeof '*))
    (define bytevector-address-set!
      (case pointer-size
        ((1) bytevector-u8-set!)
        ((2) bytevector-u16-native-set!)
        ((4) bytevector-u32-native-set!)
        ((8) bytevector-u64-native-set!)))

    (test-group "pointer"
      (test-assert "create" (bs:pointer uint16))
      (test-group "procedural"
        (define bs (bytestructure (bs:pointer uint16)))
        (define bv1 (make-bytevector 2))
        (define bv2 (make-bytevector 4))
        (define address1 (ffi:pointer-address (ffi:bytevector->pointer bv1)))
        (define address2 (ffi:pointer-address (ffi:bytevector->pointer bv2)))
        (bytevector-address-set! (bytestructure-bytevector bs) 0 address1)
        (bytevector-u16-native-set! bv1 0 321)
        (test-eqv "ref1" 321 (bytestructure-ref bs '*))
        (test-eqv "set1" 456 (begin (bytestructure-set! bs '* 456)
                                    (bytestructure-ref bs '*)))
        (test-eqv "ref2" address1 (bytestructure-ref bs))
        (test-eqv "set2" address2 (begin (bytestructure-set! bs address2)
                                         (bytestructure-ref bs)))
        (bytevector-address-set! (bytestructure-bytevector bs) 0 address2)
        (bytevector-u16-native-set! bv2 2 456)
        (test-eqv "ref3" 456 (bytestructure-ref bs 1))
        (test-eqv "set3" 789 (begin (bytestructure-set! bs 1 789)
                                    (bytestructure-ref bs 1)))
        (protect-from-gc-upto-here bv1)
        (protect-from-gc-upto-here bv2))
      (test-group "syntactic"
        (define-bytestructure-accessors (bs:pointer uint16)
          unwrapper getter setter)
        (define bv (make-bytevector pointer-size))
        (define bv1 (make-bytevector 2))
        (define bv2 (make-bytevector 4))
        (define address1 (ffi:pointer-address (ffi:bytevector->pointer bv1)))
        (define address2 (ffi:pointer-address (ffi:bytevector->pointer bv2)))
        (bytevector-address-set! bv 0 address1)
        (bytevector-u16-native-set! bv1 0 321)
        (test-eqv "ref" 321 (getter bv *))
        (test-eqv "set" 456 (begin (setter bv * 456)
                                   (getter bv *)))
        (test-eqv "ref2" address1 (getter bv))
        (test-eqv "set2" address1 (begin (setter bv address1)
                                         (getter bv)))
        (bytevector-address-set! bv 0 address2)
        (bytevector-u16-native-set! bv2 2 456)
        (test-eqv "ref3" 456 (getter bv 1))
        (test-eqv "set3" 789 (begin (setter bv 1 789)
                                    (getter bv 1)))
        (protect-from-gc-upto-here bv1)
        (protect-from-gc-upto-here bv2)))

    (test-group "cstring-pointer"
      (let* ((cstr1-ptr (ffi:string->pointer "abc"))
             (cstr2-ptr (ffi:string->pointer "cba"))
             (cstr1-addr (ffi:pointer-address cstr1-ptr))
             (cstr2-addr (ffi:pointer-address cstr2-ptr)))
        (test-group "procedural"
          (define bs (bytestructure cstring-pointer))
          (bytevector-address-set! (bytestructure-bytevector bs) 0 cstr1-addr)
          (test-equal "ref" "abc" (bytestructure-ref bs))
          (test-equal "set" "cba" (begin (bytestructure-set! bs cstr2-addr)
                                         (bytestructure-ref bs))))
        (test-group "syntactic"
          (define-bytestructure-accessors cstring-pointer
            unwrapper getter setter)
          (define bv (make-bytevector pointer-size))
          (bytevector-address-set! bv 0 cstr1-addr)
          (test-equal "ref" "abc" (getter bv))
          (test-equal "set" "cba" (begin (setter bv cstr2-addr)
                                         (getter bv))))))))

 (else
  ))

;; Do this before test-end since it removes the auto-inserted test runner.
(define success
  (let ((runner (test-runner-current)))
    (and (zero? (test-runner-xpass-count runner))
         (zero? (test-runner-fail-count runner)))))

(test-end "bytestructures")

(exit (if success 0 1))

;; Local Variables:
;; eval: (put (quote test-group) (quote scheme-indent-function) 1)
;; eval: (put (quote test-=) (quote scheme-indent-function) 2)
;; End:
