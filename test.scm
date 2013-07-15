(define-module (bytestructures test))

(use-modules (bytestructures procedural)
             (ice-9 format)
             (rnrs bytevectors)
             (srfi srfi-11))

(let-syntax
    ((df (syntax-rules ()
           ((df fn ...)
            (begin
              (define fn (@@ (bytestructures procedural) fn))
              ...)))))
  (df bytestructure-descriptor-constructor
      bytestructure-descriptor-type-size
      bytevector-ref-helper
      bytevector-ref-proc
      bytevector-set-proc
      bytestructure-descriptor-type
      bytestructure-descriptor-content))

(define-syntax test
  (syntax-rules ()
    ((test name expression ...)
     (begin
       (unless expression
         (error (format #f "Test ~s failed:" name) 'expression))
       ...))))

(define (main)
  (descriptor-type)
  (descriptors)
  (bytestructures)
  (builtin-types))


;;; Descriptor types

(define (descriptor-type)
  (let ((constructor (lambda () #f))
        (size (lambda () #f))
        (size-accessor (lambda () #f))
        (ref-helper (lambda () #f))
        (ref-proc (lambda () #f))
        (set-proc (lambda () #f)))
    (let ((desc-type
           (make-bytestructure-descriptor-type
            constructor size ref-helper ref-proc set-proc)))
      (test "descriptor type"
            (bytestructure-descriptor-type? desc-type)
            (eq? constructor (bytestructure-descriptor-constructor desc-type))
            (eqv? size (bytestructure-descriptor-type-size desc-type))
            (eq? ref-helper (bytevector-ref-helper desc-type))
            (eq? ref-proc (bytevector-ref-proc desc-type))
            (eq? set-proc (bytevector-set-proc desc-type))))))


;;; Descriptors

(define (descriptors)
  (wrong-argument-to-constructor)
  (argumentless-constructor)
  (constructor-with-arguments))

(define (wrong-argument-to-constructor)
  (test "wrong argument to `make-bytestructure-descriptor'"
        (not (false-if-exception
              (make-bytestructure-descriptor 'error)))))

(define (argumentless-constructor)
  (let* ((desc-type
          (make-bytestructure-descriptor-type
           (lambda () 0) 1 #f #f #f))
         (desc (make-bytestructure-descriptor desc-type)))
    (test "argumentless descriptor constructor"
          (bytestructure-descriptor? desc)
          (eq? desc-type (bytestructure-descriptor-type desc))
          (= 0 (bytestructure-descriptor-content desc))
          (= 1 (bytestructure-descriptor-size #f #f desc)))))

(define (constructor-with-arguments)
  (let* ((desc-type
          (make-bytestructure-descriptor-type
           (lambda (x) x) 1 #f #f #f))
         (desc (make-bytestructure-descriptor (list desc-type 'test))))
    (test "descriptor constructor with arguments"
          (bytestructure-descriptor? desc)
          (eq? desc-type (bytestructure-descriptor-type desc))
          (eq? 'test (bytestructure-descriptor-content desc))
          (= 1 (bytestructure-descriptor-size #f #f desc)))))


;;; Bytestructures

(define (bytestructures)
  (anonymous-descriptor)
  (descriptor-instance)
  (descriptor-instance-with-default-ref-set)
  (write-error))

(define (anonymous-descriptor)
  (let ((desc-type
         (make-bytestructure-descriptor-type
          (lambda (x) x) 3
          #f
          (lambda (bv offs desc)
            (+ desc (bytevector-u8-ref bv offs)))
          (lambda (bv offs desc val)
            (bytevector-u8-set! bv offs (+ desc val))))))
    (test "anonymous descriptor"
          (let ((bs (bytestructure (list desc-type 5))))
            (bytestructure? bs))
          (let ((bs (bytestructure (list desc-type 5))))
            (= 3 (bytevector-length (bytestructure-bytevector bs))))
          (let ((bs (bytestructure (list desc-type 5) 0)))
            (= 10 (bytestructure-ref bs)))
          (let ((bs (bytestructure (list desc-type 5))))
            (bytestructure-set! bs 1)
            (= 11 (bytestructure-ref bs))))))

(define (descriptor-instance)
  (let* ((desc-type
          (make-bytestructure-descriptor-type
           (lambda (size desc) (cons size desc))
           (lambda (bv offs desc) (car desc))
           (lambda (bv offs desc idx) (values bv idx (cdr desc)))
           (lambda (bv offs desc)
             (bytestructure-ref* bv offs (cdr desc)))
           (lambda (bv offs desc val)
             (bytestructure-set!* bv offs (cdr desc) val))))
         (desc (make-bytestructure-descriptor `(,desc-type 3 ,uint8))))
    (test "descriptor instance"
          (let ((bs (bytestructure desc)))
            (bytestructure? bs))
          (let ((bs (bytestructure desc)))
            (= 3 (bytevector-length (bytestructure-bytevector bs))))
          (let ((bs (bytestructure desc 42)))
            (= 42 (bytestructure-ref bs)))
          (let ((bs (bytestructure desc)))
            (bytestructure-set! bs 0 1)
            (= 1 (bytestructure-ref bs 0))))))

(define (descriptor-instance-with-default-ref-set)
  (let* ((desc-type
          (make-bytestructure-descriptor-type
           (lambda (size desc) (cons size desc))
           (lambda (bv offs desc) (car desc))
           (lambda (bv offs desc idx) (values bv idx (cdr desc)))
           #f
           #f))
         (desc (make-bytestructure-descriptor `(,desc-type 3 ,uint8))))
    (test "descriptor instance"
          (let ((bs (bytestructure desc #vu8(0 1 2))))
            (= 1 (bytestructure-ref bs 1)))
          (let ((bs (bytestructure desc)))
            (equal? (bytestructure-ref bs) bs)))))

(define (write-error)
  (let* ((desc-type
          (make-bytestructure-descriptor-type
           (lambda () 0) 0 #f #f #f))
         (desc (make-bytestructure-descriptor desc-type)))
    (test "write error"
          (let ((bs (bytestructure desc)))
            (not (false-if-exception (bytestructure-set! bs 'error)))))))


;;; Built-in types

(define (builtin-types)
  (numeric-types)
  (vector-type)
  (struct-type)
  (union-type)
  (pointer-type))

(define (numeric-types)
  (let-syntax
      ((test*
        (syntax-rules ()
          ((test* (type test-value) ...)
           (test "numeric types"
                 (begin (let ((bs (bytestructure type test-value)))
                          (= test-value (bytestructure-ref bs)))
                        (let ((bs (bytestructure type)))
                          (bytestructure-set! bs test-value)
                          (= test-value (bytestructure-ref bs))))
                 ...)))))
    (let* ((bv (make-bytevector 64 1))
           (test-float (bytevector-ieee-single-native-ref bv 0))
           (test-double (bytevector-ieee-double-native-ref bv 0)))
      (test* (float test-float)
             (double test-double)
             (int8 127)
             (uint8 255)
             (int16 32767)
             (uint16 65535)
             (int32 2147483647)
             (uint32 4294967295)
             (int64 9223372036854775807)
             (uint64 18446744073709551615)
             (float-le test-float)
             (double-le test-double)
             (int16le 32767)
             (uint16le 65535)
             (int32le 2147483647)
             (uint32le 4294967295)
             (int64le 9223372036854775807)
             (uint64le 18446744073709551615)
             (float-be test-float)
             (double-be test-double)
             (int16be 32767)
             (uint16be 65535)
             (int32be 2147483647)
             (uint32be 4294967295)
             (int64be 9223372036854775807)
             (uint64be 18446744073709551615)
             (short 32767)
             (unsigned-short 65535)
             (int 2147483647)
             (unsigned-int 4294967295)
             (long 9223372036854775807)
             (unsigned-long 18446744073709551615)
             ))))

(define (vector-type)
  (let ((desc (make-bytestructure-descriptor `(,bs:vector 2 ,uint16))))
    (test "vector"
          (= 4 (bytestructure-descriptor-size #f #f desc))
          (let ((bs (bytestructure desc)))
            (= 4 (bytevector-length (bytestructure-bytevector bs))))
          (let ((bs (bytestructure desc '(42 65535))))
            (and (= (bytestructure-ref bs 0) 42)
                 (= (bytestructure-ref bs 1) 65535)))
          (let ((bs (bytestructure desc #vu8(0 0 255 255))))
            (and (= (bytestructure-ref bs 0) 0)
                 (= (bytestructure-ref bs 1) 65535)))
          (let ((bs (bytestructure desc)))
            (bytestructure-set! bs 0 42)
            (bytestructure-set! bs 1 65535)
            (and (= (bytestructure-ref bs 0) 42)
                 (= (bytestructure-ref bs 1) 65535)))
          (let ((bs (bytestructure desc)))
            (not (false-if-exception
                  (bytestructure-set! bs 'error)))))))

(define (struct-type)
  (let ((desc (make-bytestructure-descriptor
               `(,bs:struct (x ,uint16) (y ,uint16)))))
    (test "struct"
          (= 4 (bytestructure-descriptor-size #f #f desc))
          (let ((bs (bytestructure desc)))
            (= 4 (bytevector-length (bytestructure-bytevector bs))))
          (let ((bs (bytestructure desc '(42 65535))))
            (and (= (bytestructure-ref bs 'x) 42)
                 (= (bytestructure-ref bs 'y) 65535)))
          (let ((bs (bytestructure desc #vu8(0 0 255 255))))
            (and (= (bytestructure-ref bs 'x) 0)
                 (= (bytestructure-ref bs 'y) 65535)))
          (let ((bs (bytestructure desc)))
            (bytestructure-set! bs 'x 42)
            (bytestructure-set! bs 'y 65535)
            (and (= (bytestructure-ref bs 'x) 42)
                 (= (bytestructure-ref bs 'y) 65535)))
          (not (false-if-exception
                (bytestructure desc '(0 1 2))))
          (let ((bs (bytestructure desc)))
            (not (false-if-exception
                  (bytestructure-ref bs 'z))))
          (let ((bs (bytestructure desc)))
            (not (false-if-exception
                  (bytestructure-set! bs 'error)))))))

(define (union-type)
  (let ((desc (make-bytestructure-descriptor
               `(,bs:union (x ,int8) (y ,uint8)))))
    (test "union"
          (= 1 (bytestructure-descriptor-size #f #f desc))
          (let ((bs (bytestructure desc)))
            (= 1 (bytevector-length (bytestructure-bytevector bs))))
          (let ((bs (bytestructure desc '(y 255))))
            (and (= (bytestructure-ref bs 'x) -1)
                 (= (bytestructure-ref bs 'y) 255)))
          (let ((bs (bytestructure desc #vu8(255))))
            (and (= (bytestructure-ref bs 'x) -1)
                 (= (bytestructure-ref bs 'y) 255)))
          (let ((bs (bytestructure desc)))
            (bytestructure-set! bs 'y 255)
            (and (= (bytestructure-ref bs 'x) -1)
                 (= (bytestructure-ref bs 'y) 255)))
          (let ((bs (bytestructure desc)))
            (not (false-if-exception
                  (bytestructure-ref bs 'z))))
          (let ((bs (bytestructure desc)))
            (not (false-if-exception
                  (bytestructure-set! bs 'error)))))))

(define sizeof (@ (system foreign) sizeof))

(define (pointer-type)
  (let ((desc (make-bytestructure-descriptor
               `(,bs:pointer (,bs:vector 2 ,uint8)))))
    (test "pointer"
          (= (sizeof '*) (bytestructure-descriptor-size #f #f desc))
          (let ((bs (bytestructure desc)))
            (= (sizeof '*) (bytevector-length (bytestructure-bytevector bs))))
          (let ((bs (bytestructure desc #vu8(0 42))))
            (and (equal? (bytestructure-ref bs) #vu8(0 42))
                 (= (bytestructure-ref bs '* 1) 42)
                 (= (bytestructure-ref bs 1) 42)))
          (let ((bs (bytestructure desc '(#vu8(0 42)))))
            (= (bytestructure-ref bs 1) 42))
          (let ((bs (bytestructure desc '((0 42)))))
            (= (bytestructure-ref bs 1) 42))
          (not (false-if-exception (bytestructure desc '(0 1))))
          (let ((bs (bytestructure desc)))
            (bytestructure-set! bs #vu8(0 42))
            (= (bytestructure-ref bs 1) 42))
          (let ((bs (bytestructure desc)))
            (bytestructure-set! bs '* #vu8(0 42))
            (= (bytestructure-ref bs 1) 42))
          (let ((bs (bytestructure desc)))
            (bytestructure-set! bs '* 1 42)
            (= (bytestructure-ref bs 1) 42))
          (let ((bs (bytestructure desc)))
            (bytestructure-set! bs 1 42)
            (= (bytestructure-ref bs 1) 42)))))

;;; test.scm ends here
