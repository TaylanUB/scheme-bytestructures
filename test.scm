(define-module (bytestructures test))

(use-modules (bytestructures procedural)
             (ice-9 format)
             (rnrs bytevectors))

(define-syntax test
  (syntax-rules ()
    ((test name expression ...)
     (begin
       (unless expression
         (error (format #f "Test ~s failed: " name) 'expression))
       ...))))

(define (main)

  (let-syntax
      ((df (syntax-rules ()
             ((df fn ...)
              (begin
                (define fn (@@ (bytestructures procedural) fn))
                ...)))))
    (df bytestructure-descriptor-type-compound?
        bytestructure-descriptor-constructor
        bytestructure-descriptor-type-size
        bytevector-constructor-helper
        bytevector-ref-helper
        bytevector-ref-fn
        bytevector-set-fn
        bytestructure-descriptor-type
        bytestructure-descriptor-content))

;;; Sanity-check for internals

  (let ((constructor (lambda () #f))
        (size 0)
        (size-accessor (lambda () #f))
        (bv-constructor-helper (lambda () #f))
        (bv-ref-helper (lambda () #f))
        (bv-ref-fn (lambda () #f))
        (bv-set-fn (lambda () #f)))
    (let ((desc-type
           (make-bytestructure-descriptor-type
            constructor size bv-ref-fn bv-set-fn)))
      (test "non-compound descriptor type"
            (bytestructure-descriptor-type? desc-type)
            (not (bytestructure-descriptor-type-compound? desc-type))
            (eq? constructor (bytestructure-descriptor-constructor desc-type))
            (eqv? size (bytestructure-descriptor-type-size desc-type))
            (eq? bv-ref-fn (bytevector-ref-fn desc-type))
            (eq? bv-set-fn (bytevector-set-fn desc-type))))
    (let ((desc-type
           (make-bytestructure-descriptor-compound-type
            constructor size-accessor
            bv-constructor-helper bv-ref-helper)))
      (test "compound descriptor type"
            (bytestructure-descriptor-type? desc-type)
            (bytestructure-descriptor-type-compound? desc-type)
            (eq? constructor (bytestructure-descriptor-constructor desc-type))
            (eq? size-accessor (bytestructure-descriptor-type-size desc-type))
            (eq? bv-constructor-helper
                 (bytevector-constructor-helper desc-type))
            (eq? bv-ref-helper (bytevector-ref-helper desc-type)))))

  (let ((desc-type
         (make-bytestructure-descriptor-type
          (lambda () 0) 1
          (lambda (bv offset desc) 0)
          (lambda (bv offset desc val) *unspecified*))))
    (test "an argumentless bytestructure descriptor constructor"
          (bytestructure-descriptor?
           (make-bytestructure-descriptor desc-type))))

  (test "a wrong argument to `make-bytestructure-descriptor'"
        (not (false-if-exception
              (make-bytestructure-descriptor 'error))))

  (let* ((desc-type
          (make-bytestructure-descriptor-type
           (lambda (x) (cons 'test x)) 3
           (lambda (bv offset desc)
             (+ (cdr desc) (bytevector-u8-ref bv offset)))
           (lambda (bv offset desc val)
             (bytevector-u8-set! bv offset (+ (cdr desc) val)))))
         (desc (make-bytestructure-descriptor (list desc-type 5))))
    (test "an instance of a braindead bytestructure descriptor type"
          (bytestructure-descriptor? desc)
          (eq? desc-type (bytestructure-descriptor-type desc))
          (= 5 (cdr (bytestructure-descriptor-content desc)))
          (= 3 (bytestructure-descriptor-size desc))
          (let ((bs (bytestructure desc)))
            (bytestructure? bs))
          (let ((bs (bytestructure desc)))
            (= 3 (bytevector-length (bytestructure-bytevector bs))))
          (let ((bs (bytestructure desc 0)))
            (= 10 (bytestructure-ref bs)))
          (let ((bs (bytestructure desc)))
            (bytestructure-set! bs 1)
            (= 11 (bytestructure-ref bs)))))

  (let* ((desc-type
          (make-bytestructure-descriptor-compound-type
           (lambda (x) (cons 'test x)) (lambda (desc) (cadr desc))
           (lambda (desc idx) (values idx (cddr desc)))
           (lambda (desc idx) (values idx (cddr desc)))))
         (desc (make-bytestructure-descriptor (list desc-type (cons 3 uint8)))))
    (test "an instance of a braindead bytestructure descriptor compound type"
          (bytestructure-descriptor? desc)
          (eq? desc-type (bytestructure-descriptor-type desc))
          (eq? uint8 (cddr (bytestructure-descriptor-content desc)))
          (= 3 (bytestructure-descriptor-size desc))
          (let ((bs (bytestructure desc)))
            (bytestructure? bs))
          (let ((bs (bytestructure desc)))
            (= 3 (bytevector-length (bytestructure-bytevector bs))))
          (let ((bs (bytestructure desc (0 1 2))))
            (= 1 (bytestructure-ref bs 1)))
          (let ((bs (bytestructure desc)))
            (bytestructure-set! bs 0 1)
            (= 1 (bytestructure-ref bs 0)))))

;;; External API

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
             (uint64 18446744073709551615))))

  (let ((desc (make-bytestructure-descriptor `(,bsd:vector 2 ,uint16))))
    (test "vector"
          (= 4 (bytestructure-descriptor-size desc))
          (let ((bs (bytestructure desc)))
            (= 4 (bytevector-length (bytestructure-bytevector bs))))
          (let ((bs (bytestructure desc (42 65535))))
            (and (= (bytestructure-ref bs 0) 42)
                 (= (bytestructure-ref bs 1) 65535)))
          (let ((bs (bytestructure desc)))
            (bytestructure-set! bs 0 42)
            (bytestructure-set! bs 1 65535)
            (and (= (bytestructure-ref bs 0) 42)
                 (= (bytestructure-ref bs 1) 65535)))))

  (let ((desc (make-bytestructure-descriptor
               `(,bsd:struct (x ,uint16) (y ,uint16)))))
    (test "struct"
          (= 4 (bytestructure-descriptor-size desc))
          (let ((bs (bytestructure desc)))
            (= 4 (bytevector-length (bytestructure-bytevector bs))))
          (let ((bs (bytestructure desc (42 65535))))
            (and (= (bytestructure-ref bs 'x) 42)
                 (= (bytestructure-ref bs 'y) 65535)))
          (let ((bs (bytestructure desc)))
            (bytestructure-set! bs 'x 42)
            (bytestructure-set! bs 'y 65535)
            (and (= (bytestructure-ref bs 'x) 42)
                 (= (bytestructure-ref bs 'y) 65535)))
          (not (false-if-exception
                (bytestructure desc (0 1 2))))
          (let ((bs (bytestructure desc)))
            (not (false-if-exception
                  (bytestructure-ref bs 'z))))))

  (let ((desc (make-bytestructure-descriptor
               `(,bsd:union (x ,int8) (y ,uint8)))))
    (test "union"
          (= 1 (bytestructure-descriptor-size desc))
          (let ((bs (bytestructure desc)))
            (= 1 (bytevector-length (bytestructure-bytevector bs))))
          (let ((bs (bytestructure desc (0 255))))
            (and (= (bytestructure-ref bs 'x) -1)
                 (= (bytestructure-ref bs 'y) 255)))
          (let ((bs (bytestructure desc)))
            (bytestructure-set! bs 'y 255)
            (and (= (bytestructure-ref bs 'x) -1)
                 (= (bytestructure-ref bs 'y) 255)))
          (let ((bs (bytestructure desc)))
            (not (false-if-exception
                  (bytestructure-ref bs 'z))))))

  )
