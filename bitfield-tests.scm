;;; Warning: nasal demons.
;;;
;;; Will output differences between GCC's behavior and our behavior, but not in
;;; a very nice format.  Zero output is good.  The C code and Scheme procedure
;;; we generate are fairly straightforward so read the code to understand.

(define-module (bytestructures bitfield-tests))

(export run-bitfield-tests)

(use-modules (srfi srfi-1)
             (srfi srfi-9)
             (ice-9 rdelim)
             (bytestructures r6 bytevectors)
             (bytestructures guile))

(define-record-type <struct>
  (make-struct name fields)
  struct?
  (name   struct-name)
  (fields struct-fields))

(define-record-type <field>
  (make-field name int-size bit-size signed? value)
  struct?
  (name     field-name)
  (int-size field-int-size)
  (bit-size field-bit-size)
  (signed?  field-signed?)
  (value    field-value))

(define *keep-files* (make-parameter #f))

(define (run-bitfield-tests count random-seed-string keep-files)
  (set! *random-state* (seed->random-state random-seed-string))
  (parameterize ((*keep-files* keep-files))
    (test-structs (generate-structs count))))

(define (generate-structs n)
  (remove-bad-structs (map random-struct (iota n))))

(define (remove-bad-structs structs)
  (filter (lambda (struct)
            (find (lambda (field)
                    (not (zero? (field-bit-size field))))
                  (struct-fields struct)))
          structs))

(define (random-struct i)
  (let ((field-count (+ 1 (random 50))))
    (make-struct (format #f "s~a" i)
                 (map random-field (iota field-count)))))

(define (random-field i)
  (let* ((name (format #f "f~a" i))
         (int-size (* 8 (expt 2 (random 4))))
         (bit-size (random (+ 1 int-size)))
         (signed? (= 0 (random 2)))
         (value (random (expt 2 bit-size)))
         (value (if (and signed? (> value (+ -1 (expt 2 (- bit-size 1)))))
                    (- value (expt 2 bit-size))
                    value)))
    (make-field name int-size bit-size signed? value)))

(define (test-structs structs)
  (let* ((c-code (c-code-for-structs structs))
         (c-output (get-c-output c-code))
         (scm-code (scm-code-for-structs structs))
         (scm-output (get-scm-output scm-code)))
    (diff-outputs c-output scm-output)))

(define (c-code-for-structs structs)
  (string-concatenate
   (append
    (list "#include <stdio.h>\n"
          "#include <stdint.h>\n"
          "#include <strings.h>\n"
          "int main(){\n")
    (map c-code-for-struct structs)
    (list "return 0;}"))))

(define (c-code-for-struct struct)
  (let ((name (struct-name struct))
        (fields (struct-fields struct)))
    (string-concatenate
     (append
      (list (format #f "struct ~a {\n" name))
      (map c-decl-for-field fields)
      (list "};\n"
            (format #f "{ struct ~a foo;\n" name)
            (format #f "  bzero((void*)&foo, sizeof(foo));\n"))
      (map c-assignment-for-field fields)
      (list (format #f "  printf(\"struct ~a:\\n\");\n" name)
            "  uint8_t *ptr = (void*)&foo;\n"
            "  for (int i = 0; i < sizeof(foo); ++i) {\n"
            "    printf(\"%d \", *(ptr+i));\n"
            "  }\n"
            "  printf(\"\\n\");\n"
            "}\n")))))

(define (c-decl-for-field field)
  (let ((name (field-name field))
        (int-size (field-int-size field))
        (bit-size (field-bit-size field))
        (signed? (field-signed? field)))
    (format #f "  ~aint~a_t ~a:~a;\n"
            (if signed? "" "u")
            int-size
            (if (zero? bit-size) "" name)
            bit-size)))

(define (c-assignment-for-field field)
  (let ((name (field-name field))
        (bit-size (field-bit-size field))
        (signed? (field-signed? field))
        (value (field-value field)))
    (if (zero? bit-size)
        ""
        (format #f "  foo.~a = ~a~a;\n" name value (if signed? "" "u")))))

(define (get-c-output code)
  (let* ((port (mkstemp! (string-copy "/tmp/bitfield-XXXXXX")))
         (file (port-filename port))
         (exe-port (mkstemp! (string-copy "/tmp/bitfield-compiled-XXXXXX")))
         (exe-file (port-filename exe-port))
         (output-port (mkstemp! (string-copy "/tmp/bitfield-output-XXXXXX")))
         (output-file (port-filename output-port)))
    (close-port exe-port)
    (close-port output-port)
    (display code port)
    (force-output port)
    (unless (zero? (system* "gcc" "-x" "c" "-std=c11" file "-o" exe-file))
      (error "gcc failed"))
    (unless (zero? (system (format #f "~a > ~a" exe-file output-file)))
      (error "exe failed"))
    (let ((out (read-string (open output-file O_RDONLY))))
      (unless (*keep-files*)
        (for-each delete-file (list file exe-file output-file)))
      out)))

(define (scm-code-for-structs structs)
  (lambda ()
    (string-concatenate
     (map scm-code-for-struct structs))))

(define (scm-code-for-struct struct)
  (let* ((name (struct-name struct))
         (fields (struct-fields struct))
         (descriptor (struct->descriptor struct))
         (values (map field-value (filter-nonzero-fields fields)))
         (bs (bytestructure descriptor (list->vector values))))
    (string-concatenate
     (append
      (list (format #f "struct ~a:\n" name))
      (let ((bv (bytestructure-bytevector bs)))
        (map (lambda (i)
               (format #f "~a " (bytevector-u8-ref bv i)))
             (iota (bytevector-length bv))))
      (list "\n")))))

(define (struct->descriptor struct)
  (let ((fields (struct-fields struct)))
    (bs:struct (map field->struct-descriptor-field fields))))

(define (field->struct-descriptor-field field)
  (let ((name (field-name field))
        (int-size (field-int-size field))
        (bit-size (field-bit-size field))
        (signed? (field-signed? field)))
    (list name
          (module-ref (resolve-module
                       '(bytestructures bitfield-tests))
                      (string->symbol
                       (format #f "~aint~a"
                               (if signed? "" "u")
                               int-size)))
          bit-size)))

(define (filter-nonzero-fields fields)
  (filter (lambda (field)
            (not (zero? (field-bit-size field))))
          fields))

(define (get-scm-output code)
  (code))

(define (diff-outputs o1 o2)
  (let* ((p1 (mkstemp! (string-copy "/tmp/bitfield-out1-XXXXXX")))
         (f1 (port-filename p1))
         (p2 (mkstemp! (string-copy "/tmp/bitfield-out2-XXXXXX")))
         (f2 (port-filename p2)))
    (display o1 p1)
    (display o2 p2)
    (flush-all-ports)
    (close-port p1)
    (close-port p2)
    (let ((retval (system* "diff" "-y" "--suppress-common" f1 f2)))
      (unless (*keep-files*)
        (for-each delete-file (list f1 f2)))
      retval)))
