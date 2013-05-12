Structured access to bytevector contents
========================================

A "bytestructure-descriptor" describes a layout for the contents of a
bytevector, or how the bytes are to be accessed and converted into
Scheme objects.

You can think of it like C's weak typing/type-casting system.

Every bytestructure-descriptor is of a certain type, which can be
either compound, meaning that its instances are containers for other
descriptors (e.g. a vector type whose instances describe vectors of a
certain size and contained type), or not compound, meaning that its
instances describe how to convert a sequence of bytes into a Scheme
object (e.g. an "integer" type whose instances can have a specific
size, endianness, etc.).


The "simple" type
-----------------

Most of the time, the pre-provided non-compound type "simple" will
fulfil all needs for non-compound descriptors.  Its instances are
created with a size, bytevector-ref function, and bytevector-set
function.  E.g. the following is the definition of uint8:

    ;; The "simple" type is stored in the variable `bsd:simple'.
    ;; ("BSD" stands for byte-structure-descriptor.)
    (define uint8
      (make-bytestructure-descriptor
        ;; The 1 is the size, in bytes.
        (list bsd:simple 1 bytevector-u8-ref bytevector-u8-set!)))

All the usual numeric types are readily shipped with the module:
float, double, (u)int(8,16,32,64)


Compound types
--------------

The module comes with the three canonical compound types:
vector, struct, union

Given those, we can have e.g. a descriptor for uint8 vectors of size 5
(that's "unsigned char[5]" in C terminology):

    (define uint8-v5
      (make-bytestructure-descriptor
        (list bsd:vector 5 uint8)))

The position in which the uint8 appears, which can be thought of as
the second argument to the constructor-procedure of the vector
descriptor-type, is actually recursively passed to another call to
`make-bytestructure-descriptor', so we could have a nested vector like
this:

    (define uint8-v5-v3
      (make-bytestructure-descriptor
        `(,bsd:vector 3 (,bsd:vector 5 ,uint8))))

Putting uint8 directly in that position is allowed because
`make-bytestructure-descriptor' allows it:

    (define my-uint8
      (make-bytestructure-descriptor uint8)) ;; A useless copy.

Given that, here's a struct with a uint8 and a uint8-v5:

    (define my-struct
      (make-bytestructure-descriptor
        `(,bsd:struct (x ,uint8) (y ,uint8-v5))))

Or without using the intermediate uint8-v5 descriptor, because the
struct constructor has the same recursive behavior:

    (define my-struct
      (make-bytestructure-descriptor
        `(,bsd:struct (x ,uint8) (y (,bsd:vector 5 ,uint8)))))

Union definitions look exactly the same as struct definitions.


Creating and initializing bytevectors
-------------------------------------

Bytestructures can of course be used with any existing bytevector, but
the syntax `bytestructure' can be used to create a new bytevector with
the right size for a descriptor, and optionally initialized with
values.

Using the uint8-v5 type from the previous section, we could:

    (define bv (bytestructure uint8-v5))

which is equivalent to

    (define bv (make-bytevector 5))

because the size of uint8-v5 is 5.

The syntax for the initialization values depends on the descriptors.
For vectors it is very obvious:

    (define bv (bytestructure uint8-v5 (0 1 2 3 4)))

For structs, it is like in C, meaning it looks like a vector because
the field names are omitted and their order determines which value
belongs to which field.

    (let ((my-struct (make-bytestructure-descriptor
                       `(,bsd:struct (x ,uint8) (y ,uint8)))))
      (define bv (bytestructure my-struct (0 1)))) ;; x = 0, y = 1

The implementation of unions has unintentionally led to the ability of
setting a value for a certain field by providing any value (which will
subsequently be overwritten) to all preceding fields, and not
providing any more values.  For good style, the fields to be
overwritten should probably all use 0.

    (let ((my-union
           (make-bytestructure-descriptor
             `(,bsd:union (x ,uint8) (y ,uint16) (z ,uint32))))
      (define bv (bytestructure my-union (0 42))))) ;; uint16 wins

The initialization of the compound types can be done recursively,
reflecting their structure:

    (define my-struct
      (make-bytestructure-descriptor
        `(,bsd:struct (x ,uint8) (y (,bsd:vector 5 ,uint8)))))

    (define bv (bytestructure my-struct (0 (0 1 2 3 4))))

The nesting must be correct and cannot be flattened like in C.


Mutating and accessing
----------------------

Setting and getting values is fairly straightforward:

    (bytestructure-ref bytevector descriptor index ...)

For example, using the `my-struct' from above:

    (bytestructure-ref bv my-struct y 2) ;; my_struct.y[2]

(The field-name "y" is also called an "index" in our terminology.)

Setting values is analogous:

    (bytestructure-set! bytevector descriptor index ... value)

E.g.:

    (bytestructure-set! bv my-struct y 2 42) ;; my_struct.y[2] = 42


Creating new types
------------------

TODO
