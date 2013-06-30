Structured access to bytevector contents
========================================

A "bytestructure descriptor" describes a layout for the contents of a
bytevector, or how the bytes are to be accessed and converted into
Scheme objects.

You can think of it like C's weak typing/type-casting system.

Every bytestructure descriptor is of a certain type, which can be
either compound, meaning that its instances are containers for other
descriptors (e.g. a vector type whose instances describe vectors of a
certain length and contained type), or not compound, meaning that its
instances describe how to convert a sequence of bytes into a Scheme
object (e.g. an "integer" type whose instances can have a specific
size, endianness, etc.).


Creating bytestructure descriptors
----------------------------------

The function `make-bytestructure-descriptor` takes one argument, the
"bytestructure description," which may be one of the following:

1. A "bytestructure descriptor type" object; this will call the
constructor for that type with no arguments.

2. A list whose first element is a bytestructure descriptor type; this
will apply the constructor for that type to the rest of the list.

3. A bytestructure descriptor; this will return the same bytestructure
descriptor as is.  This is so that in places where a bytestructure
description is expected, an existing bytestructure descriptor can be
passed instead.

A simple strategy that can be used by constructors of compound
bytestructure descriptor types is to recursively call
`make-bytestructure-descriptor` on some of their arguments.  Rule 3
above helps in this situation.  E.g. consider the following example:

    (define uint8-v3
      (make-bytestructure-descriptor (list bsd:vector 3 uint8)))

The `bsd:vector` variable holds the compound bytestructure descriptor
type "vector."  The constructor for this type takes a length and the
description for the type of which we want a vector.  We provided the
existing `uint8` bytestructure descriptor, so we get a descriptor for
uint8 vectors of length 3.  In C terminology, a `uint8_t[3]`.  The
following creates a `uint8_t[3][5]` descriptor:

    (define uint8-v3-v5
      (make-bytestructure-descriptor
        `(,bsd:vector 5 (,bsd:vector 3 ,uint8))))

As does the following, using our previous `uint8-v3`:

    (define uint8-v3-v5
      (make-bytestructure-descriptor
        `(,bsd:vector 5 ,uint8-v3)))


The "simple" type
-----------------

Most of the time, the pre-provided non-compound type "simple" will
fulfil all needs for non-compound descriptors.  Its instances are
created with a size, bytevector-ref function, and bytevector-set
function.  E.g. the following is the definition of `uint8`:

    (define uint8
      (make-bytestructure-descriptor
        ;; The 1 is the size, in bytes.
        (list bsd:simple 1 bytevector-u8-ref bytevector-u8-set!)))

All the usual numeric types are readily shipped with the module:
float, double, (u)int(8,16,32,64)


Compound types
--------------

The module comes with the three canonical compound types: vector,
struct, union

We've already covered the vector type in the first section.

Given the `uint8-v3` from the vector examples, here's a struct with a
`uint8` and a `uint8-v3`:

    (define my-struct
      (make-bytestructure-descriptor
        `(,bsd:struct (x ,uint8) (y ,uint8-v3))))

Or without using the intermediate `uint8-v3` descriptor:

    (define my-struct
      (make-bytestructure-descriptor
        `(,bsd:struct (x ,uint8) (y (,bsd:vector 3 ,uint8)))))

Union definitions look exactly the same as struct definitions.


Creating and initializing bytevectors
-------------------------------------

Bytestructures can of course be used with any existing bytevector, but
the syntax `bytestructure` can be used to create a new bytevector with
the right size for a descriptor, and optionally initialized with
values.

Using the `uint8-v3` descriptor from the previous section, we could:

    (define bv (bytestructure uint8-v3))

which is equivalent to

    (define bv (make-bytevector 3))

because the size of a `uint8-v3` is 3.

The syntax for the initialization values depends on the descriptors.
For vectors it is very obvious:

    (define bv (bytestructure uint8-v3 (0 1 2 3 4)))

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
        `(,bsd:struct (x ,uint8) (y (,bsd:vector 3 ,uint8)))))

    (define bv (bytestructure my-struct (0 (0 1 2 3 4))))

The nesting must be correct and cannot be flattened like in C.


Mutating and accessing
----------------------

Setting and getting values is fairly straightforward:

    (bytestructure-ref bytevector descriptor index ...)

For example, using the `my-struct` from above:

    (bytestructure-ref bv my-struct 'y 2) ;; my_struct.y[2]

(The field-name `y` is also called an "index" in our terminology.)

Setting values is analogous:

    (bytestructure-set! bytevector descriptor index ... value)

E.g.:

    (bytestructure-set! bv my-struct 'y 2 42) ;; my_struct.y[2] = 42

A "ref-helper" exists which, although part of the internals, could be
useful to user code and so is exported from the module as well.  It's
like the referencing syntax, but takes no bytevector, and returns two
values; an offset, and a descriptor for continuing the referencing
from whatever point the provided indices ended:

    (bytestructure-ref-helper uint8-v3-v5 2)
    ===> 6, uint8-v3 ;; Two uint8-v3s were skipped, so offset 6.

    (bytestructure-ref-helper uint8-v3-v5 2 1)
    ===> 7, uint8 ;; Two uint8-v3s and one uint8 was skipped.


Creating new types
------------------

For non-compound descriptors, the pre-provided type "simple" can
fulfill all use-cases; since a size, and a pair of "bytes to scheme
object" and "scheme object to bytes" conversion functions is
absolutely everything that's necessary to describe any kind of binary
object.  However, sometimes it might be convenient, make sense, or be
otherwise useful to have a separate type of non-compound descriptor,
like for example an "integer" type whose instances can be created with
a size, signedness, endianness, etc.. (Note however that the default
implementation for the numeric types just uses the "simple" type.)

    (make-bytestructure-descriptor-type
     constructor predicate size-or-size-accessor
     bytevector-ref-fn bytevector-set-fn)

This will return a descriptor-type object which can be used with
`make-bytestructure-descriptor` as explained in the first section.
The constructor is the one already mentioned, the predicate must
identify instances of the descriptor type, the size-or-size-accessor
must be either a non-negative exact integer, or a unary function that
will return the size of a specific instance of the type (think of our
"integer" type example).

The `bytevector-ref-fn` must be a ternary function that takes a
bytevector, an offset, and a descriptor of this type; and returns a
value, according to that descriptor and residing at that offset in the
bytevector.  The `bytevector-set-fn` is similar but takes an
additional value argument, whose binary representation it should fill
into the bytevector at that offset.

The ref and set functions of the "simple" type, for example, pass on
said arguments in the same order, excluding the descriptor, to the ref
and set functions of the descriptor instance.

New compound types are created as following:

    (make-bytestructure-descriptor-compound-type
     constructor predicate size-accessor
     bytevector-constructor-helper bytevector-ref-helper)

The size cannot be a constant this time, since it cannot possibly be
known in advance; it depends on whatever descriptor(s) the compound
one will hold, and should calculate this correctly.  It can use
`bytestructure-descriptor-size` for this; for example the
size-accessor of the vector type simply multiplies the length of the
vector with the size of the contained descriptor.

The `bytevector-ref-helper` must be a binary function that takes a
descriptor of this type, and an "index" object; it must return two
values: the byte-offset for this index, and the descriptor contained
at its position.  For example the ref helper of the vector type
multiplies the size of its contained-type with the index (which it
expects to be a non-negative exact integer), and always returns its
one contained type.  The ref helper of the struct type on the other
hand must iterate through its fields, accumulating the sizes of the
descriptors it skips until it comes to the field with the requested
index (a symbol), and returns the accumulated offset and the
descriptor of the field with the requested index.

The `bytevector-constructor-helper` is similar to the ref helper, but
the index it takes is always positional (non-negative exact integer).
The constructor helper for the struct type, for example, simply skips
over that many fields, instead of seeking for the correct field.  This
is why initializing structs works the way it does (see relevant
section).
