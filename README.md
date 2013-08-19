Structured access to bytevector contents
========================================

*This module is written with Guile Scheme in mind, specifically with
regard to the FFI integration; the basic idea behind it however is
aimed at any `(rnrs bytevectors)` implementation, and the code-base
keeps implementation-specific features to a minimum except for the
FFI-heavy parts.*

A "bytestructure descriptor" describes a logic for the contents of a
bytevector, or how its bytes are to be accessed and converted into
Scheme objects.  It is analogous to a "type" in the weak typing and
type-casting system of the C programming language, since those types
also offer "views" on raw bytes.

Every descriptor is of a certain type.  Going on with the C analogy,
if a descriptor is a type, then a descriptor type would be a
type-type, or meta-type; the C programming language does not give
control over these, although the array, struct, union, function, and
pointer "types" it supports are all not stand-alone types, but indeed
such meta-types that construct derived types.  (E.g. a variable never
has type "array," it has a type "array of \<type\>" for an underlying
type \<type\>, called its element type.)

In our system, both descriptors and descriptor types are first-class
objects; a descriptor type can define how descriptors of that type are
created, how their size (or rather, the size of the structure they
describe) is calculated, what it means to use indices with them, what
it means to reference a value with them, and what it means to assign a
value with them.

For example, descriptors of the vector type are created with a length,
and an element descriptor; their size is calculated by multiplying the
length with the size of the element descriptor; and one can use
non-negative exact integer indices on them, which results in an offset
(calculated by multiplying the index with the size of the element
descriptor), and the element descriptor, such that referencing or
assigning a value can continue from there; referencing or assigning a
value directly as a vector does not make much sense, although the
vector type also defines a certain assignment semantics as a
convenience manner, which can be used to mutate multiple elements of
the vector at once, or replace its whole contents with another.

As another example, instances of a hypothetical "number" type could be
created with a specific size, endianness, etc., and their referencing
and assignment semantics would be to encode and decode numeric values
to and from byte-sequences as specified by said properties.  In this
case, in contrast to the vector type, indexing semantics would be left
out, since instances of this descriptor type would work with singular
objects.


Creating bytestructure descriptors
----------------------------------

The procedure `make-bytestructure-descriptor` takes one argument, the
"bytestructure description," which may be one of the following:

1. A descriptor type; this will call the constructor for that type
with no arguments.

2. A list whose first element is a descriptor type; this will apply
the constructor for that type to the rest of the list.

3. An existing descriptor; this will return the same descriptor as is.
This is so that in places where a bytestructure description is
expected, an existing bytestructure descriptor can be passed instead.

A simple strategy that can be used by constructors of "compound"
descriptor types (those who contain other descriptors, e.g. vector) is
to recursively call `make-bytestructure-descriptor` on some of their
arguments.  Rule 3 above helps in this situation; consider the
following example:

    (define uint8-v3
      (make-bytestructure-descriptor (list bs:vector 3 uint8)))

The `bs:vector` variable holds the descriptor type "vector."  The
constructor for this type takes a length and an element description.
We provided the existing `uint8` descriptor, so we get a descriptor
for uint8 vectors of length 3.  In C terminology, a `uint8_t[3]`.  The
following creates a `uint8_t[3][5]` descriptor:

    (define uint8-v3-v5
      (make-bytestructure-descriptor
        `(,bs:vector 5 (,bs:vector 3 ,uint8))))

As does the following, using our previous `uint8-v3`:

    (define uint8-v3-v5
      (make-bytestructure-descriptor
        `(,bs:vector 5 ,uint8-v3)))

The size of a descriptor is returned via the procedure
`bytestructure-descriptor-size`.  Some descriptors might have dynamic
sizes though, in which case this procedure cannot be used on solely
the descriptor; it must be given a bytevector, an offset indicating
where the described structure starts in the bytevector, and then the
descriptor.

    (bytestructure-descriptor-size uint8-v3-v5)
    => 15, because it's 3×5 8-bit integers in total.

    (bytestructure-descriptor-size crazy-descriptor)
    => error

    (bytestructure-descriptor-size bv offset crazy-descriptor)
    => whatever


The "simple" type
-----------------

The module comes with the type "simple," which can fulfill all needs
of "non-compound" descriptors -- those whose sole purpose is to
convert between Scheme objects and byte-sequences.  Its instances are
created with a constant size, a bytevector-ref procedure, and a
bytevector-set procedure.  E.g. the following is the definition of
`uint8`:

    (define uint8
      (make-bytestructure-descriptor
        ;; The 1 is the size, in bytes.
        (list bs:simple 1 bytevector-u8-ref bytevector-u8-set!)))

All the usual numeric types are readily provided by the module:
float\[-le,-be\], double\[-le,-be\], \[u\]int(8,16,32,64)\[le,be\]

Also native-sized ones, derived from Guile's FFI module:
\[unsigned-\](short,int,long), size_t, ssize_t, ptrdiff_t


Compound types
--------------

The module comes with the four usual compound types: vector, struct,
union, and pointer.

We've already covered the vector type.  One more thing to note about
them is that they don't do bounds-checking; an off-bounds index will
either raise an error due to an off-bounds bytevector index, or
attempt to decode whatever bytes are found at the relevant place in
the bytevector, which might just result in a valid value without
raising an error.

Given the `uint8-v3` from the vector examples, here's a struct with a
`uint8` and a `uint8-v3`:

    (define my-struct
      (make-bytestructure-descriptor
        `(,bs:struct (x ,uint8) (y ,uint8-v3))))

Or without using the intermediate `uint8-v3` descriptor:

    (define my-struct
      (make-bytestructure-descriptor
        `(,bs:struct (x ,uint16) (y (,bs:vector 3 ,uint8)))))

Union definitions look exactly the same as struct definitions.

The pointer type takes only the description for the pointed-to
substructure:

    (define uint8-v3-ptr
      (make-bytestructure-descriptor
        `(,bs;pointer (,bs:vector 3 ,uint8))))

The pointer type only uses the underlying bytevector to store a
pointer to another bytevector, which holds said pointed-to
substructure.  Note that this makes your Scheme program memory-unsafe!
If no references are left to a bytevector whose address has been saved
in a bytestructure, then references into the bytestructure which go
through the pointer might try to access invalid memory addresses.

*The pointer makes heavy use of Guile's FFI functionality.*

As an additional feature, the pointer type accepts a promise for the
description of the pointed-to substructure.  The promise must evaluate
to a normal bytestructure description when forced, as accepted by
`make-bytestructure-descriptor'.  This helps when creating cyclic
structures:

    (define linked-uint8-list
      (make-bytestructure-descriptor
        `(,bs:pointer ,(delay `(,bs:struct
                                (head ,uint8)
                                (tail ,linked-uint8-list))))))


The bytestructure data-type
---------------------------

Any bytestructure descriptor can be used with any bytevector to work
on the vector momentarily with accordance to the structure described
by the given descriptor, but in the usual case a bytevector will be
dedicated to a certain structure, so it is most convenient to be able
to bundle a descriptor onto a bytevector and not be required to
provide it explicitly at each access into the bytevector.  Similarly,
a section of a bytevector starting from a certain offset might be
dedicated to the structure, so being able to bundle this offset too is
also useful.

    (define bs (make-bytestructure bytevector offset descriptor))
    (bytestructure? bs)           => #t
    (bytestructure-bytevector bs) => bytevector
    (bytestructure-offset bs)     => offset
    (bytestructure-descriptor bs) => descriptor


Creating and initializing bytestructures conveniently
-----------------------------------------------------

The `bytestructure` procedure can be used to create a bytestructure
with a new bytevector of the right size for its descriptor, and
optionally initialize it with values.

E.g.

    (define bs (bytestructure `(,bs:vector 3 ,int16)))

is equivalent to

    (define bs (make-bytestructure
                 (make-bytevector 6)
                 0
                 (make-bytestructure-descriptor
                   `(,bs:vector 3 ,int16))))

Providing the optional second argument to `bytestructure` will invoke
the assignment procedure of the descriptor type.  While this procedure
is mainly intended for descriptors of atomic types to implement
conversion from Scheme objects to byte-sequences, compound types can
use it to provide convenient content initialization.

Vectors accept a list of elements to be written:

    (define bs (bytestructure uint8-v3 '(0 1 2)))

Structs, like in C, also accept a sequential list of elements:

    (define a-simple-struct (make-bytestructure-descriptor
                              `(,bs:struct (x ,uint8) (y ,uint8))))

    (define bs (bytestructure a-simple-struct '(0 1))) ;; x = 0, y = 1

Unions take a two-element list, whose first element is a field-name,
and the second element the value:

    (define a-simple-union
      (make-bytestructure-descriptor
        `(,bs:union (x ,uint8) (y ,uint16) (z ,uint32))))

    (define bs (bytestructure a-simple-union '(y 42)))

Vectors, structs, and unions also accept a bytevector, from which they
will copy as many bytes as their size:

    (define bs (bytestructure a-simple-struct #vu8(0 1)))

Pointers take either a pointer object (from the FFI module), or a
bytevector (whose pointer will be used):

    (define bs (bytestructure `(,bs:pointer ,uint8) ffi-pointer))

    (define bs (bytestructure `(,bs:pointer ,uint8) bv))

_**Having an address written into a bytevector does not protect it
from garbage collection.**_

The initialization of the compound types can be done recursively,
reflecting their structure, since the assignment procedures are
implemented such that they again use the underlying descriptor's
assignment procedure.

    (define my-struct
      (make-bytestructure-descriptor
        `(,bs:struct (x ,uint16) (y (,bs:vector 3 ,uint8)))))

    (define bs (bytestructure my-struct '(0 (0 1 2))))

    (define bs (bytestructure `(,bs:pointer ,my-struct)
                              '((0 (0 1 2)))))


Referencing and assignment
--------------------------

Referencing and assigning values is fairly straightforward:

    (bytestructure-ref bytestructure index ...)
    (bytestructure-set! bytestructure index ... value)

For example, given

    (define my-struct
      (make-bytestructure-descriptor
        `(,bs:struct (x ,uint16) (y (,bs:vector 3 ,uint8)))))

    (define bs (bytestructure my-struct))

we can

    (bytestructure-ref bs 'y 2)     ;; bs->y[2]
    (bytestructure-set! bs 'y 2 42) ;; bs->y[2] = 42;

(The field-name `y` is also called an "index" in our terminology.)

The pointer type accepts the index `*` to induce dereferencing; if any
other index is given, it implicitly dereferences and re-uses the index
for its contained descriptor.

Both forms are syntax, not procedures, because an arbitrary-length
argument list would require heap allocation, which is undesirable for
the ubiquitous accessing and mutating operations.

Note that `bytestructure-set!` invokes the same assignment procedure
mentioned in the section "Creating and initializing bytestructures
conveniently" on its last argument, which matches the second argument
to the `bytestructure` syntax, so the same types of arguments are
accepted.

    (bytestructure-set! bs '(0 (1 2 3))) ;; Re-fill the whole struct.

The pointer type accepts an additional type of argument not mentioned
in the section about initialization: a one-element list whose contents
are passed to the underlying bytestructure's assignment procedure.
This usage is only valid if the pointer is already pointing to a valid
bytevector, so using it at initialization does not make sense.

    (bytestructure-set! uint8-v3-ptr '((0 1 2)))

When a descriptor does not provide a referencing or assignment
procedure, a default action is taken: when referencing, a
bytestructure is returned which encapsulates the bytevector, offset,
and descriptor at which we arrived, such that referencing or assigning
could continue from that point by using that bytestructure; when
assigning, the value is expected to be a bytevector, and as many bytes
from it are copied into the target bytestructure's bytevector
(starting at the offset at which we had arrived) as the size of the
descriptor at which we arrived.  This is the same behavior which
vectors, structs, and unions provide for a bytevector argument.

In other words, given our previous `bs` with descriptor `my-struct`:

    (bytestructure-ref bs 'y)
    => A bytestructure with
         bytevector: Same as bs.
         offset:     The offset of bs, plus 2.
         descriptor: uint8-v3
         ;; Offset is plus 2 because a uint16 was skipped.

    (bytestructure-set! bs 'y #vu8(...))
    ;; Equivalent to:
         (bytevector-copy! #vu8(...) 0
                           (bytestructure-bytevector bs)
                           (+ 2 (bytestructure-offset bs))
                           (bytestructure-descriptor-size
                             (bytestructure-bytevector bs)
                             (+ 2 (bytestructure-offset bs))
                             uint8-v3))

The syntaxes `bytestructure-ref*` and `bytestructure-set!*` are
equivalent to `bytestructure-ref` and `bytestructure-set!`, except
that in the position of the bytestructure argument, they instead
directly take the values it encapsulates as separate arguments: a
bytevector, an initial offset into the bytevector, and a bytestructure
descriptor.

    (bytestructure-ref* bytevector offset descriptor index ...)
    (bytestructure-set!* bytevector offset descriptor index ... value)

The `bytestructure-ref-helper` and `bytestructure-ref-helper*`
syntaxes are equivalent to `bytestructure-ref` and
`bytestructure-ref*`, except that they always return three values; a
bytevector, an offset, and a bytestructure descriptor; instead of a
bytestructure encapsulating these or the ultimate value of the
referencing.

    (bytestructure-ref-helper bs 'y 1)
    => (bytestructure-bytevector bs), 3, uint8-v3
    ;; Offset is 3 because one uint16 and one uint8 was skipped.

Note that by providing no indices, this can be used to destructure a
bytestructure object into its components:

    (let-values (((bytevector offset descriptor)
                  (bytestructure-ref-helper bytestructure)))
      ...)

In `bytestructure-ref-helper*`, if you know that the bytevector is
irrelevant to the calculation (i.e. all involved descriptors have
static sizes), you can provide a bogus value and get it back as-is;
good style should probably use `#f`:

    (bytestructure-ref-helper* #f 0 uint8-v3-v5 2)
    => #f, 6, uint8-v3 ;; Two uint8-v3s were skipped, so offset 6.

    (bytestructure-ref-helper* #f 0 uint8-v3-v5 2 1)
    => #f, 7, uint8 ;; Two uint8-v3s and one uint8 was skipped.


Creating new types
------------------

    (make-bytestructure-descriptor-type
      constructor size-or-size-accessor
      ref-helper bytevector-ref-proc bytevector-set-proc)

This will return a descriptor type object which can be used with
`make-bytestructure-descriptor` as explained in the section "Creating
bytestructure descriptors."  Explanation of the arguments follows.

The `constructor` is the one mentioned in the section "Creating
bytestructure descriptors"; it should return an object holding the
contents, or payload, for this descriptor instance.

The `size-or-size-accessor` must either be a non-negative exact
integer, or a ternary procedure taking a bytevector, an offset, and
the descriptor contents; returning the size of the instance with this
content.  The bytevector and offset are given to satisfy possible
use-cases where the size of a structure is dynamic, depending on parts
of its bytes; the procedure should expect to receive `#f` for these
arguments if the descriptor will be used in situations where the size
is requested independently of a bytevector.  Specifically, the vector,
struct, and union types calculate their sizes at creation time to save
work, thus they cannot contain dynamic-sized descriptors; the pointer
type, too, cannot hold dynamic-sized descriptors, because getting a
bytevector from a pointer requires the size to be declared beforehand.

The size-accessor procedure of a compound type can use
`bytestructure-descriptor-size` to acquire the size of other
descriptors it holds.  This procedure takes either only a
bytestructure descriptor (for when to calculating the size statically,
typically in the constructor), or a bytevector, offset, and descriptor
(for when requesting the size of a descriptor with a possibly dynamic
size).

The `ref-helper` must be a four-argument procedure that takes a
bytevector, an offset, the contents of the descriptor, and an "index"
object; it returns three values: the same or a different bytevector,
and a new offset and descriptor as determined by the index.  For
example the ref-helper of the vector type multiplies the size of its
contained descriptor with the index and adds the resulting value to
the old offset, and always returns its one contained descriptor.  The
ref-helper of the struct type, on the other hand, must iterate through
its fields, adding to the offset the sizes of the descriptors it skips
until it comes to the field with the requested index (a symbol), and
returns the accumulated offset and the descriptor of the field at
which it arrived.

Note that while the ref-helper receives contents of a descriptor, it
is expected to return an actual descriptor.  This means it cannot
return the same object to induce recursion on the same descriptor.

The ref-helper may be `#f` to indicate that it doesn't make sense to
use an index with a descriptor type.  In that case, an error will be
signaled if the user attempts this.

The `bytevector-ref-proc` must be a ternary procedure that takes a
bytevector, an offset, and the descriptor contents; returning a
decoded value, as specified by the descriptor contents, residing at
the given offset in the bytevector.

The `bytevector-set-proc` is similar, but takes an additional value
argument, whose encoded representation it should fill into the
bytevector, at the given offset and as specified by the descriptor
contents.

The ref and set procedures of the "simple" type, for example, pass on
said arguments in the same order --excluding the descriptor contents--
to the ref and set procedures of the descriptor instance.

The ref and set procedures may be `#f`, specifying default behavior
(see section "Accessing and mutating").
