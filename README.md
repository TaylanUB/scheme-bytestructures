Structured access to bytevector contents
========================================

*This module is written with Guile Scheme in mind, specifically with
regard to the FFI integration; the basic idea behind it however is
aimed at any `(rnrs bytevectors)` implementation, and the code-base
keeps implementation-specific features to a minimum except for the
FFI-heavy parts.*

A "bytestructure descriptor" describes a layout for the contents of a
bytevector, or how the bytes are to be accessed and converted into
Scheme objects.

You can think of it like C's weak typing/type-casting system.

Every bytestructure descriptor is of a certain type, which can be
either compound, meaning that its instances can contain other
descriptors (e.g. a vector type whose instances describe vectors of a
certain length and contained type), or not compound, meaning that its
instances only describe how to convert a sequence of bytes into a
Scheme object (e.g. an "integer" type whose instances can have a
specific size, endianness, etc.).


Creating bytestructure descriptors
----------------------------------

The procedure `make-bytestructure-descriptor` takes one argument, the
"bytestructure description," which may be one of the following:

1. A "bytestructure descriptor type" object; this will call the
constructor for that type with no arguments.

2. A list whose first element is a bytestructure descriptor type; this
will apply the constructor for that type to the rest of the list.

3. An existing bytestructure descriptor; this will return the same
bytestructure descriptor as is.  This is so that in places where a
bytestructure description is expected, an existing bytestructure
descriptor can be passed instead.

A simple strategy that can be used by constructors of compound
bytestructure descriptor types is to recursively call
`make-bytestructure-descriptor` on some of their arguments.  Rule 3
above helps in this situation.  E.g. consider the following example:

    (define uint8-v3
      (make-bytestructure-descriptor (list bs:vector 3 uint8)))

The `bs:vector` variable holds the compound bytestructure descriptor
type "vector."  The constructor for this type takes a length and the
description for the type of which we want a vector.  We provided the
existing `uint8` bytestructure descriptor, so we get a descriptor for
uint8 vectors of length 3.  In C terminology, a `uint8_t[3]`.  The
following creates a `uint8_t[3][5]` descriptor:

    (define uint8-v3-v5
      (make-bytestructure-descriptor
        `(,bs:vector 5 (,bs:vector 3 ,uint8))))

As does the following, using our previous `uint8-v3`:

    (define uint8-v3-v5
      (make-bytestructure-descriptor
        `(,bs:vector 5 ,uint8-v3)))


The "simple" type
-----------------

The module comes with the non-compound type "simple," which can
fulfill all needs of non-compound descriptors.  Its instances are
created with a size, bytevector-ref procedure, and bytevector-set
procedure.  E.g. the following is the definition of `uint8`:

    (define uint8
      (make-bytestructure-descriptor
        ;; The 1 is the size, in bytes.
        (list bs:simple 1 bytevector-u8-ref bytevector-u8-set!)))

All the usual numeric types are readily provided by the module:
float\[-le,-be\], double\[-le,-be\], \[u\]int(8,16,32,64)\[le,be\]

Also native-sized ones: \[unsigned-\](short,int,long), size_t,
ssize_t, ptrdiff_t


Compound types
--------------

The module comes with the three canonical compound types, vector,
struct, union, and a somewhat magical "pointer" type.

We've already covered the vector type in the section "Creating
bytestructure descriptors."  One more thing to note about them is that
like in C, they do *not* do bounds-checking on indices; an off-bounds
index will either raise an error due to an off-bounds bytevector
index, or attempt to decode whatever bytes are found at the relevant
place in the bytevector, which might just result in a valid value
without raising an error.

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

The pointer type is "magical" in that it only uses the underlying
bytevector to store a pointer to another bytevector, which holds said
pointed-to substructure.


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
also useful:

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

    (define bs (bytestructure `(,bs:vector 3 ,int16))

is equivalent to

    (define bs (make-bytestructure
                 (make-bytevector 6)
                 0
                 (make-bytestructure-descriptor
                   `(,bs:vector 3 ,int16))))

The format for the initialization values, which are the optional
second argument to `bytestructure`, depends on the descriptors in
play.  For vectors, it is a list of the contained values:

    (define bs (bytestructure uint8-v3 '(0 1 2)))

For structs, it is like in C, meaning it looks like a vector because
the field names are omitted and their order determines which value
belongs to which field.

    (define a-simple-struct (make-bytestructure-descriptor
                              `(,bs:struct (x ,uint8) (y ,uint8))))

    (define bs (bytestructure a-simple-struct '(0 1))) ;; x = 0, y = 1

The implementation of unions has unintentionally led to the ability of
setting a value for a certain field by providing any value (which will
subsequently be overwritten) to all preceding fields, and not
providing any more values.  For good style, the fields to be
overwritten should probably all use 0, `#f`, or similar when possible.

    (define a-simple-union
      (make-bytestructure-descriptor
        `(,bs:union (x ,uint8) (y ,uint16) (z ,uint32))))

    (define bs (bytestructure a-simple-union '(0 42))) ;; uint16 wins

Pointers take either directly a bytevector (whose pointer will be
used), or directly a pointer, or a one-element list containing the
values for the pointer destination (this will automatically allocate a
bytevector and use its pointer):

    (define bs (bytestructure `(,bs:pointer ,uint8) #vu8(42)))

    (define bs (bytestructure `(,bs:pointer ,uint8) '(42)))
    
The initialization of the compound types can be done recursively,
reflecting their structure:

    (define my-struct
      (make-bytestructure-descriptor
        `(,bs:struct (x ,uint16) (y (,bs:vector 3 ,uint8)))))

    (define bs (bytestructure my-struct '(0 (0 1 2))))

    (define bs (bytestructure `(,bs:pointer ,my-struct)
                              '((0 (0 1 2)))))

The nesting must be correct and cannot be flattened like in C.

The procedure `bytestructure-fill!` can be used to fill a bytevector
with values as when using `bytestructure`.  The second argument
provides an initial offset into the bytevector, where the structure
will start:

    (bytestructure-fill! bytevector offset descriptor contents)

The third argument must be a bytestructure descriptor; it cannot be a
bytestructure description.


Accessing and mutating
----------------------

Getting and setting values is fairly straightforward:

    (bytestructure-ref bytestructure index ...)
    (bytestructure-set! bytestructure index ... value)

For example, given

    (define my-struct
      (make-bytestructure-descriptor
        `(,bs:struct (x ,uint16) (y (,bs:vector 3 ,uint8)))))

    (define bs (bytestructure my-struct))

we can

    (bytestructure-ref bs 'y 2)           ;; bs->y[2]
    (bytestructure-set! bs 'y 2 42)       ;; bs->y[2] = 42;

(The field-name `y` is also called an "index" in our terminology.)

The pointer type accepts the index `*` to induce dereferencing; if any
other index is given, it implicitly dereferences and re-uses that
index for its contained descriptor.

Both forms are syntax, not procedures, because an arbitrary-length
argument list would require heap allocation, which is undesirable for
the ubiquitous accessing and mutating operations.

For both syntaxes, if the given indices don't exhaust the nested
descriptors of the bytestructure, i.e. following the indices still
leaves us on a compound descriptor, then either the compound
descriptor on which we arrived can provide a custom action, or a
default action is taken: when referencing, a bytestructure is returned
which encapsulates the bytevector, offset, and descriptor at which we
arrived, such that referencing or setting could continue from that
point by using that bytestructure; when setting, the value is expected
to be a bytevector, and as many bytes as the size of the descriptor at
which we arrived are copied from it into the target bytestructure's
bytevector, starting at the offset at which we had arrived.

In other words:

    (bytestructure-ref bs 'y)
    => A bytestructure with:
         bytevector: (bytestructure-bytevector bs)
         offset:     (+ 2 (bytestructure-offset bs))
         descriptor: uint8-v3
         ;; Offset is 2 because a uint16 was skipped.

    (bytestructure-set! bs 'y #vu8(...))
    ;; Equivalent to:
         (bytevector-copy! #vu8(...) 0
                           (bytestructure-bytevector bs)
                           (+ 2 (bytestructure-offset bs))
                           (bytestructure-descriptor-size
                             (bytestructure-bytevector bs)
                             (+ 2 (bytestructure-offset bs))
                             uint8-v3))

The pointer type defines a custom action: when referencing, it returns
the bytevector to which it points; when setting, if it is given a
bytevector, it will use the pointer of that bytevector, otherwise it
expects directly a pointer; it will write the address of the pointer.

The syntaxes `bytestructure-ref*` and `bytestructure-set!*` are
equivalent to `bytestructure-ref` and `bytestructure-set`, except that
in the position of the bytestructure argument, they instead directly
take the values it encapsulates as separate arguments: a bytevector,
an initial offset into the bytevector, and a bytestructure descriptor.

    (bytestructure-ref* bytevector offset descriptor index ...)
    (bytestructure-set!* bytevector offset descriptor index ... value)

The `bytestructure-ref-helper` and `bytestructure-ref-helper*`
syntaxes are equivalent to `bytestructure-ref` and
`bytestructure-ref*`, except that they always return three values; a
bytevector, an offset, and a bytestructure descriptor; instead of a
bytestructure encapsulating these (in the case of a non-exhaustive
index list) or the ultimate value of the referencing (in the case of
an exhaustive index list).

    (bytestructure-ref-helper bs 'y 1)
    => (bytestructure-bytevector bs), 3, uint8-v3
    ;; Offset is 3 because one uint16 and one uint8 was skipped.

Note that by providing no indices, this can be used to destructure a
bytestructure object into its components:

    (let-values (((bytevector offset descriptor)
                  (bytestructure-ref-helper bytestructure)))
      ...)

In `bytestructure-ref-helper*`, if you know that the bytevector is
irrelevant to the calculation (all involved descriptors are
static-sized), you can provide a bogus value and get it back as-is;
good style should probably use `#f`:

    (bytestructure-ref-helper* #f 0 uint8-v3-v5 2)
    => #f, 6, uint8-v3 ;; Two uint8-v3s were skipped, so offset 6.

    (bytestructure-ref-helper* #f 0 uint8-v3-v5 2 1)
    => #f, 7, uint8 ;; Two uint8-v3s and one uint8 was skipped.


Creating new types
------------------

For non-compound descriptors, the "simple" type can fulfill all
use-cases; since a size, and a pair of "bytes to scheme object" and
"scheme object to bytes" conversion procedures is absolutely
everything that's needed to describe any kind of binary object.
However, sometimes it might be convenient, make sense, or be otherwise
useful to have a separate type of non-compound descriptor, like for
example an "integer" type whose instances can be created with a size,
signedness, endianness, etc. (Note however that the default
implementation for the numeric types just uses the "simple" type.)

    (make-bytestructure-descriptor-type
      constructor size-or-size-accessor
      bytevector-ref-proc bytevector-set-proc)

This will return a descriptor-type object which can be used with
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
type, too, cannot hold dynamic-sized descriptors, because creating a
bytevector from a pointer requires the size to be declared beforehand.

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

New compound types are created as follows:

    (make-bytestructure-descriptor-compound-type
      constructor size-or-size-accessor
      bytevector-constructor-helper bytevector-ref-helper
      bytevector-ref-proc bytevector-set-proc)

The `constructor` and `size-or-size-accessor` are just as before.
Note that the size is less likely to be a constant this time, since it
will usually depend on whatever descriptor(s) the compound one will
hold.  It can use `bytestructure-descriptor-size` on its contained
descriptors to calculate the size; this procedure takes either only a
bytestructure descriptor (for when trying to calculate the size
statically, typically in the constructor-helper; see below), or a
bytevector, offset so-far, and descriptor (for when requesting the
size of a potentially dynamic-sized type); for example the
size-accessor of the vector type simply multiplies the length of the
vector with the size of the contained descriptor.

The `bytevector-ref-helper` must be a four-argument procedure that
takes a bytevector, an offset so-far, the contents of the descriptor,
and an "index" object; it returns three values: the same or a
different bytevector, and a new offset and descriptor as determined by
the index.  For example the ref-helper of the vector type multiplies
the size of its contained descriptor with the index and adds the
resulting value to the offset so-far, and always returns its one
contained descriptor.  The ref-helper of the struct type, on the other
hand, must iterate through its fields, adding to the offset the sizes
of the descriptors it skips until it comes to the field with the
requested index (a symbol), and returns the accumulated offset and the
descriptor of the field at which it arrived.

Note that while the ref-helper receives contents of a descriptor, it
is expected to return an actual descriptor; as a special-case, if it
returns the same object it received, then it is taken to mean the
descriptor to which that content object belongs; this is to satisfy
possible use-cases where a ref-helper wants to recurse on the same
descriptor.

The `bytevector-constructor-helper` is similar to the ref-helper, but
the index it takes is always positional (non-negative exact integer).
The constructor helper for the struct type, for example, simply skips
over that many fields, instead of seeking for the correct field.  This
is why initializing structs works the way it does (see relevant
section).

The last two arguments are the same as before; they define the custom
behavior for non-exhaustive index lists mentioned in section
"Accessing and mutating."  They may be `#f` to indicate default
behavior (see again said section).
