Structured access to bytevector contents
========================================

This library offers a system imitating the type system of the C
programming language, to be used on bytevectors.  C's type system
works on raw memory, and ours works on bytevectors which are an
abstraction over raw memory in Scheme.

A C type corresponds to a "bytestructure descriptor" object in our
system.

    ;; typedef uint8_t uint8_v3_t[3];
    (define uint8-v3 (bs:vector 3 uint8))

    ;; typedef struct { uint16_t x; uint8_v3_t y; } my_struct;
    (define my-struct (bs:struct `((x ,uint16) (y ,uint8-v3))))

These can then be bundled with a bytevector, yielding a
"bytestructure" object on which referencing and assignment work in
accordance with the types declared in the descriptor.

    ;; my_struct str;
    (define str (bytestructure my-struct))

    ;; str.y[2]
    (bytestructure-ref str 'y 2)

    ;; str.y[2] = 42;
    (bytestructure-set! str 'y 2 42)

If your Scheme implementation supports syntax-case, then a macro-based
API is available as well, for when the procedural API is too slow for
your purposes.

    (define-bytestructure-accessors my-struct
      my-struct-ref-helper my-struct-ref my-struct-set!)

    ;; foo.y[2]
    (my-struct-ref bytevector y 2)

    ;; foo.y[2] = 42;
    (my-struct-set! bytevector y 2 42)

Note that we don't use the bytestructure data type anymore; we work
directly on bytevectors.  The struct fields are also implicitly quoted
and can't be variable references, since their look-up will happen at
compile time.  The ref-helper will be explained later.


Supported platforms
-------------------

R7RS and GNU Guile are supported.  Import `(bytestructures r7)` in
R7RS, and `(bytestructures guile)` in Guile.  These will import all of
the supported sub-libraries, which you can see by peeking into the
`r7/` and `guile/` directories.  They are mostly documented below.

To make the library available to Guile, rename the repository
directory to `bytestructures` (it will likely be
`scheme-bytestructures` when you clone it), then add its parent
directory to `GUILE_LOAD_PATH`.  Don't use the `-L` flag with a
relative path because `include-from-path` doesn't work well with that.


Bytestructure descriptors
-------------------------

- `(make-bytestructure-descriptor size alignment ref-helper getter setter)`

This is the low-level procedure for creating descriptors, but you will
usually be using one of the higher-level procedures such as
`bs:vector`.

You can look into the implementations of those higher-level procedures
to get some ideas on how such descriptor constructors can be
implemented, but here's a quick summary of the arguments taken by this
procedure.

The `size` will typically be a non-negative integer.  However,
descriptors can have dynamic sizes.  For instance the first few bytes
of a bytevector could be tag bytes giving information about the layout
of the rest of the bytevector.  For such cases, the `size` argument
may be a procedure taking three arguments and returning the size.  The
first argument is a Boolean indicating whether we are currently in the
macro-expand phase, or at run-time.  If run-time, the other two
arguments are a bytevector and an offset into the bytevector, and we
just calculate and return the size.  If we're in the macro-expand
phase, then the two arguments are instead syntax objects that would
evaluate to a bytevector and an offset at run-time, and we have to
return a syntax object that contains code that would calculate the
size at run-time.  (We can't possibly calculate it in the macro-expand
phase, since it depends on the bytevector that will only be available
at run-time; we only have a syntax object that will later evaluate to
the bytevector, so we have to construct code that will later do the
calculation).

The `alignment` must be an integer specifying the type's preferred
memory alignment (e.g. equal to `size` for numeric types).  Struct
descriptors make use of this to apply typical C struct field
alignment.

The `ref-helper` is a procedure taking four arguments: a Boolean
indicating whether we're in the macro-expand phase, a bytevector (or
syntax object thereof), an offset (or syntax object thereof), and an
index object (or syntax object thereof).  The procedure returns three
values: the same or another bytevector (or syntax object thereof), a
new offset (or syntax object thereof), and a bytestructure descriptor.
Vectors, structs, etc. provide a ref-helper that implements their
indexing semantics; see their implementations to get an idea.
Non-compound descriptors like for numbers should pass `#f` for the
`ref-helper` argument, since they can't be indexed through.

The `getter` takes a Boolean, a bytevector (or syntax object), and an
offset (or syntax object).  It typically decodes a value from the
bytevector found at the given offset (or generates syntax doing so).
It may be `#f` for compound descriptors like vectors and structs where
referencing them directly (instead of referencing through them with an
index) doesn't make sense.

The `setter` takes a Boolean, a bytevector (or syntax object), an
offset (or syntax object), and a value (or syntax object).  It
typically encodes the given value into the bytevector at the given
offset (or generates syntax doing so).  It may be `#f` for compound
descriptors like vectors and structs where assigning directly (instead
of referencing through them with an index) doesn't make sense,
although you may also provide some convenient semantics for such
descriptors; see the documentation for vector, struct, union, and
pointer descriptors.

- `(bytestructure-descriptor-size descriptor)`
- `(bytestructure-descriptor-size descriptor bytevector offset)`

This procedure returns the size of a descriptor.  If the descriptor
has variable size (as described above for the `size` argument), then
this procedure cannot be used solely on the descriptor; it must also
be given a bytevector, and an offset indicating where the structure
described by the descriptor starts in the bytevector.

    (bytestructure-descriptor-size uint8-v3-v5)
    => 15, because it's 3×5 8-bit integers in total.

    (bytestructure-descriptor-size crazy-descriptor)
    ;;; error

    (bytestructure-descriptor-size crazy-descriptor bytevector offset)
    => whatever

To make dynamic-sized structures work with the macro API as well,
there is a syntactic variant, `bytestructure-descriptor-size/syntax`,
which can be used in the macro-expand phase to generate code that
would then calculate the size at run-time.  This takes three
arguments: a syntax object that would evaluate to a bytevector, a
syntax object that would evaluate to an offset, and the descriptor.

The `bytestructure-descriptor-size` and
`bytestructure-descriptor-size/syntax` procedures call the given
descriptor's `size` procedure with the "in macro-expand phase" flag
set to false and true respectively.

- `(bytestructure-descriptor-alignment descriptor)`

Returns the preferred memory alignment of the descriptor.  Struct
descriptors make use of this to apply typical C struct field
alignment.


Numeric types
-------------

Some descriptors for numeric types are readily provided in the
`numeric` sub-library.  It contains, at the very least, `uint8`.  If
your Scheme implementation supports the library `(rnrs bytevectors)`
or `(r6rs bytevectors)`, then all of the following are available:
`float32[le,be]`, `double64[le,be]`, `[u]int(8,16,32,64)[le,be]`.

On Guile, the following native types are also available:
\[unsigned-\](short,int,long), `size_t`, `ssize_t`, `ptrdiff_t`

These descriptors cannot be indexed through as vectors and structs
can; they can only be used to directly reference or assign values.
For instance,

    (define bs (bytestructure uint32))
    (bytestructure-set! bs #xdeadbeef)

is comparable to:

    (define bv (make-bytevector 4))
    (bytevector-u32-native-set! bv #xdeadbeef)


Compound types
--------------

A few high-level procedures for creating descriptors for compound
types are provided: `bs:vector`, `bs:struct`, and `bs:union` for R7RS,
and additionally `bs:pointer` for Guile.

- `(bs:vector length element-descriptor)`

This returns a descriptor for vectors (arrays in C terms) of a given
length and element type.

The elements are indexed with non-negative integers as usual, and no
bounds checking is done; an off-bounds index will either raise an
error due to an off-bounds bytevector index, or attempt to decode
whatever bytes are found at the relevant place in the bytevector,
which might just result in a valid value without raising an error.

While vectors are meant to be indexed through, you can also assign to
them directly; in that case the value you provide is expected to be a
regular Scheme vector of the same length as the vector descriptor, and
each element of it will be assigned into the corresponding element of
the vector (using the assignment procedure of the `element-descriptor`
for each).

    (define bs (bytestructure (bs:vector 3 uint8)))
    (bytestructure-set! bs #(0 1 2))  ;bytevector-u8-set! times three

You may also provide a bytevector, in which case its contents will
simply be copied into your bytestructure, but only as many bytes as
the size of your vector descriptor.

Vectors don't accept variable-size descriptors as their element
descriptor, because they calculate their own total size eagerly.

- ```(bs:struct `((key1 ,descriptor1) (key2 ,descriptor2) ...)```
- ```(bs:struct alignment `((key1 ,descriptor1) (key2 ,descriptor2) ...)```

This returns a descriptor for a struct as in C, with the given fields.

Conventional C struct alignment is applied if `alignment` is omitted
or `#t`.  When `#f`, there are no padding fields at all.  It can also
be an integer specifying the maximum alignment value for the fields.

The elements are indexed by symbols.  In the macro API, they are
quoted implicitly and looked up at compile-time.

Similar to vectors, a direct assignment procedure is provided for
convenience purposes.  This accepts a Scheme vector for assigning the
fields sequentially, and a list like the one in the constructor for
assigning any number of fields by name.

    (define bs (bytestructure (bs:struct `((x ,uint8) (y ,uint8)))))
    (bytestructure-set! bs #(0 1))  ;x = 0, y = 1
    (bytestructure-set! bs '((y 2) (x 1)))

You may also provide a bytevector, in which case its contents will
simply be copied into your bytestructure, but only as many bytes as
the size of your struct descriptor.

Structs don't accept variable-size descriptors as field descriptors,
because they calculate their own total size eagerly.

- ```(bs:union `((key1 ,descriptor1) (key2 ,descriptor2) ...)```

This returns a descriptor for a union as in C, with the given fields.

The elements are indexed by symbols.  In the macro API, they are
quoted implicitly and looked up at compile-time.

Union descriptors also offer an assignment procedure for convenience,
which accepts a two-element list where the first element is the key
and the second the value.

    (define bs (bytestructure (bs:union `((x ,uint8) (y ,uint16)))))
    (bytestructure-set! bs '((y 312)))

This is actually more verbose than the plain variant:

    (bytestructure-set! bs 'y 312)

but it's supported anyhow.

You may also provide a bytevector, in which case its contents will
simply be copied into your bytestructure; as many bytes as the size of
your union descriptor, i.e. the size of the biggest field.

Unions don't accept variable-size descriptors as field descriptors,
because they calculate their own total size eagerly.

- `(bs:pointer content-descriptor)`

*(Only available on Guile so far.)*

This returns a descriptor for a pointer value as in C.  I.e. the
bytevector with which you use this descriptor will be expected to hold
a pointer-sized numeric value, a memory address.

The index `*` (symbol) can be used to dereference through the pointer.
(Implicitly quoted when used in the macro API.)  Any other index may
be provided as well, in which case the dereferencing will happen
implicitly and the provided index passed through to the next
descriptor's referencing procedure (that of the `content-descriptor`).

_**On Guile, having an address written into a bytevector does not
protect it from garbage collection.**_ So using pointer descriptors
can make your program memory unsafe.

For the `content-descriptor`, a promise is accepted as well, which
should evaluate to a descriptor when forced.  This helps when creating
cyclic structures:

    (define linked-uint8-list
      (bs:pointer (delay (bs:struct `((head ,uint8)
                                      (tail ,linked-uint8-list))))))

Pointers also have a direct referencing and a direct assignment
procedure.  Referencing the pointer bytestructure yields the
pointed-to bytevector.  Assigning to it accepts a variety of values:
an integer, that will be written directly (taken to be a memory
address); a pointer object from Guile's FFI module, whose address
value will be written; a bytevector, whose memory address will be
written; or a one-element list, whose element will be assigned into
the pointed-to bytevector, using the `content-descriptor`.

    (define bs (bytestructure linked-uint8-list))
    (bytestructure-set! bs some-address)
    (bytestructure-ref bs)  ;=> a bytevector, containing the head/tail
    (bytestructure-ref bs '* 'head)  ;=> some uint8 value
    (bytestructure-ref bs 'head)  ;same as previous

    (bytestructure-set! some-pointer #x012345678))
    (bytestructure-set! some-pointer ffi-pointer))
    (bytestructure-set! some-pointer bytevector))
    (bytestructure-set! uint8-v3-ptr '(#(0 1 2)))

Pointers don't accept variable-size descriptors as their content
descriptor, because the bytevector for the content descriptor is
created on the fly, so the size must be known in advance.
(Bytevectors have fixed length; the constructor procedure from the FFI
module that makes a bytevector from a memory address also needs the
desired length to be specified.  A variable-size descriptor needs the
bytevector to calculate that length, so there's a circular dependency
between creating the bytevector, and calculating what its size should
be.)


The bytestructure data type
---------------------------

Any bytestructure descriptor can be used with any bytevector to work
on that vector momentarily in accordance with the descriptor, but in
the usual case a bytevector will be dedicated to a certain structure,
so it is most convenient to be able to bundle a descriptor onto a
bytevector and not be required to provide it explicitly at each access
into the bytevector.  Similarly, a section of a bytevector starting
from a certain offset might be dedicated to the structure, so being
able to bundle this offset is also useful.

    (define bs (make-bytestructure bytevector offset descriptor))
    (bytestructure? bs)           => #t
    (bytestructure-bytevector bs) => bytevector
    (bytestructure-offset bs)     => offset
    (bytestructure-descriptor bs) => descriptor

The `bytestructure` procedure can be used to create a bytestructure
with a new bytevector of the right size for its descriptor, and
optionally initialize it with values.

E.g.

    (define bs (bytestructure (bs:vector 3 int16)))

is equivalent to

    (define bs (make-bytestructure
                 (make-bytevector 6)
                 0
                 (bs:vector 3 int16)))

Providing the optional second argument to `bytestructure` will invoke
the assignment procedure of the descriptor type.  See the
documentation for vectors, structs, and unions for the detailed
description; here's a quick overview again:

Vectors accept a (Scheme) vector of elements to be written:

    (define bs (bytestructure uint8-v3 #(0 1 2)))

Structs accept quasi-alists (the entries are two-element lists and not
plain pairs), as well as vectors for sequential assignment:

    (define a-simple-struct (bs:struct `((x ,uint8) (y ,uint8))))

    (define bs (bytestructure a-simple-struct '((x 0) (y 1))))

    (define bs (bytestructure a-simple-struct #(0 1))) ;x = 0, y = 1

Unions take a two-element list, whose first element is a field-name,
and the second element the value:

    (define a-simple-union (bs:union `((x ,uint8) (y ,uint16))))

    (define bs (bytestructure a-simple-union '(y 42)))

Vectors, structs, and unions also accept a bytevector, from which they
will copy as many bytes as their size:

    (define bs (bytestructure a-simple-struct #u8(0 1)))

Pointers take integers, pointer objects (from the FFI module), or a
bytevector (whose pointer will be used):

    (define bs (bytestructure (bs:pointer uint8) ffi-pointer))

    (define bs (bytestructure (bs:pointer uint8) bv))

_**On Guile, having an address written into a bytevector does not
protect it from garbage collection.**_ So using pointer descriptors
can make your program memory unsafe.

Note that pointers also accept a one-element list in their assignment
procedure.  (See the pointer documentation.)  It doesn't make sense to
use this during initialization, because there exists no bytevector
that is being pointed to yet.  (And if we created it automatically, it
would have no references to it as soon as it goes out of scope, and
would be garbage collected promptly, leaving you with a dangling
pointer in your bytestructure.)

The initialization of the compound types can be done recursively,
reflecting their structure, since the assignment procedures are
implemented such that they again use the underlying descriptor's
assignment procedure.

    (define my-struct
      (bs:struct `((x ,uint16) (y ,(bs:vector 3 uint8)))))

    (define bs (bytestructure my-struct '((x 0) (y #(0 1 2)))))


Referencing and assignment
--------------------------

- `(bytestructure-ref bytestructure index ...)` (syntax)

References through the bytestructure with the given zero or more
indices to arrive at a certain bytevector, byte-offset, and
bytestructure descriptor (see the `ref-helper` argument of
`make-bytestructure-descriptor`), then does a referencing operation
there (see `getter` argument to `make-bytestructure-descriptor`).

If the descriptor at which we arrive has no `getter`, then a
bytestructure object is created with the bytevector, offset, and
descriptor at which we arrived, and this bytestructure returned.

This is a macro and not a procedure so we needn't allocate a
rest-argument list for the indices.

- `(bytestructure-set! bytestructure index ... value)` (syntax)

Reference through the bytestructure with the given zero or more
indices to arrive at a certain bytevector, byte-offset, and
bytestructure descriptor (see the `ref-helper` argument of
`make-bytestructure-descriptor`), then does an assignment operation
there (see `setter` argument to `make-bytestructure-descriptor`).

If the descriptor at which we arrive has no `setter`, then the given
value must be a bytevector; as many bytes as the size of the arrived
descriptor will be copied into the arrived bytevector starting from
the arrived offset.

This is a macro and not a procedure so we needn't allocate a
rest-argument list for the indices.

- `(bytestructure-ref* bytevector offset descriptor index ...)`
- `(bytestructure-set!* bytevector offset descriptor index ... value)`

These macros are like `bytestructure-ref` and `bytestructure-set!`,
except that instead of a bytestructure, they directly take an initial
bytevector, initial offset, and initial descriptor.

- `(bytestructure-ref-helper bytestructure index ...)`
- `(bytestructure-ref-helper* bytevector offset descriptor index ...)`

These do the "referencing through" part of `bytestructure-ref` and
`bytestructure-set!`, but instead of referencing or assigning a value
at the end, they simply return the bytevector, offset, and descriptor
at which they arrive, as three values.

Note that you can use `bytestructure-ref-helper` with zero indices to
destructure a bytestructure into its contents.

    (let-values (((bytevector offset descriptor)
                  (bytestructure-ref-helper bytestructure)))
      ...)

In `bytestructure-ref-helper*`, if you know that the bytevector is
irrelevant to the calculation (i.e. all involved descriptors have
fixed sizes), you can provide a bogus value and get it back as-is:

    (bytestructure-ref-helper* #f 0 uint8-v3-v5 2)
    => #f, 6, uint8-v3 ;; Two uint8-v3s were skipped, so offset 6.

    (bytestructure-ref-helper* #f 0 uint8-v3-v5 2 1)
    => #f, 7, uint8 ;; Two uint8-v3s and one uint8 was skipped.


Macro-based API
---------------

For when you need maximum performance in your code, a macro-based API
is offered, which does the bulk of the work at macro-expand time, so
there is zero or near-zero run-time overhead.  This API requires
syntax-case.

The main entry point of this API is `define-bytestructure-accessors`,
which takes a bytestructure descriptor, and defines three macros: a
referencing helper (to calculate offsets only), a referencer (to
actually acquire values), and a setter.

    (define-bytestructure-accessors
      (bs:vector 5 (bs:vector 3 uint8))
      uint8-v3-v5-ref-helper uint8-v3-v5-getter uint8-v3-v5-setter)

    (uint8-v3-v5-ref-helper #f 0 3 2)  ;the #f is a bogus bytevector
                                       ;the 0 is the initial offset
    => 11 (3 * 3 + 2)

    (define bv (bytevector 0 1 2 3 ...))
    (uint8-v3-v5-getter bv 2 1) => 7
    (uint8-v3-v5-setter bv 2 1 42)
    (uint8-v3-v5-getter bv 2 1) => 42

Don't forget that struct and union fields and the `*` symbol for
pointers will be quoted implicitly when used with these macros.

Referencing and assignment operations which can't be resolved to bare
bytevector references or assignments are turned into copying
operations.  E.g. if you have a vector of vectors, but you provide
only one index while referencing, then you will get a fresh bytevector
that is a copy of a slice of the original; the slice which contained
the inner vector to which your one index pointed to.  While assigning,
similarly, the value you provide is expected to be a bytevector, and
its contents will be written over the part of the original bytevector
which contains the inner vector.

The following procedures may be useful in the `ref-helper`, `getter`,
and `setter` arguments to `make-bytestructure-descriptor`.

- `(bytestructure-ref-helper/syntax bytevector offset descriptor indices)`

All the arguments except the descriptor are syntax objects.  The
procedure returns a syntax object that will evaluate to two values:
the bytevector and offset at which we arrived.

- `(bytestructure-ref/syntax bytevector offset descriptor indices)`

Like above, but returns a syntax object that will do the actual
referencing instead of just yielding values.

- `(bytestructure-set!/syntax bytevector offset descriptor indices values)`

Like above, but returns a syntax object that will do the actual
assignment instead of just yielding values.


Performance
===========

Macro API
---------

The macro API incurs zero run-time overhead for normal referencing and
assignment operations, since most things happen in the macro-expand
phase.

Plain bytevector reference:

    > (define times (iota 1000000)) ;A million
    > (define bv (make-bytevector 1))
    > (define-inlinable (ref x) (bytevector-u8-ref bv 0))
    > ,time (for-each ref times)
    ;; ~0.14s real time

Bytestructure reference:

    > (define bv (make-bytevector 1000))
    > (define-bytestructure-accessors
        (bs:vector 5 (bs:vector 5 (bs:struct `((x ,uint8)
                                               (y ,uint8)
                                               (z ,uint8)))))
        bs-ref-helper bs-ref bs-set)
    > (define-inlinable (ref x) (bs-ref bv 4 4 z))
    > ,time (for-each ref times)
    ;; ~0.14s real time

(Ignoring the jitter for both.)

Note that if we used variables instead of constant values for the
vector indices, then *some* calculation would need to happen at
run-time (multiplying the variables' value with the vectors' lengths),
but that isn't really overhead either because the same goes for the
plain bytevector referencing example, except there you would need to
do said multiplications manually in your code.


Procedural API
--------------

When descriptors are statically apparent, an aggressively constant
propagating and partial evaluating optimizer would be able to turn
bytestructure references into direct bytevector references, which,
through further optimization, could even end up identical to the
results of hand-written C code.  That is the most optimal outcome,
which would obsolete the macro API, but the opposite situation is that
even offset calculation happens at run-time, let alone type and bound
checks being removed.  In Guile 2.0, the latter is the case, so using
a bytestructure reference will be much slower than a direct bytevector
reference.

Nevertheless, the offset calculation at least avoids the consing of
rest-argument lists, so no heap allocation happens, which will make
speed predictable, although offset calculation takes linear time with
regard to the depth of a structure and, for structs and unions, the
positions of referenced fields.

When possible, the performance issues can be alleviated, without
relying on compiler optimization or the macro API, by hoisting offset
calculations to outside of speed-critical sections by using the
ref-helper forms, only leaving bare bytevector references at
bottlenecks.  This way a big part of the convenience offered by the
procedural API can still be used.

Following are some figures from Guile 2.0.11.

Plain bytevector reference, for comparison:

    > (define times (iota 1000000)) ;A million
    > (define bv (make-bytevector 1))
    > (define-inlinable (ref x) (bytevector-u8-ref bv 0))
    > ,time (for-each ref times)
    ;; 0.130245s real time

Equivalent bytestructure reference:

    > (define bs (bytestructure (bs:vector 1 uint8)))
    > (define-inlinable (ref x) (bytestructure-ref bs 0))
    > ,time (for-each ref times)
    ;; 0.656734s real time

Showcasing the effect of a deeper structure:

    > (define bs (bytestructure (bs:vector 1
                                   (bs:vector 1
                                     (bs:vector 1 uint8)))))
    > (define-inlinable (ref x) (bytestructure-ref bs 0 0 0))
    > ,time (for-each ref times)
    ;; 1.043576s real time

Showcasing the effect of referencing latter fields of a struct:

    > (define bs (bytestructure (bs:struct `((x ,uint8)
                                             (y ,uint8)
                                             (z ,uint8)))))
    > (define-inlinable (ref x) (bytestructure-ref bs 'x))
    > ,time (for-each ref times)
    ;; 0.920915s real time
    > (define-inlinable (ref x) (bytestructure-ref bs 'z))
    > ,time (for-each ref times)
    ;; 1.934573s real time
