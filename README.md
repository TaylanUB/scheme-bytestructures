Structured access to bytevector contents
========================================

This library offers a system imitating the type system of the C
programming language, to be used on bytevectors.  C's type system
works on raw memory, and ours works on bytevectors which are an
abstraction over raw memory in Scheme.

An abstract class of C types like "array" or "struct" corresponds to a
"bytestructure descriptor type" object in our system.

A concrete C type like `uint8_t[5]` (i.e. `uint8_t` array of length 5)
corresponds to a "bytestructure descriptor" object.  Every such object
is of a given descriptor type, and created with certain parameters
making it unique.

For example we could define a descriptor type called "number" which
takes a Boolean parameter specifying whether it's signed, a parameter
defining its endianness, and a parameter defining how many bytes it's
long, so one could create concrete descriptors like `uint16-le`,
`sint64-be`, etc. from this type.

Some descriptor types, like vector, struct, and union, take other
descriptor objects as part of their setup, so for instance you can
create a `uint8_t[5]` descriptor by passing the `uint8` descriptor and
the number 5 as parameters to the "vector" descriptor type.  You can
then use that descriptor while setting up a descriptor of the struct
type, and so on.

Note that these concrete descriptors still don't hold any actual bytes
themselves, so there are three layers: the abstract descriptor type, a
concrete descriptor with all its parameters set, and then the actual
data (bytevector) whose contents the descriptor describes.  We have a
convenience type bundling a descriptor with a bytevector, yielding a
"bytestructure" object which both holds real data, and the meta-data
(descriptor) which declares the layout of the bytes it contains.
These correspond roughly to actual variables in C, which both point to
a memory address and hold the type information for that address.

Note that C's type system is static, whereas ours is fully dynamic,
and even the abstract descriptor type objects are first-class objects,
and not set in stone like in C (array, struct, union, pointer).
However, a static (macro-based) API is offered on top of the dynamic
(procedural) one, for when you need maximum performance.  The
macro-based API has several limitations over the procedural one, since
macros have the inherent limitation of not being able to access
run-time data.  The macro API is only available on systems supporting
syntax-case.


Supported platforms
-------------------

R7RS and Guile are supported.  Import `(bytestructures r7)` in R7RS,
and `(bytestructures guile)` in Guile.  These will import all of the
supported sub-libraries, which you can see by peeking into the `r7/`
and `guile/` directories.  They are mostly documented below.


Creating bytestructure descriptors
----------------------------------

The procedure `make-bytestructure-descriptor` takes one argument, the
"bytestructure *description*," which may be one of the following:

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

The `bs:vector` variable holds the vector descriptor type.  The
constructor for this type actually takes a length and an element
*description*.  We provided the existing `uint8` descriptor as the
description, so we get a descriptor for uint8 vectors of length 3.
I.e. a `uint8_t[3]`.  Compare to the following, which creates a
`uint8_t[3][5]` descriptor by using a nested description list:

    (define uint8-v3-v5
      (make-bytestructure-descriptor
        `(,bs:vector 5 (,bs:vector 3 ,uint8))))

And the following does the same using our previously defined
`uint8-v3`:

    (define uint8-v3-v5
      (make-bytestructure-descriptor
        `(,bs:vector 5 ,uint8-v3)))

The size of a descriptor is returned via the procedure
`bytestructure-descriptor-size`.  Some descriptors might have dynamic
sizes, in which case this procedure cannot be used on solely the
descriptor; it must be given a bytevector, an offset indicating where
the described structure starts in the bytevector, and then the
descriptor.

    (bytestructure-descriptor-size uint8-v3-v5)
    => 15, because it's 3×5 8-bit integers in total.

    (bytestructure-descriptor-size crazy-descriptor)
    ;;; error

    (bytestructure-descriptor-size bytevector offset crazy-descriptor)
    => whatever


The "simple" type
-----------------

The library comes with the type "simple," which can fulfill all needs
of "non-compound" descriptors -- those whose sole purpose is to
convert between Scheme objects and byte-sequences.  Its instances are
created with a constant size, a bytevector-ref procedure, and a
bytevector-set procedure.  E.g. the following is a definition for
`uint8`:

    (define uint8
      (make-bytestructure-descriptor
        (list bs:simple 1 bytevector-u8-ref bytevector-u8-set!)))

(The 1 is the size, in bytes.)

If your Scheme implementation supports the library `(rnrs
bytevectors)` or `(r6rs bytevectors)`, then all the usual numeric
types will be defined: float\[le,be\], double\[le,be\],
\[u\]int(8,16,32,64)\[le,be\]

On Guile, the following native types are supported as well:
\[unsigned-\](short,int,long), `size_t`, `ssize_t`, `ptrdiff_t`


Compound types
--------------

The module comes with vector, struct, and union types for R7RS, and a
pointer type for Guile.

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

As an additional feature, the pointer type accepts a promise for the
description of the pointed-to substructure.  The promise must evaluate
to a normal bytestructure description when forced, as accepted by
`make-bytestructure-descriptor`.  This helps when creating cyclic
structures:

    (define linked-uint8-list
      (make-bytestructure-descriptor
        `(,bs:pointer ,(delay `(,bs:struct
                                (head ,uint8)
                                (tail ,linked-uint8-list))))))

Descriptors generally describe a byte layout with a fixed width.
E.g. the `my-struct` above has a width of 5 bytes in total, because it
contains one 2-byte integer, and three 1-byte integers.  It is also
possible for a descriptor to have a variable width, calculated on
basis of the bytevector the descriptor is used on.  For instance the
first byte of the bytevector could be a tag-byte giving information
about the layout of the rest of the bytevector.  Such descriptors with
variable width are not accepted as components in any of the default
compound types (vector, struct, union, pointer) because they calculate
their size once at setup and not every time they're used on a
bytevector.


The bytestructure data-type
---------------------------

Any bytestructure descriptor can be used with any bytevector to work
on the vector momentarily with accordance to the structure described
by the given descriptor, but in the usual case a bytevector will be
dedicated to a certain structure, so it is most convenient to be able
to bundle a descriptor onto a bytevector and not be required to
provide it explicitly at each access into the bytevector.  Similarly,
a section of a bytevector starting from a certain offset might be
dedicated to the structure, so being able to bundle this offset is
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

Vectors accept a (Scheme) vector of elements to be written:

    (define bs (bytestructure uint8-v3 #(0 1 2)))

Structs accept alists, as well as vectors for sequential assignment:

    (define a-simple-struct (make-bytestructure-descriptor
                              `(,bs:struct (x ,uint8) (y ,uint8))))

    (define bs (bytestructure a-simple-struct '((x 0) (y 1))))

    (define bs (bytestructure a-simple-struct #(0 1))) ;; x = 0, y = 1

Unions take a two-element list, whose first element is a field-name,
and the second element the value:

    (define a-simple-union
      (make-bytestructure-descriptor
        `(,bs:union (x ,uint8) (y ,uint16) (z ,uint32))))

    (define bs (bytestructure a-simple-union '(y 42)))

Vectors, structs, and unions also accept a bytevector, from which they
will copy as many bytes as their size:

    (define bs (bytestructure a-simple-struct #u8(0 1)))

Pointers take either a pointer object (from the FFI module), or a
bytevector (whose pointer will be used):

    (define bs (bytestructure `(,bs:pointer ,uint8) ffi-pointer))

    (define bs (bytestructure `(,bs:pointer ,uint8) bv))

_**On Guile, having an address written into a bytevector does not
protect it from garbage collection.**_

The initialization of the compound types can be done recursively,
reflecting their structure, since the assignment procedures are
implemented such that they again use the underlying descriptor's
assignment procedure.

    (define my-struct
      (make-bytestructure-descriptor
        `(,bs:struct (x ,uint16) (y (,bs:vector 3 ,uint8)))))

    (define bs (bytestructure my-struct '((x 0) (y #(0 1 2)))))


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

(In the macro API, the `y` would be implicitly quoted.)

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

    (bytestructure-set! bs #(0 #(1 2 3))) ;; Re-fill the whole struct.

The pointer type accepts an additional type of argument not mentioned
in the section about initialization: a one-element list whose contents
are passed to the underlying bytestructure's assignment procedure.
This usage is only valid if the pointer is already pointing to a valid
bytevector, so using it at initialization does not make sense.

    (bytestructure-set! uint8-v3-ptr '(#(0 1 2)))

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

    (bytestructure-set! bs 'y #u8(...))
    ;; Equivalent to:
         (bytevector-copy! (bytestructure-bytevector bs)
                           (+ 2 (bytestructure-offset bs))
                           #u8(...)
                           0
                           (bytestructure-descriptor-size
                             (bytestructure-bytevector bs)
                             (+ 2 (bytestructure-offset bs))
                             uint8-v3))

The syntaxes `bytestructure-ref*` and `bytestructure-set!*` are like
`bytestructure-ref` and `bytestructure-set!`, except that in the
position of the bytestructure argument, they instead directly take the
values it encapsulates as separate arguments: a bytevector, an initial
offset into the bytevector, and a bytestructure descriptor.

    (bytestructure-ref* bytevector offset descriptor index ...)
    (bytestructure-set!* bytevector offset descriptor index ... value)

The `bytestructure-ref-helper` and `bytestructure-ref-helper*`
syntaxes are like `bytestructure-ref` and `bytestructure-ref*`, except
that they always return three values; a bytevector, an offset, and a
bytestructure descriptor; instead of a bytestructure encapsulating
these or the ultimate value of the referencing.

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


Macro-based API
---------------

For when you need maximum performance in your code, a macro-based API
is offered, which internally works similar to the procedural API but
does the bulk of the work at macro-expand time, so there is zero or
near-zero run-time overhead.  This API requires syntax-case.

The main entry point of this API is the
`define-bytestructure-accessors` macro, which takes a bytestructure
description, and defines three macros for you: a referencing helper
(to calculate offsets only), a referencer (to actually acquire
values), and a setter.

    (define-bytestructure-accessors
      uint8-v3-v5-ref-helper uint8-v3-v5-reffer uint8-v3-v5-setter
      `(,bs:vector 5 (,bs:vector 3 ,uint8)))

    (uint8-v3-v5-ref-helper 3 2)
    => 11 (third position in fourth vector-of-5)

    (define bv (bytevector 0 1 2 3 ...))
    (uint8-v3-v5-reffer bv 2 1) => 7
    (uint8-v3-v5-setter bv 2 1 42)
    (uint8-v3-v5-reffer bv 2 1) => 42

Something important to note on the macro API is that the indices you
pass to the defined accessors aren't handled the same way the
procedural API handles them.  For some types, e.g. vectors, there's no
difference, but for instance the struct and union types implicitly
quote the index object, since they can't wait for program execution to
determine what symbol you're passing them, since the symbol is used
for a (slightly) expensive look-up in a key-to-offset table for the
struct, and a similar table in the union.

Some referencing and setting operations which can't be resolved to
bare bytevector references or assignments are turned into copying
operations.  E.g. if you have a vector of vectors, but you provide
only one index while referencing, then you will get a fresh bytevector
that is a copy of a slice of the original; the slice which contained
the inner vector to which your one index pointed to.  While assigning,
similarly, the value you provide is expected to be a bytevector, and
its contents will be written over the part of the original bytevector
which contains the inner vector.

The following procedures meant for use in procedural macros might be
useful for advanced use of the library:

- `bytestructure-ref-helper/syntax` takes an offset, bytestructure
  descriptor, and a list of indices, and returns a new offset (which
  is also a syntax object by virtue of being a number).

- `bytestructure-ref/syntax` takes a syntax object that would evaluate
  to a bytevector, an offset, a bytestructure descriptor, and a list
  of indices, and returns a syntax object that would evaluate to a
  procedure call doing what `bytestructure-ref*` would do.

- `bytestructure-set!/syntax` takes a syntax object that would
  evaluate to a bytevector, an offset, a bytestructure descriptor, and
  a list of indices, and returns a syntax object that would evaluate
  to a procedure call doing what `bytestructure-set!*` would do.


Creating new types
------------------

    (make-bytestructure-descriptor-type
      constructor size-or-size-accessor
      ref-helper bytevector-ref-proc bytevector-set-proc
      syntactic-ref-helper syntactic-ref-proc syntactic-set-proc)

This will return a descriptor type object which can be used with
`make-bytestructure-descriptor` as explained in the section "Creating
bytestructure descriptors."

- The `constructor` is the one mentioned in the section "Creating
bytestructure descriptors"; it handles the arguments used to create a
descriptor instance of this type, and should return the "content" for
this instance (NOT a descriptor; just the content; this is an internal
field of a descriptor which the framework will populate for you).
This "content" object can be anything; it's private to your descriptor
type and will be passed to the other procedures you provided so they
know what to do.  Typically a record type is used, e.g. the vector
type uses a record that holds a length and an element descriptor.

A common strategy that can be utilized by a type's constructor,
usually in types that hold other descriptors (e.g. vector), is to call
`make-bytestructure-descriptor` on an argument to allow it to be an
existing bytestructure descriptor, or a description for an anonymous
descriptor.

Another useful strategy, as for example utilized by the pointer type,
is to allow a promise in place of the above mentioned bytestructure
descriptor or description argument, and not force the promise during
creation but only during use of the descriptor instance.  This way
users can provide a variable that has been bound but its value not
defined yet (e.g. via `letrec`), and will shortly after be defined to
be another (or the same) descriptor, thus creating cyclic descriptors.

- The `size-or-size-accessor` must either be a non-negative exact
integer, or a procedure taking a bytevector, an offset, and your
descriptor contents, and returning the size of the instance as
described by the contents.

The bytevector and offset are given to satisfy possible use-cases
where the size of a structure is dynamic, depending on parts of its
bytes; the procedure should expect to receive `#f` for these arguments
if the descriptor will be used in situations where the size is
requested independently of a bytevector.  Specifically, the vector,
struct, and union types calculate their sizes at creation time to save
work, thus they cannot contain dynamic-sized descriptors; the pointer
type, too, cannot hold dynamic-sized descriptors, because getting a
bytevector from a pointer requires the size to be declared beforehand
in Guile (i.e. there are no "open ended" bytevectors with no bounds
checking like in C).

The size-accessor procedure of a compound type can use
`bytestructure-descriptor-size` to acquire the size of other
descriptors it holds.

In descriptor types that don't allow dynamic size, a small performance
hack is to calculate the size of the structure in the constructor and
put it into the contents object so the size-accessor doesn't have to
recalculate it every time and instead just take it out from the given
contents object.

- The `ref-helper` must be a procedure that takes a bytevector, an
offset, the contents of the descriptor, and an "index" object; it
returns three values: the same or a different bytevector, and a new
offset and descriptor as determined by the index.

For example the ref-helper of the vector type multiplies the size of
the element descriptor (as received through the "contents" object)
with the index, and adds the resulting value to the old offset, to
calculate the new offset.  It always returns the element descriptor,
and the same bytevector that was given (which may just be `#f`).  The
ref-helper of the struct type, on the other hand, must iterate through
its fields, adding to the offset the sizes of the descriptors it skips
until it arrives at the field with the requested index (a symbol), and
returns the accumulated offset and the descriptor of the field at
which it arrived.

Note that while the ref-helper receives contents of a descriptor, it
is expected to return an actual descriptor.  This means it cannot
return the same object to induce recursion on the same descriptor.

The ref-helper mainly makes sense for compound types.  It may be `#f`
to indicate that it doesn't make sense to use an index with the
descriptor type.  (In that case, an error will be raised when the user
attempts this.)

- The `bytevector-ref-proc` must be a procedure that takes a bytevector,
an offset, and the descriptor contents, and returns a decoded value as
specified by the descriptor contents residing at the given offset in
the bytevector.

- The `bytevector-set-proc` is similar, but takes an additional value
argument, whose encoded representation it should fill into the
bytevector, at the given offset and as specified by the descriptor
contents.

The ref and set procedures of the "simple" type, for example, pass on
said arguments in the same order, excluding the descriptor contents,
to the ref and set procedures of the descriptor instance.

The ref and set procedures may be `#f`, specifying default behavior
(see section "Accessing and mutating").

- The `syntactic-ref-helper` must be a procedure that takes a syntax
  object that would evaluate to an offset, the contents of the
  descriptor, and a syntax object that would evaluate to an index or
  can directly be interpreted as an index (e.g. an implicitly quoted
  symbol); it returns two values: a syntax object that would evaluate
  to a new offset, and a new descriptor.

- The `syntactic-ref-proc` must be a procedure that takes a syntax
  object that would evaluate to a bytevector, a syntax object that
  would evaluate to an offset, and the descriptor contents, and
  returns a syntax object that would evaluate to a procedure-call that
  does what `bytevector-ref-proc` would do.

- The `syntactic-set-proc` must be a procedure that takes a syntax
  object that would evaluate to a bytevector, a syntax object that
  would evaluate to an offset, and the descriptor contents, and
  returns a syntax object that would evaluate to a procedure-call that
  does what `bytevector-set-proc` would do.

These are called during the macro-expand phase if the user uses the
macro-based API.  See the pointer type's syntactic-set-proc to see
an example use of e.g. `bytestructure-set!/syntax`.


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

Equivalent bytestructure reference:

    > (define-bytestructure-accessors
        bs-ref-helper bs-ref bs-set
        `(,bs:vector
          5 (,bs:vector
             5 (,bs:struct (x ,uint8)
                           (y ,uint8)
                           (z ,uint8)))))
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
checks being removed.  In Guile 2.0, the latter is the case (and
additionally, record types are not as efficient as they could be), so
using a bytestructure reference will be slower, by orders of
magnitude, than a direct bytevector reference, which itself is not
optimized against type and bounds checks.

Nevertheless, the offset calculation at least avoids the consing of
rest-argument lists, so no heap allocation happens, which will make
speed predictable, although offset calculation takes linear time with
regard to the depth of a structure and, for structs and unions, the
positions of referenced fields.

When possible, the performance issues can be alleviated, without
relying on automatic optimization or the macro API, by hoisting offset
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

    > (define bs (bytestructure `(,bs:vector 1 ,uint8)))
    > (define-inlinable (ref x) (bytestructure-ref bs 0))
    > ,time (for-each ref times)
    ;; 1.700115s real time

Showcasing the effect of a deeper structure:

    > (define bs (bytestructure `(,bs:vector 1
                                   (,bs:vector 1
                                     (,bs:vector 1 ,uint8)))))
    > (define-inlinable (ref x) (bytestructure-ref bs 0 0 0))
    > ,time (for-each ref times)
    ;; 3.200876s real time

Showcasing the effect of referencing latter fields of a struct:

    > (define bs (bytestructure `(,bs:struct (x ,uint8)
                                             (y ,uint8)
                                             (z ,uint8))))
    > (define-inlinable (ref x) (bytestructure-ref bs 'x))
    > ,time (for-each ref times)
    ;; 1.613504s real time
    > (define-inlinable (ref x) (bytestructure-ref bs 'z))
    > ,time (for-each ref times)
    ;; 2.857634s
