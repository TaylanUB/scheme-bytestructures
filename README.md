Structured access to bytevector contents
========================================

This library offers a system imitating the type system of the C
programming language, to be used on bytevectors.  C's type system
works on raw memory, and ours works on bytevectors which are an
abstraction over raw memory in Scheme.  The system is in fact more
powerful than the C type system, elevating types to first-class
status.

A C type corresponds to a "bytestructure descriptor" object in our
system.

    ;; typedef uint8_t uint8_v3_t[3];
    (define uint8-v3 (bs:vector 3 uint8))

    ;; typedef struct { uint16_t x; uint8_v3_t y; } my_struct_t;
    (define my-struct (bs:struct `((x ,uint16) (y ,uint8-v3))))

These can then be bundled with a bytevector, yielding a
"bytestructure" object on which referencing and assignment work in
accordance with the types declared in the descriptor.

    ;; my_struct_t str;
    (define str (bytestructure my-struct))

    ;; my_struct_t str = { 0, 1 };
    (define str (bytestructure my-struct #(0 1)))

    ;; str.y[2]
    (bytestructure-ref str 'y 2)

    ;; str.y[2] = 42;
    (bytestructure-set! str 'y 2 42)

If your Scheme implementation supports syntax-case, then a macro-based
API is available as well, for when the procedural API is too slow for
your purposes.

    (define-bytestructure-accessors my-struct
      my-struct-ref-helper my-struct-ref my-struct-set!)

    (define foo (make-bytevector ...))

    ;; foo.y[2]
    (my-struct-ref foo y 2)

    ;; foo.y[2] = 42;
    (my-struct-set! foo y 2 42)

(Note that we don't use the bytestructure data type anymore; we work
directly on bytevectors.  The struct fields are also implicitly quoted
and can't be variable references, since their look-up will happen at
compile time.  The ref-helper will be explained later.)

There are also "dynamic" bytestructure descriptors, whose behavior
depends on the bytevector on which they're used.  For instance a
binary file format may specify that there are tag bytes declaring the
lengths of following fields.  The system can express this cleanly.


Supported platforms
-------------------

R7RS and GNU Guile are supported.  Detailed instructions per Scheme
implementation follow.

### Chibi

- Clone the Larceny source repository:
  https://github.com/larcenists/larceny

- Append `$larceny_repo/tools/R6RS` to the Chibi load-path via the
  `-A` command-line flag.

- Append this directory to the Chibi load-path via the `-A`
  command-line flag.

- Import `(bytestructures r7)`.

### Gauche

- Clone the Larceny source repository:
  https://github.com/larcenists/larceny

- Go to its `tools/R6RS/r6rs/` sub-directory.

- Run the following shell command in that directory and its
  sub-directories:

      for file in *.sld; do
        name=${file%.sld}
        ln -s $file $name.scm
      done

- Add `$larceny_repo/tools/R6RS` to `GAUCHE_LOAD_PATH`.

- Add this directory to `GAUCHE_LOAD_PATH`.

- Import `(bytestructures r7)`.

### Guile

- Add this directory to `GUILE_LOAD_PATH`.

(You can use the `-L` command line flag instead of augmenting
`GUILE_LOAD_PATH`, but don't use it with a relative path, because
`include-from-path` doesn't work well with that, which we use.)

- Import `(bytestructures guile)`.

### Kawa

- Clone the Larceny source repository:
  https://github.com/larcenists/larceny

- Run Kawa with a command line flag such as the following to add
  `$larceny_repo/tools/R6RS` and this directory to the load path, and
  to make it look for `.sld` files:

      -Dkawa.import.path="$bytestructures_repo/*.sld:\
      $larceny_repo/tools/R6RS/*.sld"

(The `*` stands for any number of directories, so sub-directories will
also be searched for `.sld` files.)

- Import `(bytestructures r7)`.

### Larceny

- Add this directory to `LARCENY_LIBPATH`.

- Run Larceny with the `-r7rs` flag.

- Import `(bytestructures r7)`.


Quick overview with examples
----------------------------

### Compound types

A few high-level procedures for creating descriptors for compound
types are provided: `bs:vector`, `bs:struct`, and `bs:union` for R7RS,
and additionally `bs:pointer` for Guile.

- `(bs:vector length element-descriptor)` *procedure*

This returns a descriptor for vectors (arrays in C terms) of length
`length` and element type `element-descriptor`.

    ;; uint16_t vec[3] = { 0, 1, 2 };
    (define vec (bytestructure (bs:vector 3 uint16) #(0 1 2)))

    ;; vec[1]
    (bytestructure-ref vec 1)

    ;; vec[1] = 42;
    (bytestructure-set! vec 1 42)

The elements are indexed with non-negative integers as usual, and no
bounds checking is done; an off-bounds index will either raise an
error due to an off-bounds bytevector index, or attempt to decode
whatever bytes are found at the relevant place in the bytevector,
which might just result in a valid value without raising an error.

While vectors are meant to be indexed through, so that referencing and
assignment happen with the element descriptor, you can also assign to
them directly; in that case the value you provide is expected to be a
regular Scheme vector of the same length as the vector descriptor, and
each element of it will be assigned to the corresponding element of
the vector (using the assignment procedure of the element descriptor
for each).

    ;; Basically does bytevector-u16-set! three times.
    (bytestructure-set! vec #(21 42 84))

You may also provide a bytevector, in which case its contents will
simply be copied into your bytestructure, but only as many bytes as
the size of your vector descriptor.

    ;; The results depend on endianness here.
    (bytestructure-set! vec #u8(0 1 2 3 4 5 6 7 8))  ;6 bytes copied

Vectors don't accept dynamic descriptors as their element descriptor,
because they calculate their total size eagerly and thus need to know
the size of their element descriptor independently from the bytevector
on which they will be used.

- `(bs:struct fields)` *procedure*
- `(bs:struct pack fields)` *procedure*

This returns a descriptor for a struct as in C.  `Fields` must be a
list of field specs.  `Pack` may be `#f`, `#t`, or an exact
non-negative non-zero integer.  If `pack` is omitted or `#f`, normal C
struct alignment is used.  If `pack` is `#t`, there are no padding
fields (except for bit-fields).  If `pack` is an integer, it specifies
the maximum alignment value for the fields, akin to the `pack`
directive of GCC.

A field spec is a list of two or three elements.  The first element
must be a symbol which names the field (or `#f`; see below).  Every
field must have a distinct name.  The second element must be a
bytestructure descriptor which is the type of the field.  The third
element, if present, must be an exact non-negative integer; it
signifies that the field is a bit-field of that width.  The descriptor
of a bit-field must be one that decodes values to exact integers.

The width of a bit-field may be zero, which means padding should be
inserted in its place until the next alignment boundary of the type of
that bit-field is reached.  A zero-width bit-field must have `#f` in
place of the name symbol.

    ;; typedef struct { uint8_t x; uint16_t y; } my_struct_t;
    (define my-struct (bs:struct `((x ,uint8) (y ,uint16))))

    ;; my_struct_t str = { 0, 1 };
    (define str (bytestructure my-struct #(0 1)))

    ;; my_struct_t str = { .y = 1, .x = 0 };
    (define str (bytestructure my-struct '((y 1) (x 0))))

    ;; str.y
    (bytestructure-ref str 'y)

    ;; str.y = 42;
    (bytestructure-set! str 'y 42)

    ;; Assuming 32-bit int:

    ;; struct { int a:16; int b:16; }
    (bs:struct `((a ,int32 16) (b ,int32 16)))

    ;; struct { int a:16; int :0; int b:20; }
    (bs:struct `((a ,int32 16) (#f ,int32 0) (b ,int32 20)))

Similar to vectors, a direct assignment procedure is provided for
convenience purposes.  This accepts a Scheme vector for assigning the
fields sequentially, and a list like the one in the constructor for
assigning any number of fields by name.

    ;; str = (my_struct_t){ 0, 1 };
    (bytestructure-set! str #(0 1))

    ;; str = (my_struct_t){ .y = 2, .x = 1 };
    (bytestructure-set! str '((y 2) (x 1)))

You may also provide a bytevector, in which case its contents will
simply be copied into your bytestructure, but only as many bytes as
the size of your struct descriptor.

    ;; The uint8 is set to 0; the uint16 depends on endianness.
    (bytestructure-set! str #u8(0 1 2 3 4 5))  ;3 bytes copied

Like vectors, structs don't accept dynamic descriptors as field
descriptors, because they calculate their total size eagerly.

When using the macro API, the field names are implicitly quoted and
looked up at macro-expand time.

    (define-bytestructure-accessors my-struct
      my-struct-ref-helper my-struct-ref my-struct-set!)

    ;; foo.y
    (my-struct-ref foo-bytevector y)

    ;; foo.y = 42;
    (my-struct-set! foo-bytevector y 42)

- `(bs:union fields)` *procedure*

This returns a descriptor for a union as in C.  `Fields` has the same
format as in `bs:struct`.

    ;; typedef union { uint8_t x; uint16_t y; } my_union_t;
    (define my-union (bs:union `((x ,uint8) (y ,uint16))))

    ;; my_union_t union = { .y = 42 };
    (define union (bytestructure my-union '(y 42)))

    ;; union.y
    (bytestructure-ref union 'y)

    ;; union.y = 42;
    (bytestructure-set! union 'y 42)

Union descriptors also offer an assignment procedure for convenience,
which accepts a two-element list where the first element is the key
and the second the value.

    ;; union.y = 42;
    (bytestructure-set! union '(y 42))

This isn't really shorter than the normal way of doing it, but it's
supported anyway.  (See the documentation of `bytestructure` for the
main reason.)

You may also provide a bytevector, in which case its contents will
simply be copied into your bytestructure; as many bytes as the size of
your union descriptor, i.e. the size of the biggest field.

    ;; Value of the y field will depend on endianness.
    (bytestructure-set! union #u8(0 1 2 3 4))  ;2 bytes copied

Like vectors and structs, unions don't accept dynamic descriptors as
field descriptors, because they calculate their total size eagerly.

- `(bs:pointer content-descriptor)` *procedure*

*(Only available on Guile so far.)*

This returns a descriptor for a pointer value as in C.  I.e. the
bytevector with which you use this descriptor will be expected to hold
a pointer-sized numeric value, a memory address.  `Content-descriptor`
is the descriptor for the bytes at the memory address pointed to.

    ;; uint8_t *ptr = 0xdeadbeef;
    (define ptr (bytestructure (bs:pointer uint8) #xdeadbeef))

For the `content-descriptor`, a promise is accepted as well, which
should evaluate to a descriptor when forced.  This helps when creating
self-referencing descriptors:

    ;; typedef struct linked_uint8_list_s {
    ;;   uint8_t head;
    ;;   struct linked_uint8_list_s *tail;
    ;; } *linked_uint8_list_t;
    (define linked-uint8-list
      (bs:pointer (delay (bs:struct `((head ,uint8)
                                      (tail ,linked-uint8-list))))))

The symbol `*` can be used as an index to dereference through the
pointer.  (Implicitly quoted when used in the macro API.)

    ;; linked_uint8_list_t u8list;
    (define u8list (bytestructure linked-uint8-list))

    ;; (*u8list).head
    (bytestructure-ref u8list '* 'head)

    ;; (*u8list).head = 42;
    (bytestructure-set! u8list '* 'head 42)

One can also provide any other index, which will cause an implicit
dereference.

    ;; u8list->head
    (bytestructure-ref u8list 'head)

    ;; u8list->head = 42;
    (bytestructure-set! u8list 'head 42)

Pointers also have direct referencing and assignment semantics.  (That
is what happens when your list of indices lead you up to the pointer
but no further.)  Referencing the pointer directly yields the numeric
value of the address.

    ;; linked_uint8_list_t u8lists[3];
    (define u8lists (bytestructure (bs:vector 3 linked-uint8-list)))

    ;; Returns the address stored in u8lists[1].
    (bytestructure-ref u8lists 1)

Assigning to it accepts a numeric value to be written, and a
one-element list whose one element will be assigned to the pointed-to
bytestructure.

    ;; uint8_t (*u8v3-ptr)[3];
    (define u8v3-ptr (bytestructure (bs:pointer (bs:vector 3 uint8))))

    ;; u8v3-ptr = 0xdeadbeef;
    (bytestructure-set! u8v3-ptr #xdeadbeef)

    ;; Sets new values for the uint8 values pointed to.
    (bytestructure-set! u8v3-ptr '(#(0 1 2)))

_**Warning: On Guile, having an address written into a bytevector does not
protect it from garbage collection.  Using pointer descriptors can make
your program memory unsafe.**_

Pointers don't accept dynamic descriptors as their content descriptor,
because the bytevector for the content descriptor is created on the
fly, so the size must be known in advance.  (Bytevectors have fixed
length; the constructor procedure from the FFI module that makes a
bytevector from a memory address also needs the desired length to be
specified.  A dynamic descriptor may need the bytevector to calculate
that length, so there's a circular dependency between creating the
bytevector, and calculating what its size should be.)


### Numeric types

Some descriptors for numeric types are readily provided in the
`numeric` sub-library: `float32[le,be]`, `float64[le,be]`,
`[u]int(8,16,32,64)[le,be]`.

On Guile, the following native types are also available:
`[unsigned-](short,int,long)`, `size_t`, `ssize_t`, `ptrdiff_t`

These descriptors cannot be indexed through as vectors and structs
can; they can only be used to directly reference or assign values.
I.e. your list of indices should lead to a descriptor of such a type
and contain no indices beyond that.  For instance,

    ;; uint32_t x;
    (define x (bytestructure uint32))

    ;; x = 42;
    (bytestructure-set! x 42)

    ;; uint32_t xs[3];
    (define xs (bytestructure (bs:vector 3 uint32)))

    ;; xs[1] = 42;
    (bytestructure-set! xs 1 42)

Note that the `pointer` type (available on Guile) can be indexed
through, but at the same time defines such direct referencing and
assignment semantics.


Specification
-------------

In this section the `descriptor` argument name signifies that an
argument must be a bytestructure descriptor, and `offset` signifies
that an argument must be an exact non-negative integer.

A *dynamic descriptor* is a bytestructure descriptor whose `size`
and/or `ref-helper` procedures reference their bytevector and/or
offset arguments.


### Bytestructure descriptors

- `(make-bytestructure-descriptor size alignment ref-helper getter
  setter)` *procedure*

Low-level procedure for creating descriptors.  Usually one of the
high-level procedures such as `bs:vector` should be used.

`Size` must be an exact non-negative integer, or a procedure taking
three arguments and returning a non-negative integer (this is for
dynamic descriptors).  The first argument to the procedure is a
Boolean indicating whether the call to the procedure is happening in
the macro-expand phase.  If it's false, the other two arguments are a
bytevector and an offset into the bytevector respectively.  If it's
true, then the two arguments are instead syntax objects that would
evaluate to a bytevector and an offset respectively.  The offset is
the position in the bytevector at which the bytes belonging to the
descriptor start.  The procedure should return the size of the
structure described by the descriptor, or return a syntax object that
would evaluate to the size.

`Alignment` must be an exact non-negative non-zero integer specifying
the type's preferred memory alignment.

`Ref-helper` must be `#f` or a procedure taking four arguments: a
Boolean indicating whether the call to the procedure is happening in
the macro-expand phase, a bytevector (or syntax object thereof), an
offset (or syntax object thereof), and an index object (or syntax
object thereof).  The procedure must return three values: the same or
another bytevector (or syntax object thereof), a new offset (or syntax
object thereof), and a bytestructure descriptor (NOT a syntax object
thereof).  This procedure implements the indexing semantics of
compound types.  The bytevector argument is provided to satisfy
dynamic descriptors; the `ref-helper` of non-dynamic descriptors
should ignore its value and return it back untouched.

`Getter` must be `#f` or a procedure taking three arguments: a Boolean
indicating whether the call to the procedure is happening in the
macro-expand phase, a bytevector (or syntax object thereof), and an
offset (or syntax object thereof).  The procedure should decode the
bytes at the given offset in the given bytevector (or return a syntax
object whose evaluation would do this), thus implementing the
referencing semantics of the descriptor.

`Setter` must be `#f` or a procedure taking three arguments a Boolean
indicating whether the call to the procedure is happening in the
macro-expand phase, a bytevector (or syntax object thereof), an offset
(or syntax object thereof), and a value (or syntax object thereof).
The procedure should encode the given value into given offset in the
given bytevector (or return a syntax object whose evaluation would do
this), thus implementing the assignment semantics of the descriptor.

- `(bytestructure-descriptor-size descriptor)` *procedure*
- `(bytestructure-descriptor-size descriptor bytevector offset)`
  *procedure*

Returns the size of `descriptor`.  If `descriptor` is dynamic, then
the `bytevector` and `offset` arguments must be provided, which will
be passed to the `size` procedure of `descriptor`, with the
macro-expand Boolean argument set to false.

    (bytestructure-descriptor-size uint8-v3-v5)
    => 15, because 3×5 8-bit integers in total.

    (bytestructure-descriptor-size a-dynamic-descriptor)
    ;;; error

    (bytestructure-descriptor-size
     a-dynamic-descriptor bytevector offset)
    => 42

- `(bytestructure-descriptor-size/syntax descriptor)` *procedure*
- `(bytestructure-descriptor-size/syntax descriptor bytevector-syntax
  offset-syntax)` *procedure*

Returns a syntax object that would evaluate to the size of
`descriptor`.  If `descriptor` is dynamic, then the
`bytevector-syntax` and `offset-syntax` arguments must be provided,
which will be passed to the `size` procedure of `descriptor`, with the
macro-expand Boolean argument set to false.

- `(bytestructure-descriptor-alignment descriptor)` *procedure*

Returns the preferred memory alignment of the descriptor.


### The bytestructure data type

Any bytestructure descriptor can be used with any bytevector to work
on it momentarily in accordance with the descriptor, but in most cases
a bytevector is dedicated to a certain structure, so it makes sense to
bundle a descriptor with the bytevector.  Or only a portion of the
bytevector, starting from a certain offset, might be dedicated to the
structure, so being able to bundle that offset is also useful.

- `(make-bytestructure bytevector offset descriptor)` *procedure*

Returns a bytestructure object with the given bytevector, offset into
the bytevector, and bytestructure descriptor.

- `(bytestructure? obj)` *procedure*

Returns a Boolean indicating whether `obj` is a bytestructure.

- `(bytestructure-bytevector bytestructure)` *procedure*

Returns the bytevector of `bytestructure`.

- `(bytestructure-offset bytestructure)` *procedure*

Returns the offset of `bytestructure`.

- `(bytestructure-descriptor bytestructure)` *procedure*

Returns the descriptor of `bytestructure`.

- `(bytestructure-size bytestructure)` *procedure*

Returns the size of the structure contained within `bytestructure`.

- `(bytestructure descriptor)` *procedure*
- `(bytestructure descriptor initial-value)` *procedure*

Creates a bytestructure with a newly allocated bytevector of the right
size for the descriptor and an offset of 0, and optionally initializes
it with values.

I.e.

    (define bs (bytestructure descriptor))

is equivalent to

    (define bs (make-bytestructure
                (make-bytevector (bytestructure-descriptor-size
                                  descriptor))
                0
                descriptor))

The optional second argument will be passed to `bytestructure-set!` to
assign the given values to the bytestructure.

I.e.

    (define bs (bytestructure descriptor) values)

is equivalent to

    (let ((bs (bytestructure descriptor)))
      (bytestructure-set! bs values)
      bs)

Since the setter procedures of compound descriptors tend to delegate
the assignment of individual elements to their respective descriptors,
one can easily initialize structures to arbitrary depth.

    (define my-struct
      (bs:struct `((x ,uint16) (y ,(bs:vector 3 uint8)))))

    (define bs (bytestructure my-struct '((x 0) (y #(0 1 2)))))


### Referencing and assignment

- `(bytestructure-ref bytestructure index ...)` *syntax*

First references through `bytestructure` with the given zero or more
`index` values to arrive at a certain bytevector, byte-offset, and
bytestructure descriptor.  This is done by calling the `ref-helper` of
the descriptor of `bytestructure` on the bytevector and offset of
`bytestructure` and the first index, yielding an intermediate triple
of a bytevector, offset and descriptor; then calling the `ref-helper`
of that descriptor on that bytevector and offset and the second index,
and so on until the indices are exhausted.  Finally, calls the
`getter` of the finally arrived descriptor on the finally arrived
bytevector and offset.

If the descriptor to which the indices lead has no `getter`, then a
bytestructure consisting of the bytevector, offset, and descriptor at
which the indexing arrived is created and returned.

- `(bytestructure-set! bytestructure index ... value)` *syntax*

First references through `bytestructure` in the same way as
`bytestructure-ref` does.  Then, calls the `setter` of the finally
arrived descriptor on the finally arrived bytevector and offset, and
`value`.

If the descriptor to which the indices lead has no `setter`, then the
given value must be a bytevector.  As many bytes as the size of the
arrived descriptor will be copied into the arrived bytevector starting
from the arrived offset.

- `(bytestructure-ref* bytevector offset descriptor index ...)`
  *syntax*
- `(bytestructure-set!* bytevector offset descriptor index ... value)`
  *syntax*

These macros have the same semantics as `bytestructure-ref` and
`bytestructure-set!` respectively, except that they start the
referencing process with the given `bytevector`, `offset`, and
`descriptor`, instead of the bytevector, offset, and descriptor of a
given bytestructure.

- `(bytestructure-ref-helper bytestructure index ...)` *syntax*
- `(bytestructure-ref-helper* bytevector offset descriptor index ...)`
  *syntax*

These macros have the same semantics as `bytestructure-ref` and
`bytestructure-ref*` respectively, except that at the final step they
don't call the `getter` of the arrived descriptor, and instead return
the arrived bytevector, offset, and descriptor as three values.

Note that `bytestructure-ref-helper` can be used with zero indices to
destructure a bytestructure into its contents.

    (let-values (((bytevector offset descriptor)
                  (bytestructure-ref-helper bytestructure)))
      ...)

When a descriptor is not dynamic, `bytestructure-ref-helper*` may be
given a bogus `bytevector` argument.

    (bytestructure-ref-helper* #f 0 uint8-v3-v5 2)
    => #f, 6, uint8-v3 ;; Two uint8-v3s were skipped, so offset 6.

    (bytestructure-ref-helper* #f 0 uint8-v3-v5 2 1)
    => #f, 7, uint8 ;; Two uint8-v3s and one uint8 was skipped.


### Macro-based API

For when maximal efficiency is desired, a macro-based API is offered,
so that the bulk of the work involved in offset calculation can be
offloaded to the macro-expand phase.

- `(define-bytestructure-accessors descriptor ref-helper getter
  setter)` *syntax*

The `descriptor` expression is evaluated during the macro-expand phase
to yield a bytestructure descriptor.  The `ref-helper`, `getter`, and
`setter` identifiers are bound to a triple of macros implementing the
indexing, referencing, and assignment semantics of the given
descriptor.

    (define-bytestructure-accessors (bs:vector 5 (bs:vector 3 uint8))
      uint8-v3-v5-ref-helper uint8-v3-v5-getter uint8-v3-v5-setter)

    (uint8-v3-v5-ref-helper #f 0 3 2)  ;the #f is a bogus bytevector
                                       ;the 0 is the initial offset
    => 11 (3 * 3 + 2)

    (define bv (apply bytevector (iota 15)))
    (uint8-v3-v5-getter bv 2 1) => 7
    (uint8-v3-v5-setter bv 2 1 42)
    (uint8-v3-v5-getter bv 2 1) => 42

- `(bytestructure-ref-helper/syntax bytevector-syntax offset-syntax
  descriptor indices-syntax)` *procedure*

The semantics are akin to `bytestructure-ref-helper*`, except that
some arguments are syntax objects, and the return value is a syntax
object that would evaluate to two values: the bytevector and offset
that are the result of the indexing process.

- `(bytestructure-ref/syntax bytevector-syntax offset-syntax
  descriptor indices-syntax)` *procedure*

The semantics are akin to `bytestructure-ref*`, except that some
arguments are syntax objects, and the return value is a syntax object
that would evaluate to the decoded value.

- `(bytestructure-set!/syntax bytevector offset descriptor indices
  values)` *procedure*

The semantics are akin to `bytestructure-set!*`, except that some
arguments are syntax objects, and a syntax object is returned that
would perform the actual assignment when evaluated.


Performance
-----------

### Macro API

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


### Procedural API

When descriptors are statically apparent, an aggressively constant
propagating and partial evaluating optimizer might be able to turn
bytestructure references into direct bytevector references, yielding
identical results to the macro API.  That is the most optimal outcome,
but more realistic is that most of the work happens at run-time, which
is the case in Guile 2.0.

The offset calculation avoids allocation, which will make its speed
predictable.  It takes linear time with regard to the depth of a
structure.  For structs and unions, it's also linear with regard to
the position of the referenced field, but the constant factor involved
in that is so small that this should usually not be noticed unless you
have a very large number of struct or union fields.

If performance becomes an issue but you can't or don't want to switch
to the macro API, you can improve performance by hoisting as much work
to outside of your tight loops or other performance critical sections
of your code.  E.g. if you were doing `(bytestructure-ref bs x y z)`
within a loop, you can instead do

    (let-values (((bytevector offset descriptor)
                  (bytestructure-ref-helper bs x y z)))
      (loop
        (bytestructure-ref* bytevector offset descriptor)))

or if for instance the last index in that example, `z`, changes at
every iteration of the loop, you can do

    (let-values (((bytevector offset descriptor)
                  (bytestructure-ref-helper bs x y)))
      (loop (for z in blah)
        (bytestructure-ref* bytevector offset descriptor z)))

so at least you don't repeat the indexing of `x` and `y` at every
iteration.


Following are some benchmark figures from Guile.  (These are only
meant for a broad comparison against plain bytevector reference.)

Plain bytevector reference:

    > (define times (iota 1000000)) ;a million
    > (define bv (make-bytevector 1))
    > (define-inlinable (ref x) (bytevector-u8-ref bv 0))
    > ,time (for-each ref times)
    ;; 0.130245s real time

Equivalent bytestructure reference:

    > (define times (iota 1000000)) ;a million
    > (define bs (bytestructure (bs:vector 1 uint8)))
    > (define-inlinable (ref x) (bytestructure-ref bs 0))
    > ,time (for-each ref times)
    ;; 0.721888s real time

Showcasing the effect of a deeper structure:

    > (define times (iota 1000000)) ;a million
    > (define bs (bytestructure (bs:vector 1
                                   (bs:vector 1
                                     (bs:vector 1 uint8)))))
    > (define-inlinable (ref x) (bytestructure-ref bs 0 0 0))
    > ,time (for-each ref times)
    ;; 1.079202s real time
