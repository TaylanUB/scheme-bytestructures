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
      my-struct-unwrap my-struct-ref my-struct-set!)

    (define foo (make-bytevector ...))

    ;; foo.y[2]
    (my-struct-ref foo y 2)

    ;; foo.y[2] = 42;
    (my-struct-set! foo y 2 42)

(Note that we don't use the bytestructure data type anymore; we work
directly on bytevectors.  The struct fields are also implicitly quoted
and can't be variable references, since their look-up will happen at
compile time.  The unwrapper will be explained later.)

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


Specification
-------------

A *bytestructure descriptor*, also called simply a descriptor within
this specification, is an object encapsulating information about the
layout and meanings of the bytes in a bytevector object.

A *bytestructure* is an object bundling a bytevector with a
bytestructure descriptor so that values can be extracted from that
bytevector conveniently, using the information in the descriptor.

A *dynamic descriptor* is a bytestructure descriptor whose `size`
and/or `unwrapper` procedures reference their `bytevector` and/or
`offset` arguments.  (See below.)

The argument name `descriptor` signifies that an argument must be a
bytestructure descriptor, `bytestructure` signifies that it must be a
bytestructure, and `offset` signifies that it must be an exact
non-negative integer.

Knowledge of the C programming language is recommended for a proper
understanding of this specification.  Specifically, example code is
often annotated with conceptually equivalent C code.


### High-level API

A set of predefined bytestructure descriptors, as well as procedures
for creating compound descriptors of certain kinds, are provided to
the user, mostly obviating the need to work with the bytestructure
descriptor API directly, which is explained further below.


#### Constructors for compound descriptors

##### bs:vector

- `(bs:vector length descriptor)` *procedure*

Returns a descriptor for vectors, also called a *vector descriptor*,
of length `length` and the *element descriptor* `descriptor`.  This
corresponds to an *array type* in the C programming language.

    ;; uint16_t vec[3] = { 0, 1, 2 };
    (define vec (bytestructure (bs:vector 3 uint16) #(0 1 2)))

    ;; vec[1]
    (bytestructure-ref vec 1)

    ;; vec[1] = 42;
    (bytestructure-set! vec 1 42)

The elements are indexed with exact non-negative integers, and no
bounds checking is done; an off-bounds index will either raise an
error due to an off-bounds bytevector index, or attempt to decode
whatever bytes are found at the relevant place in the bytevector,
which might result in a valid value without raising an error.

Vector descriptors are normally meant for indexing through, but also
allow direct assignment.  The value provided for assignment must be a
regular Scheme vector of the same length as the vector descriptor.
Each element of that vector is assigned to the corresponding element
of the vector bytestructure, using the assignment semantics of the
element descriptor.

    ;; (Reusing 'vec' from the previous example.)

    ;; Uses bytevector-u16-set! three times.
    (bytestructure-set! vec #(21 42 84))

One may also provide a bytevector, in which case as many bytes as
the size of the bytestructure will be copied into it.

    ;; The results of this depend on endianness.
    ;; Only the first 6 bytes from the bytevector will be copied.
    (bytestructure-set! vec #u8(0 1 2 3 4 5 6 7 8))

These assignment semantics may not be used with the macro API.

Vector descriptors don't accept dynamic descriptors as their element
descriptor, because they calculate their total size eagerly and thus
need to know the size of their element descriptor independently from
the bytevector on which they will be used.

##### bs:struct

- `(bs:struct fields)` *procedure*
- `(bs:struct pack fields)` *procedure*

Returns a descriptor for structs, also called a *struct descriptor*.
`Fields` must be a list of *field specs* (see below).  `Pack` may be
`#f`, `#t`, or an exact positive integer.  If `pack` is omitted or
`#f`, the struct alignment of the platform's C ABI is used.  If `pack`
is `#t`, there are no padding fields (except for those resulting from
bit-fields).  If `pack` is an integer, it specifies the maximum
alignment value for the fields, similar to the `#pack` directive of
the GCC C compiler.

A *field spec* is a list of two or three elements.  The first element
must be a symbol which names the field (or `#f`, see below).  Every
field must have a distinct name (except if `#f`).  The second element
must be a bytestructure descriptor which becomes the descriptor of the
field.  The third element, if present, must be an exact non-negative
integer; it signifies that the field is a bit-field of that width.
The descriptor of a bit-field must be one that decodes values to exact
integers, such as for instance `uint8` or `int32`.

The width of a bit-field may be zero, which means padding should be
inserted in its place until the next alignment boundary of the
descriptor of that bit-field is reached.  A zero-width bit-field must
have `#f` as its name.

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

    ;; Assuming a 32-bit platform:

    ;; struct { unsigned int a:16; unsigned int b:16; }
    (bs:struct `((a ,uint32 16) (b ,uint32 16)))

    ;; struct { unsigned int a:16; int :0; signed int b:20; }
    (bs:struct `((a ,uint32 16) (#f ,int32 0) (b ,int32 20)))

Struct descriptors are normally meant for indexing through, but also
allow direct assignment.  The value provided for assignment may be a
Scheme vector as long as there are fields in the struct descriptor,
which will assign all fields sequentially; or a list of two-element
lists, which will assign any number of fields by name.

    ;; (Reusing 'str' from the previous example.)

    ;; str = (my_struct_t){ 0, 1 };
    (bytestructure-set! str #(0 1))

    ;; str = (my_struct_t){ .y = 2, .x = 1 };
    (bytestructure-set! str '((y 2) (x 1)))

One may also provide a bytevector, in which case as many bytes as
the size of the bytestructure will be copied into it.

    ;; The field 'x' is set to 0; the value of the field 'y' will
    ;; depend on endianness.
    ;; Only the first 3 bytes from the bytevector will be copied.
    (bytestructure-set! str #u8(0 1 2 3 4 5))

These assignment semantics may not be used with the macro API.

Struct descriptors don't accept dynamic descriptors as field
descriptors, because they calculate their total size eagerly.

When using the macro API, the field names are implicitly quoted and
looked up at macro-expand time.

    (define-bytestructure-accessors my-struct
      my-struct-unwrap my-struct-ref my-struct-set!)

    ;; foo.y
    (my-struct-ref foo-bytevector y)

    ;; foo.y = 42;
    (my-struct-set! foo-bytevector y 42)

##### bs:union

- `(bs:union fields)` *procedure*

Returns a descriptor for unions, also called a *union descriptor*.
`Fields` has the same format as in `bs:struct`.

    ;; typedef union { uint8_t x; uint16_t y; } my_union_t;
    (define my-union (bs:union `((x ,uint8) (y ,uint16))))

    ;; my_union_t union = { .y = 42 };
    (define union (bytestructure my-union '(y 42)))

    ;; union.y
    (bytestructure-ref union 'y)

    ;; union.y = 42;
    (bytestructure-set! union 'y 42)

Union descriptors are normally meant for indexing through, but also
allow direct assignment.  The value provided for assignment must be a
two-element list, whose first element names the field whose descriptor
should be used for the assignment, and the second element provides the
value to be actually assigned.

    ;; union.y = 42;
    (bytestructure-set! union '(y 42))

*Rationale:* This syntax isn't shorter than the normal way of
assigning a value into the union, but is supported for reasons that
should become apparent after reading the specification of the
`bytestructure` constructor procedure.

One may also provide a bytevector, in which case as many bytes as
the size of the bytestructure will be copied into it.

    ;; The value of the y field will depend on endianness.
    ;; Only the first 2 bytes from the bytevector will be copied.
    (bytestructure-set! union #u8(0 1 2 3 4))

These assignment semantics may not be used with the macro API.

Union descriptors don't accept dynamic descriptors as field
descriptors, because they calculate their total size eagerly.

##### bs:pointer

- `(bs:pointer descriptor)` *procedure*

Returns a descriptor for pointers, also called a *pointer descriptor*,
with the *content descriptor* `descriptor`.  Such a descriptor
indicates that the bytes in a given bytevector are to be interpreted
as a memory address.  The content descriptor is the descriptor for the
bytes found at that memory address.

    ;; uint8_t *ptr = 0xdeadbeef;
    (define ptr (bytestructure (bs:pointer uint8) #xdeadbeef))

As a special case, the `descriptor` argument may be a promise, which
must evaluate to a descriptor when forced.  This is to allow creating
self-referencing descriptors:

    ;; typedef struct linked_uint8_list_s {
    ;;   uint8_t head;
    ;;   struct linked_uint8_list_s *tail;
    ;; } *linked_uint8_list_t;
    (define linked-uint8-list
      (bs:pointer (delay (bs:struct `((head ,uint8)
                                      (tail ,linked-uint8-list))))))

The symbol `*` can be used as an index to dereference the pointer.
(It's implicitly quoted when used in the macro API.)  An array of
bytes as large as the size of the content descriptor, starting from
the memory address of the pointer, are reified into a bytevector
object, and bundled with the content descriptor, to yield a new
bytestructure object.

    ;; linked_uint8_list_t u8list;
    (define u8list (bytestructure linked-uint8-list))

    ;; (*u8list).head
    (bytestructure-ref u8list '* 'head)

    ;; (*u8list).head = 42;
    (bytestructure-set! u8list '* 'head 42)

One may however also provide any other index, which will cause an
implicit dereference.

    ;; u8list->head
    (bytestructure-ref u8list 'head)

    ;; u8list->head = 42;
    (bytestructure-set! u8list 'head 42)

Since pointers are also values themselves, pointer descriptors also
have direct referencing and assignment semantics.  Referencing the
pointer yields the numeric value of the address.

    ;; linked_uint8_list_t u8lists[3];
    (define u8lists (bytestructure (bs:vector 3 linked-uint8-list)))

    ;; Returns the address stored in u8lists[1].
    (bytestructure-ref u8lists 1)

Assignment with a pointer descriptor allows a variety of values.
Firstly, a numeric value (taken to be a memory address) may be given,
which causes that value itself to be written.

    ;; uint8_t (*u8v3-ptr)[3];
    (define u8v3-ptr (bytestructure (bs:pointer (bs:vector 3 uint8))))

    ;; u8v3-ptr = 0xdeadbeef;
    (bytestructure-set! u8v3-ptr #xdeadbeef)

A bytevector may be given, in which case the memory address of the
first byte of the bytevector is written.

    ;; Makes the pointer point to 'a-bytevector'.
    (bytestructure-set! u8v3-ptr a-bytevector)

Lastly, providing a bytestructure is equivalent to providing the
bytevector of that bytestructure.

    ;; Makes the pointer point to the bytevector of 'a-bytestructure'.
    (bytestructure-set! u8v3-ptr a-bytestructure)

These assignment semantics may be used with the macro API as well.

Pointers don't accept dynamic descriptors as their content descriptor.

*Rationale:* The bytevector that is pointed to is reified "on the fly"
during referencing operations, for which its size needs to be known in
advance.  Needing the bytevector to already exist for calculating its
size (as is the case for dynamic descriptors) imposes a problem of
circularity.

*Note:* Having an address written into a bytevector may not protect it
from garbage collection.  Thus using pointer descriptors might make a
Scheme program memory unsafe even if the Scheme implementation is
otherwise memory safe.


#### Numeric descriptors

The following descriptors for numeric types are provided:
`[u]int(8,16,32,64)[le,be]`, `float(32,64)[le,be]`,
`complex(64,128)[le,be]`

On platforms with little-endian byte order, the descriptors whose name
ends in `le` are equivalent as per `eqv?` to their variant without an
explicit endianness marker.  The same applies for the big-endian
descriptors on big-endian platforms.

The following are each equivalent as per `eqv?` to one of the above
listed descriptors, depending on the platform on which the Scheme
program is run: `[unsigned-](short,int,long,long-long)`, `size_t`,
`ssize_t`, `ptrdiff_t`, `float`, `double`

These descriptors cannot be indexed through as for instance vectors
and structs can; they can only be used to directly reference or assign
values.

    ;; uint32_t x;
    (define x (bytestructure uint32))

    ;; x = 42;
    (bytestructure-set! x 42)

    ;; uint32_t xs[3];
    (define xs (bytestructure (bs:vector 3 uint32)))

    ;; xs[1] = 42;
    (bytestructure-set! xs 1 42)


#### The bytestructure data type

- `(make-bytestructure bytevector offset descriptor)` *procedure*

Returns a bytestructure object with the given bytevector, offset into
the bytevector, and bytestructure descriptor.

*Rationale:* Any bytestructure descriptor can be used with any
bytevector to work on it momentarily in accordance with the
descriptor, but in most cases a bytevector is dedicated to a certain
structure, so it makes sense to bundle a descriptor with the
bytevector.  Or only a portion of the bytevector, starting from a
certain offset, might be dedicated to the structure, so being able to
bundle that offset is also useful.

- `(bytestructure? obj)` *procedure*

Returns a Boolean indicating whether `obj` is a bytestructure.

- `(bytestructure-bytevector bytestructure)` *procedure*
- `(bytestructure-offset bytestructure)` *procedure*
- `(bytestructure-descriptor bytestructure)` *procedure*

These procedures return the `bytevector`, `offset`, and `descriptor`
values respectively, with which `bytestructure` was created.

- `(bytestructure-size bytestructure)` *procedure*

Returns the size of the structure contained within `bytestructure`.

- `(bytestructure descriptor)` *procedure*
- `(bytestructure descriptor initial-value)` *procedure*

Creates a bytestructure with a newly allocated bytevector of the right
size for `descriptor` and an offset of 0, and optionally initializes
it with values.

The following two expressions are equivalent:

    (define bs (bytestructure descriptor))

    (define bs (make-bytestructure
                (make-bytevector (bytestructure-descriptor-size
                                  descriptor))
                0
                descriptor))

The optional second argument is passed to `bytestructure-set!` to
assign the given values to the bytestructure after creation, meaning
the following two expressions are equivalent:

    (define bs (bytestructure descriptor) values)

    (let ((bs (bytestructure descriptor)))
      (bytestructure-set! bs values)
      bs)

Since the setter procedures of compound descriptors tend to delegate
the assignment of individual elements to their respective descriptors,
one can easily initialize structures to arbitrary depth.

    (define my-struct
      (bs:struct `((x ,uint16) (y ,(bs:vector 3 uint8)))))

    (define bs (bytestructure my-struct '((x 0) (y #(0 1 2)))))


#### Referencing and assignment

- `(bytestructure-ref bytestructure index ...)` *syntax*

Traverses through `bytestructure` using `bytestructure-unwrap` with
the given indices to acquire a triple of a bytevector, offset, and
descriptor.  Then, applies the `getter` of that descriptor to the
bytevector and offset.  Or if the getter if `#f`, then a bytestructure
encapsulating that bytevector, offset, and descriptor is returned.

- `(bytestructure-set! bytestructure index ... value)` *syntax*

Traverses through `bytestructure` using `bytestructure-unwrap` with
the given indices to acquire a triple of a bytevector, offset, and
descriptor.  Then, applies the `setter` of that descriptor to the
bytevector, offset, and `value`.  Or if the setter if `#f`, then
`value` must be a bytevector; as many bytes as the size of the
descriptor are copied from it into the bytevector, starting from the
offset.

- `(bytestructure-ref* bytevector offset descriptor index ...)`
  *syntax*
- `(bytestructure-set!* bytevector offset descriptor index ... value)`
  *syntax*

These macros have the same semantics as `bytestructure-ref` and
`bytestructure-set!` respectively, except that they start the
referencing process with the given `bytevector`, `offset`, and
`descriptor`, instead of the bytevector, offset, and descriptor of a
given bytestructure.

- `(bytestructure-unwrap bytestructure index ...)` *syntax*

This macro executes the following algorithm:

1. Extract the bytevector, offset, and descriptor of `bytestructure`.
   Let us call the triple of these values the *working set*.

2. If no indices are left, return the working as three values.

3. Apply the `unwrapper` procedure of the descriptor to the
   bytevector, the offset, and the first index.  The return values
   replace the working set.  Pop the index from the list of indices.

4. Go to step 2.

*Note:* `bytestructure-unwrap` can be used with zero indices to
destructure a bytestructure into its contents.

    (let-values (((bytevector offset descriptor)
                  (bytestructure-unwrap bytestructure)))
      ...)

- `(bytestructure-unwrap* bytevector offset descriptor index ...)`
  *syntax*

This macro has the same semantics as `bytestructure-unwrap`, except
that it starts the traversal process with the given `bytevector`,
`offset`, and `descriptor`, instead of the bytevector, offset, and
descriptor of a given bytestructure.

When a descriptor is not a dynamic descriptor, `bytestructure-unwrap*`
may be given a bogus `bytevector` argument.

    (bytestructure-unwrap* #f 0 uint8-v3-v5 2)
    => #f, 6, uint8-v3 ;; Two uint8-v3s were skipped, so offset 6.

    (bytestructure-unwrap* #f 0 uint8-v3-v5 2 1)
    => #f, 7, uint8 ;; Two uint8-v3s and one uint8 was skipped.

- `(bytestructure-ref/dynamic bytestructure index ...)` *procedure*
- `(bytestructure-set!/dynamic bytestructure index ... value)`
  *procedure*

These procedures are equivalent to the macros `bytestructure-ref` and
`bytestructure-set!` respectively.

*Rationale:* Since these procedures take a variable number of
arguments, they have to allocate rest-arguments lists, which might be
undesirable in the general case.


#### Macro-based API

For when maximal efficiency is desired, a macro-based API is offered,
so that the bulk of the work involved in offset calculation can be
offloaded to the macro-expand phase.

- `(define-bytestructure-accessors descriptor unwrapper getter
  setter)` *syntax*

The `descriptor` expression is evaluated during the macro-expand phase
to yield a bytestructure descriptor.  The `unwrapper`, `getter`, and
`setter` identifiers are bound to a triple of macros implementing the
indexing, referencing, and assignment semantics of the given
descriptor.

    (define-bytestructure-accessors (bs:vector 5 (bs:vector 3 uint8))
      uint8-v3-v5-unwrap uint8-v3-v5-ref uint8-v3-v5-set!)

    (uint8-v3-v5-unwrap #f 0 3 2)  ;the #f is a bogus bytevector
                                   ;the 0 is the initial offset
    => 11 (3 * 3 + 2)

    (define bv (apply bytevector (iota 15)))
    (uint8-v3-v5-ref bv 2 1) => 7
    (uint8-v3-v5-set! bv 2 1 42)
    (uint8-v3-v5-ref bv 2 1) => 42

- `(bytestructure-unwrap/syntax bytevector-syntax offset-syntax
  descriptor indices-syntax)` *procedure*

The semantics are akin to `bytestructure-unwrap*`, except that some
arguments are syntax objects, and the return value is a syntax object
that would evaluate to two values: the bytevector and offset that are
the result of the indexing process.

- `(bytestructure-ref/syntax bytevector-syntax offset-syntax
  descriptor indices-syntax)` *procedure*

The semantics are akin to `bytestructure-ref*`, except that some
arguments are syntax objects, and the return value is a syntax object
that would evaluate to the decoded value.

- `(bytestructure-set!/syntax bytevector offset descriptor indices
  value)` *procedure*

The semantics are akin to `bytestructure-set!*`, except that some
arguments are syntax objects, and a syntax object is returned that
would perform the actual assignment when evaluated.


### The bytestructure descriptors API

- `(make-bytestructure-descriptor size alignment unwrapper getter
  setter)` *procedure*

`Size` must be an exact non-negative integer, or a procedure taking
three arguments and returning an exact non-negative integer (this is
for dynamic descriptors).  The first argument to the procedure is a
Boolean indicating whether the call to the procedure is happening in
the macro-expand phase.  If it's false, the other two arguments are a
bytevector and an offset into the bytevector respectively.  If it's
true, then the two arguments are instead syntax objects that would
evaluate to a bytevector and an offset respectively.  The offset is
the position in the bytevector at which the bytes belonging to the
descriptor start.  The procedure should return the size of the
structure described by the descriptor, or return a syntax object that
would evaluate to the size.

`Alignment` must be an exact positive integer specifying the type's
preferred memory alignment.

`Unwrapper` must be `#f` or a procedure taking four arguments: a
Boolean indicating whether the call to the procedure is happening in
the macro-expand phase, a bytevector (or syntax object thereof), an
offset (or syntax object thereof), and an index object (or syntax
object thereof).  The procedure must return three values: the same or
another bytevector (or syntax object thereof), a new offset (or syntax
object thereof), and a bytestructure descriptor (NOT a syntax object
thereof).  This procedure implements the indexing semantics of
compound types.  The bytevector argument is provided to satisfy
dynamic descriptors; the `unwrapper` of non-dynamic descriptors should
ignore its value and return it back untouched.

`Getter` must be `#f` or a procedure taking three arguments: a Boolean
indicating whether the call to the procedure is happening in the
macro-expand phase, a bytevector (or syntax object thereof), and an
offset (or syntax object thereof).  The procedure should decode the
bytes at the given offset in the given bytevector (or return a syntax
object whose evaluation would do this), thus implementing the
referencing semantics of the descriptor.

`Setter` must be `#f` or a procedure taking four arguments: a Boolean
indicating whether the call to the procedure is happening in the
macro-expand phase, a bytevector (or syntax object thereof), an offset
(or syntax object thereof), and a value (or syntax object thereof).
The procedure should encode the given value into given offset in the
given bytevector (or return a syntax object whose evaluation would do
this), thus implementing the assignment semantics of the descriptor.

- `(bytestructure-descriptor-size descriptor)` *procedure*
- `(bytestructure-descriptor-size descriptor bytevector offset)`
  *procedure*

Returns the size of `descriptor`.  If `descriptor` is a dynamic
descriptor, then the `bytevector` and `offset` arguments must be
provided, which will be passed to the `size` procedure of
`descriptor`, with the macro-expand Boolean argument set to false.

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
`descriptor`.  If `descriptor` is a dynamic descriptor, then the
`bytevector-syntax` and `offset-syntax` arguments must be provided,
which will be passed to the `size` procedure of `descriptor`, with the
macro-expand Boolean argument set to true.

- `(bytestructure-descriptor-alignment descriptor)` *procedure*
- `(bytestructure-descriptor-unwrapper descriptor)` *procedure*
- `(bytestructure-descriptor-getter descriptor)` *procedure*
- `(bytestructure-descriptor-setter descriptor)` *procedure*

These procedures return the `alignment`, `unwrapper`, `getter`, and
`setter` values respectively, with which `descriptor` was created.


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
        bs-unwrap bs-ref bs-set!)
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
                  (bytestructure-unwrap bs x y z)))
      (loop
        (bytestructure-ref* bytevector offset descriptor)))

or if for instance the last index in that example, `z`, changes at
every iteration of the loop, you can do

    (let-values (((bytevector offset descriptor)
                  (bytestructure-unwrap bs x y)))
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
