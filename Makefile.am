include guile.am

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

SOURCES =						\
	bytestructures/guile.scm			\
	bytestructures/guile/base.scm			\
	bytestructures/guile/bitfields.scm		\
	bytestructures/guile/bytevectors.scm		\
	bytestructures/guile/cstring-pointer.scm	\
	bytestructures/guile/explicit-endianness.scm	\
	bytestructures/guile/ffi.scm			\
	bytestructures/guile/numeric-all.scm		\
	bytestructures/guile/numeric-data-model.scm	\
	bytestructures/guile/numeric-metadata.scm	\
	bytestructures/guile/numeric.scm		\
	bytestructures/guile/pointer.scm		\
	bytestructures/guile/string.scm			\
	bytestructures/guile/struct.scm			\
	bytestructures/guile/union.scm			\
	bytestructures/guile/utils.scm			\
	bytestructures/guile/vector.scm

EXTRA_DIST +=						\
	README.md					\
	pre-inst-env.in
