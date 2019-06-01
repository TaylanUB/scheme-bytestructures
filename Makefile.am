include guile.am

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

SOURCES =								\
	bytestructures/guile.scm					\
	bytestructures/guile/base.scm					\
	bytestructures/guile/bitfields.scm				\
	bytestructures/guile/bytevectors.scm				\
	bytestructures/guile/cstring-pointer.scm			\
	bytestructures/guile/explicit-endianness.scm			\
	bytestructures/guile/ffi.scm					\
	bytestructures/guile/numeric-all.scm				\
	bytestructures/guile/numeric-data-model.scm			\
	bytestructures/guile/numeric-metadata.scm			\
	bytestructures/guile/numeric.scm				\
	bytestructures/guile/pointer.scm				\
	bytestructures/guile/string.scm					\
	bytestructures/guile/struct.scm					\
	bytestructures/guile/union.scm					\
	bytestructures/guile/utils.scm					\
	bytestructures/guile/vector.scm					\
	bytestructures/r6/bytevectors.scm

NOCOMP_SOURCES =							\
	bytestructures/body/align.scm					\
	bytestructures/body/base.scm					\
	bytestructures/body/base.syntactic.scm				\
	bytestructures/body/bitfields.scm				\
	bytestructures/body/explicit-endianness.scm			\
	bytestructures/body/numeric.scm					\
	bytestructures/body/string.scm					\
	bytestructures/body/struct.scm					\
	bytestructures/body/union.scm					\
	bytestructures/body/utils.scm					\
	bytestructures/body/vector.scm					\
	bytestructures/r7/base.exports.sld				\
	bytestructures/r7/bitfields.exports.sld				\
	bytestructures/r7/explicit-endianness.exports.sld		\
	bytestructures/r7/numeric.exports.sld				\
	bytestructures/r7/numeric-metadata.exports.sld			\
	bytestructures/r7/string.exports.sld				\
	bytestructures/r7/struct.exports.sld				\
	bytestructures/r7/union.exports.sld				\
	bytestructures/r7/vector.exports.sld

EXTRA_DIST +=								\
	COPYING								\
	README.md							\
	pre-inst-env.in							\
	run-tests.body.scm						\
	run-tests.guile.scm						\
	run-tests.r7.scm

check:
	$(top_builddir)/pre-inst-env		\
	  $(GUILE) --no-auto-compile		\
	  $(top_srcdir)/run-tests.guile.scm

.PHONY: check
