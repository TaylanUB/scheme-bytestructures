AC_INIT([bytestructures], [2.0.2], [], [], [https://github.com/TaylanUB/scheme-bytestructures/])
AC_CONFIG_SRCDIR(bytestructures)
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([-Wall -Werror foreign color-tests])

dnl Enable silent rules by default.
AM_SILENT_RULES([yes])

GUILE_PKG([3.0 2.2 2.0])
GUILE_PROGS

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])

if test "$cross_compiling" != no; then
   GUILE_TARGET="--target=$host_alias"
   AC_SUBST([GUILE_TARGET])
fi

AC_OUTPUT
