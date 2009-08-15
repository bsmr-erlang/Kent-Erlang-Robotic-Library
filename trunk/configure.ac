#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.63])
AC_INIT([KERL], [0.4.0], [none])
AC_CONFIG_SRCDIR([kerlplayerlib/driver_kerl.cpp])
AC_CONFIG_HEADERS([config.h])

# Checks for programs.
AC_PROG_CXX
AC_PROG_CC
AC_PROG_CPP
AC_PROG_LN_S
AC_PROG_MAKE_SET

# Checks for libraries.
# FIXME: Replace `main' with a function in `-lei':
AC_CHECK_LIB([ei], [ei_encode_string])
# FIXME: Replace `main' with a function in `-lm':
AC_CHECK_LIB([m], [acos])
# FIXME: Replace `main' with a function in `-lplayerc':
AC_CHECK_LIB([playerc], [playerc_client_create])
# FIXME: Replace `main' with a function in `-lplayererror':
AC_CHECK_LIB([playererror], [playerc_error_str])
# FIXME: Replace `main' with a function in `-lplayerxdr':
AC_CHECK_LIB([playerxdr], [player_foo_pack])

# Checks for header files.
AC_CHECK_HEADERS([stdlib.h string.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL

# Checks for library functions.
AC_FUNC_MALLOC

AM_INIT_AUTOMAKE([1.9 tar-ustar dist-bzip2])


AC_CONFIG_FILES([Makefile
                 doc/Makefile
                 doc/cdoc/Makefile
                 doc/edoc/Makefile
                 examples/Makefile
                 examples/traffic/Makefile
                 examples/wallfollow/Makefile
                 kerlplayerlib/Makefile
                 module/Makefile])
AC_OUTPUT
