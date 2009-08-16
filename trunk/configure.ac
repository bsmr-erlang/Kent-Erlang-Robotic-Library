#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.63])

AC_INIT(FULLPACKAGENAME, VERSION, BUGREPORTADDRESS)
AC_CONFIG_SRCDIR([kerlplayerlib/driver_kerl.cpp])
AC_CONFIG_HEADERS([config.h])

# Checks for programs.
AC_PROG_CXX
AC_PROG_CC
AC_PROG_CPP
AC_PROG_LN_S
AC_PROG_MAKE_SET

# Checks for libraries.

AC_CHECK_LIB([ei], [ei_encode_string])
AC_CHECK_LIB([m], [acos])
AC_CHECK_LIB([playerc], [playerc_client_create])
AC_CHECK_LIB([playererror], [playerc_error_str])
AC_CHECK_LIB([playerxdr], [player_foo_pack])



# Checks for header files.
AC_CHECK_HEADERS([stdlib.h string.h unistd.h])
AC_CHECK_HEADERS([erl_driver.h ei.h ])
AC_CHECK_HEADERS([libplayerc/playerc.h ], /usr/local/share/player)

# Can I has Erlang
AC_ERLANG_NEED_ERLC()
#AC_ERLANG_SUBST_ROOT_DIR
#AC_ERLANG_SUBST_LIB_DIR

AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR(kerl, 0.4.0)
#AC_ERLANG_INSTALL_LIB_DIR


# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL

# Checks for library functions.
AC_FUNC_MALLOC



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
