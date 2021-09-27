AC_PREREQ([2.69])
AC_INIT([debug_adapter], [0.1.8], [eshelshay.yaron@gmail.com])
AC_PATH_PROG([PROLOG], [swipl])
AC_PROG_MKDIR_P
AM_INIT_AUTOMAKE([-Wall foreign subdir-objects dist-zip])
AC_CONFIG_SRCDIR([prolog/debug_adapter/main.pl])

AC_CONFIG_FILES([
 pack.pl
 Makefile
 prolog/Makefile
 test/Makefile
 test/server.plt
])

AC_OUTPUT
