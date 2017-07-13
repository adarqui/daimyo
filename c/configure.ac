AC_PREREQ(2.59)
AC_INIT([daimyo], [0.1])

AM_INIT_AUTOMAKE([-Wall -Werror])

AC_PROG_CC
AC_ENABLE_SHARED
AC_DISABLE_STATIC
# AC_PROG_LIBTOOL(libtool)
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile lib/Makefile include/Makefile bin/Makefile])
m4_ifdef([AM_PROG_AR], [AM_PROG_AR])
LT_INIT

dnl check required headers
AC_CHECK_HEADER(stdlib.h, , [echo "require stdlib.h"; exit 1])

AC_OUTPUT
