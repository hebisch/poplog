/* --- Copyright University of Sussex 1998. All rights reserved. ----------
 > File:            C.unix/lib/include/unix_errno.ph
 > Purpose:         (Selected) Unix error numbers
 > Author:          John Gibson Jun 14 1994 (see revisions)
 > Documentation:
 */

#_TERMIN_IF DEF UNIX_ERRNO_INCLUDED

section;

include sysdefs.ph;

iconstant macro (
    EPERM           = 1,
    ENOENT          = 2,
    ESRCH           = 3,
    EINTR           = 4,
    EIO         = 5,
    ENXIO           = 6,
    E2BIG           = 7,
    ENOEXEC         = 8,
    EBADF           = 9,
    ECHILD          = 10,
    EAGAIN          = #_IF DEF ALPHA_LINUX 35 #_ELSE 11 #_ENDIF,
    ENOMEM          = 12,
    EACCES          = 13,
    EEXIST          = 17,
    EXDEV           = 18,
    ENODEV          = 19,
    ENOTDIR         = 20,
    EISDIR          = 21,
    EINVAL          = 22,
    ENFILE          = 23,
    EMFILE          = 24,
    EFBIG           = 27,
    ENOSPC          = 28,
    EPIPE           = 32,
    EDOM            = 33,
    ERANGE          = 34,
    );


#_IF DEFV SYSTEM_V >= 4.0

iconstant macro (
    EWOULDBLOCK     = EAGAIN,
    ENOSR           = 63,
    ELOOP           = 90,
    EDQUOT          = -1,       ;;; not defined
    ENOTSOCK        = 95,
    EADDRINUSE      = 125,
    EISCONN         = 133,
    ENOTCONN        = 134,
    ECONNREFUSED    = 146,
    EALREADY        = 149,
    EINPROGRESS     = 150,
    );

#_ELSEIF DEF LINUX and not(DEF ALPHA)

iconstant macro (
    EWOULDBLOCK     = EAGAIN,
    ENOSR           = 63,
    ELOOP           = 40,
    EDQUOT          = 122,
    ENOTSOCK        = 88,
    EADDRINUSE      = 98,
    EISCONN         = 106,
    ENOTCONN        = 107,
    ECONNREFUSED    = 111,
    EALREADY        = 114,
    EINPROGRESS     = 115,
    );

#_ELSEIF DEF HPUX

iconstant macro (
    EWOULDBLOCK     = 246,
    ENOSR           = 53,
    ELOOP           = 249,
    EDQUOT          = -1,       ;;; not defined
    ENOTSOCK        = 216,
    EADDRINUSE      = 226,
    EISCONN         = 234,
    ENOTCONN        = 235,
    ECONNREFUSED    = 239,
    EALREADY        = 244,
    EINPROGRESS     = 245,
    );

#_ELSEIF DEF AIX

iconstant macro (
    EWOULDBLOCK     = EAGAIN,
    ENOSR           = 118,
    ELOOP           = 85,
    EDQUOT          = 88,       ;;; not defined
    ENOTSOCK        = 57,
    EADDRINUSE      = 67,
    EISCONN         = 75,
    ENOTCONN        = 76,
    ECONNREFUSED    = 79,
    EALREADY        = 56,
    EINPROGRESS     = 55,
    );

#_ELSE

iconstant macro (
    EWOULDBLOCK     = 35,
    ENOSR           = #_IF DEF OSF1 or DEF LINUX 82
                      #_ELSEIF not(DEF ULTRIX) 74
                      #_ELSE -1     ;;; not defined
                      #_ENDIF,
    ELOOP           = 62,
    EDQUOT          = 69,
    ENOTSOCK        = 38,
    EADDRINUSE      = 48,
    EISCONN         = 56,
    ENOTCONN        = 57,
    ECONNREFUSED    = 61,
    EALREADY        = 37,
    EINPROGRESS     = 36,
    );

#_ENDIF

iconstant UNIX_ERRNO_INCLUDED = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 30 1998
        Added AIX stuff
--- John Gibson, Feb 19 1997
        Added correct value for OSF ENOSR; also changed incorrect lconstants
        to iconstants.
--- John Gibson, Nov 24 1995
        Added EADDRINUSE as well
--- John Gibson, Nov 20 1995
        Added EISCONN, EALREADY, EINPROGRESS
--- Poplog System, Jan 18 1995 (Julian Clinton)
        Added Linux errors.
 */
