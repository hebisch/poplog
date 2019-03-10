/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.unix/src/sysdate.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *TIMES
 */

;;;-------------------- DATE PROCEDURES ----------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'unixdefs.ph'

global constant
        procedure (sys_real_time, Sys$-Check_integral)
    ;

;;; ---------------------------------------------------------------------

section $-Sys => sys_convert_date, sysdaytime;

#_IF DEFV SUNOS < 5.0 or DEF OSF1 or DEFV ULTRIX >= 4.0
lconstant macro USE_TM_ZONE = true;
#_ELSEIF DEF SYSTEM_V or DEF HPUX or DEF LINUX or DEF AIX
#_IF DEF LINUX
lconstant macro USE_TM_ZONE = true;
#_ENDIF
lconstant macro TZNAME = [tzname:data];     ;;; Timezone name for System V systems
#_ELSEIF DEF IRIX
lconstant macro TZNAME = [_tzname:data];    ;;; Timezone name for System V systems
#_ENDIF

    ;;; Entry returned by _extern localtime and gmtime
    ;;; -- see man ctime(3)
struct TM
  { int     TM_SEC,
            TM_MIN,
            TM_HOUR,
            TM_MDAY,
            TM_MON,
            TM_YEAR,
            TM_WDAY,
            TM_YDAY,
            TM_ISDST;
#_IF DEFV SUNOS < 5.0
    (byte)  TM_ZONE;
    long    TM_GMTOFF;
#_ELSEIF DEF USE_TM_ZONE
    long    TM_GMTOFF;
    <byte>  TM_ZONE;        ;;; <type> means pointer field of C size
#_ENDIF
  };

    /*  Convert a time in seconds since midnight 1 Jan 1970 to a string
    */
define sys_convert_date(seconds, local_time);
    lvars seconds, local_time, _tzlen, _tzone, _date_addr, _timep;
    lstackmem time_t _secs, byte _buf[128];

;;;        _extern printf('before Check_integral') -> _;
;;;        _extern fflush(_0) -> _;
    Check_integral(seconds);
#_IF EXCEEDS_POPINT_SIGNED(time_t)
    Pint_->_sint
#_ELSE
    Simpint_->_sint
#_ENDIF (seconds, _MOST_POSITIVE_SIGNED(time_t)) -> _secs!(time_t);

    if local_time then
;;;        _extern printf('before localtime') -> _;
;;;        _extern fflush(_0) -> _;
        _extern localtime(_secs)
    else
;;;        _extern printf('before gmtime') -> _;
;;;        _extern fflush(_0) -> _;
        _extern gmtime(_secs)
    endif -> _timep;

;;;        _extern printf('before asctime') -> _;
;;;        _extern fflush(_0) -> _;

    _extern asctime(_timep) -> _date_addr;

    ;;; get timezone name
#_IF DEF USE_TM_ZONE
    _timep!TM_ZONE -> _tzone;

#_ELSEIF DEF TZNAME
    ;;; get it from tzname -- 1st word is timezone, 2nd is dst timezone
    if _zero(_timep!TM_ISDST) then
;;;     _extern printf('before TZNAME!((b))[_0]') -> _;
;;;        _extern fflush(_0) -> _;

        _extern TZNAME!((b))[_0]
    else
;;;     _extern printf('before TZNAME!((b))[_1]') -> _;
;;;        _extern fflush(_0) -> _;
        _extern TZNAME!((b))[_1]
    endif -> _tzone;

#_ELSEIF DEF BERKELEY
    lstackmem struct TIMEVAL _tvp;
;;;     _extern printf('before gettimeofday') -> _;
;;;        _extern fflush(_0) -> _;

    _extern gettimeofday(_tvp, _buf) -> ;
    _extern timezone(_buf!TZN_MINUTESWEST, _timep!TM_ISDST) -> _tzone;

#_ELSE ERROR
#_ENDIF

    ;;; move timezone name into date -- 24 chars ignores the newline etc
;;;     _extern printf('before _bmove') -> _;
;;;        _extern fflush(_0) -> _;

    _bmove(@@(b)[_24], _date_addr, _buf) -> ;
    ;;; insert timezone string
    ##(b){_locc(_tzone, @@(b)[_128], _0)} -> _tzlen;
    ;;; shift year along
    _bmove(@@(b)[_4], _buf@(b)[_20], _buf@(b)[_tzlen _add _21]) -> ;
    _:`\s` -> _bmove(@@(b)[_tzlen], _tzone, _buf@(b)[_20])!(b);
    Consstring_bptr(_buf, _tzlen _add _25, CSB_LSTACKMEM)
enddefine;

    /*  Get current date and time as a string
    */
define sysdaytime();
    sys_convert_date(sys_real_time(), true)
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar  5 1997
        Made it use Consstring_bptr
--- John Gibson, Aug 15 1996
        Added :data after tzname
--- John Gibson, Jun 30 1995
        Changed to use lstackmem for _secs.
--- Robert John Duncan, Mar  3 1995
        Removed reference to RIS*COS
--- John Gibson, Mar  2 1995
        OSF1 changes
--- Poplog System, Jan 18 1995 (Julian Clinton)
        Added case for Linux.
--- John Gibson, Jun  2 1994
        Uses lstackmem
--- Robert John Duncan, May 21 1992
        Extra fields in struct TM removed again in SunOS 5.0 (!)
--- Robert John Duncan, Nov 29 1991
        Added TZNAME macro to allow for leading "_"
--- Robert John Duncan, Jun 21 1991
        Added definitions for SG IRIX
--- Robert John Duncan, Jan 10 1991
        Added definitions for MIPS Risc/os
--- Robert John Duncan, Oct  3 1990
        Extended new version of timezone name to include Ultrix.
--- John Gibson, Aug 22 1989
        Added new Sun version of timezone name and included HPUX in
        System V version (since Bobcat has BERKELEY set)
 */
