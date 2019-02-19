/* --- Copyright University of Birmingham 2000. All rights reserved. ------
 > File:            $poplocal/local/????/systime.p
 > Purpose:
 > Author:          Aaron Sloman, Aug 21 2000 (see revisions)
 > Documentation:
 > Related Files:
 */

/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.unix/src/systime.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *TIMES
 */

;;; ------------------------- CPU TIME -----------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'unixdefs.ph'


;;; ---------------------------------------------------------------------

section $-Sys => systime;

lvars _ticks = _0;  ;;; clock ticks per second

;;; for most systems, clock rate is machine dependent -- use sysconf(2)
#_IF DEF POSIX1
lconstant macro _SC_CLK_TCK =   #_IF DEF HPUX or DEF LINUX _2
                                #_ELSE _3
                                #_ENDIF;
#_ENDIF

        ;;; get accumulated cpu time (in 1/100ths sec)
define systime();
    lvars _time, _rem;
    lstackmem struct TMS _tmsp;

    if _zero(_ticks) then
#_IF DEF _SC_CLK_TCK
        _extern sysconf(_SC_CLK_TCK) -> _ticks;
#_ELSE
        ;;; HERTZ should be defined in "sysdefs.p"
        _:HERTZ -> _ticks;
#_ENDIF
    endif;

    _extern times(_tmsp) -> ;
    _tmsp!TMS_UTIME _add _tmsp!TMS_STIME -> _time;
    ;;; convert from _ticks to 100ths secs rounded up
    (_time _mult _100) _div _ticks -> (_rem, _time);
    if (_rem _mult _2) _gr _ticks then
        _time _add _1 -> _time
    endif;
    _pint(_time)
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Aug 21 2000
    Changed ALPHA_LINUX to LINUX in order to fix systime error resulting
    from wrong setting for _SC_CLK_TCK (thanks to Robert Duncant)
--- John Gibson, Feb 28 1995
        Added test for POSIX1 around _SC_CLK_TCK definition
--- John Gibson, Jun  2 1994
        Uses lstackmem
--- Robert John Duncan, Jun  7 1993
        Enabled use of sysconf for SVR4
--- Robert John Duncan, Jun  8 1992
        HP-UX and SunOS now measure time in clock-ticks got from sysconf(2)
 */
