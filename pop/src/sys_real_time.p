/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.unix/src/sys_real_time.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *TIMES
 */

;;;----------------------- REAL TIME -----------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'unixdefs.ph'

uses (biginteger_key);

;;; -------------------------------------------------------------------

section $-Sys => sys_real_time, sys_microtime;

    /*  Time in seconds since midnight 1 Jan 1970 GMT
    */
define sys_real_time();
#_IF DEFV BERKELEY >= 4.2
    lstackmem struct TIMEVAL _tvp;
    _extern gettimeofday(_tvp, _NULL) -> ;
    Sint_->_pint(_tvp!TIM_SEC)
#_ELSE
    Sint_->_pint(_extern time(_NULL))
#_ENDIF
enddefine;

define sys_microtime();
#_IF DEFV BERKELEY >= 4.2
    lstackmem struct TIMEVAL _tvp;
    _extern gettimeofday(_tvp, _NULL) -> ;
    Sint_->_pint(_tvp!TIM_SEC)*1000000 + Sint_->_pint(_tvp!TIM_USEC)
#_ELSE
    Sint_->_pint(_extern time(_NULL))*1000000
#_ENDIF
enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun  2 1994
        Uses lstackmem
 */
