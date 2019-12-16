/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.unix/src/sys_host_name.p
 > Purpose:         Return host machine name (UNIX version)
 > Author:          John Williams, Feb 15 1989 (see revisions)
 > Documentation:   REF * SYSTEM/sys_host_name
 > Related Files:   C.vms/src/sys_host_name.p
 */


#_INCLUDE 'declare.ph'

section $-Sys => sys_host_name;

constant procedure (sysinfo);

define sys_host_name();
#_IF DEFV SYSTEM_V >= 4.0 or DEF OSF1
    ;;; use sysinfo(2)
    lconstant SI_HOSTNAME = 2;  /* from <sys/systeminfo.h> */
    sysinfo(SI_HOSTNAME);
#_ELSE
    lvars _offs;
    lstackmem stackbuff _obuf;
    lconstant _size = _:SIZEOF(stackbuff);
    if _nonneg(_extern[SE] gethostname(_obuf, _size)) then
        if (_locc(_obuf, @@(b)[_size], _0) ->> _offs) == _-1 then
            @@(b)[_size] -> _offs
        endif;
        Consstring_bptr(_obuf, ##(b){_offs}, CSB_LSTACKMEM)
    else
        false
    endif
#_ENDIF
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar  5 1997
        Made it use Consstring_bptr
--- Robert John Duncan, Aug  7 1992
        Now uses Pop procedure sysinfo() defined in "sysutil.p"
--- Robert John Duncan, Jul 22 1992
        SVR4 uses sysinfo(2) instead of gethostname(2).
 */
