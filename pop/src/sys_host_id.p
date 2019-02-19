/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.unix/src/sys_host_id.p
 > Purpose:         Return host machine id (UNIX version)
 > Author:          John Gibson, May 21 1990 (see revisions)
 > Documentation:   REF * SYSTEM/sys_host_id
 > Related Files:   C.vms/src/sys_host_id.p
 */

#_INCLUDE 'declare.ph'


;;; --------------------------------------------------------------------

section $-Sys => sys_host_id;

constant procedure (Endstring_to_num, sysinfo);

define sys_host_id();
    lvars hostid;

#_IF DEFV SYSTEM_V >= 4.0 or DEF OSF1
    ;;; use sysinfo(2) (NB: this may include non-numeric characters)
    lvars _n;
    lconstant SI_HW_SERIAL = 7; /* from <sys/systeminfo.h> */
    returnunless(sysinfo(SI_HW_SERIAL) ->> hostid)(false);
    if Endstring_to_num(1, hostid) ->> _n then
        ;;; return as a number for backward compatability
        Uint_->_pint(_n) -> hostid;
    endif;

#_ELSEIF (DEF HPUX) or (DEF SCO)
    ;;; hasn't (yet) got -gethostid- (although the system call number for it
    ;;; is in syscall.h).
    return(false);

#_ELSEIF DEF BERKELEY
    Uint_->_pint(_extern gethostid()) -> hostid;

#_ELSE_ERROR
#_ENDIF

    hostid :: [];
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Poplog System, Jan 18 1995 (Julian Clinton)
        Added "_" in ELSE_ERROR conditional compilation and added
        condition for SCO (to return <false>).
--- Robert John Duncan, Aug  7 1992
        Added proper code for SVR4
--- Robert John Duncan, Jul 22 1992
        Added dummy case for SVR4, just so the file will compile
 */
