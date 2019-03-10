/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.unix/src/sysisdirectory.p
 > Purpose:         Test for directory
 > Author:          John Gibson, Nov 25 1991 (see revisions)
 > Documentation:   REF *SYSUTIL
 > Related Files:   C.vms/src/sysisdirectory.p
 */

#_INCLUDE 'declare.ph'
#_INCLUDE 'unixdefs.ph'


;;; ---------------------------------------------------------------------

section $-Sys => sysisdirectory;

define sysisdirectory(path);
    lvars path;
    lstackmem struct STATB _statb, stackbuff _nbuf;
    _CLAWBACK_SAVE;
    sysfileok(path, false) -> path;
    if datalength(path) == 0 then '.' -> path endif;
    _extern[NI, SE] stat(Encode_sys(path,_nbuf), _statb) _sgreq _0
        and _statb!ST_MODE _bimask _STM_IFMT == _STM_IFDIR;
    Clawback(0) ->
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar  8 1997
        Added Encode_sys
--- John Gibson, May 20 1996
        Now treats an empty string as equivalent to '.' (otherwise,
        something like sysisdirectory(sys_fname_path('foo')) will
        return false).
--- John Gibson, Jun 13 1994
        Changed to use lstackmem
 */
