/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.unix/src/syslink.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *SYSUTIL
 */

;;; --------------- LINKING/UNLINKING UNIX FILES -------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'unixdefs.ph'

section $-Sys$-Io;

global constant
        procedure (Set_enotdir, Symlink_target)
    ;

endsection;


;;; ---------------------------------------------------------------------

section $-Sys$-Io => syslink, sysunlink, syssymlink;

define sysunlink() with_nargs 1;
    lvars file = Symlink_target();
    lstackmem stackbuff _nbuf;
    while _neg(_extern[SE] unlink(Encode_sys(file,_nbuf))) do
        if _ERRNO == _:EINTR then _CHECKINTERRUPT; nextloop endif;
        Set_enotdir(file);
        returnif(_ERRNO == _:ENOENT) (false);
        Syserr_mishap(file, 1, 'CAN\'T UNLINK FILE')
    endwhile;
    true
enddefine;

define lconstant Do_link(file1, file2, _link_routine);
    lvars file1, file2, _flags = _0;
    dlvars _link_routine;
    lstackmem stackbuff _nbuf2;

    ;;; extra procedure needed because 2 stackbuffs won't fit in
    ;;; one procedure stack frame
    define lconstant try_link(f1, _f2);
        lvars f1, _f2;
        lstackmem stackbuff _nbuf1;
        _extern[INDIR, SE] _link_routine(Encode_sys(f1, _nbuf1), _f2)
    enddefine;

    if isboolean(file2) then
        if file2 then _2:01 -> _flags endif;
        ((), file1) -> (file1, file2)
    elseif isinteger(file2) then
        _int(file2) -> _flags;
        ((), file1) -> (file1, file2)
    endif;

    Symlink_target(file1, _flags _bitst _2:01) -> file1;
    Symlink_target(file2, _flags _bitst _2:10) -> file2;

    while _neg(try_link(file1, Encode_sys(file2,_nbuf2))) do
        if _ERRNO == _:EINTR then _CHECKINTERRUPT; nextloop endif;
        Set_enotdir(file1);
        Set_enotdir(file2);
        returnif(_ERRNO == _:ENOENT) (false);
        Syserr_mishap(file1, file2, 2, 'CAN\'T LINK TO FILE')
    endwhile;
    true
enddefine;

define syslink() with_nargs 2;
    Do_link(_extern link)
enddefine;

#_IF DEF BERKELEY or DEFV SYSTEM_V >= 4.0 or DEF SCO

define syssymlink() with_nargs 2;
    Do_link(_extern symlink)
enddefine;

#_ELSE

define syssymlink();
    mishap(0, 'syssymlink NOT IMPLEMENTED IN THIS VERSION OF UNIX')
enddefine;

#_ENDIF

endsection;     /* $-Sys$-Io */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar  8 1997
        Added Encode_sys calls on filenames
--- John Gibson, Jun  2 1995
        Sorted out syslink/syssymlink -- made them take optional integer arg
        where 2:01 = deref source symlinks, 2:10 = deref target symlinks,
        or boolean true = 1, false = 0
--- Poplog System, Jan 18 1995
        Added case for SCO.
--- John Gibson, May 23 1994
        _sys*error -> _ERRNO
--- John Gibson, May 10 1994
        Changed to use _extern with INDIR flag for indirect and made
        the procedures service interrupts.
--- Robert John Duncan, Jun 23 1992
        SVR4 supports symlink(2)
--- John Gibson, Sep 14 1991
        Removed N*ull_end
--- John Gibson, Aug 22 1989
        Bobcat now has BERKELEY set for symlinks
--- John Gibson, Jun 16 1989
        Transferred -Symlink_target- to sysopen.p
--- John Gibson, May  9 1988
        Added -syssymlink-
--- John Gibson, Mar 31 1988
        Moved out of sysio.p
 */
