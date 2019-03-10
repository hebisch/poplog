/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.unix/src/sys_file_move.p
 > Purpose:
 > Author:          John Gibson, May 24 1988 (see revisions)
 > Documentation:   REF *SYSUTIL
 */

;;; --------------------- MOVE A FILE (UNIX) ---------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'unixdefs.ph'

global constant
        procedure (sys_file_copy, sysunlink, syssymlink)
    ;

section $-Sys$-Io;

constant
        procedure (Set_enotdir, Symlink_target, Move_file_back,
        File_nlinks, Fname_path)
    ;

endsection;

;;; ------------------------------------------------------------------

section $-Sys$-Io => sys_file_move;

define sys_file_move(from_name, to_name);
    lvars nlinks, from_name, to_name, _res, _mode, _flags = _0;
    lconstant errms = 'CAN\'T MOVE FILE';
    lstackmem stackbuff _nbuf;

    define lconstant unlink(f);
        lvars f;
        lstackmem stackbuff _fnbuf;
        _extern[SE] unlink(Encode_sys(f,_fnbuf));
    enddefine;

    define lconstant link(f, t, _fnbuf);
        lvars f, t, _fnbuf;
        lstackmem stackbuff _tnbuf;
        _extern[SE] link(Encode_sys(f,_fnbuf), Encode_sys(t,_tnbuf));
    enddefine;

    define lconstant Rename(f, t);
        lvars f, t, _res;
        lstackmem stackbuff _nbuf2;
#_IF DEF BERKELEY or DEFV SYSTEM_V >= 4.0
        define lconstant rename(f, t, _fnbuf);
            lvars f, t, _fnbuf;
            lstackmem stackbuff _tnbuf;
            _extern[SE] rename(Encode_sys(f,_fnbuf), Encode_sys(t,_tnbuf));
        enddefine;

        while _neg(rename(f, t, _nbuf2) ->> _res)
#_ELSE
        while (_neg(unlink(t) ->> _res) and _ERRNO /== _:ENOENT)
            or _neg(link(f, t, _nbuf2) ->> _res)
            or _neg(unlink(f) ->> _res)
#_ENDIF
        and _ERRNO == _:EINTR do
            _CHECKINTERRUPT
        endwhile;
        _nonneg(_res)
    enddefine;


    if isboolean(to_name) then
        if to_name then _2:01 -> _flags endif;
        ((), from_name) -> (from_name, to_name)
    elseif isinteger(to_name) then
        _int(to_name) -> _flags;
        ((), from_name) -> (from_name, to_name)
    endif;

    Symlink_target(from_name, _flags _bitst _2:01) -> from_name;
    Symlink_target(to_name, _flags _bitst _2:10) -> to_name;

    ;;; check from_name can be deleted
    unless (File_nlinks(Fname_path(from_name, true), false) ->) then
        ;;; no write access in directory
        Syserr_mishap(from_name, 1, errms)
    endunless;

    if (File_nlinks(from_name, false) -> _mode) /== 0
    and (_mode _bimask _STM_IFMT) /== _STM_IFREG then
        ;;; not regular file
        returnif(Rename(from_name, to_name));
#_IF DEF BERKELEY or DEFV SYSTEM_V >= 4.0
        returnif(_mode _bimask _STM_IFMT == _STM_IFLNK
                and _ERRNO == _:EXDEV
                and syssymlink(from_name, to_name, 2:01)
                ) (sysunlink(from_name) ->)
#_ENDIF

    else
        ;;; regular file
        while _neg(link(from_name, to_name, _nbuf) ->> _res)
        and _ERRNO == _:EINTR do
            _CHECKINTERRUPT
        endwhile;

        returnif(_nonneg(_res)) (sysunlink(from_name) ->);

        if _ERRNO == _:EEXIST then
            ;;; to_name exists already
            if isinteger((File_nlinks(to_name, false) ->) ->> nlinks) then
                if nlinks fi_<= 1 then
                    Move_file_back(to_name, true) -> (,);
                    returnif(Rename(from_name, to_name)) ;;; may return EXDEV
                else
                    ;;; force copy for file with links
                    _:EXDEV -> _ERRNO
                endif
            ;;; else not writeable
            endif
        endif;
        if _ERRNO == _:EXDEV then
            ;;; do copy (for cross-device move, etc)
            sys_file_copy(from_name, to_name);
            return(sysunlink(from_name) ->)
        endif
    endif;

    Set_enotdir(from_name);
    Set_enotdir(to_name);
    Syserr_mishap(from_name, to_name, 2, errms)
enddefine;

endsection;     /* $-Sys$-Io */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar  8 1997
        Added Encode_sys calls as appropriate
--- John Gibson, Jun  6 1995
        Fixed it to cope with case where _:EEXIST is returned rather than
        _:EXDEV when to_name exists and is on a different device.
--- John Gibson, Jun  2 1995
        Made sys_file_move take optional integer arg where 2:01 = deref
        source symlinks, 2:10 = deref target symlinks, or boolean true = 1,
        false = 0
--- John Gibson, Jun 13 1994
        File_nlinks returns _mode result
--- John Gibson, May 23 1994
        _sys*error -> _ERRNO
--- John Gibson, May 10 1994
        Added interrupt checking around _extern calls
--- Robert John Duncan, Jun 22 1992
        rename(2) is available in SVR4
--- John Gibson, Sep 14 1991
        Removed N*ull_end
--- John Gibson, Nov 27 1988
        Made -sys_file_move- work on directories (Berkeley only).
 */
