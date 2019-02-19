/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.unix/src/sysopen.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *SYSIO
 */

;;; ------------------ OPENING DEVICES (UNIX) ----------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'io.ph'
#_INCLUDE 'unixdefs.ph'

section $-Sys$-Io;

constant
        procedure (Opencreate)
    ;

endsection;

;;; ---------------------------------------------------------------------

section $-Sys$-Io => sysopen, readable;

define Fname_path(file, dir_name);
    lvars file, dir_name, _f, _s;
    if file!KEY!K_FLAGS _bitst _:M_K_STRING16 then
        file@V_SHORTS -> _s;
        _s@(s)[file!V_LENGTH] -> _f;
        while _f >@(s) _s do
            returnif((_f--!(s) -> _f) == _:`/`)
                (substring(1, _pint(##(s){_f,_s} _add _1), file),
                    if dir_name then sys_>< '.' endif)
        endwhile;
    else
        file@V_BYTES -> _s;
        _s@(b)[file!V_LENGTH] -> _f;
        while _f >@(b) _s do
            returnif((_f--!(b) -> _f) == _:`/`)
                (substring(1, _pint(##(b){_f,_s} _add _1), file),
                    if dir_name then sys_>< '.' endif)
        endwhile;
    endif;
    if dir_name then '.' else nullstring endif
enddefine;

define Set_enotdir(file);
    lvars file;
    lstackmem stackbuff _nbuf;
    if _ERRNO == _:ENOENT then
        ;;; check whether it's because the directory doesn't exist
        Fname_path(file, true) -> file;
        if _neg(_extern[NI, SE] access(Encode_sys(file,_nbuf), _0)) then
            _:ENOTDIR -> _ERRNO
        endif
    endif
enddefine;

    /*  Get the target filename of a symbolic link, or just return
        sysfileok(file) if don't want to follow symbolic links
        or they don't exist
    */
define Symlink_target(file) -> target;
    lvars target, linkval, file, follow_symlink = false;
    lstackmem stackbuff _nbuf;

    if isboolean(file) then (), file -> (file, follow_symlink) endif;
    sysfileok(file, false) -> target;

#_IF DEF BERKELEY or DEFV SYSTEM_V >= 4.0
    returnunless(follow_symlink);

    repeat 8 /* = MAXSYMLINK */ times
        procedure;
            lstackmem stackbuff _obuf;
            lvars _len = _extern[SE] readlink((), _obuf, _:SIZEOF(stackbuff));
            _nonneg(_len) and Consstring_bptr(_obuf, _len, CSB_LSTACKMEM)
        endprocedure( Encode_sys(target,_nbuf) ) -> linkval;
        ;;; if not a symbolic link return target
        returnunless(linkval);
        ;;; is a symbolic link -- if link value is a relative name, make it
        ;;; relative to directory of link
        if fast_subscrs(1,linkval) == `/` then
            linkval
        else
            Fname_path(target, false) dir_>< linkval
        endif -> target
    endrepeat;

    mishap(file, 1, 'TOO MANY LEVELS OF SYMBOLIC LINK')
#_ENDIF
enddefine;

define Sys_open(file, accmode, arg3, open_p, _iscreate);
    lvars   file, accmode, arg3, procedure open_p, fullname,
            _iscreate, _errc, _fd, ress;

    if isinteger(arg3) then
        ;;; optional character arg specifying error action
        ((), file, accmode, arg3) -> (file, accmode, arg3, _errc);
        unless _errc == `N` or _errc == `F`
        or (not(_iscreate) and (_errc == `D` or _errc == `A`))
        then
            mishap(_errc, 1, 'sysopen/create: INVALID ERROR CHARACTER CODE')
        endunless
    else
        if _iscreate then `N` else `F` endif -> _errc
    endif;
    Symlink_target(file, true) -> fullname;
    Check_integer(accmode, 0);

    if _iscreate then
        accmode fi_|| #_< O_CREAT || O_TRUNC >_# -> accmode;
        if _errc == `F` then
            ;;; for create, interpret this as exclusive
            accmode fi_|| O_EXCL -> accmode;
            `e` -> _errc
        endif
    endif;

        if _nonneg(open_p(file, fullname, accmode, 0) ->> _fd) 
        then
;;;          _extern printf('doing Sys_cons_device\n') -> _ ;
;;;          _extern fflush(_0) -> _ ;
      Sys_cons_device(file, fullname, accmode, arg3, _pint(_fd), false)
           -> ress;
;;;          _extern printf('after Sys_cons_device\n') -> _ ;
;;;          _extern fflush(_0) -> _ ;
          return (ress);
        endif;

    ;;; can't open/create file
    Set_enotdir(fullname);
    returnif(
           _errc == `A`
        or _errc == `D` and _ERRNO == _:ENOTDIR
        or (_errc == `D` or _errc == `F`) and _ERRNO == _:ENOENT
        or _errc == `e` and _ERRNO == _:EEXIST
        ) (false);

    Syserr_mishap(file, 1,
        if _iscreate then 'CAN\'T CREATE FILE' else 'CAN\'T OPEN FILE' endif)
enddefine;

define sysopen(/*file, accmode, arg3*/) with_nargs 3;
    Sys_open((), Opencreate, false)
enddefine;

define readable = sysopen(% 0, false, `A` %) enddefine;;

endsection;     /* $-Sys$-Io */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar  7 1997
        Added calls of Encode_sys etc.
--- John Gibson, May 24 1994
        Cons*_device -> Sys_cons_device
--- John Gibson, May 23 1994
        _sys*error -> _ERRNO
--- John Gibson, Jan 29 1994
        Added new Sys_open and changed sysopen to use it.
--- Adrian Howard, Nov 11 1992
        Added optional argument to sysfileok in Symlink_target to make
        catch false being passed as the filename
--- Robert John Duncan, Jun 23 1992
        SVR4 supports symlink(2)
--- John Gibson, Sep 14 1991
        Removed N*ull_end
--- John Gibson, May 27 1990
        Added optional character arg specifying error action to -sysopen-
--- John Gibson, Jun 16 1989
        device fullnames now have symbolic links translated
--- John Gibson, Feb 19 1989
        Included io.ph
--- Roger Evans, Jun  6 1988
        Added extra argument to Cons*_device
--- John Gibson, Mar 31 1988
        Moved out of sysio.p
 */
