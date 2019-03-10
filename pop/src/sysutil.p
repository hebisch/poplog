/* --- Copyright University of Sussex 2000. All rights reserved. ----------
 > File:            C.unix/src/sysutil.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *SYSUTIL
 */

;;;---------------- SYS- UTILITY PROCEDURES  -------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'unixdefs.ph'

constant
        procedure (Sys$-Try_encode_string),
        _useras
    ;

vars
        popenvlist, popdirectory,
        _system_stack_base, Sys$- _init_args
    ;


;;; ---------------------------------------------------------------------

section $-Sys => sysgetpasswdentry, systranslate;


    /*  Called by _extern with the [NI] flag instead of _call_sys.
        Do a Unix system call but loop while it returns EINTR.
    */
define Call_sys_nointr(_nargs, _routine) -> _res;
    lvars _n, _stkoffs = @@(w)[_nargs _sub _1], _nargs, _routine, _res;
    repeat
        ;;; dup the args
        _nargs -> _n;
        until _zero(_n) do
            _user_sp()!(w){_stkoffs};       ;;; stack it
            _n _sub _1 -> _n
        enduntil;
        ;;; do the call and loop if EINTR
        ;;; FIXME: can we have negative long result ???
        unless (_call_sys((), _nargs, _routine) ->> _res) 
                  _gr _2147483647 and _ERRNO == _:EINTR then
            _useras(@@(w)[_nargs]);         ;;; remove args
            return
        endunless
    endrepeat
enddefine;

#_IF DEF SIGN_EXTEND_EXTERN

define Call_sys_nointr_se(_nargs, _routine) -> _res;
    lvars _nargs, _routine, _res;
    Call_sys_nointr((), _nargs, _routine) -> _res;
    _res _bimask _4294967295 -> _res;
    if _res _gr _2147483647 then _res _sub _4294967296 -> _res endif;
enddefine;

#_ENDIF

    /*  Set protections on pages (_prot as defined in memseg.ph)
    */
define Set_mem_prot(_base, _lim, _prot);
    lvars _base, _lim, _prot;
#_IF DEF BSD_MPROTECT
    _nonneg(_extern[SE] mprotect(_base@(w->b), ##(b){_lim, _base | w}, _prot))
#_ELSE
    "undef"
#_ENDIF
enddefine;

    /*  Encode filenames, etc, using work area _bptr if necessary.
        _bptr points to an area of _:SIZEOF(stackbuff) bytes
    */
define Encode_sys(string, _bptr) -> _bptr;
    lvars string, _bptr, _len, _encode = sys_encoding;
    if _encode or string!KEY!K_FLAGS _bitst _:M_K_STRING16 then
        unless Try_encode_string(string, _bptr, _:SIZEOF(stackbuff) _sub _1,
                                        _encode) -> _len
        then
            mishap(string, 1, 'SYSTEM ERROR IN Encode_sys (filename string too long')
        endunless;
        _0 -> _bptr!(b)[_len]
    else
        string@V_BYTES -> _bptr
    endif
enddefine;


;;; -------------------------------------------------------------------

struct RLIMIT {long RLIM_CUR, RLIM_MAX; };
lconstant macro RLIMIT_STACK = 3;

vars _unix_usrstack = _NULL;    ;;; must be vars so it's not restored

    /*  Return the absolute limit for the callstack
    */
define Abs_callstack_lim();

#_IF DEF SUNOS or DEF NCR or DEF DGUX or DEF HPUX or DEF LINUX
    ;;; They have several different USRSTACK values (and they're liable
    ;;; to change with a new release, etc). The following code finds the
    ;;; highest arg or env string address, and then takes that rounded up
    ;;; to be end of the stack -- hardly satisfactory, but in the absence
    ;;; of a proper system call to return the value, the only alternative
    ;;; is to have lots of different systems with the value hard-wired in
    ;;; for each one.
    lvars _ptr, _s;
    if _unix_usrstack == _NULL then
        if (_init_args ->> _ptr) /== _NULL then
            until _zero(_ptr!(w)++ -> _ptr ->> _s) do
                if _s >@(b) _unix_usrstack then _s -> _unix_usrstack endif
            enduntil;
        endif;
        _extern environ:data!(w) -> _ptr;
        until _zero(_ptr!(w)++ -> _ptr ->> _s) do
            if _s >@(b) _unix_usrstack then _s -> _unix_usrstack endif
        enduntil;
        if _unix_usrstack == _NULL then
            _system_stack_base -> _unix_usrstack
        endif;
        _unix_usrstack@(b.r->segment) -> _unix_usrstack
    endif;

#_ELSE
    ;;; UNIX_USRSTACK is the end address of the stack
    _:UNIX_USRSTACK -> _unix_usrstack;
#_ENDIF

    lvars _size;
#_IF DEF BERKELEY and not(DEF HPUX) or DEFV SYSTEM_V >= 4.0
    lstackmem struct RLIMIT _rlp;
    _extern getrlimit(_:RLIMIT_STACK, _rlp) -> ;
    _rlp!RLIM_CUR -> _size;
#_ELSE
    _:UNIX_USRSTACK_SIZE -> _size;
#_ENDIF

    lvars _lim = _unix_usrstack@(csword)-[_size | b];
    ;;; if _size is large, e.g. RLIMIT_INFINITY, the offset calculation
    ;;; can wrap round
    if _lim >@(csword) _unix_usrstack then
        ;;; choose some arbitrary limit close to the appropriate end of
        ;;; memory
        _0@(csword)[_1|vpage] -> _lim;
    endif;

    _lim;
enddefine;

    /*  Called by setpop etc after clearing the callstack.
        In VMS this procedure does something, but in Unix there's no
        way of deallocating stack memory.
    */
define Dealloc_callstack_mem();
enddefine;


;;; --------------------------------------------------------------------

#_IF DEFV SYSTEM_V >= 4.0 or DEF OSF1

    /* Interface to the sysinfo() system call,
     * used for getting hostname, hostid etc.
     */
define sysinfo(command);
    lvars command, _len;
    lstackmem stackbuff _obuf;
    lconstant _size = _:SIZEOF(stackbuff);
    _extern sysinfo(_int(command), _obuf, _size) -> _len;
    returnif(_len _slteq _0) (false);
    if _len _slteq _size then
        _len _sub _1
    else
        _size
    endif -> _len;
    Consstring_bptr(_obuf, _len, CSB_LSTACKMEM)
enddefine;

#_ENDIF

;;; --------------------------------------------------------------------

lconstant macro BSD_PWENT = DEF BERKELEY and not(DEF HPUX);

    ;;; entry returned by _extern getpwnam, getpwuid
struct PASSWD_ENTRY
  { (byte)  PWE_NAME,
            PWE_PASSWD;
#_IF DEFV SYSTEM_V >= 4.0 or DEF OSF1 or DEF LINUX or DEF AIX
    uid_t   PWE_UID;
    gid_t   PWE_GID;
#_ELSE
    int     PWE_UID,
            PWE_GID;
#_ENDIF
#_IF not(DEF LINUX or DEF AIX)
  #_IF BSD_PWENT
    int     PWE_QUOTA;
  #_ELSE
    (byte)  PWE_AGE;
  #_ENDIF
    (byte)  PWE_COMMENT;
#_ENDIF
    (byte)  PWE_GECOS,
            PWE_DIR,
            PWE_SHELL;
  };

;;; sysgetpasswdentry either returns a line a line got by Sysgetpasswdentry
;;; or returns one field of the line, or parses it into into a structure
;;; (a vector of strings and numbers).
;;; It includes the two dummy entries included by the
;;; equivalent UNIX routines (e.g. getpwent).

define sysgetpasswdentry(name);
    lvars name, linewanted = false, vec = false, _field = false, _vlen;
    dlvars _pwent;
    ;;; A.S.:altered to allow optional boolean or vector containing an integer
    ;;; If boolean is true, return whole line. If vector contains integer N,
    ;;; return only N'th field of line

    define lconstant Pwent_field(_n);
        lvars _n;
        go_on _n to
            NAME PASSWD UID GID QUOTA COMMENT GECOS DIR SHELL
        else
            ERR;
        NAME:
            return(Consstring_bptr(_pwent!PWE_NAME, _-1, CSB_FIXED));
        PASSWD:
            return(Consstring_bptr(_pwent!PWE_PASSWD, _-1, CSB_FIXED, false));
        UID:
            return(Uint_->_pint(_pwent!PWE_UID));
        GID:
            return(Uint_->_pint(_pwent!PWE_GID));
        QUOTA:
          #_IF DEF LINUX or DEF AIX
            ;;; dummy
            return(0);
          #_ELSEIF DEF BSD_PWENT
            return(Uint_->_pint(_pwent!PWE_QUOTA));
          #_ELSE
            return(Consstring_bptr(_pwent!PWE_AGE, _-1, CSB_FIXED));
          #_ENDIF
        COMMENT:
          #_IF DEF LINUX or DEF AIX
            ;;; dummy
            return(nullstring);
          #_ELSE
            return(Consstring_bptr(_pwent!PWE_COMMENT, _-1, CSB_FIXED));
          #_ENDIF
        GECOS:
            return(Consstring_bptr(_pwent!PWE_GECOS, _-1, CSB_FIXED));
        DIR:
            return(Consstring_bptr(_pwent!PWE_DIR, _-1, CSB_FIXED));
        SHELL:
            return(Consstring_bptr(_pwent!PWE_SHELL, _-1, CSB_FIXED));
        ERR:
            mishap(_n, 'PASSWORD ENTRY FIELD NUMBER OUT-OF-RANGE');
    enddefine;

    if isboolean(name) then
        name -> linewanted -> name
    elseif isvector(name) then
        if name!V_LENGTH == _1 then
            ;;; 1 element vector specifies field wanted
            fast_subscrv(1, name) -> _field -> name;
            unless isinteger(_field) and 1 fi_<= _field and _field fi_<= 9 then
                mishap(_field, 1, 'VECTOR SHOULD CONTAIN INTEGER 1 - 9')
            endunless
        elseif name!V_LENGTH _gr _9 then
            mishap(name, 1, 'VECTOR HAS MORE THAN 9 FIELDS')
        else
            name -> vec -> name
        endif
    endif;

    ;;; Block signals while we're doing this -- in SunOS 5.2 something
    ;;; isn't checking for an interrupted system call (i.e. EINTR).

    dlocal 0 %_extern _pop_sigmask(_1)->, _extern _pop_sigmask(_0)-> %;

    if isstring(name) then
        _extern getpwnam(name@V_BYTES)
    else
        _extern getpwuid(Pint_->_uint(name, _-1))
    endif -> _pwent;

    if _pwent == _NULL then
        ;;; entry not found
        false
    elseif linewanted then
        ;;; whole entry -- is this really wanted?
        Pwent_field(1) sys_>< ':' sys_>< Pwent_field(2);
#_IF not(BSD_PWENT)
        ;;; password age
        lvars age = Pwent_field(5);
        if age /= nullstring then () sys_>< ',' sys_>< age endif;
#_ENDIF
        fast_for _field from 3 to 9 do
            unless _field == 5 or _field == 6 then
                () sys_>< ':' sys_>< Pwent_field(_field)
            endunless
        endfast_for
    elseif _field then
        ;;; one field wanted
        Pwent_field(_field)
    else
        ;;; get _vlen fields into vector.
        unless vec then initv(9) -> vec endunless;
        _pint(vec!V_LENGTH) -> _vlen;
        fast_for _field to _vlen do
            Pwent_field(_field) -> fast_subscrv(_field, vec)
        endfast_for;
        vec
    endif;

    _extern endpwent() ->
enddefine;

    /*  Translate an environment variable -string-
    */
define systranslate(string);
    lvars string, envstring, _len;

    ;;; for compatibitliy with VMS version
    if isinteger(string) then () -> string endif;

    Check_string(string);
    if (_pint(string!V_LENGTH) ->> _len) == 0 then
        return(false)
    elseif fast_subscrs(1, string) == `~` then
        ;;; user login directory
        return( if _len == 1 then
                    systranslate('HOME')
                else
                    sysgetpasswdentry(Str_allbutfirst(1, string), #_< {8} >_#)
                endif)
    elseif fast_subscrs(1, string) == `$` then
        Str_allbutfirst(1, string) -> string;
        _len fi_- 1 -> _len
    endif;

    _len fi_+ 1 -> _len;
    fast_for envstring in popenvlist do
        if isstartstring(string, envstring) and envstring(_len) == `=` then
            return(Str_allbutfirst(_len, envstring))
        endif
    endfor;

    false
enddefine;

    ;;; <string|false> -> systranslate(string)
define updaterof systranslate(newval, string);
    lvars string, pair, envstring, last, newval;

    ;;; for compatibitliy with VMS version
    if isinteger(string) then ((), newval) -> (newval, string) endif;

    Check_string(string);
    if newval then Check_string(newval) endif;

    if datalength(string) == 0 then
        mishap(string, 1, 'NON-EMPTY STRING NEEDED')
    elseif fast_subscrs(1, string) == `~` then
        mishap(string, 1, 'CAN\'T UPDATE LOGIN DIRECTORY')
    elseif fast_subscrs(1, string) == `$` then
        Str_allbutfirst(1, string) -> string
    endif;

    string sys_>< '=' -> string;
    false -> last;
    fast_for pair on popenvlist do
        fast_front(pair) -> envstring;
        if isstartstring(string, envstring) then
            ;;; N.B. since the pairs in the initial -popenvlist-
            ;;; are in the 'no-restore' heap segment, we must remove pairs
            ;;; rather than updating them with a heap string pointer.
            fast_back(pair) ->  if last then fast_back(last)
                                else popenvlist
                                endif;
            quitloop
        endif;
        pair -> last
    endfor;

    ;;; The old entry has now been removed from popenvlist
    if newval then
        conspair(string sys_>< newval, popenvlist) -> popenvlist
    endif
enddefine;


;;; --- UNIX ERROR MESSAGES ---------------------------------------------

    ;;; 'Improved' versions of some error messages
lconstant
    unix_errms = [
        ^ENOMEM     'Not enough memory'
        ^ENOTDIR    'Invalid directory'
        ^EISDIR     'File is a directory'
        ^ENFILE     'Too many files open in system'
        ^EMFILE     'Too many files open'
        ^EPIPE      'Write on a broken pipe'
        ^EDOM       'Invalid argument'
        ^ERANGE     'Out-of-range argument'
        ^ELOOP      'Too many levels of symbolic link'
        ^EDQUOT     'Disc quota exceeded'
    ];

define Os_error_message();
    lvars ms, errno = _pint(_ERRNO);
    if fast_lmember(errno, unix_errms) ->> ms then
        fast_front(fast_back(ms))
    else
#_IF DEFV SUNOS < 5.0
        if _ERRNO _greq _extern sys_nerr!(int) then
            _NULL
        else
            _extern sys_errlist!((b))[_ERRNO]
        endif;
#_ELSE
        _extern strerror(_ERRNO);
#_ENDIF
        Consstring_bptr((), _-1, CSB_FIXED)
    endif -> ms;
    if isstring(ms) then
        ms
    else
        'Error number ' sys_>< errno
    endif
enddefine;


endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Jan  6 2000
        Fixed RLIMIT structure definition
--- Robert Duncan, Mar  1 1999
        In Abs_callstack_lim, included X86_LINUX in systems where the top of
        stack is computed dynamically
--- Robert Duncan, Feb 17 1999
        In Abs_callstack_lim, included HPUX in systems where the top of
        stack is computed dynamically
--- Robert Duncan, Jun 05 1998
        In Abs_callstack_lim, included DGUX in systems where the top of
        stack is computed dynamically
--- John Gibson, Mar  7 1997
        Made sysinfo use Consstring_bptr. Replaced Get*_nt_string with
        Consstring_bptr also. Added Encode_sys.
--- Robert Duncan, Jan 13 1997
        Added fix to Abs_callstack_lim for case where the stack size limit
        is large enough to cause arithmetic overflow
--- Robert Duncan, Aug  9 1996
        In Abs_callstack_lim, included NCR in systems where the top of
        stack is computed dynamically
--- John Gibson, Apr 12 1996
        sysio*message -> nonexported Os_error_message without the brackets
--- Robert John Duncan, Mar 21 1995
        Changes to sysgetpasswdentry for LINUX -- deosn't have all fields
        in struct passwd
--- John Gibson, May 23 1994
        _sys*error -> _ERRNO
--- John Gibson, May 10 1994
        Added Call_sys_nointr
--- John Gibson, May  6 1994
        Changed sys*iomessage to use _extern strerror etc
--- John Gibson, Nov  8 1993
        Added blocking of signals inside sysgetpasswdentry (appears something
        in the SunOS 5.2 getpwnam is not checking for EINTR after an
        interrupted system call).
--- John Gibson, May 11 1993
        Changed systranslate to allow an integer last arg for compatibility
        with VMS version
--- John Gibson, Sep 30 1992
        Changed lvars _usrstack to vars _unix_usrstack so the value doesn't
        get restored from a saved image.
--- Robert John Duncan, Aug  7 1992
        Added -sysinfo-
--- Robert John Duncan, Jun 23 1992
        SVR4 supports getrlimit(2)
--- John Gibson, Sep 14 1991
        Removed N*ull_end
--- John Gibson, Jan 24 1991
        -N*ull_end- moved to string.p
--- John Gibson, Jan  3 1991
        Uses _INIT_NONPOP_STRUCT
--- John Gibson, Oct 26 1990
        Added some more Unix error messages
--- John Gibson, Jul 27 1990
        Changed -Abs_callstack_lim- for SUN to (shakily) compute
        UNIX_USRSTACK so it doesn't have to be hard-wired in for each system.
--- John Gibson, Nov 14 1989
        Added updater of -systranslate-
--- John Gibson, Sep  8 1989
        Added -Dealloc_callstack_mem-
--- John Gibson, Aug 31 1989
        Added -Abs_callstack_lim-
--- John Gibson, Aug 23 1989
        Moved -sysexit- stuff to C.all file sysexit.p
--- John Gibson, Aug 22 1989
        Aded test for not(DEF HPUX) to decide format of password
        entry, since Bobcat now has BERKELEY set.
--- John Gibson, Jul 30 1989
        Added more error messages
--- John Gibson, Jul 17 1989
        Removed writing of newline to -popdevin- from -Exit-
--- Roger Evans, Mar  7 1989
        Modified sysexit to clobber all IO and interrupts before running
        popexit, vedpopexit etc. when pop_exit_ok is false
--- John Gibson, Feb 19 1989
        Included io.ph
--- Roger Evans, Nov 17 1988
        Modified sysexit to do no IO if pop_exit_ok is false
        Added fast_sysexit
--- Rob Duncan, Oct  6 1988
        Replaced reference to BSD_MMAP with BSD_MPROTECT, since the former
        doesn't imply the latter on the Symmetry
--- John Gibson, Aug 11 1988
        Added -Set_mem_prot-
--- John Gibson, Mar 17 1988
        Various procedures moved to separate files
--- Roger Evans, Feb 23 1988
    Installation of signals:
        Added #_INCLUDE signals.ph
        Moved -sys_send_signal- -sys_reset-signal- -Quit_signal- to signals.p
--- John Gibson, Feb 22 1988
        Check_string into section Sys
--- John Gibson, Feb 16 1988
        Weakref'ed Sys$-Extern$-Delete_link_files
--- John Gibson, Feb 11 1988
        Pint_->_uint, Check_integer etc in section Sys
 */
