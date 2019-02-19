/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.unix/src/unix_dir.p
 > Purpose:
 > Author:          John Gibson et al (see revisions)
 */

;;;------------------------ UNIX UTILITIES -----------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'unixdefs.ph'

constant
        procedure (Sys_fd_open_check)
    ;

#_IF not(DEF BERKELEY or DEFV SYSTEM_V >= 4.0)
global constant
        procedure (Sys$-Io$-Opencreate)
    ;
#_ENDIF

;;; -----------------------------------------------------------------------

section $-Sys$-Dir => popdirectory, current_directory;

protected vars
    popdirectory;

vars
    current = false;    ;;; holds current directory


#_IF DEF BERKELEY or DEFV SYSTEM_V >= 4.0

#_IF DEFV IRIX < 5.0
lconstant macro (
    opendir     = "BSDopendir",
    closedir    = "BSDclosedir",
    readdir     = "BSDreaddir",
);
#_ENDIF

define Open_stream(dirname) -> _dirp;
    lvars dirname, _dirp, _retry = 1;
    lstackmem stackbuff _nbuf;
    while (_extern opendir(Encode_sys(dirname,_nbuf)) ->> _dirp) == _NULL do
        quitif((Sys_fd_open_check(-1, false, _retry) ->> _retry) fi_< 0)
    endwhile
enddefine;

define Close_stream(_dirp);
    lvars _dirp;
    _extern[NI] closedir(_dirp) ->
enddefine;

define Read_entry(_dirp) -> _dir_entry;
    lvars _dirp, _dir_entry;
    while (_extern readdir(_dirp) ->> _dir_entry) == _NULL
    and _ERRNO == _:EINTR
    do
        _0 -> _ERRNO;
    endwhile
enddefine;

#_ELSE

define Open_stream(dirname) -> _fd;
    lvars dirname, _fd;
    if _neg($-Sys$-Io$-Opencreate(dirname, dirname, O_RDONLY, 0) ->> _fd)
    then
        _NULL -> _fd
    endif
enddefine;

define Close_stream(_fd);
    lvars _fd;
    _extern[NI] pop_close(_fd) ->
enddefine;

define Read_entry(_fd) -> _dir_entry;
    lvars _fd, _n, _dir_entry =
        #_< (writeable inits(_pint(##DIR_END_BYTE)))@V_WORDS >_#;
    repeat
        _extern[NI] read(_fd, _dir_entry@(w->b), ##DIR_END_BYTE) -> _n;
        unless _n == ##DIR_END_BYTE then
            if _nonzero(_n) then
                if _n == __-1 then
                    Syserr_mishap(0, 'ERROR READING ..')
                else
                    mishap(0, 'ERROR READING .. (premature end of file)')
                endif
            else
                return(_NULL -> _dir_entry);
            endif
        elseif _nonzero(_dir_entry!DIR_INO) then
            return;
        endunless
    endrepeat
enddefine;

#_ENDIF

define Decode_dir_entry(_dir_entry, _sbuf) -> (_cptr, _clim, is16);
    lvars   is16 = false, _p, _len, _cptr, _clim, _dir_entry, _sbuf,
            _decode = sys_encoding;
    lstackmem int _ilenp, int _olenp, int _state;

    ;;; Establish the length of the name
#_IF DEFV SYSTEM_V >= 4.0 or DEF LINUX or DEF AIX
    ;;; no DIR_NAMLEN, but the name must fit in the record
    ;;; (NB: AIX has DIR_NAMLEN, but it's buggy with NFS files.)
    _dir_entry!DIR_RECLEN -> _len;
#_ELSEIF DEF BERKELEY
    _dir_entry!DIR_NAMLEN -> _len;
#_ELSE
    ##DIR_END_BYTE -> _len;
#_ENDIF
    _dir_entry@DIR_NAME ->> _p -> _cptr;
    _p@(b)[_len] -> _clim;
    while _p <@(b) _clim do
        if _zero(_p!(b)++ -> _p) then
            _p--@(b) -> _clim;
            quitloop
        endif
    endwhile;

    returnunless(_decode);

    _decode!XP_PTR!CDFN_DECODE -> _decode;
    ##(b){_clim, _cptr} -> _ilenp!(i);
    ##(s)[_1|s_stackbuff] -> _olenp!(i);
    _0 -> _state!(i);
    _extern[INDIR] _decode(_cptr, _ilenp, _sbuf, _olenp, _state, _0) -> ;
    unless _zero(_ilenp!(i)) then
        mishap(0, 'SYSTEM ERROR IN Decode_dir_entry (directory entry name too long')
    endunless;
    ;;; else everything used up
    _olenp!(i) -> _len;
    _sbuf -> _cptr;
    _cptr@(s)[_len] -> _clim;
    true -> is16
enddefine;

define Explode_name(_cptr, _clim, is16);
    lvars _cptr, _clim, is16;
    if is16 then
        while _cptr <@(s) _clim do _pint(_cptr!(s)++ -> _cptr) endwhile
    else
        while _cptr <@(b) _clim do _pint(_cptr!(b)++ -> _cptr) endwhile
    endif
enddefine;

    /*  Get the current directory pathname
    */

#_IF DEF LINUX

/* New version for Linux by Waldek Hebisch, installed by A.Sloman 28 Sep 2007
    uses external C utility
    get_current_dir_name
*/

define lconstant Get_curr_dir() -> path;
    lvars _n0, _n1, _c, k, path;

    ;;; not sure this is needed [AS]
    ;;; _CLAWBACK_SAVE;

    _extern get_current_dir_name() -> _n0;

    if _nonzero(_n0) then
        0 -> k;
        _n0 -> _n1;
        while _nonzero((_n1!(b)++ -> _n1) ->> _c) do
            _pint(_c);
            k fi_+ 1 -> k;
        endwhile;
        _extern free(_n0) -> _;
        consstring(k) -> path;
    else
        mishap(0, 'GET DIRECTORY USING get_current_dir_name FAILED')
    endif;

    ;;; not sure this is needed [AS]
    ;;; Clawback(path)
enddefine;

#_ELSE
    ;;; not for linux

define lconstant Get_curr_dir();
    lvars   path, _dir_entry, _dotdev, _dotdev2, _dotino,
            _rootdev, _rootdev2, _rootino, _dirp;
    lstackmem struct STATB _statb, stackbuff _nbuf;

    lconstant
        PEOF_ms     = 'READ ERROR: premature eof on directory ..',
        _dotdir     = '.'@V_BYTES,
        _dotdotdir  = '..'@V_BYTES,
        _rootdir    = '/'@V_BYTES,
        ;

    _CLAWBACK_SAVE;

    _extern[NI] stat(_rootdir, _statb) -> ;
    DEV_T_TO_VARS(_statb, ST_DEV, _rootdev, _rootdev2);
    _statb!ST_INO -> _rootino;

    '/' -> path;
    repeat
        _extern[NI] stat(_dotdir, _statb) -> ;
        quitif( DEV_T_==_VARS(_statb, ST_DEV, _rootdev, _rootdev2)
                and _statb!ST_INO == _rootino);

        DEV_T_TO_VARS(_statb, ST_DEV, _dotdev, _dotdev2);
        _statb!ST_INO -> _dotino;
        if (Open_stream('..') ->> _dirp) == _NULL then
            mishap(0, 'CAN\'T OPEN DIRECTORY ..')
        endif;
;;;                _extern printf('_dirp = %ld\n', _dirp) -> _;
;;;                _extern fflush(_0) -> _;
        _extern[NI] stat(_dotdotdir, _statb) -> ;
        if _neg(_extern[NI, SE] chdir(_dotdotdir)) then
            mishap(0, 'CAN\'T CHANGE DIRECTORY TO ..');
        endif;

        if DEV_T_==_VARS(_statb, ST_DEV, _dotdev, _dotdev2) then
            quitif(_statb!ST_INO == _dotino);
            repeat
                if (Read_entry(_dirp) ->> _dir_entry) == _NULL then
                    mishap(0, PEOF_ms)
                endif;
;;;                                _extern printf('_dirp = %ld\n', _dirp) -> _;
;;;                                _extern fflush(_0) -> _;
                quitif(_dir_entry!DIR_INO == _dotino)
            endrepeat
        else
            repeat
                if (Read_entry(_dirp) ->> _dir_entry) == _NULL then
                    mishap(0, PEOF_ms)
                endif;
;;;                 _extern printf('_dirp = %ld\n', _dirp) -> _;
;;;                _extern fflush(_0) -> _;
                _extern[NI] stat(_dir_entry@DIR_NAME, _statb) ->;
                quitif( DEV_T_==_VARS(_statb, ST_DEV, _dotdev, _dotdev2)
                        and _statb!ST_INO == _dotino)
            endrepeat
        endif;

        consstring(#|
            `/`,
            procedure();
                lstackmem s_stackbuff _sbuf;
                Explode_name(Decode_dir_entry((), _sbuf))
            endprocedure(_dir_entry),
            deststring(path) ->
        |#) -> path;
;;;                _extern printf('3 _dirp = %ld\n', _dirp) -> _;
;;;                _extern fflush(_0) -> _;
        Close_stream(_dirp)
    endrepeat;

    ;;; change back to current directory!!!
    _extern[NI, SE] chdir(Encode_sys(path,_nbuf)) -> ;

;;;        _extern printf('chdir(%s)\n', Encode_sys(path,_nbuf))-> _;
;;;        _extern fflush(_0) -> _;
    Clawback(path)
enddefine;

#_ENDIF

define active current_directory;
    copy(if current then
            current
         else
            Get_curr_dir() ->> current
         endif)
enddefine;

define updaterof active current_directory new_dir;
    lvars new_dir;
    lstackmem stackbuff _nbuf;
    returnif((sysfileok(new_dir) ->> new_dir) = current);
    if new_dir = nullstring then systranslate('HOME') -> new_dir endif;
    if _neg(_extern[NI, SE] chdir(Encode_sys(new_dir,_nbuf))) then
        Syserr_mishap(new_dir, 1, 'CAN\'T CHANGE DIRECTORY')
    endif;
    false -> current
enddefine;

endsection;     /* $-Sys$-Dir */


/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Oct  1 2007
        New version of Get_curr_dir for linux, provided by Waldek Hebisch
        See http://www.cs.bham.ac.uk/research/projects/poplog/bugfixes/BUGREPORTS
        Bug 32
--- John Gibson, Mar 20 1999
        Changes to fix x86 Linux "dev_t" type problem
--- John Gibson, Mar 13 1997
        Char encoding changes
--- Integral Solutions Ltd, Aug 31 1995 (Julian Clinton)
        Removed HPUX-specific code (no longer needed)
--- Poplog System, Jan 18 1995
        Changes for Linux and SCO/SVR3.2.
--- John Gibson, Jun 13 1994
        Uses lstackmem for stat buff
--- Robert John Duncan, May 31 1994
        Restored previous behaviour of Read_entry -- returning _NULL at end
        of stream -- for compatibility with its use in "unixfmatch.p".
--- John Gibson, May 17 1994
        Added [NI] flag on _externs requiring it, and other changes
--- John Gibson, May  9 1994
        Changed Open_stream to use Sys_fd_open_check
--- Robert John Duncan, Mar 25 1994
        Changed for IRIX 5 (SVR4)
--- Robert John Duncan, Jul 22 1992
        Added cases for SVR4
--- Robert John Duncan, Jun 16 1992
        System calls {close,read} replaced by pop_{close,read} from
        "c_core.c"
--- John Gibson, Sep 14 1991
        Removed N*ull_end
--- Robert John Duncan, Jun 21 1991
        Added BSD definitions for SG IRIX.
 */
