/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.unix/src/syscreate.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *SYSIO
 */

;;; ------------------ CREATE A DEVICE (UNIX) ----------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'io.ph'
#_INCLUDE 'unixdefs.ph'

global constant
        procedure (syslink, sysunlink)
    ;

section $-Sys$-Io;

constant
        procedure (Set_enotdir, Symlink_target, Opencreate, Sys_open)
    ;

endsection;


;;; ------------------------------------------------------------------

section $-Sys$-Io =>    pop_file_mode, pop_file_versions,
                        sys_file_copy, syscreate;

vars
    ;;; default access mode for file creation
    pop_file_mode       = 8:0664,
    ;;; no of file versions to maintain
    pop_file_versions   = 2,
    ;


    /*  Check file writeable and return number of links (0 if nonexistent)
    */
define File_nlinks(file, follow_symlinks) /* -> (nlinks, _mode) */;
    lvars file, follow_symlinks, _mode, _file;
    lstackmem struct STATB _statb, stackbuff _nbuf;

    Encode_sys(file, _nbuf) -> _file;
    if follow_symlinks then
        _extern[NI] stat(_file, _statb)
    else
        _extern[NI] lstat(_file, _statb)
    endif -> ;
    _statb!ST_MODE -> _mode;

    if _neg(_extern[NI, SE] access(_file, _2)) then
        Set_enotdir(file);
        if _ERRNO == _:ENOENT then
            0       ;;; doesn't exist
        else
            false   ;;; meaning write inaccessible
        endif
    else
        if (_mode _bimask _STM_IFMT) == _STM_IFREG then
            ;;; regular file -- return number of links
            _pint(_statb!ST_NLINK)
        else
            ;;; else return true
            true
        endif
    endif;
    _mode
enddefine;

    /*  Copy a file when creating/deleting files with links.
        The copy has the same mode and access/modified times
    */
define File_copy(file1, file2, keep_time, pop_file_versions);
    lvars file1, file2, buff, f2name, keep_time, _n, _um, _atime, _mtime;
    dlocal
        pop_file_mode, pop_file_versions,
        0 % _extern umask(_0) -> _um, _extern umask(_um) -> %,
        ;
    lstackmem   struct STATB _statb, struct UTIMBUF _utimb,
                stackbuff _nbuf;

    returnunless(sysopen(file1, 0, true) ->> file1) (false);

    ;;; get the old file mode
    _extern[NI] fstat(file1!D_FILE_DESC, _statb) -> ;
    _pint(_statb!ST_MODE _bimask _8:7777) -> pop_file_mode;
    ;;; get the old file times
    _statb!ST_ATIME -> _atime;
    _statb!ST_MTIME -> _mtime;
    ;;; create the new file and do the copy
    file2 -> f2name;
    syscreate(file2, 1, true) -> file2;
    inits(512) -> buff;
    while (fast_sysread(file1, 1, buff, 512) ->> _n) /== 0 do
        fast_syswrite(file2, 1, buff, _n)
    endwhile;
    sysclose(file1);
    sysflush(file2, true);      ;;; flush and sync
    sysclose(file2);
    if keep_time then
        ;;; set the new file's times
        _atime -> _utimb!UTM_ACTIME;
        _mtime -> _utimb!UTM_MODTIME;
        _extern[NI] utime(Encode_sys(f2name,_nbuf), _utimb) ->
    endif;
    true
enddefine;

define sys_file_copy(from_name, to_name);
    lvars from_name, to_name;
    unless File_copy(from_name, to_name, false, pop_file_versions) then
        Syserr_mishap(from_name, to_name, 2, 'CAN\'T COPY FILE')
    endunless
enddefine;

define Move_file_back(file, remove) -> (_nlinks, _mode);
    lvars file, oldfile, remove, _nlinks, _mode = false, _fmode;
    dlocal pop_file_versions;
    unless isinteger(pop_file_versions) and pop_file_versions fi_> 0 then
        1 -> pop_file_versions
    endunless;

    ;;; check write accessible and get number of links / file mode
    File_nlinks(file, true) -> (_nlinks, _fmode);
    ;;; return if doesn't exist / not writeable / not regular file
    returnunless(isinteger(_nlinks) and _nlinks /== 0);

    _pint(_fmode _bimask _8:7777) -> _mode;
    if pop_file_versions fi_<= 1 then
        ;;; must remove name if remove true
        if remove then sysunlink(file, true) -> endif;
        return
    endif;
    Symlink_target(file sys_>< '-') -> oldfile;
    pop_file_versions fi_- 1 -> pop_file_versions;
    if _nlinks fi_<= 1 or caller(1) == Move_file_back then
        ;;; not accessible / doesn't have links / old version
        Move_file_back(oldfile, true) -> (,);   ;;; unlinks the old name
        syslink(file, oldfile, false) -> ;
        sysunlink(file, false) ->
    else
        ;;; file has links
        Move_file_back(oldfile, false) -> (,);  ;;; no need to unlink the old name
        File_copy(file, oldfile, true, 1) ->
    endif
enddefine;      /* Move_file_back */


define syscreate(/*file, accmode, arg3*/) with_nargs 3;

    define lconstant Create(file, fullname, _accmode, _mode);
        lvars file, fullname, _accmode, _mode;

        define lconstant No_umask_Opencreate() with_nargs 4;
            lvars _um;
            dlocal 0 % _extern umask(_0) -> _um, _extern umask(_um) -> %;
            Opencreate()
        enddefine;

        if _int(_accmode) _bitst _:O_EXCL then
            false -> _mode
        else
            returnunless(Move_file_back(fullname, false) -> _mode) (_-1)
        endif;

        fast_chain(file, fullname, _accmode,
                            if isinteger(_mode) then
                                _mode, No_umask_Opencreate
                            else
                                pop_file_mode, Opencreate
                            endif)
    enddefine;

    Sys_open((), Create, true)
enddefine;

endsection;     /* $-Sys$-Io */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar  8 1997
        Added calls of Encode_sys etc.
--- John Gibson, Jun  2 1995
        Added extra follow_symlinks arg to File_nlinks
--- John Gibson, Jun 23 1994
        Fixed bug in Move_file_back caused by last change
--- John Gibson, Jun 13 1994
        Made File_nlinks return file mode as well
--- John Gibson, May 23 1994
        _sys*error -> _ERRNO
--- John Gibson, May 17 1994
        Added [NI] flag on _externs requiring it
--- John Gibson, Jan 29 1994
        Changed syscreate to use new Sys_open (in sysopen.p)
--- John Gibson, Sep 28 1993
        Changed File_copy to use new 2nd arg to sysflush to sync the
        file.
--- Robert John Duncan, Jun 22 1992
        SVR4 supports fsync(2)
--- Robert John Duncan, Jun 16 1992
        System call -close- replaced by -pop_close- from "c_core.c"
--- John Gibson, Sep 14 1991
        Removed N*ull_end
--- John Gibson, Jun 16 1989
        device fullnames now have symbolic links translated
--- John Gibson, Feb 19 1989
        Included io.ph
--- John Gibson, Mar 28 1988
        Moved out of sysio.p
 */
