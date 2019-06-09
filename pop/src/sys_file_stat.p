/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.unix/src/sys_file_stat.p
 > Purpose:         sys_file_stat   (unix)
 > Author:          John Gibson (see revisions)
 */


#_INCLUDE 'declare.ph'
#_INCLUDE 'io.ph'
#_INCLUDE 'unixdefs.ph'

section $-Sys;

constant
        procedure (Check_vector)
    ;

endsection;

uses (biginteger_key);


;;; ---------------------------------------------------------------------

section $-Sys => sys_file_stat;

define sys_file_stat(file, statbuff) -> statbuff;
    lvars file, follow_symlinks = true, statbuff, len, _arg, _res, _routine;
    lstackmem struct STATB _stbuf, stackbuff _nbuf;

    if isboolean(statbuff) then
        ((), file, statbuff) -> (file, statbuff, follow_symlinks)
    endif;
    Check_vector(statbuff);
    if isstring(file) or isword(file) then
        sysfileok(file) -> file;
        if follow_symlinks then _extern stat else _extern lstat endif
    else
        Check_device(file, 2:1001);     ;;; flush if open for writing
        _extern fstat
    endif -> _routine;

    repeat
        if _routine == _extern fstat then
            file!D_FILE_DESC
        else
            Encode_sys(file, _nbuf)
        endif -> _arg;
        _extern[INDIR, SE] _routine(_arg, _stbuf) -> _res;
        quitunless(_neg(_res) and _ERRNO == _:EINTR);
        _CHECKINTERRUPT
    endrepeat;
    returnif(_neg(_res)) (false -> statbuff);

    datalength(statbuff) -> len;
;;;        printf(len, 'statbuff len: %p\n');
    returnif(len == 0);

    go_on len to ONE TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE else TEN;
TEN:
    ;;; last status change time
    Sint_->_pint(_stbuf!ST_CTIME)   -> fast_subscrv(10,statbuff);

NINE:
    ;;; last accessed time
    Sint_->_pint(_stbuf!ST_ATIME)   -> fast_subscrv(9,statbuff);

EIGHT:
    ;;; inode
    Uint_->_pint(_stbuf!ST_INO)     -> fast_subscrv(8,statbuff);

SEVEN:
    ;;; major/minor device
#_IF ##(w)[_1|dev_t] = _2
    Fld$-Double_val_u(_stbuf@ST_DEV)
#_ELSE
    Uint_->_pint(_stbuf!ST_DEV)
#_ENDIF                             -> fast_subscrv(7,statbuff);

SIX:
    ;;; number of links
    _pint(_stbuf!ST_NLINK)          -> fast_subscrv(6,statbuff);

FIVE:
    ;;; mode flags
    _pint(_stbuf!ST_MODE)           -> fast_subscrv(5,statbuff);

FOUR:
    ;;; user id
;;; _pint(_stbuf!ST_UID)            -> fast_subscrv(4,statbuff);
    Sint_->_pint(_stbuf!ST_UID)         -> fast_subscrv(4,statbuff);

THREE:
    ;;; group id
;;; _pint(_stbuf!ST_GID)            -> fast_subscrv(3,statbuff);
    Sint_->_pint(_stbuf!ST_GID)         -> fast_subscrv(3,statbuff);

TWO:
    ;;; last modified time
    Sint_->_pint(_stbuf!ST_MTIME)   -> fast_subscrv(2,statbuff);
;;;        _pint(_stbuf!ST_MTIME)   -> fast_subscrv(2,statbuff);
;;;        printf(_pint(_stbuf!ST_MTIME), '_pint(_stbuf!ST_MTIME) = %p\n');
;;;        printf(fast_subscrv(2,statbuff), 'fast_subscrv(2,statbuff) = %p\n');
ONE:
    ;;; file size
    Sint_->_pint(_stbuf!ST_SIZE)    -> fast_subscrv(1,statbuff);
;;;        printf(statbuff, 'statbuff = %p\n');
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 20 1999
        Changes to fix x86 Linux "dev_t" type problem
--- John Gibson, Mar  8 1997
        Added Encode_sys call
--- John Gibson, Jun  2 1994
        Rewritten to use lstackmem instead of global _s*tat_buf
--- John Gibson, May 10 1994
        Added interrupt checking around _extern call
--- John Williams, Aug  7 1992
        Got _stat and _lstat the wrong way round
--- John Williams, Aug  7 1992
        Optional extra boolean argument follow_symlinks added
--- John Gibson, Sep 14 1991
        Removed N*ull_end
--- John Williams, Mar 18 1991
        Now allows words as well as strings (fixes BR isl-er.151).
 */
