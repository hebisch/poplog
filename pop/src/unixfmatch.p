/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.unix/src/unixfmatch.p
 > Purpose:
 > Author:          John Gibson, Jun 12 1987 (see revisions)
 > Documentation:
 > Related Files:
 */

;;;---------------- MATCHING FILES IN DIRECTORIES ---------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'unixdefs.ph'

constant
        procedure (Sys$-Bcmp16),
        _scmp
    ;

section $-Sys$-Dir;

constant
        procedure (Read_entry, Explode_name, Open_stream, Close_stream,
        Decode_dir_entry)
    ;

endsection;

;;; -----------------------------------------------------------------------

section $-Sys$-Dir => sys_match_filename, sys_matchin_dir;

lconstant macro (
    M_CHAR          = 1,        ;;; single character
    M_STRING        = 2,        ;;; string of characters
    M_NCHARS        = 3,        ;;; fixed number of characters
    M_STAR          = 4,        ;;; zero or more characters
    M_VERS          = 5,        ;;; matching version only (0 or more trailing -'s)
    M_OR            = 6,        ;;; OR node
    M_XOR           = 7,        ;;; Exclusive OR node
    M_NOT           = 8,        ;;; NOT node
    M_CHAR_RANGE    = 9,        ;;; range of characters
    );

lvars
    _match_start,
    _match_end,
    _match_furthest,
    ;

define :inline lconstant MATCH_AT(T, MATCHP, S8_CMP, S16_CMP);
    until patt == [] do
        go_on fast_destpair(patt) -> patt to
            m_CHAR m_STRING m_NCHARS m_STAR m_VERS m_OR m_XOR m_NOT
                    m_CHAR_RANGE else m_ERR;
        m_CHAR:
            nextif(
                _cptr <@(T) _match_end
                and _int(fast_destpair(patt)->patt) == (_cptr!(T)++ -> _cptr));
            return(false);

        m_STRING:
            fast_destpair(patt) -> (_item, patt);
            returnif(##(T){_match_end, _cptr} _lt _item!V_LENGTH) (false);
            if _item!KEY!K_FLAGS _bitst _:M_K_STRING16 then
                S16_CMP(@@(T)[_item!V_LENGTH], _cptr, _item@V_SHORTS)
            else
                S8_CMP(@@(b)[_item!V_LENGTH], _item@V_BYTES, _cptr)
            endif;
            returnunless() (false);
            _cptr@(T)[_item!V_LENGTH] -> _cptr;
            nextloop;

        m_NCHARS:
            _int(fast_destpair(patt) -> patt) -> _item;
            returnif(
                ##(T){_match_end, _cptr} _lt _item
                or (_cptr == _match_start and _cptr!(T) == _:`.`)) (false);
            _cptr@(T)[_item] -> _cptr;
            nextloop;

        m_STAR:
            ;;; don't match . at beginning
            returnif(_cptr == _match_start and _cptr <@(T) _match_end
                        and _cptr!(T) == _:`.`) (false);
            while _cptr <@(T) _match_end do
                returnif(MATCHP(_cptr, patt)) (true);
                _cptr@(T)++ -> _cptr
            endwhile;
            nextloop;

        m_VERS:
            _cptr -> _item;
            while _item <@(T) _match_end do
                returnif((_item!(T)++ -> _item) /== _:`-`) (false)
            endwhile;
            returnif(_cptr!(T)[_-1] == _:`-`) (false);
            nextloop;

        m_XOR:
            "return";               ;;; make it return if it succeeds
            fast_destpair(patt) -> patt -> _item;
            until _item == [] do
                if MATCHP(_cptr, fast_destpair(_item) -> _item) then
                    ;;; carry on with the continuation
                    ;;; (no option to try the other alternatives)
                    _match_furthest -> _cptr;
                    nextloop(2)
                endif
            enduntil;
            () -> ;                 ;;; remove the "return" continuation
            return(false);

        m_OR:
            fast_back(patt);        ;;; leave continuation on stack
            fast_front(patt) -> patt;
            until patt == [] do
                returnif(MATCHP(_cptr, fast_destpair(patt)->patt)) (true)
            enduntil;
            () -> ;                 ;;; remove the continuation
            return(false);

        m_NOT:
            "return";               ;;; make it return if it succeeds
            returnif(MATCHP(_cptr, fast_front(patt))) (false);
            () -> ;                 ;;; remove the "return" continuation
            ;;; carry on with the continuation, skipping _item chars
            fast_back(patt) -> patt;
            goto m_NCHARS;

        m_CHAR_RANGE:
            if _cptr <@(T) _match_end then
                _cptr!(T) -> _item;
                if _int(fast_destpair(patt)->patt) _lteq _item
                and _item _lteq _int(fast_destpair(patt)->patt)
                then
                    _cptr@(T)++ -> _cptr;
                    nextloop
                endif;
            endif;
            return(false);

        m_ERR:
            mishap(0, 'INVALID TYPE IN FILENAME PATTERN');
    enduntil;

    ;;; get next continuation off stack
    if () ->> patt then
        if patt == "return" then
            _cptr -> _match_furthest;
            return(true)
        elseif MATCHP(_cptr, patt) then
            return(true)
        else
            patt;               ;;; put continuation back
            return(false)
        endif;
    else
        false;
        return(_cptr >=@(T) _match_end);
    endif
enddefine;

define lconstant Match_at8(_cptr, patt);
    lvars patt, _cptr, _item;
    MATCH_AT(b, Match_at8, _bcmp, Bcmp16)
enddefine;

define lconstant Match_at16(_cptr, patt);
    lvars patt, _cptr, _item;
    MATCH_AT(s, Match_at16, Bcmp16, _scmp)
enddefine;


define sys_match_filename(name, patt) -> _result;
    lvars name, patt, _cptr, _len = name!V_LENGTH, _result;
    false;                      ;;; marks end of continuations on stack
    if name!KEY!K_FLAGS _bitst _:M_K_STRING16 then
        name@V_SHORTS -> _cptr;
        _cptr@(s)[_len] -> _match_end;
        Match_at16(_cptr ->> _match_start, patt)
    else
        name@V_BYTES -> _cptr;
        _cptr@(b)[_len] -> _match_end;
        Match_at8(_cptr ->> _match_start, patt)
    endif -> (, _result)
enddefine;


    /*  Match entries in directory dir against patt, returning a
        list of entries according to the mode argument.
        Returns <false> if dir doesn't exist/isn't a directory.
    */

    ;;; Bits in mode argument
lconstant macro (
    SUBDIR      = 2:1e0,    ;;; return sub-directories that match
    ORDFILE     = 2:1e1,    ;;; return ordinary files that match
    SUBDIR_LIST = 2:1e2,    ;;; return a separate list of ALL
                            ;;; sub-directories (excluding . and ..)
    );

define sys_matchin_dir(dir, patt, _mode);
    lvars   dirlist, patt, string, dir, changedir = false, is16,
            _cptr, _mode = _int(_mode), _match, _fmode, _dir_entry, _dirp;
    dlocal  current_directory;
    lstackmem s_stackbuff _sbuf;

    define lconstant Fmode(_dir_entry);
        lvars _dir_entry;
        lstackmem struct STATB _statb;
        _extern[NI] stat(_dir_entry@DIR_NAME, _statb) -> ;
        if (_statb!ST_MODE _bimask _STM_IFMT) == _STM_IFDIR then
            _:SUBDIR
        else
            _:ORDFILE
        endif
    enddefine;

    if dir = nullstring then '.' -> dir endif;
    returnif((Open_stream(dir) ->> _dirp) == _NULL) (false);

    if _mode _bitst _:SUBDIR_LIST then
        _mode _biclear _:SUBDIR_LIST -> _mode;
        []
    else
        false
    endif -> dirlist;
    if patt = #_< [^M_STAR] >_# then false -> patt endif;

    [%  while (Read_entry(_dirp) ->> _dir_entry) /== _NULL do
            Decode_dir_entry(_dir_entry, _sbuf) -> (_cptr, _match_end, is16);
            if patt then
                ;;; false marks end of continuations
                (false, _cptr ->> _match_start, patt);
                if is16 then Match_at16() else Match_at8() endif -> (, _match)
            else
                ;;; match everything not starting with `.`
                if is16 then _cptr!(s) else _cptr!(b) endif /== _:`.` -> _match
            endif;
            nextunless(_match or dirlist);

            if dirlist or _mode /== (_:SUBDIR _biset _:ORDFILE) then
                ;;; need to determine what the entry is
                unless changedir then
                    dir -> current_directory;
                    true -> changedir
                endunless;
                Fmode(_dir_entry) -> _fmode;
                nextunless(_match or _fmode == _:SUBDIR);
                if _match and not(_mode _bitst _fmode) then
                    false -> _match
                endif
            endif;
            consstring(#| Explode_name(_cptr, _match_end, is16) |#) -> string;
            if _match then string endif;
            if dirlist and _fmode == _:SUBDIR
            and string /= '.' and string /= '..' then
                string :: dirlist -> dirlist
            endif;
            _checkall()
        endwhile
    %];
    if dirlist then dirlist endif;
    Close_stream(_dirp)
enddefine;

endsection;     /* $-Sys$-Dir */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 13 1997
        Char encoding changes
--- Poplog System, Jan 18 1995 (Julian Clinton)
        Changes for Linux.
--- John Gibson, May 17 1994
        Added [NI] flag on _externs requiring it
--- Robert John Duncan, Jul 22 1992
        Added case for SVR4
--- John Gibson, May 29 1990
        Corrected problem with M_XOR matching and revised operation of
        M_NOT.
--- John Gibson, Aug 22 1989
        Bobcat now has BERKELEY set
--- John Gibson, Jul 16 1987
        Fixed -Match_at- to make * and ? compatible with Unix by not
        matching . at the beginning of a name. Also cleaned up.
--- Aled Morris, May 18 1987
        Bobcat fix
 */
