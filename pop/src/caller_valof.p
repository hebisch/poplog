/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/caller_valof.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

;;; ---------------- CALLER_VALOF AND UPDATER ------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'ident.ph'

section $-Sys;

constant
        procedure (Get_perm_ident, Checkr_upd_idval)
    ;

endsection;

;;; -----------------------------------------------------------------------

section $-Sys => caller_valof, nonactive_caller_valof;

define lconstant Get_local_address(id, target, _nonactive) -> _lastfound;
    lvars p, id, target, _sframe, _offs, _lastfound, _sflim, _nonactive;
    id@ID_VALOF -> _lastfound;          ;;; global if nowhere local

    _nextframe(_caller_sp_flush()) -> _sframe;
    _call_stack_seg_hi -> _sflim;

    repeat
        if _sframe == _sflim then
            quitif(_sflim == _call_stack_hi);
            _sframe!SF_NEXT_SEG_HI -> _sflim;
            _sframe!SF_NEXT_SEG_SP -> _sframe
        endif;

        _sframe!SF_OWNER -> p;
        if issimple(target) then
            returnif(target == 0);
            target fi_- 1 -> target
        else
            returnif(p == target)
        endif;

        ;;; search intervening procedure locals to find if id is a local
        ;;; returns additive offset if so, zero if not
        if _nonzero(Dlocal_frame_offset(id, p, _nonactive) ->> _offs) then
            _sframe@(csword){_offs}@(csword->w) -> _lastfound
        endif;

        _nextframe(_sframe) -> _sframe
    endrepeat
enddefine;

define lconstant Caller_valof(id, target, _nonactive) with_props caller_valof;
    lvars id, target, _valptr, _m, _nonactive;
    unless isident(id) then Get_perm_ident(id, false) -> id endunless;
    if id!ID_IDENTPROPS _bitst _:M_ID_ACTIVE and not(_nonactive) then
        Get_local_address(id, target, false) -> _valptr;
        if _valptr == id@ID_VALOF then
            ;;; not local anywhere -- call procedure to get value(s)
            fast_apply(fast_idval(id))
        else
            if id!ID_IDENTPROPS _bitst _:M_ID_OPERATOR then
                _1
            else
                id!ID_NUM_ATTRIBUTE
            endif -> _m;
            _valptr@(w->csword) -> _valptr;
            until _zero(_m) do
                _valptr!(csword)++ -> _valptr;
                _m _sub _1 -> _m
            enduntil
        endif
    else
        Get_local_address(id, target, true)!(w)
    endif
enddefine;
;;;
define updaterof Caller_valof(/*newvalue,*/ word, target, _nonactive)
                                        with_nargs 4 with_props caller_valof;
    lvars id, word, target, _valptr, _m, _idprops, _nonactive;
    if isident(word) then
        word, false -> word
    else
        Get_perm_ident(word, false)
    endif -> id;
    id!ID_IDENTPROPS -> _idprops;

    if _idprops _bitst _:M_ID_ACTIVE and not(_nonactive) then
        Get_local_address(id, target, false) -> _valptr;
        if _valptr == id@ID_VALOF then
            ;;; not local anywhere -- call procedure to assign value(s)
            () -> fast_apply(fast_idval(id))
        else
            if _idprops _bitst _:M_ID_OPERATOR then
                _1
            else
                id!ID_NUM_ATTRIBUTE
            endif -> _m;
            _valptr@(w->csword)[_m] -> _valptr;
            until _zero(_m) do
                () -> _valptr--!(csword) -> _valptr;
                _m _sub _1 -> _m
            enduntil
        endif
    else
        if _idprops _bitst _:M_ID_ASSIGN_CHECK then
            Checkr_upd_idval((), id, word)
        endif;
        () -> Get_local_address(id, target, true)!(w)
    endif
enddefine;

define caller_valof             = Caller_valof(% false %) enddefine;
define nonactive_caller_valof   = Caller_valof(% true %)  enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, May 27 1995
        Changes to make it cope with active variables
--- John Gibson, Jan 16 1993
        Now allows idents.
--- John Gibson, Oct  6 1992
        Extra arg for Get_perm_ident
--- John Gibson, Nov  9 1989
        Changes for segmented callstack (allowing external callback)
--- John Gibson, May 15 1989
        Included ident.ph
--- John Gibson, Jan 29 1989
        Uses new procedure -Get_perm_ident-
--- John Gibson, Jul 16 1988
        Replaced _caller_sp() with _caller_sp_flush()
--- John Gibson, Mar 13 1988
        Renamed caller_valof.p (previously cllrvalof.p)
--- John Gibson, Oct 31 1987
        Replaced callstack.ph macros with csword-type operations
 */
