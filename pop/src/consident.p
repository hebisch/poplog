/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/src/consident.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *IDENT
 */

;;; ----------------- CONSTRUCT A FREE IDENTIFIER ------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'ident.ph'

section $-Sys;

constant
        procedure (Idprops_flags, Make_idval_undef)
    ;


endsection;


;;; ---------------------------------------------------------------------

section $-Sys => consident;

define consident(idprops, _const, kind) -> id;
    lvars id, kind, idprops, _idprops, _num, _const, _nodef = false;
    if isinteger(_const) then
        _int(_const) -> _const;
        _const _bitst _2:10 -> _nodef;
        _const _bitst _2:01 -> _const
    endif;
    Idprops_flags(false, idprops, _const) -> _num -> _idprops;
    if kind == "lextoken" then
        "lex" -> kind;
        _idprops _biset _:M_ID_LEX_TOKEN -> _idprops
    endif;
    Get_record(ident_key) -> id;
    0 -> fast_idval(id);        ;;; make safe for GC
    _num -> id!ID_NUM_ATTRIBUTE;
    if kind == "lex" then
        _0 -> id!ID_LEX_FLAGS;
        _idprops _biset _:M_ID_LEX -> id!ID_IDENTPROPS
    elseif kind == "perm" then
        if _nodef then _:M_PERM_NOT_DEF else _0 endif -> id!ID_PERM_FLAGS;
        _idprops -> id!ID_IDENTPROPS
    else
        mishap(kind, 1, 'UNKNOWN IDENTIFIER KIND')
    endif;
    Make_idval_undef(id)
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Dec 17 1989
        Removed construction of short identifiers (no longer possible with
        new pop pointers).
--- John Gibson, May 15 1989
        Included ident.ph
--- John Gibson, Mar 13 1988
        Moved out of ident.p
 */
