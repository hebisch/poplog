/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/src/rtlex_ident.p
 > Purpose:
 > Author:          John Gibson, Mar 10 1988 (see revisions)
 */

;;; ---------- CONSTRUCTING RUN-TIME LEXICAL IDENTIFIERS -------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'ident.ph'

;;; ------------------------------------------------------------------------

section $-Sys;

    /*  Construct _n (> 0) untyped idents for initialising non-local lvars
    */
define Cons_rt_idents_untyped(_n);
    lvars _id, _n = _int(_n);
    _CHECKUSER;
    Get_store(@@(struct IDENT)[_n]) -> _id;
    ;;; initialise and push them
    repeat
        ident_key -> _id!KEY;
        0 -> fast_idval(_id);
        _0 -> _id!ID_INFO;
        _id;
        quitif(_zero(_n _sub _1 ->> _n));
        _id@(struct IDENT)++ -> _id
    endrepeat
enddefine;

/* Old name -- can be removed after new pop pointers are in */
constant procedure Cons_rt_short_idents = Cons_rt_idents_untyped;

    /*  Construct a total of _n (> 0) idents for initialising non-local lvars.
        -idlist- is a non-empty list of m typed lextoken idents; construct
        these m with appropriate identprops, followed by _n - m (possibly 0)
        untyped ones.
    */
define Cons_rt_idents(idlist, _n);
    lvars id, idlist, _id, _n = _int(_n);
    _CHECKUSER;
    Get_store(@@(struct IDENT)[_n]) -> _id;
    ;;; initialise and push the typed ones first
    repeat
        ident_key -> _id!KEY;
        0 -> fast_idval(_id);
        fast_front(idlist) -> id;
        id!ID_INFO -> _id!ID_INFO;
        _id!ID_IDENTPROPS _biclear _:M_ID_LEX_TOKEN -> _id!ID_IDENTPROPS;
        _id;
        fast_back(idlist) -> idlist;
        quitif(idlist == []);
        _id@(struct IDENT)++ -> _id;
        _n _sub _1 -> _n
    endrepeat;
    ;;; then the remaining untyped ones
    until _zero(_n _sub _1 ->> _n) do
        _id@(struct IDENT)++ -> _id;
        ident_key -> _id!KEY;
        0 -> fast_idval(_id);
        _0 -> _id!ID_INFO;
        _id
    enduntil
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Dec 17 1989
        Removed use of short idents (no longer possible with new pop
        pointers), and extended -Cons_rt_idents- to deal with both
        typed and untyped.
--- John Gibson, May 15 1989
        Included ident.ph
 */
