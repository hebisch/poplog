/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/src/list_dl.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *LISTS
 */

;;; -------------------- DESTRUCTING A LIST -------------------------------

#_INCLUDE 'declare.ph'

;;; ----------------------------------------------------------------------

define dl(list);
    lvars list;
    until null(list) do
        _CHECKUSER;
        fast_front(list);
        fast_back(list) -> list
    enduntil
enddefine;
;;;
define updaterof dl(list);
    lvars list, _offs, _n = listlength(list), _len;
    stacklength() -> _len;
    if (_n fi_- _len ->> _len) fi_> 0 then
        ;;; simulate what would happen filling the list in reverse
        ;;; this is a bit silly, but consistent
        fast_repeat _len times fast_back(list) -> list endfast_repeat;
        _n fi_- _len -> _n
    endif;
    @@(w)[_int(_n)] -> _offs;
    until _zero(_offs) do
        --@@(w){_offs} -> _offs;
        _user_sp()!(w){_offs} -> fast_front(list);
        fast_back(list) -> list
    enduntil;
    erasenum(_n);
    if _len fi_> 0 then
        ;;; produce stack empty mishap!
        ->
    endif
enddefine;

define maplist(list, pdr);
    lvars list, pdr;
    [% applist(list, pdr) %]
enddefine;
;;;
define updaterof maplist(vals, list, pdr);
    lvars vals, list, pdr;
    dl(vals) -> applist(list, pdr)
enddefine;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  9 1988
        Moved out of old lists.p
 */
