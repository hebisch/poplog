/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/src/list_hdtl.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *LISTS
 */

;;; ------------------- HEAD AND TAIL OF LISTS ---------------------------

#_INCLUDE 'declare.ph'

constant
        procedure (Sys$-Dynamic_add_end, Sys$-Checkr_list_tl)
    ;

;;; ---------------------------------------------------------------------

section $-Sys => hd, tl, dest;

    ;;; check for and return list pair (much the same as -null-)
define lconstant Checkr_list(item) -> item;
    lvars item, work, _savelen;
    if ispair(item) and iscompound(fast_back(item) ->> work) then
        if work!KEY == pair_key or work == [] then
            return
        elseif work!KEY == procedure_key then
            ;;; dynamic list
            if fast_front(item) then      ;;; not (yet) dead
                _stklength() -> _savelen;
                ;;; N.B. Applying this procedure can recursively call -null-
                ;;; or -hd- etc on -item-, causing the list to be expanded
                ;;; when it returns. (This is awful, but we have to allow for
                ;;; it since xved can be called inside sysread etc.)
                fast_apply(work);
                _stklength() _sub _savelen -> _savelen;
                if _savelen /== @@(w)[_1] then
                    mishap(work,
                        if _savelen _slteq _0 then
                            1, 'NO RESULT FROM DYNAMIC LIST PROCEDURE'
                        else
                            _pint(##(w){_savelen} _add _1),
                            'MORE THAN 1 RESULT FROM DYNAMIC LIST PROCEDURE'
                        endif)
                endif;
                ;;; result on stack
                if fast_back(item) /== work then
                    ;;; back got replaced by recursive expansion
                    Dynamic_add_end((), item);
                    return
                elseif dup() == termin then
                    ->, false -> fast_front(item)
                else
                    () -> fast_front(item);
                    conspair(true, work) -> fast_back(item);
                    return
                endif
            endif;
            ;;; come here if dead dynamic list
            mishap(item, 1, 'NON-EMPTY LIST NEEDED')
        endif
    endif;
    mishap(item, 1, if item == [] then 'NON-EMPTY LIST NEEDED'
                    else 'LIST NEEDED' endif)
enddefine;

define hd() with_nargs 1;
    fast_front(Checkr_list())
enddefine;
;;;
define updaterof hd() with_nargs 2;
    -> fast_front(Checkr_list())
enddefine;

define tl() with_nargs 1;
    fast_back(Checkr_list())
enddefine;
;;;
define updaterof tl() with_nargs 2;
    lvars list;
    Checkr_list() -> list;
    Checkr_list_tl() -> fast_back(list)
enddefine;

define dest() with_nargs 1;
    lvars list;
    Checkr_list() -> list;
    fast_front(list), fast_back(list)
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug  4 1993
        Changed 'MORE THAN 1 RESULT FROM DYNAMIC LIST PROCEDURE' mishap in
        Checkr_list so that all results returned are given in INVOLVING list
--- John Gibson, Sep  4 1991
        Fixed Checkr_list to cope with recursive expansion of dynamic list
        through call to dynamic list procedure
--- John Gibson, Sep  2 1991
        Fixed problem in -Checkr_list-
--- John Gibson, Apr  9 1988
        Moved out of old lists.p
 */
