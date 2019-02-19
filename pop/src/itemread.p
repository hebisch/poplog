/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/itemread.p
 > Purpose:
 > Author:          John Gibson, Mar  8 1988 (see revisions)
 > Documentation:   REF *PROGLIST
 */

;;; ------- READING ITEMS FROM PROGLIST WITH MACRO EXPANSION ------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'ident.ph'

constant
        procedure (isundef, sys_current_ident, sys_autoload,
        Sys$-Prglst$-Unstack, Sys$-Prglst$-Reset
        )
    ;

vars
        procedure (sysdeclare), poplastitem, pop_autoload,
        Sys$-Prglst$- _next_code
    ;


;;; --------------------------------------------------------------------

section $-Sys$-Prglst => proglist_macro_pair, nonmac, nextitem, itemread;

vars
    proglist_macro_pair,
    was_nonmac      = false,
    next_ident      = false,
    ;


define Next_valof(word);
    lvars word;
    if not(_next_code _bitst _:M_ID_LEX)
    and next_ident!ID_PERM_FLAGS _bitst _:M_PERM_NOT_DEF then
        returnif(vm_pas_mode == "popc") (valof("popc_synmac_idval")(word));
        sysdeclare(word)
    endif;
    idval(next_ident)
enddefine;

define macro nonmac;
    proglist -> was_nonmac
enddefine;

define nextitem() -> item;
    lvars item, id;

    define lconstant Expand_macro(item);
        lvars item, slength, numargs;
        dlocal proglist_macro_pair = proglist;
        fast_back(proglist) -> proglist;

        ;;; call the macro if a procedure
        ;;; concatenate to proglist if a list
        ;;; otherwise put the valof on proglist
        _CHECKINTERRUPT;
        if isprocedure(item) then
            pdnargs(item) -> numargs;       ;;; number of args
            stacklength() -> slength;
            while numargs fi_> 0 do
                itemread();                 ;;; get macro args
                numargs fi_- 1 -> numargs;
            endwhile;
            fast_apply(item);               ;;; apply macro
            stacklength() fi_- slength -> slength;
            while slength fi_> 0 do
                conspair(proglist) -> proglist;
                slength fi_- 1 -> slength
            endwhile
        elseif islist(item) then
            item <> proglist -> proglist
        elseif isundef(item) then
            mishap(item, 1, 'MACRO IDENTIFIER HAS UNDEFINED VALUE')
        else
            conspair(item, proglist) -> proglist
        endif
    enddefine;

    ;;; The next rather strange test is because calling -null- on -proglist-
    ;;; can actually change it (due to recursive calls of itemread,
    ;;; readitem etc inside xved inside the dynamic list procedure)
    if null(proglist ->> item) or (proglist /== item and null(proglist)) then
        if Unstack() then chain(nextitem) endif;
        termin -> item;
        _:M_ID_TERMIN -> _next_code;
        return
    elseunless isword(fast_front(proglist) ->> item) then
        _0 -> _next_code;
        return
    endif;


    lconstant macro HAS_ID = [
        (if testdef sys_current_ident then
            weakref sys_current_ident(item, true) ->> id
         else
            iscompound(item!W_IDENTIFIER ->> id)
         endif)
    ];

    if HAS_ID
    or (vm_pas_mode == "popc" and weakref pop_autoload
        and (valof("popc_auto_declare")(item),
                iscompound(item!W_IDENTIFIER ->> id)))
    or (testdef sys_autoload and (weakref sys_autoload(item) ->, HAS_ID))
    then
        id -> next_ident;
        id!ID_IDENTPROPS _biset _:M_ID_IS_WORD -> _next_code;
        if id!ID_IDENTPROPS _bitst _:M_ID_MACRO and proglist /== was_nonmac
        then
            ;;; expand macro
            false -> was_nonmac;
            Expand_macro(Next_valof(item));
            chain(nextitem)
        endif
    else
        _:M_ID_IS_WORD -> _next_code
    endif
enddefine;

define Chop();
    unless _next_code == _:M_ID_TERMIN then
        fast_front(proglist) -> poplastitem;
        fast_back(proglist) -> proglist;
        false -> was_nonmac
    endunless
enddefine;

define itemread();
    nextitem();
    if _next_code == _:M_ID_TERMIN then
        Reset()
    else
        Chop()
    endif
enddefine;

endsection;     /* $-Sys$-Prglst */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep 16 1996
        Changed call of sys_current_ident to pass true for the new
        'try no-fast' 2nd arg.
--- John Gibson, Aug 16 1993
        Test for Popc now vm_pas_mode == "popc"
--- John Gibson, Nov 15 1992
        Change to nextitem for POPC
--- John Gibson, Oct 12 1992
        Added POPC code in Next_valof
--- John Gibson, Sep  4 1991
        Fixed horrible problem with null(proglist) -- see comment in code.
--- John Gibson, Aug 23 1991
        Moved _next_code to readitem.p
--- John Gibson, Aug 27 1990
        Changed n*ote to weak
--- John Gibson, May 15 1989
        Included ident.ph
--- John Gibson, May 14 1989
        Fixed -nextitem- so that procedures in -popliblist- can declare
        lexical macros.
--- John Gibson, Jan 29 1989
        Changes for 'weak' declarations
 */
