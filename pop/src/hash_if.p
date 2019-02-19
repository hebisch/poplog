/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/hash_if.p
 > Purpose:
 > Author:          John Gibson, Mar  7 1988 (see revisions)
 > Documentation:   REF *PROGLIST
 */

;;; ----------- PROGLIST MACROS THAT USE THE COMPILER ------------------------

#_INCLUDE 'declare.ph'

constant
        procedure (pop11_comp_expr, pop11_comp_stmnt_seq_to,
        pop11_exec_compile, isincharitem, discin_device)
    ;


;;; ------------------------------------------------------------------------

section $-Sys$-Prglst => #_IF, #_ELSEIF, #_ELSE, #_ELSE_ERROR, #_ENDIF,
                         #_TERMIN_IF,
                         #_<, >_# ;


;;; --- CONDITIONAL ITEM-READING --------------------------------------------

vars
    #_if_stack  = [];

lconstant
    IGNORE      = 2:001,
    IGNOREREST  = 2:010,
    GOTELSE     = 2:100,
    ;

define lconstant Ignore_clause();
    lvars item;
    lconstant #_names = [#_IF #_ELSEIF #_ELSE #_ELSE_ERROR #_ENDIF];
    hd(#_if_stack) || IGNORE -> hd(#_if_stack);
    until fast_lmember(readitem() ->> item, #_names) do
        if item == termin then
            mishap(0, 'UNTERMINATED #_IF')
        endif
    enduntil;
    chain(valof(item))
enddefine;

define lconstant eval_condition();
    dlocal  popclosebracket = "\n", popnewline = true, pop_pas_mode = false;
    pop11_exec_compile(pop11_comp_expr, false);
    if nextitem() == popclosebracket then
        Chop()
    endif
enddefine;

define lconstant Get_condition(iselse);
    lvars item, iselse;

    if hd(#_if_stack) &&/=_0 IGNOREREST then
        chain(Ignore_clause)
    else
        hd(#_if_stack) &&~~ IGNORE -> hd(#_if_stack)
    endif;
    if iselse or eval_condition() then
        hd(#_if_stack) || IGNOREREST -> hd(#_if_stack)
    else
        chain(Ignore_clause)
    endif
enddefine;

define macro #_IF;
    if #_if_stack /== [] and hd(#_if_stack) &&/=_0 IGNORE then
        IGNOREREST
    else
        0
    endif :: #_if_stack -> #_if_stack;
    chain(false, Get_condition)
enddefine;

define macro #_ELSEIF;
    if #_if_stack == [] or hd(#_if_stack) &&/=_0 GOTELSE then
        mishap(0, 'UNEXPECTED #_ELSEIF')
    endif;
    chain(false, Get_condition)
enddefine;

define macro #_ELSE;
    if #_if_stack == [] or hd(#_if_stack) &&/=_0 GOTELSE then
        mishap(0, 'UNEXPECTED #_ELSE')
    endif;
    hd(#_if_stack) || GOTELSE -> hd(#_if_stack);
    chain(true, Get_condition)
enddefine;

define macro #_ELSE_ERROR;
    if #_if_stack == [] or hd(#_if_stack) &&/=_0 GOTELSE then
        mishap(0, 'UNEXPECTED #_ELSE_ERROR')
    elseif hd(#_if_stack) &&=_0 IGNOREREST then
        mishap(0, 'NO CLAUSE SATISFIED IN #_IF')
    else
        hd(#_if_stack) || GOTELSE -> hd(#_if_stack);
        chain(Ignore_clause)
    endif
enddefine;

define macro #_ENDIF;
    if #_if_stack == [] then
        mishap(0, 'UNEXPECTED #_ENDIF')
    else
        tl(#_if_stack) -> #_if_stack;
        if #_if_stack /== [] and hd(#_if_stack) &&/=_0 IGNORE then
            chain(Ignore_clause)
        endif
    endif
enddefine;

define macro #_TERMIN_IF;
    lvars char_rep, dev;
    if eval_condition() then
        if (isincharitem(readitem, true) ->> char_rep)
        and (discin_device(char_rep, false) ->> dev) then
            ;;; close the device
            sysclose(dev)
        endif;
        [] ->> proglist -> #_if_stack
    endif
enddefine;


;;; --- INSERTING POP-11 EXPRESSIONS INTO PROGLIST --------------------------

define macro #_<;
    pop11_exec_compile(termin, pop11_comp_stmnt_seq_to, true) ->
enddefine;

protected vars macro >_# = termin;


endsection;     /* Sys$-Prglst */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 15 1996
        Changed #_TERMIN_IF so that it closes an underlying device in
        proglist (otherwise, these devices get left open).
--- John Gibson, Aug 16 1993
        pop_pas_mode now an active var
--- John Gibson, Oct 19 1992
        Added #_TERMIN_IF
--- John Gibson, Jul  7 1990
        Replaced -C*ompile_time_eval- with -pop11_exec_compile-
--- John Gibson, Jun  5 1989
        Added #_ELSE_ERROR
 */
