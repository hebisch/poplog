/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/pop11_proglist_util.p
 > Purpose:         Pop-11 associated utilities for trying/checking the next
 >                  proglist item
 > Author:          John Gibson, May 20 1993 (see revisions)
 > Documentation:   REF * POPCOMPILE
 */

    ;;; N.B. There should be no strong references in this file to the
    ;;; Pop-11 compiler or compiler procedures (So incorporating this file
    ;;; doesn't bring in the compiler. The procedures in this file shouldn't
    ;;; really be called pop11_ at all.)

#_INCLUDE 'declare.ph'
#_INCLUDE 'ident.ph'

constant
        procedure (sys_current_ident)
    ;

section $-Sys$-Prglst;

constant
        procedure (Next_valof),
    ;

vars
        _next_code
    ;

endsection;


;;; ----------------------------------------------------------------------

section $-Sys$-Prglst =>    pop11_try_nextitem, pop11_try_nextreaditem,
                            pop11_need_nextitem, pop11_need_nextreaditem;

define Next_is_expr_opener(nxtitem);
    lvars nxtitem;
    if _next_code _bitst _:M_ID_IS_WORD then
        not(_next_code _bitst _:M_ID_SYNTAX)
        or isprocedure(Next_valof(nxtitem))
    else
        nxtitem /== termin
    endif
enddefine;

define Next_is_equal(nxtitem, choices);
    lvars nxtitem, choices, item;
    returnif(nxtitem == choices) (true);
    if ispair(choices) then
        repeat
            returnif(null(choices)) (false);
            fast_destpair(choices) -> (item, choices);
            returnif(item == nxtitem) (true);
            quitif(isvector(item) and subscrv(1,item) == nxtitem)
        endrepeat
    else
        returnunless(isvector(choices) and subscrv(1,choices) == nxtitem)
                                    (false)
    endif;
    ;;; only if nxtitem is syntax
    returnunless(isword(nxtitem)) (false);
    if testdef sys_current_ident then
        weakref sys_current_ident(nxtitem)
    else
        iscompound(nxtitem!W_IDENTIFIER) and nxtitem!W_IDENTIFIER
    endif -> item;
    item and item!ID_IDENTPROPS _bitst _:M_ID_SYNTAX
enddefine;

define pop11_try_nextitem(choices) -> item;
    lvars item = nextitem(), choices;
    if Next_is_equal(item, choices) then
        Chop()
    else
        false -> item
    endif
enddefine;

define pop11_try_nextreaditem(choices) -> item;
    lvars item = nextreaditem(), choices;
    if Next_is_equal(item, choices) then
        Chop()
    else
        false -> item
    endif
enddefine;

define lconstant Need_mishap(choices);
    lvars item = nextreaditem(), choices;
    mishap(consword('FOUND'), item, consword('READING TO'), choices, 4,
                if Next_is_expr_opener(item) then
                    'mei: MISPLACED EXPRESSION ITEM', 'pop11-mei:syntax'
                else
                    'msw: MISPLACED SYNTAX WORD', 'pop11-msw:syntax'
                endif)
enddefine;

define pop11_need_nextitem(choices) -> item;
    lvars item = pop11_try_nextitem(choices), choices;
    unless item then Need_mishap(choices) endunless
enddefine;

define pop11_need_nextreaditem(choices) -> item;
    lvars item = pop11_try_nextreaditem(choices), choices;
    unless item then Need_mishap(choices) endunless
enddefine;

endsection;     /* $-Sys$-Prglst */


;;; ---------- LISTS OF SYNTAX WORDS IN PROCEDURE HEADERS ---------------

section;

;;; Syntax words that can follow "define" and precede the procedure name
;;; or first formal parameter in the case of infix operations
;;; These are also used by ved_f and ved_tidy
vars define_headers
        = [ updaterof global nonglobal constant vars lconstant lvars dlvars
            iconstant macro syntax active procedure dlocal nonactive :
            define_form ];

;;; Syntax words that can occur after the occurrence of the procedure name
;;; These are used by ved_f

vars define_terminators = [; => -> with_nargs with_props () ^termin];

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 11 1996
        Added Next_is_equal and changed pop11_try/need_nextitem to use
        it.
--- John Gibson, Apr  3 1996
        Added some mishap id-strings.
--- John Gibson, Jun 18 1993
        Moved in define_headers and define_terminators from pop11_syntax.p
 */
