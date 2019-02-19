/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/src/sys_autoload.p
 > Purpose:
 > Author:          John Gibson, Mar  7 1988 (see revisions)
 > Documentation:   REF *LIBRARY
 */

;;; ---------------- AUTOLOADING IDENTIFIERS -------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'ident.ph'

constant
        procedure (syslibcompile, sys_grbg_destpair, word_identifier,
        syscancel
        )
    ;

vars
        pop_autoload
    ;

;;; ----------------------------------------------------------------------

define sys_autoload_action =
    newproperty([], 8, false, "perm")
enddefine;

lvars
    autoload_list = [];

define sys_autoload(word);
    dlvars word, id, autoload_p;

    define lconstant action(context);
        lvars context;
        if context == 1 then
            word :: autoload_list -> autoload_list
        endif
    enddefine;
    ;;;
    define updaterof action(context);
        lvars context;
        returnif(context fi_> 2);
        sys_grbg_destpair(autoload_list) -> (, autoload_list);
        if context == 2 then
            ;;; restore to previous state on abnormal exit
            if iscompound(id) then
                id!ID_PERM_FLAGS _biset _:M_PERM_NOT_DEF -> id!ID_PERM_FLAGS
            elseif iscompound(word!W_IDENTIFIER) then
                syscancel(word_identifier(word, current_section, true))
            endif;
            autoload_p -> sys_autoload_action(word)
        endif
    enddefine;

    Sys$-Check_word(word);
    word!W_IDENTIFIER -> id;
    if iscompound(id) and not(id!ID_PERM_FLAGS _bitst _:M_PERM_NOT_DEF) then
        true
    elseif pop_autoload and not(fast_lmember(word, autoload_list)) then
        sys_autoload_action(word) -> autoload_p;
        procedure;
            dlocal 0 % action(dlocal_context) %;
            if autoload_p then
                autoload_p(word)
            else
                syslibcompile(word)
            endif
        endprocedure()
    else
        false
    endif
enddefine;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov  7 1992
        Added sys_autoload_action
--- John Gibson, Aug 27 1990
        Changed n*ote to weak
--- John Gibson, Jul  5 1990
        Added -pop_autoload-
--- John Gibson, May 15 1989
        Included ident.ph
--- John Gibson, Jan 29 1989
        Changes for 'weak' declarations
 */
