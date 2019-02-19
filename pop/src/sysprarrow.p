/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/sysprarrow.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *PRINT
 */

;;; ----------------------- PRINT ARROW ---------------------------------

#_INCLUDE 'declare.ph'

weak vars
        Sys$-Pop11$-compile_stacklen
    ;

;;; ----------------------------------------------------------------------

vars
    pop_=>_flag     = '** ';

define sysprarrow(print_whole_stack) with_props =>;
    lvars print_whole_stack, item;
    if print_whole_stack then
        ;;; print whole stack
        stacklength() -> item;
        if testdef pop11_compile then
            item fi_- weakref[pop11_compile] Sys$-Pop11$-compile_stacklen
                                                    -> item;
        endif;
        item fi_> 0 and consvector(item)
    endif -> item;
    if isstring(pop_=>_flag) then
        Sys$-Print_str(pop_=>_flag)
    else
        sys_syspr(pop_=>_flag)
    endif;
    if print_whole_stack then
        if item then appdata(item, spr) endif
    else
        ;;; one only
        pr(item)
    endif;
    cucharout(`\n`)
enddefine;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 30 1996
        Changed so it works properly with cucharout set to identfn
--- John Gibson, Aug 27 1990
        Changed n*ote to weak
--- John Gibson, Mar 13 1988
        Moved here from print.p
 */
