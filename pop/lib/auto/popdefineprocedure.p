/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/lib/auto/popdefineprocedure.p
 > Purpose:         Old variables replaced by flags in -pop_pop11_flags-
 > Author:          John Gibson, Jun  1 1989
 > Documentation:   REF *POPCOMPILE
 > Related Files:   $usepop/pop/lib/include/pop11_flags.ph
 */

#_TERMIN_IF DEF POPC_COMPILING

#_INCLUDE '$usepop/pop/lib/include/pop11_flags.ph'

section;

define lconstant testflag(/*flag*/) with_nargs 1;
    (/*flag*/) &&/=_0 pop_pop11_flags
enddefine;

define updaterof testflag(/*bool*/ flag);
    lvars flag;
    if (/*bool*/) then
        nonop fi_||
    else
        nonop fi_&&~~
    endif (pop_pop11_flags, flag) -> pop_pop11_flags
enddefine;

global constant active (
    popdefineprocedure  = testflag(% POP11_DEFINE_PROCEDURE %),
    popdefineconstant   = testflag(% POP11_DEFINE_CONSTANT %),
    popconstruct        = testflag(% POP11_CONSTRUCTOR_CONSTS %),
    pop_args_warning    = testflag(% POP11_VARS_CHECK %),
    );

endsection;
