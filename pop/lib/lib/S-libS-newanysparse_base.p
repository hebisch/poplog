/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lib/lib/S-libS-newanysparse_base.p
 > Purpose:         Runtime procedure for newanysparse
 > Author:          John Gibson, May 22 1996
 */
compile_mode :pop11 +strict :vm -pentch -bjmpch;

section $-lib;

dlocal pop_debugging = false;

define newanysparse_base(Ndims, prop, default, active_default) -> prop
                                                with_props sparse_array;
    lvars n = Ndims;
    until n == 0 do
        if (fast_apply(subscr_stack(n), prop) ->> prop) == default then
            if active_default then fast_chain(active_default) endif;
            quitloop
        endif;
        n fi_- 1 -> n
    enduntil;
    erasenum(Ndims)
enddefine;
;;;
define updaterof newanysparse_base(Ndims, prop, default, dimlist)
                                                with_props sparse_array;
    lvars n = Ndims, arg, nextprop, dim;
    until dimlist == [] do
        fast_destpair(dimlist) -> (dim, dimlist);
        subscr_stack(n) -> arg;
        if (fast_apply(arg, prop) ->> nextprop) == default then
            newproperty([], dim, default, "perm") ->> nextprop -> prop(arg)
        endif;
        nextprop -> prop;
        n fi_- 1 -> n
    enduntil;
    ;;; one arg left
    () ->> arg;
    erasenum(Ndims);
    (/* newval*/) -> fast_apply(arg, prop)
enddefine;

endsection;
