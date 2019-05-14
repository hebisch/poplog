/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/auto/sys_restore_matchvars.p
 > Purpose:         Restore pop_matchvars_bound to a saved value
 > Author:          John Gibson, Dec 23 1995
 > Documentation:   REF * RECORDS
 */
compile_mode :pop11 +strict;

section;

    /*  The values for save_conv and save_matchvars can be saved with
            fast_destpair(pop_matchvars_bound) -> (save_conv, save_matchvars)
        etc (this works with []).
    */
define sys_restore_matchvars(save_conv, save_matchvars);
    lvars   matchvars, conv, save_matchvars, save_conv,
            pmvb = pop_matchvars_bound;
    returnif(pmvb == []);
    fast_destpair(pmvb) -> (conv, matchvars);
    until matchvars == save_matchvars do
        sys_grbg_destpair(matchvars) -> (, matchvars)
    enduntil;
    if matchvars == [] then
        sys_grbg_list(conv);
        sys_grbg_destpair(pmvb) -> (,);
        [] -> pop_matchvars_bound
    else
        matchvars -> fast_back(pmvb);
        until conv == save_conv do
            sys_grbg_destpair(conv) -> (, conv)
        enduntil;
        conv -> fast_front(pmvb)
    endif
enddefine;

endsection;
