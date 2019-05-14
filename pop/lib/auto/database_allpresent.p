/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/auto/database_allpresent.p
 > Purpose:         Tries to bind variables to satisfy a list of patterns
 > Author:          John Gibson, Dec 23 1995
 > Documentation:   REF * DATABASE
 */
compile_mode :pop11 +strict;

;;;  database_allpresent takes a whole list of patterns and tries to find a
;;;  way of binding variables so that all items are present in database

section;

define lconstant AllPresent(pattlist);
    lvars item, patt, save_conv, save_matchvars, procedure eq_p;
    if pattlist == [] then [] -> database_them; return(true); endif;
    fast_destpair(pop_matchvars_bound) -> (save_conv, save_matchvars);
    dest(pattlist) -> (patt, pattlist);
    class_=(datakey(patt)) -> eq_p;
    for item in database do
        if eq_p(item, patt) and AllPresent(pattlist) then
            item :: database_them -> database_them;
            return(true)
        endif;
        sys_restore_matchvars(save_conv, save_matchvars)
    endfor;
    return(false)
enddefine;

define database_allpresent(pattlist);
    dlocal pop_matchvars_bound = [];
    AllPresent(pattlist);
    if pop_matchvars_bound /== [] then sys_finish_matchvars() endif
enddefine;

endsection;
