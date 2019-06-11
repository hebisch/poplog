/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_hkeys.p
 > Purpose:         Get help on current VED key bindings
 > Author:          John Williams, Oct  5 1990 (see revisions)
 > Documentation:   REF * ved_hkeys
 > Related Files:   C.all/lib/ved/vedhelpkey.p
 */
compile_mode :pop11 +strict;

section;

define vars ved_hkeys();
    dlocal vedargument;
    if vedkeymapname then
        if issubstring('keys', vedkeymapname) then
            vedkeymapname
        else
            vedkeymapname sys_>< 'keys'
        endif
    elseif veddefaultkeys == vednewkeys then
        'vednewkeys'
    else
        'vedkeys'
    endif -> vedargument;
    ved_help();
enddefine;

endsection;
