/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lib/auto/sysprmessage.p
 > Purpose:         Old version of sys_pr_message
 > Author:          John Gibson, Feb  6 1996
 > Documentation:   REF * OBSOLETE
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

define sysprmessage(count, message, message_header, detail);
    sys_pr_message(count, {^message ^message_header ^detail}, nullstring, `W`)
enddefine;

endsection;
