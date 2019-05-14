/*  --- Copyright University of Sussex 1987.  All rights reserved. ---------
 > File:           C.unix/lib/auto/sysgetusername.p
 > Purpose:        Get User name (or false) given login name. UNIX only
 > Author:         Aaron Sloman, 25 March 1985 (see revisions)
 > Machines:       unix
 > Documentation:  REF *SYSUTIL
 > Related Files:   /etc/passwd
 */
compile_mode :pop11 +strict;

/* sysgetusername(<user name or id>) -> <actual name>
 Get user name corresponding to login name, from /etc/passwd
 Return false if user not found.
*/

section;

define sysgetusername(user_id) -> user_name;
    lvars user_name, len, user_id, c;
    unless sysgetpasswdentry(user_id, #_<{7}>_#) ->> user_name then
        return
    endunless;
    if locchar(`,`, 1, user_name) ->> c then
        substring(1, c-1, user_name) -> user_name
    endif
enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Jul 17 1987
        Made to use -sysgetpasswdentry- to select field
*/
