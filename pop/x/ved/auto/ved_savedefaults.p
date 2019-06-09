/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/ved/auto/ved_savedefaults.p
 > Purpose:         Save resource settings in a defaults file
 > Author:          Jonathan Meyer, Jun 18 1991 (see revisions)
 > Documentation:
 > Related Files:
 */
compile_mode:pop11 +strict;

section $-xved => ved_savedefaults;

define vars ved_savedefaults;
    lvars item;
    vedputmessage('please wait');
    if vedargument /= nullstring then
        ;;; should check for write permission
        vedargument -> xved_value("application", "ResourceFile");
    endif;
    xved_save_defaults();
    vedputmessage('done');
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Jul 25 1991 : Removed references to UserLevel resource
--- Jonathan Meyer, Jul  5 1991
        Added setting of default window parameters for novice users
 */
