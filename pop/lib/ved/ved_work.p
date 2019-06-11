/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_work.p
 > Purpose:         Create a work buffer
 > Author:          John Gibson, Mar 15 1994
 > Documentation:   REF * VEDCOMMS
 */
compile_mode :pop11 +strict;

section;

vars vedworkname = nullstring;

define vars vedworkdefaults();
    vedveddefaults()
enddefine;

define vars ved_work;
    lvars arg = vedargument;
    if arg = nullstring then
        if vedworkname = nullstring then
            'temp' <> pop_default_type -> vedworkname
        endif;
        vedworkname -> arg
    else
        arg -> vedworkname
    endif;
    ;;; N.B. This is a pseudo-file and will be given vedfileprops "workbuff"
    chain('workbuff\s' <> arg, vedworkdefaults, vededit)
enddefine;

endsection;
