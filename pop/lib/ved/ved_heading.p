/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 > File:           C.all/lib/ved/ved_heading.p
 > Purpose:        Formatting section headings in HELP or TEACH files
 > Author:         Aaron Sloman, Apr 11 1986 (Suggested by J.Cunningham) (see revisions)
 > Documentation:  HELP * VEDCOMMS/heading
 > Related Files:  LIB * VED_G ,  LIB * VED_INDEXIFY, HELP *ENTER_G
 */
compile_mode :pop11 +strict;

/*
LIB VED_HEADING
For inserting headings in HELP and TEACH files, compatible with ved_g
and ved_indexify

Given a line with text like this one
the command <ENTER> heading will add leading and trailing hyphens
to produce:

-- Given a line with text like this -----------------------------------

The trailing hyphens will go up to column vedlinemax or 72, whichever
is smaller. (vedlinemax should be 72 for help or teach files.)

If leading and trailing hyphens exist they are adjusted if necessary.

ved_g_string can be changed to alter the 'lead-in' string for special
applications. See LIB * VED_G
*/

section;

define vars ved_heading();
    lvars char, changed=vedchanged;
    dlocal vedlinemax, vedautowrite=false, vedstatic=false;

    if vedlinemax > 72 then
        72 -> vedlinemax;
        vedputmessage('VEDLINEMAX 72 for HELP FILES')
    endif;
    if vvedlinesize /== 0 then
        1 -> vedcolumn;
        ;;; get rid of leading hyphens or spaces
        while (vedcurrentchar()->>char) == `-` or char == ` ` do
            vedwordrightdelete();
        endwhile;
        ;;; get rid of trailing hyphens
        vvedlinesize -> vedcolumn;
        while (vedcurrentchar()->>char) == `-` or char== ` ` do
            vedcharleft();
        endwhile;
        vedcharright(); vedcleartail();
        1 -> vedcolumn;
        vedinsertstring(ved_g_string);
        vedtextright();
        vedcharinsert(` `);
    endif;
    until vedcolumn >= vedlinemax do vedcharinsert(`-`) enduntil;
    if changed then changed + 1 else 1 endif -> vedchanged;

enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Nov 14 1988
    Previous "fix" removed. Didn't work
--- Ian Rogers, Oct 10 1988
    Fixed so that cursor position is unchanged after operation.
--- Aaron Sloman, Aug  7 1988
    Fixed to cope with vedstatic=true
--- Aaron Sloman, Aug  7 1986 changed to deal with leading spaces or
    hyphens
--- Aaron Sloman, Apr 11 1986 derived from ved_- but the latter can't
    be used as file name on VMS
    ved_g_string made available for modification.
*/
