/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 >  File:           C.all/lib/ved/ved_yankc.p
 >  Purpose:        Yank marked range into command line
 >  Author:         Unknown, ??? (see revisions)
 >  Documentation:
 */
compile_mode :pop11 +strict;

section;

define global ved_yankc;
    dlocal vveddump, ved_on_status = false;
    ved_copy();
    true -> ved_on_status;
    ved_y();
    vedchardown();
enddefine;

endsection;


/*  --- Revision History ---------------------------------------------------
--- John Gibson, Mar  3 1992
        Now uses ved_on_status. Also calls -vedchardown- so the yanked
        command is immediately visible.
--- Mark Rubinstein, Oct  4 1985 - sectionised.
 */
