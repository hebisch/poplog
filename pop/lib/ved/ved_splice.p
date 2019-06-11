/*  --- Copyright University of Sussex 1991.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_splice.p
 >  Purpose:        for re-inserting text cut out by ved_cut.
 >  Author:         Aaron Sloman, Jan 1985 (see revisions)
 >  Documentation:  REF * VEDCOMMS/ved_splice
 >  Related Files:  LIB * VED_CUT
 */
compile_mode :pop11 +strict;

section;

vars vvedcut_dump;

define vars ved_splice;
    ;;; yank stuff deleted by ds or cut
    lvars flag;
    dlocal vveddump;
    unless islist(vvedcut_dump) and not(null(vvedcut_dump)) then
        vederror('NOTHING TO SPLICE')
    elseif length(vvedcut_dump) == 1 then
        vedinsertstring(front(vvedcut_dump));
    else
        vvedcut_dump -> vveddump;
        if vedcolumn == 1 then
            if vedline == 1 then vedlineabove() else vedcharup() endif
        elseif vedcolumn > vvedlinesize then ;;; don't break line
        else vedcharinsert(`\n`); vedcharup()
        endif;
        ved_y(); vednextline();
    endunless
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Jun 22 1991
        Changed vars statement to dlocal to fix it for compile mode
        strict
--- Aaron Sloman, Jan 30 1991
    Fixed error message if called before ved_cut (Bugreport isl.43)
--- Aaron Sloman, Jun 1985 - removed spurious local vedediting
--- Aaron Sloman, July 1985 - Now uses global variable vvedcut_dump
    fixed to deal with top of file.
 */
