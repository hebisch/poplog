/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.unix/lib/ved/dired_dcd.p
 > Purpose:         Delete listing of sub-directory produced by ved_dired
 > Author:          Aaron Sloman, Nov 15 1990 (see revisions)
 > Documentation:   HELP * DIRED
 > Related Files:   $popvedlib/dired*.p LIB * VED_DIRED
 */

#_TERMIN_IF DEF POPC_COMPILING

/*
If you compile the following procedure (it can go into an autolodable
file called dired_dcd.p) then the command

    ENTER dired -dcd

will delete the listing bracketed by DIRED ... ENDDIRED surrounding
the current cursor location.

It is possible to assign to a VED key sequence veddo(%dired -dcd%)

*/

section;

define global dired_dcd() with_nargs 4;
    ;;; delete current directory listing
    lvars line=vedline, col = vedcolumn, count;

    lconstant startstring = 'DIRED:-', endstring = 'ENDDIRED';

    dlocal vvedmarkprops, vveddump;

    erasenum(4);    ;;; eat unwanted arguments

    ;;; save existing marked range, and, suppress temporary marking
    vedmarkpush();
    false -> vvedmarkprops;

    ;;; Search back for DIRED and forward for ENDDIRED and mark range
    if vedthisline() = endstring then vedcharup() endif;
    0 -> count;
    ;;; search back, skipping nested pairs
    repeat
        if vedline == 1 then
            vedjumpto(line,col);
            vederror('DIRED NOT FOUND')
        endif;
        if vedthisline() = endstring then
            count fi_+ 1 -> count
        elseif vedlinestart(startstring) then
            quitif(count == 0);
            count fi_- 1 -> count;
        endif;
        vedcharup();
    endrepeat;
    vedmarklo();
    line -> vedline;
    if vedlinestart(startstring) then vedchardown() endif;
    ;;; now search forward
    0 -> count;
    repeat
        if vedlinestart(startstring) then count fi_+ 1 -> count
        elseif vedthisline() = endstring then
            quitif(count == 0);
            count fi_- 1 -> count;
        endif;
        if vedline == vvedbuffersize then
            vedjumpto(line,col);
            vederror('ENDDIRED NOT FOUND')
        endif;
        vedchardown();
    endrepeat;
    vedmarkhi();
    line -> vedline;
    ved_d();

    vedmarkpop();
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Sep 29 1993
        Removed dlocal of vvedsr*ch vars (not needed).
--- Aaron Sloman, Dec  3 1990
    Made to work if on first or last line of sub-directory listing
--- Aaron Sloman, Nov 19 1990
    fixed to cope properly with nested sub-directories
 */
