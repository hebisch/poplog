/*  --- Copyright University of Sussex 1989.  All rights reserved. ---------
 >  File:           C.all/lib/ved/term/vedvt52.p
 >  Purpose:        set ved for a 'DUMB' vt52 compatible terminal.
 >  Author:         Aaron Sloman, Jan 1983 (see revisions)
 >  Documentation:  HELP * VT52
 >  Related Files:
 */
compile_mode :pop11 +strict;

/*
    Set VED for a 'DUMB' vt52 compatible terminal, i.e. no insert line,
    no delete line, no character insert, no character delete, no graphic
    characters, no special functions on numeric keypad,
    only three function keys PF1 (='GOLD') PF2 PF3
*/

uses-by_name vedvt52keys, vedvt52screen;

section;

define vars vedvt52();
    veduseterm("vt52") -> ;
    identfn -> vedvt52;
enddefine;

if iscaller(vedsetup) then vedvt52() endif;

endsection;


/* --- Revision History ---------------------------------------------------
--- Jason Handby, Sep 14 1989
        Separated out into "vedvt52screen.p" and "vedvt52keys.p" files.
*/
