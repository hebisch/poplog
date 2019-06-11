/*  --- Copyright University of Sussex 1992.  All rights reserved. ---------
 >  File:           C.all/lib/ved/vedout.p
 >  Purpose:        creates a character consumer to write to a file.
 >  Author:         Aaron Sloman, Jun 1982 (see revisions)
 >  Documentation:  HELP * VEDOUT
 >  Related Files:
 */
compile_mode :pop11 +strict;

section;

define vedout(file, returntofile, onscreen, initialise, terminate) -> outchar;
    lvars file, returntofile, onscreen, initialise, terminate;
    lconstant procedure outchar;
    lconstant do_init = '';
    dlocal vedediting = onscreen;

    ;;; return lex closure of this procedure
    define lconstant outchar(char) with_props vedout;
        lvars char, old;
        dlocal vedediting = onscreen;

        define dlocal interrupt;
            if old then vedsetonscreen(old, nullstring); endif;
            vedinterrupt()
        enddefine;

        if ved_current_file == file then
            false -> old
        else
            ved_current_file -> old;
            if vedediting then
                vedsetonscreen(file, nullstring)
            else
                file -> ved_current_file
            endif;
        endif;
        if vedediting then
            vedcheck()
        endif;
        if char == termin then
            terminate()
        elseif char == do_init then
            initialise()
        else
            vedcharinsert(char)
        endif;
        if old then
            if returntofile then
                vedsetonscreen(old, nullstring)
            elseunless vedediting then
                old -> ved_current_file;
            endif;
        endif;
    enddefine;

    vedopen(file, true) -> file;     ;;; now a file record

    ;;; make sure the user's procedure is run on the new file
    outchar(do_init)
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Dec  1 1992
        Rewritten
 */
