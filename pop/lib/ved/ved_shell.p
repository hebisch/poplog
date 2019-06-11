/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:             C.unix/lib/ved/ved_shell.p
 > Purpose:          Current file should only contain SHELL commands. Obey them.
 > Author:           Aaron Sloman, Nov 1983 (see revisions)
 > Documentation:    REF * ved_shell, HELP * DEL
 > Related Files:    LIB * VED_DEL
 */

section;

define global ved_shell();
    lvars line;
    if vvedbuffersize == 0 then
        vedputmessage('NO COMMANDS IN FILE')
    else
        nprintf(';;; PLEASE WAIT - OBEYING SHELL COMMANDS');
        sysobeylist([% for line in_vector vedbuffer do
                        unless datalength(line) == 0 do
                            line
                        endunless
                    endfor %])
    endif
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Nov 17 1992
        Reinstated checks for blank lines (these will create interactive
        csh processes)
--- John Williams, Nov 17 1992
        Now uses nprintf instead of => to print message
--- Aaron Sloman, Apr  3 1989
        Slightly tidied up.
 */
