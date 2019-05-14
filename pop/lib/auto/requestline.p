/*  --- Copyright University of Sussex 1995. All rights reserved. ----------
 >  File:           C.all/lib/auto/requestline.p
 >  Purpose:        print a string and then read response to <newline>
 >  Author:         Unknown, ??? (see revisions)
 >  Documentation:  HELP * REQUESTLINE
 >  Related Files:  LIB * READLINE, * GETLINE
 */
compile_mode :pop11 +strict;

section;

define requestline(pop_readline_prompt);
    dlocal pop_readline_prompt;
    readline()
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar  1 1995
        Tidied up
 */
