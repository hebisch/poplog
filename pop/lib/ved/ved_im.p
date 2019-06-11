/*  --- Copyright University of Sussex 1991.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_im.p
 >  Purpose:        Immediate mode in ved
 >  Author:         John Gibson, 1984 (see revisions)
 >  Documentation:  HELP * VEDCOMMS/ved_im
 >  Related Files:  LIB * IMCSH  HELP * IMCSH
 */
compile_mode :pop11 +strict;

section;

global vars vedimname = false;

define global ved_im;
    vedsetup();
    if vedargument = nullstring then
        if vedimname then
            vedimname
        elseif isstring(vedlmr_print_in_file) then
            vedlmr_print_in_file
        else
            'output' <> pop_default_type
        endif -> vedargument
    elseif vedargument = '.' then
        vedcurrent -> vedargument
    endif;
    vedargument -> vedimname;
    if iscaller(vedsetpop) then
        ;;; have to get out of this first, back to vedprocess
        vedinput(ved_im)
    else
        ;;; input a procedure to call vedsetpop if necessary
        vedinput(procedure();
                    unless vedprocswaiting() then chain(vedsetpop) endunless
                 endprocedure);
        ved_ved()
    endif
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Dec 18 1991
        Simplified by making it always vedinput a procedure
--- Robert John Duncan, Jul 26 1991
        Changed default interactive file name to 'output.*'
--- John Gibson, Apr  9 1991
        Changed test iscaller(vededitor) to vedinvedprocess
--- John Gibson, Oct 31 1990
        Removed im, and changed definition in im.p. Put in test for
        iscaller(vedsetpop) to make im work properly in im
--- John Williams, Jul 12 1990
        Runs -vedsetpop- inside the call of -ved_im- if possible, so
        that things line <ENTER> PROLOG IM . <RETURN> work properly.
--- John Williams, Feb  9 1990
        'im .' now allowed - specifies IM should start in current file
--- John Williams, Feb 17 1989
        -im- now reads filename
--- John Gibson, Jan  4 1989
        Replaced -vednullstring- with -nullstring-
 */
