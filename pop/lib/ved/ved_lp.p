/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.unix/lib/ved/ved_lp.p
 > Purpose:         Ved interface to Unix lp and lpr commands
 > Author:          John Williams, Apr 11 1989 (see revisions)
 > Documentation:   HELP * LP, HELP * LPR
 > Related Files:   LIB * VED_LPR, LIB * VED_LPMR, LIB * VED_LPRMR
 */

compile_mode :pop11 +strict;

section;


define lconstant Vedprint(comm);
    lvars args, mr, lo, hi, mess;
    dlocal vednotabs = true;    ;;; to give correct spacing

    [% sys_parse_string(vedargument) %] -> args;

    if (isendstring('mr', comm) ->> mr) then
        substring(1, mr - 1, comm) -> comm;
        vvedmarklo, vvedmarkhi
    else
        1, vvedbuffersize,
    endif -> (lo, hi);

    if lo > hi then
        vederror(if mr then '\{b}no marked range' else '{b}file is empty' endif)
    endif;

    '\{b}queueing print command for '
        <> (if mr then 'marked range' else sys_fname_name(vedcurrent) endif)
        <> ' \{b}...'
        -> mess;

    if vedusewindows == "x" then
        vedputmessage(mess)
    else
        printf(mess, '\n;;; %S\n\n');
        ;;; So that output from Unix "lp" appears at
        ;;; the bottom of the screen, not on status line
    endif;

    pipeout(consref(vedwriterange(% lo, hi %)),
            consref(comm), [^comm ^^args], true);


    vedrestorescreen();
    vedputmessage('\{b}done')
enddefine;


define vars ved_lp = Vedprint(% 'lp' %) enddefine;

define vars ved_lpmr = Vedprint(% 'lpmr' %) enddefine;

define vars ved_lpr = Vedprint(% 'lpr' %) enddefine;

define vars ved_lprmr = Vedprint(% 'lprmr' %) enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Apr 30 1996
        Second arg to pipeout now a ref (i.e. use execvp).
--- John Williams, Nov 16 1993
        Added "PLEASE WAIT!" to bottom of screen message.
--- John Williams, Oct  4 1993
        Now always pipes out text from current Ved buffer, even if names of
        files to print are specified. This makes it possible to include a
        space between a flag and its argument, e.g. "-d lja".
--- John Williams, Jan 22 1993
        Changed npr to nprintf to avoid problems with pop_pr_quotes
--- John Gibson, Jul 22 1992
        Made vednotabs true inside Vedprint so printing is done with
        correct spacing.
--- John Gibson, Mar  6 1992
        Changed to use new facility in -pipeout- which enables first
        arg to be a ref containing a procedure that will write out the
        data given a device (i.e. the output pipe). Thus uses
        vedwriterange(%lo, hi%) for this argument.
--- John Williams, Oct 10 1991
        Prints message on status line under XVED (cf BR johnw.1031)
--- John Williams, Aug 23 1990
        Now uses -sys_search_unix_path- to find full pathname for
        'lp' and 'lpr'
--- John Williams, Apr 19 1989
        Stopped -Vedprint- stripping prompts
 */
