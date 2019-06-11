/*  --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:           C.all/lib/ved/ved_do.p
 > Purpose:        Multiple commands on VED command line
 > Author:         Aaron Sloman, Jan 18 1986 (see revisions)
 > Documentation:  HELP * VEDCOMMS/ved_do
 > Related Files:
 */
compile_mode :pop11 +strict;

/*
    <ENTER> do;/fred/;gsl//fried;jp;

is equivalent to a series of <ENTER> commands.

The first non-space character after 'do' is taken as the command
delimiter.
*/

section;

define vars ved_do;
    lvars   char, lo, hi, string, lim, done = false,
            argstring = copy(vedargument);
    if (datalength(argstring) ->> lim) < 2 then vederror('DO WHAT?') endif;
    argstring(skipchar(`\s`, 1, argstring) ->> hi) -> char;
    until done do
        hi + 1 -> lo;
        locchar(char, lo, argstring) -> hi;     ;;; Now check if end of line:
        if hi == lim then lim - 1 -> lim; false -> hi endif;
        unless hi then lim + 1 -> hi; true -> done endunless;
        veddo(substring(lo, hi - lo, argstring));
    enduntil
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 30 1993
        Stopped it doing vedputcommand, and made it take a copy of vedargument
        instead.
 */
