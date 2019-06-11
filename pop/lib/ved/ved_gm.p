/* --- Copyright University of Sussex 1989.  All rights reserved. ---------
 > File:           C.unix/lib/ved/ved_gm.p
 > Purpose:        Go to Message - used in file created by VED_MDIR
 > Author:         Aaron Sloman, Nov  1 1986 (see revisions)
 > Documentation:  HELP * VED_MDIR  * VED_MAIL
 > Related Files:  LIB *VED_MDIR * VED_MAIL  *VED_SEND *VED_REPLY
 */
compile_mode :pop11 +strict;

/*
LIB VED_GM  - go to message

<ENTER> gm <number>
    done inside a mail directory produced by ved_mdir.

will go to the message corresponding to the number, in the file from which
the directory file was derived. <number> defaults to the number at the
beginning of the line cursor is on.

*/

section;

define vars ved_gm;
    ;;; Go to Message. Assume in directory file produced by ved_mdir
    lvars string, loc, mailfile;
    if vedargument /= nullstring then
        ;;; find the number at beginning of line, followed by colon
        vedlocate('@a' >< vedargument >< ': ');     ;;; find the line
    endif;
    vedthisline() -> string;
    ;;; find where message header starts in the line
    issubstring('From ',1, string) -> loc;
    unless loc then vederror('Not on "From" line') endunless;
    ;;; extract the header
    substring(loc, vvedlinesize - loc + 1, string) -> string;
    ;;; replace `@` with `@@`
    if strmember(`@`, string) ->> loc then
        cons_with consstring
        {%appdata(string,
                 procedure(char); lvars char;
                     char,
                     if char ==`@` then char endif
                 endprocedure)
         %} -> string
    endif;
    ;;; get name of mail file from top of directory file
    vedpositionpush();  ;;; save position
    vedtopfile();
    locchar(`\s`,1,vedthisline()) -> loc; ;;; find where there's a space
    substring(1, loc - 1, vedthisline())-> mailfile;
    vedpositionpop();   ;;; go back to previous position
    ;;; go to mail file and search for header
    edit(mailfile);
    vedtextright(); ;;; in case the directory is in the current file
    vedlocate(string)
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Jul 14 1989
    Made it cope with `@` in mail header lines
--- Aaron Sloman, Jan 14 1989 removed vednullstring. Added cross references
 */
