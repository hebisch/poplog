/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_what.p
 > Purpose:         Old <ENTER> ?? mechanism (renamed VED_WHAT)
 > Author:          John Williams, Jan  3 1990 (see revisions)
 > Documentation:   REF * OBSOLETE
 > Related Files:   C.all/src/vdquery.p
 */


section;

syssynonym("ved_what", "ved_?");

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Oct 26 1990
        Now just a synonym for -ved_?-
--- Aaron Sloman, May  6 1988
        Stopped ved_?? without argument putting the next item into the
        current command line.
--- John Gibson, Apr  5 1988
        Replaced issubstring_lim(substr,1,1,false,string) with
        isstartstring(substr,string)
--- John Gibson, Feb 14 1988
        Replaced -vednullstring- with -nullstring-
--- John Williams, Sep  4 1987
        Fixed BR popdoc.4; also fixed blank lines problem
--- Aaron Sloman, Aug 10 1987
        Made to check if item to left of cursor is all upper case and if
        so transform to lower case, if no argument given. UNIX only
--- John Gibson, Aug  6 1987
        Combined ved_what.p, ved_whats.p, ved_what.ph from library to
        form this file, with changes necessary to put into system.
--- Aled Morris, Aug  5 1987
        Fixed properly to allow blank lines for "newmaster" format
--- Aaron Sloman, Jun 20 1987
        Altered to allow blank line to terminate entry. Needed for new format
        used by 'newmaster'.
--- Aaron Sloman, Dec  1 1986
        Tighter check for POPDATATYPES
--- Ben Rubinstein, Oct 13 1986
        ved_do_what can now take integer 'verbose' argument, in which case
        it returns a boolean indicating whether information was found, as well
        as displaying it.  If found, 0 is non-verbose, 1 is verbose.
*/
