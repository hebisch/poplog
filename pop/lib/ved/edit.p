/*  --- Copyright University of Sussex 1994.  All rights reserved. ---------
 >  File:           C.all/lib/ved/edit.p
 >  Purpose:        Given a file name, call VED, whether already in ved or not.
 >  Author:         Aaron Sloman, May 1982 (see revisions)
 >  Documentation:
 >  Related Files:
 */
compile_mode :pop11 +strict;

section;

define vars edit = vededit(%%) enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar  7 1994
        Changed to be a null closure of vededit
--- John Gibson, Dec  1 1992
        Removed call of sysfileok (not necessary since vedgetfile does
        it anyway), and it doesn't take an optional false arg anyway.
--- Adrian Howard, Nov 11 1992
        Added optional arg to sysfileok to prevent error if filename passed
        as false
--- Aaron Sloman, Jan 27 1987 put in call of sysfileok
*/
