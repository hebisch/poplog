/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.unix/lib/ved/man.p
 > Purpose:         VED interface to Unix 'man' facility
 > Author:          John Williams, Mar 22 1989 (see revisions)
 > Documentation:   HELP * MAN
 > Related Files:   LIB * VED_MAN
 */

section;

define global vars syntax man;
    popvedcommand("ved_man")
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Nov 12 1990
        Now uses -popvedcommand-
--- Aaron Sloman, Apr 16 1990
        Moved out of LIB VED_MAN so that using -ved_man- doesn't cause
        "man" to be declared as a syntax word.
 */
