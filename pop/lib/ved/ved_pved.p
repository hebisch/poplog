/*  --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_pved.p
 > Purpose:         Edit file in write-protected mode
 > Author:          Mark Rubinstein, (see revisions)
 > Documentation:   HELP * PVED
 > Related Files:   LIB * PVED
 */

section;

define global ved_pved();
    if vedinvedprocess then
        ved_ved();
        false -> vedwriteable;
    else
        vedinput(procedure(); false -> vedwriteable; endprocedure);
        ved_ved();
    endif;
enddefine;


endsection;


/*  --- Revision History ---------------------------------------------------
--- John Williams, Jul  8 1993
        Last changed stopped pved macro working outside of Ved. Fixed.
--- Adrian Howard, Jul  7 1993
        Last change removed so veddo('pved <file>') works properly
--- John Williams, Feb 24 1989
        Uses -ved_char_in_stream-
--- Mark Rubinstein, Oct  4 1985
        Sectionised.
 */
