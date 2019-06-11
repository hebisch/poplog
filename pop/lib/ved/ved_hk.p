/*  --- Copyright University of Sussex 1989. All rights reserved. ----------
 *  File:           C.all/lib/ved/ved_hk.p
 *  Purpose:        what procedure, if any, is associated with a key
 *  Author:         Unknown, ??? (see revisions)
 *  Documentation:
 *  Related Files:
 */

;;; Like VED_HKEY, but doesn't do the checking.
section;

define global ved_hk;
    vedputmessage('Please press the key. If no response, press more keys');
    vedgetproctable(vedinascii());
    vedputmessage(nullstring)
enddefine;

endsection;



/*  --- Revision History ---------------------------------------------------
--- John Gibson, Jan  4 1989
        Replaced -vednullstring- with -nullstring-
--- Mark Rubinstein, Oct  4 1985 - sectionised.
 */
