/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedvi200.p
 > Purpose:         Co-ordinate the use of vedvi200key and vedvi200screen
 > Author:          Andreas Schoter (see revisions)
 > Related Files:   vedvi200keys.p vedvi200screen.p
 */

uses-by_name vedvi200screen, vedvi200keys;

section;

define vars vedvi200();
    veduseterm("vi200") -> ;
    identfn -> vedvi200;
enddefine;

if iscaller(vedsetup) then vedvi200() endif;

endsection;


/* --- Revision History ---------------------------------------------------
--- Rob Duncan, Oct  9 1989
        Sectionised; made -vedvi200- "vars" and added redefinition to
        -identfn-
 */
