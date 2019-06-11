/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 >  File:           C.all/lib/ved/ved_ca.p
 >  Purpose:        Copy marked range and append to vveddump
 >  Author:         Aaron Sloman, May 1983 (see revisions)
 >  Documentation:  REF *VEDCOMMS
 */

section;

define global ved_ca;
    lvars temp = vveddump;
    ved_copy();
    temp nc_<> vveddump -> vveddump;
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Mark Rubinstein, Oct  3 1985 - sectionised and lvarsed.
 */
