/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/lib/ved/vedtableft.p
 > Purpose:         Move cursor left one tab stop
 > Author:          John Williams, Nov 24 1989
 > Documentation:   REF * VEDTABLEFT
 > Related Files:   SRC * VDMOVE.P (defines -vedtabright-)
 */

section;

define global vedtableft();
    lvars nspaces ntabs;
    if vedcolumn == 1 then
        vedscreenbell()
    else
        (vedcolumn fi_- 1) fi_// vedindentstep -> ntabs -> nspaces;
        if nspaces == 0 then
            ntabs fi_- 1 -> ntabs
        endif;
        (ntabs fi_* vedindentstep) fi_+ 1 -> vedcolumn
    endif
enddefine;

endsection;
