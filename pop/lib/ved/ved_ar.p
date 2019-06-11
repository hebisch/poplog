/*  --- Copyright University of Sussex 1995.  All rights reserved. ---------
 *  File:           C.all/lib/ved/ved_ar.p
 *  Purpose:        align each line of range with right hand margin
 *  Author:         Chris Slymon (after David Roberts), June 1983 (see revisions)
 *  Documentation:  HELP * FORMAT
 *  Related Files:
 */

section ;

define global ved_ar();
    dlocal vedstatic=false, vedchanged;
    lvars oldchanged=vedchanged;
    false -> vedchanged;
    vedpositionpush();
    vedmarkfind();
    until vedline fi_> vvedmarkhi do
        if vvedlinesize fi_> 0 then
            vedtextleft();
            until vvedlinesize fi_>= vedlinemax do
                vedcharinsert(` `)
            enduntil;
            until vvedlinesize = vedlinemax or vedcolumn = 1 do
                vedchardelete()
            enduntil;
        endif;
        vednextline()
    enduntil;
    vedpositionpop();
    if oldchanged then oldchanged fi_+ 1 else 1 endif -> vedchanged;
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Jun  8 1995
        dlocal instead of vars.
 */
