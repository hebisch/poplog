/* --- Copyright University of Sussex 1987. All rights reserved. ----------
 > File:            C.all/lib/ved/vedstartwordleft.p
 > Purpose:         Procedures to move and delete to start of previous/current
 >                  word.
 > Author:          Chris Slymon, October 1983 (see revisions)
 > Related Files:   LIB *VEDENDWORDRIGHT
 */
compile_mode :pop11 +strict;

section;

/* Moves to the beginning of the previous/current word,
    or if the cursor is at the start of the text on the current line,
    to the end of the preceding line */


define vedstartwordleft;
    lvars i;
    if vedcolumn == 1 or vvedlinesize = 0 then
        vedcharup();
        vedtextright()
    elseif vedcolumn > vvedlinesize + 1 then
        vvedlinesize + 1 -> vedcolumn;
    else
        vedchartype(` `) -> i;
        while vedcurrentchartype() == i do
            vedcharleft();
        endwhile;
        vedcharleft();
        until vedatitemstart(vedcolumn,vedthisline(),vvedlinesize + 1) do
            vedcharleft()
        enduntil;
    endif;
enddefine;

define vedstartwordleftdelete();
    lvars col;
    if vedcolumn == 1 then
        if vedstatic then
            vedscreenbell()
        else
            vedchardelete()
        endif;
    elseif vedcolumn fi_> vvedlinesize fi_+ 1 then
        vvedlinesize fi_+ 1 -> vedcolumn
    else
        vedcolumn -> col;
        vedstartwordleft();
        vedspandelete(vedcolumn, col, true) -> vvedworddump;
    endif
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Jun 26 1987
    merged with C.all/lib/ved/vedstartwordleftdelete.p because of 14-character
    file name clash
--- John Williams, Jan 22 1986
    changed 'chartype' to 'vedchartype', section'ed, and lvars'ed
*/
