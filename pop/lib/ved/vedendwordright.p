/* --- Copyright University of Sussex 1987. All rights reserved. ----------
 > File:           C.all/lib/ved/vedendwordright.p
 > Purpose:        Procedures to move and delete to end of current word.
 > Author:         Chris Slymon, Oct 1983 (see revisions)
 > Related Files:  LIB *VEDSTARTWORDLEFT
 */
compile_mode :pop11 +strict;

section;

/* Moves forward to the end of the next word, or
    if the cursor is at the end of a line and the next line is empty,
    to the beginning of the next line */


define vedendwordright;
    lvars i;
    vedchartype(` `) -> i;
    if vedcolumn > vvedlinesize then
        unless vedline > vvedbuffersize then
            vednextline();
        endunless;
        if vvedlinesize > 0 then
            vedtextleft();
            if vedcolumn > 1 then vedcharleft(); endif;
        endif;
    else
        ;;; traverse spaces on right
        vedcharright();
        while vedcurrentchartype() == i and vedcolumn <= vvedlinesize do
            vedcharright();
        endwhile;
        vedcharleft();
        until vedatitemend(vedcolumn,vedthisline(),vvedlinesize + 1) do
            vedcharright()
        enduntil;
        vedcharright();
    endif;
enddefine;

define vedendwordrightdelete();
lvars col;
    if vedcolumn fi_> vvedlinesize then
        vednextline()
    else
        vedcolumn -> col;
        vedendwordright();
        vedspandelete(col, vedcolumn, true) -> vvedworddump;
    endif;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Jun 26 1987
    merged with C.all/lib/ved/vedendwordrightdelete.p because of 14-character
    file name clash.
--- John Williams, Jan 22 1986
    uses 'vedchartype' for 'chartype', section'ed and lvars'ed
*/
