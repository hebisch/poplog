/*  --- Copyright University of Sussex 1990.  All rights reserved. ---------
 > File:           C.all/lib/ved/vedrefreshblock.p
 > Purpose:        Refresh an arbitrary rectangular block of text in VED
 > Author:         Aaron Sloman, May 17 1987 (see revisions)
 > Documentation:  HELP * VEDREFRESHBLOCK, REF * vedrefreshpartline
 > Related Files:  LIB * VEDCUTBLOCK, * VEDYANKBLOCK, * VEDFILLBLOCK
 */
compile_mode :pop11 +strict;

section;

;;; This is rather messy because of the need to deal with many
;;; different cases. It might be easier if more of the innards
;;; of VED were exported.

define vedrefreshblock(line1, col1, line2, col2, dotail);
    ;;; Refresh the block of ved specified. If dotail is true everything
    ;;; to the right of the block is refreshed also.
    lvars lim, width, line, lineoffset, columnoffset,
        line1, col1, line2, col2, dotail,
        maxscreencol=vedcolumnoffset fi_+ vedscreenwidth fi_- 1,
        maxscreenline=vedlineoffset fi_+ vedwindowlength fi_- 1;

    unless vedediting then return endunless;


    ;;; make sure line1 <= line1, col1 <= col2
    if line1 > line2 then line1,line2 -> line1->line2 endif;
    if col1 > col2 then col1,col2 -> col1->col2 endif;

    if dotail then maxscreencol fi_+ 1 -> col2 endif;   ;;; refresh to right also

    ;;; make sure the bounds are in the visible part of the window
    if line2 fi_<= vedlineoffset or line1 fi_> maxscreenline
    or col1 fi_> maxscreencol or col2 fi_<= vedcolumnoffset
    then return();  ;;; nothing visible to refresh
    endif;

    ;;; clip to fit in visible window
    fi_max(line1,vedlineoffset fi_+ 1) -> line1;
    fi_min(line2, maxscreenline) -> line2;
    fi_max(col1,vedcolumnoffset fi_+ 1) -> col1;
    fi_min(col2,maxscreencol + 1) -> col2;

    vedtrimline();

    vedlineoffset fi_- 1 -> lineoffset;
    col1 - vedcolumnoffset fi_+ 1 -> columnoffset;
    col2 fi_- col1 + 1 -> width;

    for line from line1 to line2 do
        vedrefreshpartline(
            false,
            line fi_- lineoffset,
            columnoffset,
            col1,
            width,
            subscrv(line, vedbuffer));
        1000 ->> vedscreenline -> vedscreencolumn
    endfor;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Oct  2 1990
    Simplified, and bugs fixed, using new system procedure vedrefreshpartline.
    Now copes with tabs
 */
