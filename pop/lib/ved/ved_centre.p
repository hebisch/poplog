/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_centre.p
 > Purpose:         Centre text in ved
 > Author:          Aaron Sloman, Nov 1982 (see revisions)
 > Documentation:   HELP * VEDCOMMS/ved_centre
 > Related Files:
 */
compile_mode :pop11 +strict;

;;; Using vedleftmargin and vedlinemax
;;; ENTER CENTRE <string>
;;; puts the string at the centre of current line, possibly after other stuff
;;; ENTER CENTRE .  or just ENTER CENTRE
;;; will centre the current line

section;

define vars ved_centre();
    lvars marginwidth, start, flag;
    dlocal vedargument;
    if vedargument = '.' or vedargument = nullstring then
        if vvedlinesize == 0 then
            vederror('NOTHING TO CENTRE')
        else
            1 -> vedcolumn;
            while vedcurrentchar() == ` ` do vedcharright() endwhile;
            subvedstring(vedcolumn, vvedlinesize - vedcolumn + 1, vedthisline())
                -> vedargument;
            1 -> vedcolumn;
            vedcleartail();
            true -> flag;
        endif
    else
        false -> flag;
    endif;

    ;;; if marginwidth is even, add 1
    (vedlinemax - vedleftmargin) || 1 -> marginwidth;
    ((marginwidth - length(vedargument)) >> 1 ) + 1 -> start;
    if start <= vvedlinesize or start + length(vedargument) - 1 > vedlinemax
    then
        if flag then vedinsertstring(vedargument) endif;
        vederror('STRING TOO LONG TO CENTRE')
    else
        vedtextright();
        until vedcolumn - vedleftmargin = start do vedcharright() enduntil;
        vedinsertstring(vedargument)
    endif;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep  7 1995
        subdstring -> subvedstring
--- James Goodlet, Feb 28 1989 - dlocalised vedargument.
 */
