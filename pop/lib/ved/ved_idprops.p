/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_idprops.p
 > Purpose:         Insert or update identprops on right of REF entry
 > Author:          Aaron Sloman, Sep 16 1990 (see revisions)
 > Documentation:   * VED_IDPROPS
 > Related Files:
 */


compile_mode:pop11 +strict;
section;

include vedscreendefs;


/*
 * <ENTER> idprops [type]
 * Insert|Update REF file idprops entry on current line
 */
lvars Last_idprops_string='[\{bi}procedure\{-bi}]';
;;;
define global ved_idprops();
    lvars line=vedthisline(), col=locchar_back(`[`, vvedlinesize, line);
    dlocal vedlinemax = 72;

    if col then
        if vedargument=nullstring and col>1 then
            substring(col+1, vvedlinesize-col-1, line) -> vedargument;
        endif;
        col -> vedcolumn; vedcleartail();
    endif;

    if vedargument=nullstring then
        Last_idprops_string
    else
        consdstring(#|
            `[`;
            lvars char;
            ;;; PUT STRING IN BOLD ITALICS
            for char in_string vedargument do;
                char || VEDCMODE_BOLD || VEDCMODE_ALTFONT
            endfor;
            `]`;
        |#) ->> Last_idprops_string;
    endif -> vedargument;

    ved_right(); vednextline();

enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Jun 15 1993
        o Moved into masters
        o Now keeps old idprops of line if no new one specified
--- Adrian Howard, Jun  7 1993
        Now inserts idprops in bold-italics as per new REF file format.
--- Jason Handby, Aug 15 1991
    Added "caching" of last argument used, so that <ENTER> idprops with no
    argument repeats the last one used.
 */
