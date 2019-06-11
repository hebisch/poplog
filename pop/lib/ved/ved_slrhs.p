/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_slrhs.p
 > Purpose:         Automatically create REF file ident entry updaters
 > Author:          Adrian Howard, Jun 16 1993 (see revisions)
 > Documentation:   * ved_slrhs
 > Related Files:
 */

compile_mode: pop11 +strict;
section;

define ved_slrhs();
    lvars arrow_pos, bracket_pos, this_line=vedthisline();

    ;;; IF THERE'S AN ASSIGNMENT ARROW
    if (issubstring(' -> ', this_line) ->> arrow_pos) then

        ;;; COPY LINE
        vedlinebelow();
        copy(this_line) ->> this_line -> vedthisline();

        ;;; REMOVE IDPROPS (IF PRESENT)
        if (locchar_back(`[`, vvedlinesize, this_line) ->> bracket_pos) then
            bracket_pos -> vedcolumn;
            vedcleartail();
        endif;

        ;;; SWAP SIDES AROUND ARROW
        (subdstring(arrow_pos+4, vvedlinesize-arrow_pos-3, this_line)
            sys_>< subdstring(arrow_pos, 4, this_line)
            sys_>< subdstring(1, arrow_pos-1, this_line)
        ) -> vedthisline();
        vedrefreshrange(vedline, vedline, undef);
    else
        vederror('NO ASSIGNMENT ARROW ON LINE');
    endif;

enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Jun 16 1993
        Tidies, made +strict, exported from REF * REFFORM.
 */
