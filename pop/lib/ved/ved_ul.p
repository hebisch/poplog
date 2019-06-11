/*  --- Copyright University of Sussex 1992.  All rights reserved. ---------
 *  File:           C.all/lib/ved/ved_ul.p
 *  Purpose:        underline in marked range
 *  Author:         Roger Evans, April 1983 (see revisions)
 *  Documentation:  HELP *VEDCOMMS/ved_ul and comments below
 *  Related Files:  HELP * FORMAT
 */

compile_mode: pop11 +strict;

section;


/*
 * <ENTER> ul [mark[toggle]]
 * This command scans the marked range taking special action when the
 * following characters are encountered:
 *      # - replace with a space and mark the next word for underlining
 *      $ - replace with space and change mode: underline all words
 *          UNLESS preceded by # (until another $ is encountered - then
 *          change back)
 * ved_ul may optionally be given a single character mark as an
 * argument, which then is used instead of #. A second character toggle,
 * if supplied, is used instead of $.
 */
define global vars ved_ul();
    lvars   mark_char = `#`,
            toggle_char = `$`,
            n_args = datalength(vedargument);

    unless n_args == 0 then
        subscrs(1, vedargument) -> mark_char;
        if n_args == 2 then
            subscrs(2, vedargument) -> toggle_char;
            if mark_char == toggle_char then
                vederror('MARK AND TOGGLE CHARACTERS MUST DIFFER');
            endif;
        elseunless n_args == 1 do
            vederror('UL MARK[TOGGLE]');
        endif;
    endunless;

    vedmarkfind();

    lvars underlining=false, line, char;
    for line from vvedmarklo to vvedmarkhi do;
        vedjumpto(line, 1);
        repeat vvedlinesize times;
            vedcurrentchar() -> char;
            if char == toggle_char then
                not(underlining) -> underlining;
                ` ` -> vedcurrentchar();
                vedcharright();
            elseif char == mark_char then
                ` ` -> vedcurrentchar();
                if underlining then
                    veddo('do;chat c +u;chat w -u');
                else
                    veddo('chat w +u');
                endif;
            elseif underlining then
                veddo('chat c +u');
            else
                vedcharright();
            endif;
        endrepeat;
    endfor;

enddefine;


endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Sep  7 1992
        Completely rewrote using new VED character attribute facilities.
 */
