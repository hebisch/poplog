/* --- Copyright University of Sussex 1995.  All rights reserved. ---------
 > File:           C.all/lib/ved/ved_jj.p
 > Purpose:        Full text justification (both left and right margins)
 > Author:         Chris Slymon, June 1983 (see revisions)
 > Documentation:  HELP * FORMAT
 */
compile_mode :pop11 +strict;

/*  From an original design by Dave Roberts.
    Any line that is followed by a line beginning with a space is assumed to
    be the last line of a paragraph and is not adjusted.
*/

section;

define vars ved_jj;
    lvars   leftmcol, string, place, num, oldmax, mustrefresh,
            oldline = vedline,
            oldoffset = vedlineoffset,
            oldchanged = vedchanged,
            wasediting = vedediting;

    dlocal  vedstatic = false, vedediting = false, vedautowrite = false;

        ;;; Move right one word, delimited by space
    define wordright() -> (inspace, spchar);
        lvars inspace, spchar, string = vedthisline(), col;

        locchar(`\s`,vedcolumn,string) -> col;
        if col and col fi_<= vvedlinesize then
            col -> vedcolumn;
            if skipchar(`\s`,col,string) ->> col then
                col -> vedcolumn;
                col-1
            else
                vedcolumn
            endif -> col;
            subscrdstring(col,string) -> spchar
        else
            vvedlinesize fi_+ 1 -> vedcolumn
        endif;
        ;;; return true unless reached end
        vedcolumn fi_< vvedlinesize -> inspace
    enddefine;

    define insertspaces();
        ;;; Pad current line with spaces
        lvars   gaps = 0, spaces = vedlinemax fi_- vvedlinesize,
                quotient, remainder, done = 0, c, spchar,
                space_free, pixels, pgaps, pquot, prem, pdone = 0;

        vedvscr_average_width(vedlinemax)
            - vedvscr_substring_width(vedthisline(), 1, vvedlinesize)
                -> space_free;

        1 -> vedcolumn;
        returnif((vedcurrentchar() ->> c) == `\Sf` or c == `\Sp`
                    or space_free <= 0);

        vedtextleft();
        ;;; count the gaps
        while (wordright() ->) do gaps fi_+ 1 -> gaps endwhile;
        returnif(gaps == 0);        ;;; nowhere to pad

        space_free // vedvscr_space_width(1) -> (pixels, spaces);
        spaces // gaps -> (remainder, quotient);
        gaps - remainder -> pgaps;
        pixels // pgaps -> (prem, pquot);

        vedtextleft();

        while (wordright() -> spchar) do
            repeat quotient times vedcharinsert(spchar) endrepeat;
            spaces - quotient -> spaces;
            done + remainder -> done;
            if done >= gaps then
                vedcharinsert(spchar);
                done - gaps -> done
            else
                ;;; in var width mode, pad with hair spaces
                (spchar &&~~ 16:FFFF) || 16:9A -> spchar;   ;;; `\Sh`
                repeat pquot times vedcharinsert(spchar) endrepeat;
                pixels - pquot -> pixels;
                pdone + prem -> pdone;
                if pdone >= pgaps then
                    vedcharinsert(spchar);
                    pdone - pgaps -> pdone
                endif
            endif;
            quitif(spaces == 0 and pixels == 0)     ;;; done
        endwhile
    enddefine;

    define windowbottom();
        vedlineoffset + vedwindowlength - 1
    enddefine;

    false -> vedchanged;
    not(vvedmarkhi <= vedlineoffset or vvedmarklo > windowbottom())
        -> mustrefresh;

    if isprocedure(vedleftmargin) then vedleftmargin()
    else vedleftmargin
    endif fi_+ 1 -> leftmcol;

    vvedmarkhi -> oldmax;

    vedpositionpush();
    ved_gobble();
    ved_fill();
    vedmarkfind();

    while vedline fi_< vvedmarkhi do
        ;;; Do not fill last line of paragraph, or line without gaps
        fast_subscrv(vedline fi_+ 1, vedbuffer) -> string;
        unless datalength(string) fi_< leftmcol
        or strmember(subscrs(leftmcol, string), '\s\t')
        or strmember(subscrs(1, string), '\Sf\Sp')
        then
            insertspaces()
        endunless;
        vednextline()
    endwhile;

    vedpositionpop();

    wasediting -> vedediting;

    oldmax - vvedmarkhi -> num;
    if mustrefresh then
        if vvedmarkhi <= vedlineoffset then
            ;;; range just disappeared off the top of screen
            oldmax - oldoffset -> num; ;;; lines disappeared
            2 -> place;
        elseif vvedmarklo <= vedlineoffset then
            ;;; Start of range off screen, part visible.
            ;;; calculate reduction
            if vvedmarkhi < windowbottom() then
                ;;; Number of lines reduced in range
                (oldmax - oldoffset) - (vvedmarkhi - vedlineoffset) -> num;
                if num > 0 then vvedmarkhi - vedlineoffset + 1
                else 2
                endif -> place; ;;; from where to scroll up or down
            else
                0 -> num;
            endif;
        else
            ;;; start of range on screen
            if vvedmarkhi >= windowbottom() then
                ;;; Start of range on screen, extends to bottom
                0 -> num;   ;;; no scrolling needed
            else
                oldmax - vvedmarkhi -> num; ;;; lines shortened
                if num > 0 then vvedmarkhi else vvedmarklo endif
                    - vedlineoffset + 1 -> place;
            endif;
        endif;
        ;;; now do refreshing and scrolling
        if num > 0 then
            ;;; scroll up and refresh bottom
            repeat num times
                vedscreenscrollregionup(place, vedwindowlength);
            endrepeat;
            vedrefreshrange(
                windowbottom() - num + 1,
                windowbottom(), undef);
        elseif num < 0 then
            repeat -num times
                vedscreenscrollregiondown(place, vedwindowlength)
            endrepeat;
        else
            ;;; num = 0, so no scrolling
        endif;
        vedrefreshrange(vvedmarklo,vvedmarkhi,undef);
    endif;
    if oldchanged then oldchanged + 1 else 1 endif -> vedchanged;
enddefine;

endsection;


/*  --- Revision History ---------------------------------------------------
--- John Gibson, Apr 24 1999
        Now does padding in var width mode with Ved hair space chars
--- John Gibson, Oct 23 1995
        Made insertspaces insert space characters with the same attributes
        as the space preceding the insert point
--- John Gibson, Apr 22 1993
        Made insertspaces ignore lines beginning with \Sf or \Sp
--- Aaron Sloman, May 29 1990
        Altered to minimize screen activity - complained about in bug report
 */
