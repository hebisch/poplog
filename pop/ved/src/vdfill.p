/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/ved/src/vdfill.p
 > Purpose:
 > Author:          John Gibson, Dec 21 1992 (see revisions)
 > Documentation:   REF *VEDCOMMS
 */

;;; -------------------- TEXT JUSTIFICATION -------------------------------

#_INCLUDE 'vddeclare.ph'

global constant
        procedure (vedmarkfind, vedmarkpush, vedlinestart, vedmarkpop,
        vedrefreshrange, vedcurrentchar, vedcurrentdchar,
        vedcurrentvedchar, Sys$-Ved$-Check_active_split,
        )
    ;

global vars
        procedure (vedvscr_space_width, vedvscr_average_width,
        vedvscr_substring_width, ved_jcp)
    ;


;;; -----------------------------------------------------------------------

section $-Sys$-Ved =>
        vedinparagraph, vedmarkparagraph, ved_fill, ved_j, ved_jp,
    ;


    /*  Determine whether the current line should be treated as part of
        a paragraph: false means no, non-false means yes, while a
        non-boolean result indicates additionally that this is the
        paragraph start.
    */
define vars vedinparagraph();
    lconstant GRAPHICS =
        '\Sf\Sp\G-\G|\G+\Gle\Gre\Gte\Gbe\Gtl\Gtr\Gbl\Gbr\Glt\Grt\Gtt\Gbt';
    if vvedlinesize == 0 then
        false;
    elseif vedlinestart('.') then
        ;;; traditional roff-type macro
        false;
    else
        ;;; look at the first non-space character
        dlocal vedcolumn;
        vedtextleft();
        lvars c = vedcurrentchar();
        if c fi_>= 16:80 and strmember(c, GRAPHICS) then
            ;;; graphics character or format space => separator line
            false;
        elseif vedcolumn fi_> Get_left_margin() fi_+ 1 then
            ;;; indented line => paragraph start
            1;
        else
            ;;; anything else => paragraph body
            true;
        endif;
    endif;
enddefine;

    /*  Mark the current paragraph, or the preceding paragraph if the
        cursor is not on a paragraph line
    */
define vedmarkparagraph();
    dlocal  vedline, vedcolumn, vvedlinesize;
    lvars   yes;
    ;;; get onto a paragraph line
    until vedinparagraph() ->> yes do
        vedcharup(); ;;; may give top-of-file error
    enduntil;
    ;;; find its first line
    while yes == true and vedline fi_> 1 do
        vedcharup();
        vedinparagraph() -> yes;
    endwhile;
    unless yes then
        vedchardown();
    endunless;
    ;;; mark it
    vedmarklo();
    ;;; find last line
    vedchardown();
    while vedinparagraph() == true do
        vedchardown();
    endwhile;
    vedcharup();
    ;;; mark it
    vedmarkhi();
enddefine;

    /*  Justify text in the marked range
    */
define vars ved_fill();
    lvars   c, tail_cleared, oldediting, line_needs_refreshing, lwidth,
            oldchanged, leftmcol, string, linemax;
    dlocal  vedediting, vvedworddump, vedpositionstack, vedstatic,
            vedautowrite;

    define Line_refresh(line);
        lvars line;
        fi_max(vvedmarkhi, line) -> vvedmarkhi;
        vedrefreshrange(line, line, undef)
    enddefine;

    define Fillable_line();
        lvars c; dlocal vedcolumn = 1;
        vvedlinesize /== 0
        and (vedcurrentchar() ->> c) /== `\Sf` and c /== `\Sp`
    enddefine;

    define Clear_trail_space();
        lvars some = false;
        vedtextright();
        while vedcolumn fi_> 1 do
            vedcharleft();
            if vedcurrentchar() == `\s` then
                true -> some
            else
                vedcharright();
                quitloop
            endif
        endwhile;
        if some then vedcleartail() endif
    enddefine;

    define Clear_tail();
        Check_active_split(vedcolumn, vedthisline()) -> ;
        vedcleartail();
        Clear_trail_space()
    enddefine;

    define Explode_spcont(prevlen, ssub, string, len);
        lvars ssub, string, len, c, prevlen, prevc = 0;
        if prevlen /== 0 then dup() -> prevc endif;
        `\s`;
        Explode_subvedstring(ssub, string, len);
        if prevc &&/=_0 VEDCMODE_ACTIVE
        and len /== 0 and ispair(subscr_stack(len) ->> c)
        and fast_back(c) = nullstring
        and (fast_front(c) ->> c) &&/=_0 VEDCMODE_ACTIVE
        then
            ;;; active continuation mark following active -- join up
            c -> subscr_stack(len);
            ;;; replace the space with an active one
            c fi_&&~~ #_< VEDCMODE_SP_INSIG_BITS||16:FFFF >_# fi_|| `\s`
                        -> subscr_stack(len fi_+ 1)
        endif
    enddefine;


    vedchanged -> oldchanged;
    false ->> vedautowrite -> vedstatic;
    vedediting -> oldediting;
    vedmarkfind();
    Get_left_margin() fi_+ 1 -> leftmcol;
    false -> line_needs_refreshing;
    vedvscr_average_width(vedlinemax) -> linemax;

    repeat
        Clear_trail_space();
        until Fillable_line() do
            ;;; leave current line alone
            vedchardown();
            quitif(vedline fi_> vvedmarkhi) (2)
        enduntil;

        ;;; adjust indentation if not already done
        vedthisline() -> string;
        vedtextleft();
        if vedcolumn fi_< leftmcol then
            consvedstring(#|
                 fast_repeat leftmcol fi_- vedcolumn times `\s` endrepeat,
                 Explode_subvedstring(string)
            |#) ->> string -> vedthisline();
            true -> line_needs_refreshing
        endif;

        ;;; trim line or refresh or both
        false -> tail_cleared;
        vedtextright();
        while (vedvscr_substring_width(string,1,vedcolumn fi_- 1) ->> lwidth)
                fi_> linemax do
            true -> tail_cleared;
            repeat
                vedcharleft();
                quitif(vedcurrentchar() == `\s`);
                if vedcolumn fi_<= leftmcol then
                    false -> tail_cleared;
                    vedputmessage('\{b}warning: \{}line ' <> (vedline sys_><
                                nullstring) <> ' \{b}cannot be justified');
                    quitloop(2)
                endif
            endrepeat
        endwhile;

        if line_needs_refreshing then
            if tail_cleared then
                false -> vedediting;
                Clear_tail();
                oldediting -> vedediting;
                Line_refresh(vedline);
            endif
        elseif tail_cleared then
            Clear_tail()
        endif;

        ;;; next line
        vedchardown(); vedtextleft();

        if tail_cleared then
            ;;; put trimmings on next line
            if vedline fi_<= vvedmarkhi
            and Fillable_line()
            and vedcolumn fi_<= leftmcol and leftmcol fi_<= vvedlinesize
            then
                consvedstring(#|
                     #| fast_repeat leftmcol fi_- 1 times `\s` endrepeat,
                        Explode_subvedstring(2, vvedworddump, datalength(vvedworddump) fi_- 1)
                     |#,
                     Explode_spcont((), vedcolumn, vedthisline(),
                                            vvedlinesize fi_- vedcolumn fi_+ 1)
                |#) -> vedthisline()
            else
                vedlineabove();
                consvedstring(#|
                     fast_repeat leftmcol fi_- 1 times `\s` endrepeat,
                     Explode_subvedstring(2, vvedworddump, datalength(vvedworddump) fi_- 1)
                |#) -> vedthisline()
            endif
        elseif vedline fi_> vvedmarkhi then
            quitloop
        elseif Fillable_line()
        and vvedlinesize fi_>= vedcolumn   ;;; this line not blank
        and vedcolumn fi_<= leftmcol
        then
            ;;; if there's a space to break at (within the width available at
            ;;; the end of the previous line), join this line to previous
            linemax fi_- lwidth fi_- vedvscr_space_width(1) -> lwidth;
            vedcolumn -> c;
            until (vedcharright(), vedcurrentchar() == `\s`) do enduntil;
            if vedvscr_substring_width(vedthisline(), c, vedcolumn fi_- c)
                        fi_<= lwidth then
                c -> vedcolumn;
                false -> vedediting;
                vedcleartail();
                oldediting -> vedediting;
                vedlinedelete();
                vedcharup();
                1 -> vedcolumn;
                consvedstring(#|
                    #| Explode_subvedstring(vedthisline()) |#,
                    Explode_spcont((), 1, vvedworddump, datalength(vvedworddump))
                |#) -> vedthisline();
                true -> line_needs_refreshing;
                nextloop
            elseif line_needs_refreshing then
                Line_refresh(vedline fi_- 1)
            endif;
        elseif line_needs_refreshing then
            Line_refresh(vedline fi_- 1)
        endif;
        tail_cleared -> line_needs_refreshing;
    endrepeat;

    if line_needs_refreshing then
        Line_refresh(vedline fi_- 1)
    endif;
    if oldchanged then oldchanged fi_+ 1 else 1 endif -> vedchanged;
    ;;; put cursor at end of text in marked range.
    vedcharup(); vedtextright();
enddefine;      /* ved_fill */

    /*  Justify the marked range, using ved_tidy for compileable files
    */
define vars ved_j();
    if vedcompileable then
        ved_tidy();
    else
        ved_fill();
    endif;
enddefine;

    /*  Justify the current paragraph or procedure
    */
define vars ved_jp();
    if vedcompileable then
        ved_jcp();
    else
        vedmarkpush();
        false -> vvedmarkprops;
        vedmarkparagraph();
        ved_fill();
        vedmarkpop();
    endif;
enddefine;

endsection;     /* $-Sys$-Ved */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov  3 1995
        Changes to cope with active continuation markers
--- John Gibson, Sep  7 1995
        Changed to use new consvedstring and Explode_subvedstring
--- John Gibson, Jul 27 1995
        Changed vedmarkparagraph to a constant
--- Robert John Duncan, May 16 1994
        Restored the previous behaviour of ved_jp so that it doesn't save
        the cursor position (re. bugreport davidy.84).
--- Robert John Duncan, Jan 18 1994
        Added paragraph procedures and simplified ved_jp accordingly
--- John Gibson, Apr 22 1993
        Changed ved_fill so that lines beginning with \Sf or \Sp are
        left untouched.
 */
