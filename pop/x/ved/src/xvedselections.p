/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.x/x/ved/src/xvedselections.p
 > Purpose:         Selecting words, sentences, paragraphs etc.
 > Author:          Jonathan Meyer, May 28 1991 (see revisions)
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

section $-xved =>
    vedselection_clear
    vedselection_adjust
    vedselection_select
    vedselection_cut
    vedselection_undo
    vedselection_copy
    vedselection_set_primary
    vedselection_paste
    vedselection_help
    vedselection_compile
    vedselectioncoords
;

uses ved_cut;


identof("xvedselectioncoords") -> identof("vedselectioncoords");


/* ======== Managing display of selections ============================= */

define lconstant buffer_column(srow, scol);
    ved_buffer_column(srow fi_- vedwlineoffset, scol fi_- vedwcolumnoffset,
                                                    false)
enddefine;

;;; highlight_part_line: basic text highlight routine
;;;     calls vedrefreshpartline.

define lconstant highlight_part_line(srow, scol_start, scol_end, colour);
    lvars   srow, scol_start, scol_end, colour, buffline, start, num, flags,
            wrline, limchar;

    /* The way we highlight a line is to set inverse mode for the text output
       and then to redraw the string. We redefine screenxy to print spaces
       rather than do a jump so that tabs will be drawn correctly. We can't
       use vedscreenblankpartline because this calls cleartail, which won't
       recognize the highlight mode.
    */

    srow fi_- vedwlineoffset -> wrline;
    wrline fi_+ vedlineoffset -> buffline;
    returnunless(1 fi_<= buffline and buffline fi_<= vvedbuffersize
             and 1 fi_<= srow and srow fi_<= vedscreenlength);

    ved_winrel_column(wrline, 1e6, 2:111) fi_+ 1 fi_+ vedwcolumnoffset -> limchar;
    unless scol_end then limchar -> scol_end endunless;
    returnif(scol_start fi_>= scol_end);

    dlocal vedediting = true, xvedscreenhighlight;

    vedscr_flush_output();
    ;;; if set to a colour num, vedscreencharmode makes text that colour
    colour -> xvedscreenhighlight;

    if colour or srow /== 1 then
        buffer_column(srow, scol_start) -> start;
        fast_subscrv(buffline, vedbuffer) -> buffline;
        if colour and vvedpromptchar and start fi_<= datalength(buffline)
        and (locchar(vvedpromptchar,start,buffline) ->> num)
        then
            ;;; don't highlight prompt
            num fi_+ 1 -> start;
            ved_winrel_column(wrline, start, false) fi_+ vedwcolumnoffset
                                                        -> scol_start;
            returnif(scol_start fi_>= scol_end)
        endif
    else
        ;;; use statusline when clearing status highlight
        vedstatusline -> buffline;
        vedwcolumnoffset fi_+ 1 -> start
    endif;
    if scol_end fi_>= limchar then
        2:00, 0
    else
        2:11, scol_end fi_- scol_start
    endif -> (flags, num);
    vedrefreshpartline(flags, srow, scol_start, start, num, buffline);
    vedscr_flush_output();
enddefine;

;;; turns buffer line/column to screen row/col (performs clipping)
define lconstant clip_coords(vline, vcol);
    lvars vline, vcol, wrline, wrcol, clim;
    vline fi_- vedlineoffset -> wrline;
    if wrline fi_< 1 then
        1 ->> wrline -> wrcol
    elseif wrline fi_>= vedwindowlength then
        vedwindowlength fi_- 1 -> wrline;
        ved_winrel_column(wrline, 1e6, 2:111) fi_+ 1 -> wrcol
    else
        ved_winrel_column(wrline, 1e6, 2:111) fi_+ 1 -> clim;
        ved_winrel_column(wrline, vcol, false) -> wrcol;
        fi_max(1, fi_min(wrcol, clim)) -> wrcol
    endif;
    (wrline fi_+ vedwlineoffset, wrcol fi_+ vedwcolumnoffset)
enddefine;

;;; convert buffer position to screen coordinates
define lconstant screen_coords(vline, vcol, end_vline, end_vcol);
    lvars vline, vcol, end_vline, end_vcol;
    if vline fi_< end_vline and end_vcol == 1 then
        end_vline fi_- 1, 1e6 -> (end_vline, end_vcol)
    endif;
    clip_coords(vline, vcol), clip_coords(end_vline, end_vcol)
enddefine;


/* Selection Adjust Routine */

;;; (should really let you specify the current status line, probably
;;; by giving a buffer line of 0).

;;; buffer coords of last selection on screen
lvars b_last_srow, b_last_scol, b_last_erow, b_last_ecol;


;;; vedselection_adjust - main selection monitoring routine
;;; set selection for window to the specified coordinates

define vedselection_adjust(vline, vcol, end_vline, end_vcol, redraw);
    lvars   i, vline, vcol, end_vline, end_vcol, redraw,
            start_row, start_col, end_row, end_col, edge,
            last_win = xvedselectionon;

    unless wvedwindow == last_win and xvedselectiononstatus == ved_on_status
    then
        ;;; a new selection - clear selection from previous window
        if last_win and wved_is_live_window(last_win) then
            xved_dispatch_event(last_win, "clearSelection", false);
        endif;
        true -> redraw;
        false -> last_win
    endunless;

    ;;; any of the variables can be false to indicate that it isn't moved
    unless vline then fast_subscrv(1,xvedselectioncoords) -> vline endunless;
    unless vcol  then fast_subscrv(2,xvedselectioncoords) -> vcol endunless;
    unless end_vline then fast_subscrv(3,xvedselectioncoords) -> end_vline endunless;
    unless end_vcol  then fast_subscrv(4,xvedselectioncoords) -> end_vcol endunless;

    if vline fi_> end_vline then
        end_vline, vline -> (vline, end_vline);
        vcol, end_vcol -> (end_vcol, vcol);
    elseif vline == end_vline then
        if vcol fi_> end_vcol then
            vcol, end_vcol -> (end_vcol, vcol);
        elseif vcol == end_vcol then
            1,1,1,1 -> (vline, vcol, end_vline, end_vcol)
        endif
    endif;

    fi_max(1, vline) -> vline;
    fi_max(1, end_vline) -> end_vline;

    if vline fi_> vvedbuffersize then
        vvedbuffersize fi_+ 1, 1
    else
        vline, fi_max(1, fi_min(vcol,vedusedsize(subscrv(vline,vedbuffer))))
    endif -> (vline, vcol);

    if end_vline fi_> vvedbuffersize then
        vvedbuffersize fi_+ 1, 1 -> (end_vline, end_vcol)
    elseif end_vcol fi_> vedusedsize(subscrv(end_vline,vedbuffer)) fi_+ 1 then
        end_vline fi_+ 1, 1 -> (end_vline, end_vcol)
    else
        fi_max(1, end_vcol) -> end_vcol
    endif;

    ;;; save new highlight status
    (vline /== end_vline or vcol /== end_vcol)
                                and wvedwindow -> xvedselectionon;
    xvedselectionon and ved_on_status -> xvedselectiononstatus;
    vline, vcol, end_vline, end_vcol -> explode(xvedselectioncoords);

    screen_coords(vline, vcol, end_vline, end_vcol)
                                -> (start_row, start_col, end_row, end_col);

    ;;; from now on we deal with screen coordinates

    dlocal vedscreencursoron = false;
    lvars colour = XptVal[fast] wvedwindow(XtN selectionColorNum:short);

    if redraw then
        ;;; FULL REFRESH
        if last_win == wvedwindow then
            ;;; need to clear the window first
            vedrefresh()
        endif;
        if start_row == end_row then
            highlight_part_line(start_row, start_col, end_col, colour)
        else
            highlight_part_line(start_row, start_col, false, colour);
            fast_for i from start_row fi_+ 1 to end_row fi_- 1 do
                ;;; highlight complete lines
                highlight_part_line(i, 2, false, colour)
            endfor;
            highlight_part_line(end_row, 2, end_col, colour)
        endif

    else
        ;;; OPTIMIZED REFRESH
        lvars   toplim, botlim, s_diff, e_diff, last_srow, last_scol,
                last_erow, last_ecol, wcol1 = vedwcolumnoffset fi_+ 1;

        ;;; synchronise where we think the last selection was with scrolls
        clip_coords(b_last_srow, b_last_scol) -> (last_srow, last_scol);
        clip_coords(b_last_erow, b_last_ecol) -> (last_erow, last_ecol);

        end_row fi_- 1 -> toplim;
        start_row fi_+ 1 -> botlim;
        fi_min(last_srow, toplim) -> toplim;
        fi_max(last_erow, botlim) -> botlim;
        start_row /== last_srow or start_col /== last_scol -> s_diff;
        end_row /== last_erow or end_col /== last_ecol -> e_diff;

        fast_for i from fi_min(start_row, last_srow)
                    to fi_max(end_row, last_erow)
        do
            if (s_diff and last_srow fi_<= i and i fi_<= start_row)
            or (e_diff and end_row fi_<= i and i fi_<= last_erow)
            then
                ;;; clear old line
                highlight_part_line(i, wcol1, false, false)
            endif;
            if i == start_row then
                if i == end_row then
                    highlight_part_line(i, start_col, end_col, colour)
                elseif s_diff or last_srow == last_erow then
                    highlight_part_line(i, start_col, false, colour)
                endif
            elseif i == end_row then
                if e_diff or last_srow == last_erow then
                    highlight_part_line(i, wcol1, end_col, colour)
                endif
            elseif (s_diff and start_row fi_< i and i fi_<= toplim)
                or (e_diff and botlim fi_<= i and i fi_< end_row)
            then
                ;;; new complete line
                highlight_part_line(i, wcol1, false, colour)
            endif
        endfor
    endif;

    ;;; save selection position on screen in buffer coords
    start_row fi_- vedwlineoffset fi_+ vedlineoffset -> b_last_srow;
    buffer_column(start_row, start_col) -> b_last_scol;
    end_row fi_- vedwlineoffset fi_+ vedlineoffset -> b_last_erow;
    buffer_column(end_row, end_col) -> b_last_ecol;

    vedsetcursor();
    vedscr_flush_output();
enddefine;



/* ========= Selection Utility Procedures ============================== */

vars xvedselectionclasses = [];     ;;; set up in vedxvedmouse

/* copies selection to clipboard */

define lconstant seln_copy(to_clip);
    lvars   vline, vcol, end_vline, end_vcol, maxlen, n, try_prompt,
            to_clip, str, saved_line, saved_col;
    dlocal  ved_on_status,
            poplastchar;    ;;; poplastchar set by vedrepeater

    define lconstant selection_lost;
        returnif(iscaller(seln_copy));
        if xvedselectionon then
            xved_raise_event(xvedselectionon, "clearSelection", false);
        endif;
    enddefine;

    define lconstant ignore_prompt();
    enddefine;

    returnunless(xvedselectionon == wvedwindow);
    xvedselectiononstatus -> ved_on_status;

    explode(xvedselectioncoords) -> (vline, vcol, end_vline, end_vcol);
    (vedline, vedcolumn) -> (saved_line, saved_col);
    vedjumpto(vline, vcol);
    vvedbuffersize -> maxlen;
    vvedpromptchar -> try_prompt;;
    consvedstring(#|
        until (vedline fi_> end_vline)
        or (vedline == end_vline and vedcolumn fi_>= end_vcol)
        or (vedline fi_> maxlen)
        do
            if try_prompt and vedcolumn fi_<= vvedlinesize
            and (locchar(try_prompt, vedcolumn, vedthisline()) ->> n)
            then
                n fi_+ 1 -> vedcolumn
            endif;
            vedvedrepeater();
            if dup() == `\n` then vvedpromptchar else false endif -> try_prompt
        enduntil |#) -> str;

    str, selection_lost /*, PRIMARY */  -> vvedclipboard;
    if to_clip then
        ;;; also copy to the clipboard
        str, "CLIPBOARD" -> vvedclipboard;
    endif;

    vedjumpto(saved_line, saved_col);
enddefine;

define vedselection_copy();
    seln_copy(true);
enddefine;

define vedselection_set_primary(trigger_autocut);
    lvars trigger_autocut;
    seln_copy(false);
    trigger_autocut -> xvedselectionautocut
enddefine;


define vedselection_select(type);
    lvars type, list, save = false;

    if type.isinteger then
        xvedselectionclasses(type fi_* 2 fi_- 1) -> list;
    elseunless fast_lmember(type, xvedselectionclasses)->>list then
        vederror('UNKNOWN SELECTION CLASS: ' sys_>< type);
    else
        list.tl.hd -> list;
    endif;

    dlocal vedscreenbell = identfn;

    vedpositionpush(), vedmarkpush(), false -> vvedmarkprops;
    fast_apply(list(1)), vedline, vedcolumn;
    vedpositionpop(), vedpositionpush();
    fast_apply(list(2)), vedline, vedcolumn;
    vedpositionpop(), vedmarkpop();
    vedselection_adjust((), false)
enddefine;

;;; clears current selection
define vedselection_clear(redraw);
    lvars redraw, sline, scol;
    if xvedselectionon then
        dlocal ved_current_file = wved_file_of_window(xvedselectionon);
        vedscreenline -> sline;
        vedscreencolumn -> scol;
        procedure;
            dlocal ved_on_status = xvedselectiononstatus;
            vedselection_adjust(1,1,1,1,redraw);    ;;; turns selection off
        endprocedure();
        if 1 fi_<= sline and sline fi_<= vedwindowlength
        and 1 fi_<= scol and scol fi_< 1000
        then
            vedwindowpoint(sline, scol);
            vedscr_flush_output()
        endif
    endif
enddefine;

/* Cut and Undo */

lconstant undo_cache = newproperty([], 15, false, "tmparg");

define vedselection_cut();
    lvars vline, vcol, end_vline, end_vcol, char, pos;
    dlocal vvedcut_dump, ved_on_status;
    returnunless(xvedselectionon == wvedwindow);
    xvedselectiononstatus -> ved_on_status;

    ;;; copy and clear xved selection
    vedselection_copy();

    ;;; delete text from buffer -
    ;;; uses ved_cut to delete the selection - creates garbage but who cares.
    explode(xvedselectioncoords) -> (vline, vcol, end_vline, end_vcol);
    vedjumpto(vline, vcol);
    if vedcurrentchar() == `\t` then vedtabright(); endif;
    vedpositionpush(); vedpositionstack.hd -> pos;
    vedjumpto(end_vline, end_vcol);
    vedpositionpush();
    ved_cut();
    vedselection_clear(true);

    /* save cut so that it can be undone */
    vvedlastclipboard -> undo_cache(pos);
    setfrontlist(pos, vedpositionstack) -> vedpositionstack;
enddefine;

define vedselection_undo();
    lvars pos, str;
    dlocal ved_on_status;
    if vedinvedprocess then
        false -> ved_on_status;
        for pos in vedpositionstack do
            if undo_cache(pos) ->> str then
                /* first string that we can undo */
                setfrontlist(pos, vedpositionstack) -> vedpositionstack;
                vedpositionpop();
                false -> undo_cache(pos);
                vedinsertstring(str);
                delete(pos, vedpositionstack) -> vedpositionstack;
                return
            endif;
        endfor;
    endif;
    vederror('nothing to undo');
enddefine;


define vedselection_paste();
    lvars clip, n, old_vedline, lines;
    unless (vvedclipboard ->> clip) then
        vederror('nothing on clipboard to paste');
        return
    endunless;

    if (ved_on_status or (vedcrinteractive and vedprocswaiting()))
    and (locchar(`\n`,1,clip) ->> n) then
        ;;; in c/r execute context -- input the text after substituting
        ;;; newline with c/r
        repeat
            `\r` -> fast_subscrs(n,clip);
            quitunless(locchar(`\n`,n+1,clip) ->> n)
        endrepeat;
        xved_raise_ascii(wvedwindow, clip);
        return
    endif;

    ;;; else just use vedinsertstring
    false -> old_vedline;
    ;;; check to see if we are pasting in the window owning the selection
    if xvedselectionon == wvedwindow
    and xvedselectiononstatus == ved_on_status then
        if vedline fi_< xvedselectioncoords(1) then
            ;;; need to move selection after paste
            vedline -> old_vedline;
        elseif vedline fi_<= xvedselectioncoords(3) then
            ;;; clear the selection if we are pasting in the middle of it.
            vedselection_clear(false);
        endif;
    endif;
    vedinsertstring(clip);
    if old_vedline then
        ;;; need to account for inserted lines
        fi_max(0, vedline fi_- old_vedline) -> lines;
        vedselection_adjust(
                xvedselectioncoords(1) fi_+ lines,
                xvedselectioncoords(2),
                xvedselectioncoords(3) fi_+ lines,
                xvedselectioncoords(4), false);
    endif;
enddefine;




define vedselection_compile();
    lvars line, col,  window, file;

    define lconstant try_select_output;
        lvars endline;
        if wvedwindow == window then
            ;;; will do nothing if the cursor has not moved
            vedselection_adjust(line, col, vedline, vedcolumn, true);
            if xvedselectionon then true -> xvedselectionautocut; endif;
        endif;
    enddefine;

    define lconstant lmr_from_clipboard();
        lvars string;

        vedputmessage('please wait');   ;;; getting clipboard may take time
        nullstring -> vedmessage;
        vvedclipboard -> string;

        unless string then vederror('nothing to compile') endunless;

        vedputmessage('doing');
        subsystem_compile(stringin(string), subsystem);
        vedputmessage('done');
    enddefine;

    if xvedselectionon == wvedwindow then
        dlocal  ved_on_status = xvedselectiononstatus,
                0 % , try_select_output() %,
                pop_charin_device, pop_charout_device, pop_charerr_device;

        vedjumpto(
            xvedselectioncoords(3),
            xvedselectioncoords(4)+1, vedselection_clear(false));
        if vedcolumn fi_> vvedlinesize then vedjumpto(vedline+1,1); endif;
        vedline -> line; vedcolumn -> col; wvedwindow -> window;
        if vedlmr_print_in_file.isstring then
            ;;; open output file for printing at end
            consveddevice(vedlmr_print_in_file, 2, true)
        else
            ;;; print in current file
            consveddevice(vedpathname, 2, false)
        endif
            ->> pop_charin_device ->> pop_charout_device -> pop_charerr_device;
        lmr_from_clipboard();
    else
        lmr_from_clipboard();
    endif;
enddefine;

lvars tmpfile = false;

define vedselection_help();
    lvars string;
    dlocal vedediting;

    vedselection_clear(false);
    vedputmessage('please wait');   ;;; getting clipboard may take time
    nullstring -> vedmessage;
    vvedclipboard -> string;
    unless string then vederror('the clipboard is empty') endunless;
    vedputmessage('doing');
    false -> vedediting;
    unless tmpfile then
        vedopen(systmpfile(false, 'clipboard', '.tmp')) -> tmpfile;
    endunless;

    tmpfile -> ved_current_file;
    ved_clear();
    vedinsertstring(string);
    1 ->> vedline -> vedcolumn;
    chain(vedgetsysfile);
enddefine;

    /*  w is always xvedselectionon
        action is next action or false
    */
procedure(w, name, action) -> (w, name, action) with_props #_<[^false]>_#;
    lvars w, name, action;
    if xvedautocut and action then
        ;;; key event in this window immediately after selection
        if action == vedchardelete then
            ;;; simply cut text
            vedjumpto(xvedselectioncoords(1), xvedselectioncoords(2));
            vedselection_cut();
            vedinsertstring('X'); ;;; the character that is deleted
            return;
        elseif action == vedinsertvedchar then
            ;;; cut out the text.
            vedjumpto(xvedselectioncoords(1), xvedselectioncoords(2));
            vedselection_cut();
            return;
        endif;
    endif;
    if action == wved_destroy_window then
        ;;; selection window being destroyed
        false ->> xvedselectionon -> xvedselectiononstatus;
        1,1,1,1 -> explode(xvedselectioncoords)
    else
        vedselection_clear(false)
    endif
endprocedure -> xvedeventtable("clearSelection");

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep  9 1997
        Changes for variable-width mode
--- John Gibson, Jan 22 1996
        Made vedselection_select assign false to vvedmarkprops
--- John Gibson, Sep  8 1995
        Changed seln_copy to use consvedstring and vedvedrepeater
--- Jonathan Meyer, Sep 23 1993
        Added vedselectioncoords as an exported global whose value
        is xvedselectioncoords.
--- John Gibson, Sep  9 1992
        Changed to use XptVal
--- John Gibson, Mar 13 1992
        Moved selection classes stuff into vedxvedmouse.p
--- John Gibson, Jan 18 1992
        Changed seln_copy to use veddrepeater and consdstring
--- John Gibson, Sep 11 1991
        Changed highlight_part_line and seln_copy so that immediate mode
        prompts are not highlighted or included in selection
--- John Gibson, Sep 10 1991
        vedrepeater fixed to deal with tabs properly -- changed seln_copy
        appropriately.
--- John Gibson, Aug 30 1991
        Changed vedselection_paste so that commands get executed in
        appropriate contexts
--- John Gibson, Aug 26 1991
        Made selections work on status line
--- John Gibson, Aug 10 1991
        Made event handlers return their args
--- John Gibson, Aug  9 1991
        Fixed problems in endword and vedselection_adjust
--- Jonathan Meyer, Aug  2 1991 Added vedselection_undo
--- Adrian Howard, Aug  2 1991
        - Made range selection select the whole of the last line in the range
        - Fixed "window" -xvedselectionclasses- option
        - Stopped -xvedselectionclasses- autoloading procedures early
--- Adrian Howard, Aug  1 1991: Fixed paragraph selection
--- Adrian Howard, Jul 31 1991
    - Fixed sentence selection
--- John Gibson, Jul 13 1991
        Made autocut only work on very next event
--- John Gibson, Jul 11 1991
        Removed annoying 'please wait' message from vedselection_paste
--- Jonathan Meyer, Jul  8 1991
        Added to vedselection_set_primary
--- John Gibson, Jul  8 1991
        Changes to vedselection_adjust
--- Jonathan Meyer, Jul  3 1991
        Made paste more clever about current selection
--- Jonathan Meyer, Jun 20 1991
    Added bounds check to vedselection_adjust
 */
