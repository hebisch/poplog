/*  --- Copyright University of Sussex 1999.  All rights reserved. ---------
 >  File:           C.x/x/ved/src/xvedscreen.p
 >  Purpose:        Defines ved settings for XVED Screen
 >  Author:         Jonathan Meyer, 20 July 1990 (see revisions)
 >  Documentation:  SYSDOC *XVED
 */
compile_mode :pop11 +strict;

section $-xved => vedxvedscreen;

#_INCLUDE 'xved_declare.ph'
include XpwScrollText.ph;
include vedscreendefs.ph;


/************************************************************************
 * Xved Screen Output
 ************************************************************************/

/* The output buffer is a fixed length string. Characters are written into
   it, and then flushed, causing XpwText widget write methods to be called.
   The buffer is also flushed whenever it becomes full. Note that flushing
   the buffer does NOT flush the X output buffer. The scrolltext widget is
   operated in non-auto-flush mode. Output flushing is performed by a read
   on the raw input device.

*/


;;; state variables

vars
    xvedscreenhighlight = false,
;

lconstant macro (
    BUF_SIZE    = 128,
    fexacc      = [exacc[fast]],
);

lconstant
    buffer8     = writeable init_fixed(BUF_SIZE+1, string_key),
    buffer16    = writeable init_fixed(BUF_SIZE+1, string16_key),
    tmpint      = EXPTRINITSTR(:int),
;

lvars
    buf_pos         = 1,
    buffer          = buffer8,
    curr_window     = false,
    did_char_flush  = false,
;

XptLoadProcedures xvedscreen [^^XPW_EXLIBS]
lvars
    XpwCallMethod(win,method,...),
    XFlush(dpy),
;


;;; Return initial vector for window's xved_screendatavec
define xved_init_screendatavec(use_buffer16);
    consvector(if use_buffer16 then buffer16 else buffer8 endif,
                0, XpwMWriteAtCursor, 0, 1, 1, SDVEC_LEN)
enddefine;


;;; INTERNAL PROCEDURES
;;; Flush_out - internal flush, calls methods to write characters to screen

define lconstant Flush_out(do_xflush);
    lvars do_xflush, win = curr_window;

    if wved_is_live_window(win) then
        if buf_pos /== 1 then
            0 -> fast_subscrs(buf_pos, buffer); ;;; null terminate string
            fexacc (4) raw_XpwCallMethod(win,
                                fast_subscrv(SDVEC_INSERT_MODE,
                                                xvedwin_screendatavec(win)),
                                buffer, buf_pos fi_- 1);
            true -> did_char_flush
        endif;
        if do_xflush and did_char_flush then
            exacc raw_XFlush(xveddisplay);
            false -> did_char_flush
        endif
    else
        false ->> win -> curr_window
    endif;
    1 -> buf_pos;

    if wvedwindow /== win and wved_is_live_window(wvedwindow) then
        wvedwindow -> curr_window
    endif
enddefine;


/* ======== Ved Screen Output Hooks ============================ */

define lconstant set_unicode_mode(sdvec);
    ;;; locally set off WM size hints and reset them again after
    ;;; setting XpwICMUnicode (which may cause the font width to change
    ;;; and the window to resize)
    dlocal % xved_size_hints_set(curr_window) % = false;
    Flush_out(false);
    XpwICMUnicode -> XptVal curr_window(XtN inputCharMode:ushort);
    buffer16 ->> buffer -> fast_subscrv(SDVEC_BUFFER,sdvec)
enddefine;

define :XVED_FOR vedscr_substring_out(string, csub, nchars);
    lvars c, take, string, csub, nchars, do_write, sdvec, charmode;
    dlocal pop_asts_enabled = false;

    define haschar16(string, csub, nchars);
        fast_repeat nchars times
            returnif(fast_subscrs(csub, string) fi_> 16:FF) (true);
            csub fi_+ 1 -> csub
        endrepeat;
        false
    enddefine;

    if curr_window /== wvedwindow then Flush_out(false) endif;
    xvedwin_screendatavec(wvedwindow) -> sdvec;

    fast_subscrv(SDVEC_BUFFER,sdvec) -> buffer;
    if isstring16(string) and buffer == buffer8
    and haschar16(string, csub, nchars) then
        set_unicode_mode(sdvec)
    endif;
    fast_subscrv(SDVEC_CHARMODE,sdvec) -> charmode;

    until nchars == 0 do
        nchars -> take; false -> do_write;
        if buf_pos fi_+ take fi_> BUF_SIZE then
            BUF_SIZE fi_- buf_pos fi_+ 1 -> take;
            true -> do_write;
        endif;
        if charmode &&/=_0 VEDCMODE_GRAPHIC then
            ;;; graphics mode
            fast_repeat take times
                fast_subscrs(csub, string) -> c;
                if `_` fi_<= c and c fi_<= `~` then
                    if (c fi_- `_` ->> c) == 0 then `\^?` -> c endif
                endif;
                c -> fast_subscrs(buf_pos, buffer);
                csub fi_+ 1 -> csub;
                buf_pos fi_+ 1 -> buf_pos;
            endrepeat
        else
            move_subvector(csub, string, buf_pos, buffer, take);
            csub fi_+ take -> csub;
            buf_pos fi_+ take -> buf_pos;
        endif;
        nchars fi_- take -> nchars;
        if do_write then Flush_out(false) endif;
    enduntil
enddefine;

define :XVED_FOR vedscr_char_out(c);
    lvars c, sdvec;
    if isstring(c) then
        chain(c, 1, datalength(c), vedscr_substring_out)
    endif;

    dlocal pop_asts_enabled = false;
    if curr_window /== wvedwindow then Flush_out(false) endif;
    xvedwin_screendatavec(wvedwindow) -> sdvec;

    fast_subscrv(SDVEC_BUFFER,sdvec) -> buffer;
    if c fi_> 16:FF and buffer == buffer8 then
        set_unicode_mode(sdvec)
    endif;

    if fast_subscrv(SDVEC_CHARMODE,sdvec) &&/=_0 VEDCMODE_GRAPHIC
    and `_` fi_<= c and c fi_<= `~` then
        if (c fi_- `_` ->> c) == 0 then `\^?` -> c endif
    endif;
    c -> fast_subscrs(buf_pos, buffer);
    buf_pos fi_+ 1 -> buf_pos;
    if buf_pos fi_> BUF_SIZE then Flush_out(false) endif
enddefine;

define :XVED_FOR vedscr_flush_output();
    dlocal pop_asts_enabled = false;
    Flush_out(true);
enddefine;


;;; --- THINGS REDEFINED IN VAR WIDTH MODE ------------------------------

lvars varwidth_idents = [%
    conspair(true,  ident vedvarwidthmode),
    conspair(16:9A, ident vedscreenhairspacemark),  ;;; = `\Sh`
%];

define :define_form lconstant XVED_VSCR;
    DO_XVED_FOR("XVED_VSCR_", "varwidth_idents")
enddefine;

define lconstant tw_substringout() with_nargs 4;
    dlocal vedscreencolumn, vedscreencharmode, vedonstatus = false;
    vedscreensubstringout();
    Flush_out(false);
enddefine;

define :XVED_VSCR vedvscr_substring_width(string, csub, nchars);
    dlocal pop_asts_enabled = false;
    unless nchars then
        vedusedsize(string) fi_- csub fi_+ 1 -> nchars
    endunless;
    Flush_out(false);
    lvars w = wvedwindow;
    fexacc (2) raw_XpwCallMethod(w, XpwMBeginTextWidthMode1);
    tw_substringout(string, csub, nchars, true);
    fexacc (2):int raw_XpwCallMethod(w, XpwMEndTextWidthMode)
enddefine;

define :XVED_VSCR vedvscr_substring_num_fit(string, csub, maxchars, width);
    dlocal pop_asts_enabled = false;
    unless maxchars then
        vedusedsize(string) fi_- csub fi_+ 1 -> maxchars
    endunless;
    Flush_out(false);
    lvars w = wvedwindow;
    fexacc (3) raw_XpwCallMethod(w, XpwMBeginTextWidthMode2, width);
    tw_substringout(string, csub, maxchars, true);
    fexacc (2):int raw_XpwCallMethod(w, XpwMEndTextWidthMode)
enddefine;

define :XVED_VSCR vedvscr_offsets_of_column(string, col, alt_char);
    lvars usize, nchars;
    dlocal pop_asts_enabled = false;
    Flush_out(false);
    lvars w = wvedwindow;

    ;;; get width from start of string to previous col
    fexacc (2) raw_XpwCallMethod(w, XpwMBeginTextWidthMode1);
    vedusedsize(string) -> usize;
    col fi_- 1 -> col;
    if col fi_> usize then usize else col endif -> nchars;
    tw_substringout(string, 1, nchars, true);
    if col fi_> usize then
        fexacc (4) raw_XpwCallMethod(w, XpwMWriteAtCursor, 0, col fi_- usize)
    endif;
    fexacc (2):int raw_XpwCallMethod(w, XpwMEndTextWidthMode);

    ;;; now additional width for char at col, or alt_char if supplied
    fexacc (2) raw_XpwCallMethod(w, XpwMBeginTextWidthMode1);
    if alt_char then
        procedure();
            dlocal vedscreencolumn, vedscreencharmode, vedonstatus = false;
            vedscreenoutput();
            Flush_out(false);
        endprocedure(alt_char)
    else
        col fi_+ 1 -> col;
        if col fi_> usize then
            fexacc (4) raw_XpwCallMethod(w, XpwMWriteAtCursor, 0, 1)
        else
            tw_substringout(string, col, 1, true)
        endif
    endif;
    dup() fi_+ fexacc (2):int raw_XpwCallMethod(w, XpwMEndTextWidthMode)
enddefine;

define :XVED_VSCR vedvscr_set_wlinewidth();
    dlocal pop_asts_enabled = false;
    Flush_out(false);
    fexacc (3):int raw_XpwCallMethod(wvedwindow, XpwMGetVarRowWidth,
                                        if vedonstatus then 0 else 1 endif)
            -> vedwlinewidth
enddefine;


define :XVED_VSCR vedvscr_set_wline_start(wline, string, usize)
                                            /* -> (index, pixrem, diffs) */;
    lvars res, ncols, pixrem;
    dlocal pop_asts_enabled = false;
    Flush_out(false);
    lvars w = wvedwindow;
    fexacc (3) raw_XpwCallMethod(w, XpwMBeginTextWidthMode2, vedcolumnoffset);
    tw_substringout(string, 1, usize, true);
    fexacc (2):int raw_XpwCallMethod(w, XpwMEndTextWidthMode) -> res;
    res fi_&& 16:FFFF -> ncols;
    ncols fi_+ 1,
    res fi_>> 16 ->> pixrem,
    fexacc (5):int raw_XpwCallMethod(w, XpwMSetVarColumnOffset, wline fi_- 1,
                                        ncols, pixrem)
enddefine;

define :XVED_VSCR vedvscr_refresh_part_line(wline, wcol, string, csub, nchars,
                                            usize, no_opt);
    dlocal pop_asts_enabled = false;
    vedscreensubstringout(string, csub, nchars, no_opt);
    Flush_out(false);
    ;;; get column reached    **** replace with fast get method?
    XptVal[fast] wvedwindow(XtN cursorColumn:int) fi_+ 1 -> vedscreencolumn
enddefine;

define :XVED_VSCR vedvscr_average_width(cols);
    dlocal pop_asts_enabled = false;
    Flush_out(false);
    ((XptVal[fast] wvedwindow(XtN fontAverageWidth:int) fi_* cols) fi_+ 9)
                    fi_div 10
enddefine;

define :XVED_VSCR vedvscr_space_width(cols);
    dlocal pop_asts_enabled = false;
    Flush_out(false);
    XptVal[fast] wvedwindow(XtN spaceWidth:int) fi_* cols
enddefine;

define :XVED_VSCR vedvscr_window_column_bounds(wrline, need_lastcol);
    lvars wline;
    dlocal pop_asts_enabled = false;
    Flush_out(false);
    wrline fi_+ vedwlineoffset fi_- 1 -> wline;
    dup(fexacc (4):int raw_XpwCallMethod(wvedwindow,
                                XpwMGetVarColumnOffset, wline, tmpint)),
        if fexacc :int tmpint /== 0 then () fi_+ 1 endif;
    if need_lastcol then
        dup(fexacc (4):int raw_XpwCallMethod(wvedwindow,
                            XpwMGetVarColumnLimOffset, wline, tmpint)),
            if fexacc :int tmpint /== 0 then () fi_- 1 endif
    else
        0, 0
    endif
enddefine;

define xved_set_varwidthmode();
    lvars   vwm = XptVal wvedwindow(XtN varWidthMode:XptBoolean),
            pair, val, id;
    returnif(vwm == vedvarwidthmode);
    fast_for pair in varwidth_idents do
        fast_destpair(pair) -> (val, id);
        vwm -> is_vedfile_local(id);
        if vwm then val -> fast_idval(id) endif
    endfor;
    vedvscr_set_wlinewidth()
enddefine;


/**************************************************************************
 * Screen Control Procedures
 **************************************************************************/


/* SCROLL OPERATIONS */

define lconstant scroll(col, row, ncols, nrows, cdis, rdis);
    lvars col, row, ncols, nrows, cdis, rdis;
    dlocal pop_asts_enabled = false;
    Flush_out(false);

    ;;; adjust for base 0
    col fi_- 1 -> col;
    row fi_- 1 -> row;

    if rdis /== 0 then
        ;;; up/down
        if rdis fi_< 0 then
            ;;; up
            row fi_- rdis -> row;
            nrows fi_+ rdis -> nrows
        else
            ;;; down
            nrows fi_- rdis -> nrows
        endif;
    else
        ;;; left/right
        if cdis < 0 then
            ;;; left
            col fi_- cdis -> col;
            ncols fi_+ cdis -> ncols
        else
            ;;; right
            ncols fi_- cdis -> ncols
        endif
    endif;
    fexacc (8) raw_XpwCallMethod(wvedwindow, XpwMScroll, col, row,
                                                ncols, nrows, cdis, rdis)
enddefine;

define lconstant setscrollregion(top, bot) with_props vedsetscrollregion;
    lvars top, bot;
    dlocal pop_asts_enabled = false;
    Flush_out(false);
    (top fi_<< 12) fi_|| bot
        -> fast_subscrv(SDVEC_SCROLLREGION, xvedwin_screendatavec(wvedwindow))
enddefine;

define lconstant scrollregion(rdis);
    lvars rdis, top, bot, scrollregion;
    dlocal pop_asts_enabled = false;
    Flush_out(false);
    fast_subscrv(SDVEC_SCROLLREGION, xvedwin_screendatavec(wvedwindow))
            -> scrollregion;
    scrollregion fi_&& 16:FFF -> bot;
    scrollregion fi_>> 12 -> top;
    scroll(1, top, 0, bot fi_- top fi_+ 1, 0, rdis)
enddefine;
;;;
define lconstant scrollup = scrollregion(% -1 %) enddefine;
define lconstant scrolldown = scrollregion(% 1 %) enddefine;

define lconstant Scrollvertrange(srow, erow, rdis, buffline);
    lvars   n, row, incr, srow, erow, rdis, buffline,
            vbl = datalength(vedbuffer);

    until rdis == 0 do
        if rdis fi_> 0 then
            ;;; scroll up
            erow, 1, fi_min(xvedvscrollstep, rdis)
        else
            ;;; scroll down
            srow, -1, fi_max(-xvedvscrollstep, rdis)
        endif -> (row, incr, n);
        row fi_- n  -> row;
        rdis fi_- n -> rdis;
        scroll(1, srow, 0, erow fi_- srow fi_+ 1, 0, -n);
        repeat
            row fi_+ incr -> row;
            vedrefreshline(true, row, if buffline fi_> vbl then nullstring
                                       else fast_subscrv(buffline, vedbuffer)
                                       endif, vedmarked(buffline));
            buffline fi_+ incr -> buffline;
            quitif((n fi_- incr ->> n) == 0)
        endrepeat
    enduntil
enddefine;

define lconstant Scrollregion(srow, erow, rdis, buffline) with_props vedscrollregion;
    lvars srow, erow, rdis, buffline, tmp;
    returnunless(vedediting);
    -rdis -> rdis;      ;;; has opposite sign
    if rdis fi_< 0 then
        ;;; scrolling down -- the erow arg specifies the first line that
        ;;; will be deleted (and the rdis arg can take it off the screen)
        if (srow fi_- rdis fi_- 1 fi_- vedwindowlength ->> tmp) fi_> 0 then
            rdis fi_+ tmp -> rdis
        endif;
        fi_min(erow fi_- rdis fi_- 1, vedwindowlength) -> erow;
        buffline fi_- rdis fi_- 1 -> buffline
    else
        ;;; scrolling up
        if (erow fi_- rdis fi_- 1 ->> tmp) fi_< 0 then
            rdis fi_+ tmp  -> rdis;
            buffline fi_- tmp -> buffline;
            2 -> srow
        endif
    endif;
    Scrollvertrange(srow, erow, rdis, buffline)
enddefine;

define lconstant Scrollvert(nrows) with_props vedscrollvert;
    lvars nrows, newoffs = vedlineoffset fi_+ nrows;
    if newoffs fi_< 0 then
        nrows fi_- newoffs -> nrows;
        0 -> newoffs
    endif;
    Scrollvertrange(2, vedwindowlength, nrows,
                        if nrows fi_> 0 then
                            vedlineoffset fi_+ vedwindowlength
                        else
                            vedlineoffset
                        endif);
    newoffs -> vedlineoffset;
    if vedlineoffset fi_+ vedwindowlength fi_>= datalength(vedbuffer) then
        vedbufferextend()
    endif
enddefine;

define lconstant Scrollhorz(dis) with_props vedscrollhorz;
    lvars n, scrollstep = vedvscr_average_width(xvedhscrollstep);
    dlocal vedscreencursoron = false;

    define do_scrollhorz(dis);
        lvars dis;
        chain(vedwcolumnoffset fi_+ 1, vedwlineoffset fi_+ 1,
                vedwlinewidth, vedwindowlength fi_- 1, dis, 0, scroll)
    enddefine;

    define var_stack_nextcols();
        lvars wline;
        dlocal pop_asts_enabled = false;
        Flush_out(false);
        fast_for wline from vedwindowlength fi_+ vedwlineoffset fi_- 2
                    by -1 to vedwlineoffset do
            fexacc (4):int raw_XpwCallMethod(wvedwindow,
                                XpwMGetVarColumnLimOffset, wline, tmpint),
                if fexacc :int tmpint == 0 then () fi_+ 1 endif
        endfor
    enddefine;

    define refreshrange(wcol, ncols, draw_mark);
        lvars   col, n, wcol, ncols, scol, line, vrow = 1,
                coffs = vedcolumnoffset, clim = coffs fi_+ vedwlinewidth,
                vbl = datalength(vedbuffer), used, string, draw_mark,
                procedure subscrp = if vedonstatus then fast_subscrvedstring
                                      else fast_subscrdstring
                                      endif;
        unless draw_mark then 0 -> clim endunless;
        coffs fi_+ wcol fi_- vedwcolumnoffset -> scol;
        while (vrow fi_+ vedlineoffset ->> line) fi_<= vbl do
            fast_subscrv(line, vedbuffer) -> string;
            vedusedsize(string) -> used;
            unless scol fi_> used then
                vedwindowpoint(vrow fi_+ vedwlineoffset, wcol);
                scol -> col;
                ncols -> n;
                repeat
                    vedscreenoutput(
                                if col == clim and col /== used then
                                    vedscreenmoremark
                                else
                                    subscrp(col, string)
                                endif);
                    quitif((n fi_- 1 ->> n) == 0
                            or (col fi_+ 1 ->> col) fi_> used)
                endrepeat;
            endunless;
            quitif((vrow fi_+ 1 ->> vrow) == vedwindowlength)
        endwhile
    enddefine;

    define varrefreshrange(right);
        lvars   nextcol, wline, wcol, line, vrow,
                vbl = datalength(vedbuffer), string;
        dlocal pop_asts_enabled = false;
        Flush_out(false);
        fast_for vrow from 1 to vedwindowlength fi_- 1 do
            vrow fi_+ vedlineoffset -> line;
            if line fi_> vbl then
                nullstring
            else
                fast_subscrv(line, vedbuffer)
            endif -> string;
            vrow fi_+ vedwlineoffset -> wline;

            if right then
                () -> nextcol;
                nextcol fi_- fexacc (4):int raw_XpwCallMethod(wvedwindow,
                                XpwMGetVarColumnOffset, wline fi_- 1, tmpint)
                       fi_+ vedwcolumnoffset -> wcol;
                (2:01, wline, wcol, nextcol, 0)
            else
                (2:11, wline, vedwcolumnoffset fi_+ 1, 0, false)
            endif;
            vedrefreshpartline((), string)
        endfor
    enddefine;

    until dis == 0 do
        if dis fi_> 0 then
            ;;; scroll left
            fi_min(scrollstep, dis) -> n;
            if vedvarwidthmode then var_stack_nextcols() endif;
            do_scrollhorz(-n);
            vedcolumnoffset fi_+ n -> vedcolumnoffset;
            dis fi_- n -> dis;
            if vedvarwidthmode then
                varrefreshrange(true)
            else
                refreshrange(vedscreenwidth fi_- n, n fi_+ 1, dis == 0)
            endif

        else
            ;;; scroll right
            fi_min(vedcolumnoffset, fi_min(scrollstep, -dis)) -> n;
            returnif(n == 0);
            do_scrollhorz(n);
            vedcolumnoffset fi_- n -> vedcolumnoffset;
            dis fi_+ n -> dis;
            if vedvarwidthmode then
                varrefreshrange(false)
            else
                refreshrange(vedwcolumnoffset fi_+ 1, n, false);
                if dis == 0 then refreshrange(vedscreenwidth, 1, true) endif
            endif
        endif
    enduntil
enddefine;


/* INSERTING AND DELETING LINES / CHARS */

define lconstant call_simple(method);
    lvars method;
    dlocal pop_asts_enabled = false;
    Flush_out(false);
    fexacc (2) raw_XpwCallMethod(wvedwindow, method);
enddefine;

define lconstant cleartail();
    call_simple(if iscaller(vedscreenblankpartline) then
                    XpwMWriteTrailSpacesAtCursor
                else
                    XpwMClearTailAtCursor
                endif)
enddefine;

define lconstant clear = call_simple(% XpwMClearScreen %) enddefine;

define lconstant insertline = call_simple(% XpwMInsertLineAtCursor %) enddefine;
define lconstant deleteline = call_simple(% XpwMDeleteLineAtCursor %) enddefine;
define lconstant deletechar = call_simple(% XpwMDeleteCharAtCursor %) enddefine;

define lconstant sendidseq();
    dlocal pop_asts_enabled = false;
    Flush_out(false);
    ;;; '\[xved' :: ved_char_in_stream -> ved_char_in_stream;
enddefine;


/* BELL */

define lconstant bell();
    dlocal pop_asts_enabled = false;
    Flush_out(false);
    if wved_is_live_window(wvedwindow) then
        ;;; safe to bell
        fexacc (3) raw_XpwCallMethod(wvedwindow, XpwMBell,
                                    xved_value("application","BellVolume"));
    else
        ;;; should work on most terminals
        rawcharout('\^G');
    endif;
enddefine;


/* CURSOR CONTROL */

define lconstant charleft = call_simple(% XpwMCursorLeft %) enddefine;
define lconstant charright = call_simple(% XpwMCursorRight %) enddefine;
define lconstant chardown = call_simple(% XpwMCursorDown %) enddefine;
define lconstant charup = call_simple(% XpwMCursorUp %) enddefine;

define lconstant screenleft();
    dlocal pop_asts_enabled = false;
    Flush_out(false);
    0 -> XptVal[fast] wvedwindow(XtN cursorColumn:int);
enddefine;


/* OUTPUT MODE CONTROL */

define lconstant set_insert_mode();
    dlocal pop_asts_enabled = false;
    Flush_out(false);
    () -> fast_subscrv(SDVEC_INSERT_MODE, xvedwin_screendatavec(wvedwindow))
enddefine;
;;;
define lconstant insertmode= set_insert_mode(% XpwMInsertAtCursor %) enddefine;
define lconstant overmode  = set_insert_mode(% XpwMWriteAtCursor %) enddefine;

define lconstant screencharmode with_props vedscreencharmode;
    if wvedwindow then
        fast_subscrv(SDVEC_CHARMODE, xvedwin_screendatavec(wvedwindow))
    else
        0
    endif
enddefine;
;;;
define updaterof screencharmode(mode) with_props vedscreencharmode;
    lvars mode, diff, old, tmp, sc, sdvec;
    lconstant macro COLORMASK = `\[7A]`;
    returnunless(wvedwindow);

    if xvedscreenhighlight then
        ;;; xvedscreenhighlight is a colour number
        xvedscreenhighlight fi_<< VEDCMODE_COLOUR_SHIFT -> sc;
        ;;; set selection colour -- xvedscreenhighlight if not already that,
        ;;; 0 otherwise.
        mode -> tmp;
        mode fi_&&~~ COLORMASK -> mode; ;;; set colour 0
        unless tmp fi_&& COLORMASK == sc then
            mode fi_|| sc -> mode
        endunless
    endif;

    xvedwin_screendatavec(wvedwindow) -> sdvec;
    fast_subscrv(SDVEC_CHARMODE, sdvec) -> old;
    mode -> fast_subscrv(SDVEC_CHARMODE, sdvec);
    (mode ||/& old) fi_&&~~ VEDCMODE_GRAPHIC -> diff;
    returnif(diff == 0);

    dlocal pop_asts_enabled = false;
    Flush_out(false);

    ;;; This assumes the VEDCMODE_ char attribute flags are the same
    ;;; as the XpwScrollText ones
    fexacc (3) raw_XpwCallMethod(wvedwindow, XpwMSetCharAttributes, mode)
enddefine;


/* OTHER CONTROLS */

define lconstant screenxy(col, row) with_props vedscreenxy;
    lvars col, row;
    dlocal pop_asts_enabled = false;
    Flush_out(false);
    unless col.isinteger and row.isinteger then
        mishap(col, row, 2, 'INVALID SCREEN POINT');
    endunless;
    fexacc (4) raw_XpwCallMethod(wvedwindow, XpwMCursorTo,
                                            col fi_- 1, row fi_- 1);
    col -> vedscreencolumn;
    row -> vedscreenline;
enddefine;


;;; vedscreenraw - remove annoying bell when we go into raw mode
define lconstant screenraw() with_props vedscreenraw;
    1000 ->> vedscreenline -> vedscreencolumn;
enddefine;

define lconstant screencooked() with_props vedscreencooked;
    vedscreencontrol(vvedscreenovermode);
    0 -> vedscreencharmode;
    vedscr_flush_output()
enddefine;

define lconstant screencursoron() with_props vedscreencursoron;
    lvars cursor_dchar = fexacc (2):uint raw_XpwCallMethod(wvedwindow,
                                                    XpwMGetTextCursor);
    cursor_dchar /== 0 and cursor_dchar
enddefine;
;;;
define updaterof screencursoron(cursor_dchar) with_props vedscreencursoron;
    lvars cursor_dchar;
    if cursor_dchar then
        if buf_pos /== 1 then
            procedure;
                dlocal pop_asts_enabled = false;
                Flush_out(false)
            endprocedure()
        endif;
        unless isinteger(cursor_dchar) and cursor_dchar fi_> 0 then
            `O` -> cursor_dchar
        endunless
    else
        0 -> cursor_dchar
    endif;
    fexacc (3) raw_XpwCallMethod(wvedwindow, XpwMSetTextCursor, cursor_dchar);
enddefine;

    /*  Called for graphics chars in the range 16:81 - 16:9A
        (currently only upto 16:92 are used)
        Also uses 16:9A as a 1-pixel-wide space in var width mode.

        This translates direct to the widget codes and bypasses the
        'graphic mode' mechanism
    */
define screengraphtrans(char) with_props vedscreengraphtrans;
    lvars char, c;
    ;;;                1  2  3  4  5  6  7  8  9  A  B  C  D  E  F  0  1  2  3456789A
    lconstant trans = '\^R\^R\^R\^Y\^N\^K\^W\^Y\^M\^L\^X\^Y\^U\^V\^O\^G\^A\^_-------\^H';
    fast_subscrs(char fi_- 16:80, trans) -> c;
    ;;; 2nd result true means graphics mode (not used)
    if c == `-` then char else c endif, false
enddefine;


define vedxvedscreen();
    ;;; general control variable assignments

    "xved"              -> vedterminalname;
    false               -> vednokeypad;
    false               -> vedscreenwrap;

    ;;; procedure values

    screenraw           -> vedscreenraw;
    screencooked        -> vedscreencooked;
    screenxy            -> vedscreenxy;
    screencursoron      -> nonactive vedscreencursoron;
    screencharmode      -> nonactive vedscreencharmode;
    setscrollregion     -> vedsetscrollregion;
    Scrollvert          -> vedscrollvert;
    Scrollhorz          -> vedscrollhorz;
    Scrollregion        -> vedscrollregion;
    ;;; translation for Ved standard graphic codes 16:81 - 16:9A
    screengraphtrans -> vedscreengraphtrans;

    ;;; vedset cannot cope with integers in variables,
    ;;; so do the setting ourselves

    sendidseq           -> vvedscreensendidseq;
    cleartail           -> vvedscreencleartail;
    scrollup            -> vvedscreenscrollup;
    scrolldown          -> vvedscreenscrolldown;
    insertline          -> vvedscreeninsertline;
    deleteline          -> vvedscreendeleteline;
    bell                -> vvedscreenbell;
    charleft            -> vvedscreencharleft;
    charright           -> vvedscreencharright;
    chardown            -> vvedscreenchardown;
    charup              -> vvedscreencharup;
    clear               -> vvedscreenclear;
    screenleft          -> vvedscreenscreenleft;
    insertmode          -> vvedscreeninsertmode;
    overmode            -> vvedscreenovermode;
    deletechar          -> vvedscreendeletechar;

    ;;; assign identfn to these so that tests for them = nullstring
    ;;; will return false (but they aren't expected to be used,
    ;;; because vedscreencharmode should be used instead).
    identfn ->> vvedscreengraphic ->> vvedscreencharnormal
            ->> vvedscreencharbold ->> vvedscreencharhighlight
            -> vvedscreencharunderline;

    ;;; set the following to nullstring
    vedset screen
        reset =
        setpad =
        resetpad =
        init =
    endvedset;

enddefine;
;;;
uses-by_name (vedxvedscreen);

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, May 24 1999
        Changed vedvscr_average_width to use new XtN fontAverageWidth
        resource in widget
--- John Gibson, Apr 24 1999
        Added vedscreenhairspacemark to varwidth_idents
--- John Gibson, Mar 26 1999
        More changes for variable-width mode
--- John Gibson, Aug 18 1997
        Changes for variable-width mode
--- John Gibson, Jun 19 1997
        Fixed vedscr_substring_out so it doesn't switch window to Unicode
        output unless the given string actually contains 16-bit chars.
        Also unset window size hints before setting Unicode mode and reset
        them again after.
--- John Gibson, Apr 29 1997
        Changes for switching to Unicode output in vedscr_substring_out
        and vedscr_char_out.
--- John Gibson, Dec 13 1996
        Fixed bugs in refreshrange in Scrollhorz.
--- John Gibson, Sep 12 1995
        Added test for wvedwindow being false in updater of screencharmode
        (likely to get called when wvedwindow is false)
--- John Gibson, May  3 1994
        sys_s*ignals_enabled -> pop_asts_enabled
--- John Gibson, Jan  9 1994
        Changed screencursoron to get/set XtN cursorChar.
        Also fixed bug in Flush_out (wasn't clearing output buffer after
        curr_window becomes defunct).
--- John Gibson, Dec 19 1993
        Made vedscr_flush_output do XFlush
--- John Gibson, Sep  7 1992
        Changed to use XptVal
--- John Gibson, Jun  5 1992
        Added XVed version of -vedscrollregion-
--- John Gibson, Feb 13 1992
        Added use of xvedselncolour in -screencharmode-
--- John Gibson, Jan 16 1992
        Made the updater of -screencharmode- use XpwMSetCharAttributes
--- John Gibson, Dec 21 1991
        Added -vedscreencharmode- code. Corrected handling of chars in
        graphics mode.
--- John Gibson, Nov 16 1991
        Combined with xvedrawout.p (which is now redundant) and completely
        rewritten. All vved- variables are now procedures and no
        'control chars' are necessary (no longer any pretence of there being
        a device).
        Made relevant procedures immune to interrupts to ensure output for
        different windows can't get mixed up.
--- John Gibson, Oct 28 1991
        Added Scrollvert for vedscrollvert and Scrollhorz for vedscrollhorz
--- John Gibson, Sep 20 1991
        Made procedures that get reset by set_term_defaults (vdinitseq.p)
        be set by vedxvedscreen instead of being :XVED_FOR
--- John Gibson, Jul  9 1991
        Replaced vvedscreencursoron/off with active variable
        vedscreencursoron.
        Made other procedures lconstants
 */
