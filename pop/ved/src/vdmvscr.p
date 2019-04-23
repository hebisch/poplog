/* --- Copyright University of Sussex 1998. All rights reserved. ----------
 > File:            C.all/ved/src/vdmvscr.p
 > Purpose:
 > Author:          Aaron Sloman & John Gibson (see revisions)
 */

;;; ------------- MOVING TO WINDOW POSITIONS ------------------------------

#_INCLUDE 'vddeclare.ph'

constant
        procedure vedscreenscreenleft
    ;


;;; -----------------------------------------------------------------------

section $-Sys$-Ved =>   vedvscr_window_column_bounds, vedwindowpoint,
                        ved_winrel_line, ved_winrel_column, ved_buffer_column,
                        vedcursorset, vedsetcursor;

    /*  Redefined by XVed in variable-width mode
    */
define vars vedvscr_window_column_bounds(wrline, need_lastcol)
                    /* -> (coloffs, vis_coloffs, lastcol, vis_lastcol) */ ;
    lvars wrline, need_lastcol;
    dup(dup(dup(vedcolumnoffset)) fi_+ vedwlinewidth);
    if dup() fi_< vvedlinesize then
        ;;; exclude 'more' mark
        () fi_- 1
    endif
enddefine;

    /*  These are co-ordinates within the window. So to get screen
        co-ordinates it is necessary to add vedscreenoffset to wline.
    */
define vedwindowpoint(wline, wcol);
    lvars wline, wcol, _lineoffset, _columnoffset;

    returnunless(vedediting);

    wline fi_+ vedscreenoffset -> wline;
    wline fi_- vedscreenline -> _lineoffset;
    wcol fi_- vedscreencolumn -> _columnoffset;
    if XWINDOWS then
        ;;; vedscreenxy is as quick as anything else
        unless _lineoffset == 0 and _columnoffset == 0 then
            vedscreenxy(wcol, wline)
        endunless;
        return
    elseif _lineoffset == 0 then
        if _columnoffset == 0 then
           return   ;;; already at location
        elseif wcol == 1 then
            vedscreenscreenleft()
        elseif _columnoffset == 1 then
            vedscreencharright()
        elseif _columnoffset fi_> -4 and _columnoffset fi_< 0 then
            until _columnoffset == 0 do
                vedscreencharleft();
                _columnoffset fi_+ 1 -> _columnoffset
            enduntil
        else
            vedscreenxy(wcol, wline)
        endif
    elseif _lineoffset == 1 then
        if wcol == 1 then
            vedscreenscreenleft(); vedscreenchardown()
        elseif _columnoffset == 0 then
            vedscreenchardown()
        elseif _columnoffset == 1 then
            vedscreenchardown(); vedscreencharright()
        elseif _columnoffset fi_> -3 and _columnoffset fi_< 0 then
            vedscreenchardown();
            until _columnoffset == 0 do
                vedscreencharleft();
                _columnoffset fi_+ 1 -> _columnoffset
            enduntil;
        else
            vedscreenxy(wcol, wline)
        endif
    else
        vedscreenxy(wcol, wline)
    endif
enddefine;

define Bottom_of_window();
    ;;; return buffer line number of bottom of current window
    ;;; n.b. -1 since window is always 1 less in size
    ;;; to account for statusline
    vedlineoffset fi_+ vedwindowlength fi_- 1
enddefine;

define ved_winrel_line(buffline);
    lvars buffline, diff = buffline fi_- vedlineoffset;
    ;;; n.b. < in the 2nd test not <= since window is always 1 less in size
    ;;; to account for statusline
    0 fi_< diff and diff < vedwindowlength and diff
enddefine;

define Window_line(buffline);
    lvars buffline;
    buffline fi_- vedlineoffset fi_+ vedwlineoffset
enddefine;

define ved_winrel_column(wrline, buffcol, _mode);
    lvars   wrline, buffcol, _mode,
            (coloffs, vis_coloffs, lastcol, vis_lastcol) =
                        vedvscr_window_column_bounds(wrline, _mode);
    if _mode then
        _int(_mode) -> _mode;
        if _mode _bitst _2:001 then
            ;;; treat partly-visible chars as visible
            coloffs -> vis_coloffs, lastcol -> vis_lastcol
        endif;
        unless vis_coloffs fi_< buffcol then
            returnunless(_mode _bitst _2:100) (false);
            vis_coloffs fi_+ 1 -> buffcol
        endunless;
        unless buffcol fi_<= vis_lastcol then
            returnunless(_mode _bitst _2:010) (false);
            vis_lastcol -> buffcol
        endunless
    endif;
    buffcol fi_- coloffs
enddefine;

define ved_buffer_column(wrline, wrcol, _mode);
    lvars   wrline, wrcol, _mode, buffcol,
            (coloffs, vis_coloffs, lastcol, vis_lastcol) =
                        vedvscr_window_column_bounds(wrline, _mode);

    wrcol fi_+ coloffs -> buffcol;
    returnunless(_mode) (buffcol);
    _int(_mode) -> _mode;
    if _mode _bitst _2:001 then
        ;;; treat partly-visible chars as visible
        coloffs -> vis_coloffs, lastcol -> vis_lastcol
    endif;
    unless vis_coloffs fi_< buffcol then
        returnunless(_mode _bitst _2:100) (false);
        vis_coloffs fi_+ 1 -> buffcol
    endunless;
    unless buffcol fi_<= vis_lastcol then
        returnunless(_mode _bitst _2:010) (false);
        vis_lastcol -> buffcol
    endunless;
    buffcol
enddefine;

    /*  line and column are BUFFER co-ordinates
        put cursor in right place if it is within screen window
        and return true if so, otherwise false
    */
define Set_cursor_to(line, column);
    lvars line, column, wrline, wrcol;
    if (ved_winrel_line(line) ->> wrline)
    and (ved_winrel_column(wrline, column, 2:001) ->> wrcol) then
        vedwindowpoint(wrline fi_+ vedwlineoffset,
                                wrcol fi_+ vedwcolumnoffset);
        true
    else
        false
    endif
enddefine;

define vedcursorset();
    ;;; returns true or false
    Set_cursor_to(vedline, vedcolumn)
enddefine;

define vedsetcursor();
    vedcursorset() ->
enddefine;

endsection;     /* $-Sys$-Ved */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jul  9 1998
        Fixed bug in ved_buffer_column (was sometimes returning 2 results
        instead of 1).
--- John Gibson, Aug 15 1997
        Changes for XVed variable-width mode
--- John Gibson, Apr 21 1992
        Sectionised.
--- John Gibson, Dec  6 1987
        Tidied up.
 */
