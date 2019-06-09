/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.x/x/ved/src/xvedgeneral.p
 > Purpose:         Multi-window Ved Code not specific to X
 > Author:          Jonathan Meyer, Apr  6 1991 (see revisions)
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

include xpt_xscreen.ph;

section $-xved;

define xved_scroll_to_lineoffset(lineoffset);
    lvars dist, lineoffset, current;
    dlocal vedscreencursoron = false;

    define lconstant should_jump(amount);
        lvars amount, limit;
        if vedmaxscrollvert.isprocedure then
            fast_apply(vedmaxscrollvert)
        elseif vedmaxscrollvert.isinteger then
            vedmaxscrollvert
        else
            ;;; default - half a screen, or 20 lines max, times xvedvscrollstep
            fi_min(fi_max(1, vedwindowlength fi_>> 1), 20) * xvedvscrollstep
        endif -> limit;
        amount fi_> limit;
    enddefine;

    max(0, min(lineoffset, vvedbuffersize fi_- 1)) -> lineoffset;

    vedline - vedlineoffset -> current;
    vedtrimline();
    lineoffset - vedlineoffset -> dist;
    if ved_on_status or should_jump(abs(dist)) then
        ;;; jump scroll
        lineoffset -> vedlineoffset;
        lineoffset + current -> vedline;
        vedalignscreen();
    else
        ;;; smooth scroll
        vedscrollvert(dist)
    endif;

    lineoffset + current -> vedline;
    vedsetcursor();
    vedscr_flush_output();
enddefine;

    /*  Max width of lines in buffer or showing in the window
    */

define xved_max_linewidth(in_window) -> maxlw;
    lvars   line = 1, maxlw = 0, string, endline = vvedbuffersize,
            varwidthmode = vedvarwidthmode;

    if in_window then
        vedlineoffset fi_+ 1 -> line;
        fi_min(line fi_+ vedwindowlength fi_- 2, endline) -> endline
    endif;

    while line fi_<= endline do
        fast_subscrv(line,vedbuffer) -> string;
        if varwidthmode or datalength(string) fi_> maxlw then
            fi_max(maxlw, vedvscr_substring_width(string, 1, false)) -> maxlw
        endif;
        line fi_+ 1 -> line
    endwhile
enddefine;

define xved_scroll_to_columnoffset(coloffset);
    lvars dist, coloffset, wrline, wrcol;
    dlocal vedscreencursoron = false;

    define lconstant should_jump(amount);
        lvars amount, limit, width;
        if vedmaxscrollhor.isprocedure then
            vedvscr_average_width(fast_apply(vedmaxscrollhor))
        elseif vedmaxscrollhor.isinteger then
            vedvscr_average_width(vedmaxscrollhor)
        else
            ;;; default -- half a screen, or 20 cols max, times xvedhscrollstep
            vedwlinewidth fi_>> 1 -> width;
            fi_min( fi_max(1,width), vedvscr_average_width(20) )
                            * xvedhscrollstep
        endif -> limit;
        amount fi_> limit
    enddefine;

    fi_max(0, fi_min(coloffset, xved_max_linewidth(true) fi_- 1)) -> coloffset;

    returnunless(ved_winrel_line(vedline) ->> wrline);
    ved_winrel_column(wrline, vedcolumn, false) -> wrcol;

    coloffset - vedcolumnoffset -> dist;
    if ved_on_status or should_jump(abs(dist)) then
        ;;; jump scroll
        coloffset -> vedcolumnoffset;
        vedrefreshwindow(false)
    else
        ;;; smooth scroll
        vedscrollhorz(dist)
    endif;

    ved_buffer_column(wrline, wrcol, 2:110) -> vedcolumn;
    vedsetcursor();
    vedscr_flush_output();
enddefine;


    /*  Used to get the scrollbar/menubar widget of a window. Returns
        a widget or false.

        mode arg values:
            false:    Return subpart if exists;
            true: Return subpart if exists and is managed;
            'init': Return subpart if exists, or create it if the
                    field contains a create procedure;
            string:    As 'init', but string is a boolean resource name for
                    the window -- create only if that resource is true.
    */

define xved_get_subpart(xvwin, fieldp, mode);
    lvars xvwin, fieldp, mode, field = xvwin.fieldp;
    if isstring(mode) then
        if isprocedure(field) and not(isundef(field))
        and (mode = 'init' or XptVal xvwin(%mode%:XptBoolean)) then
            ;;; run the init procedure to try and create the field
            field(xvwin);
            xvwin.fieldp -> field;
            if isvector(field) then
                ;;; reconfigure window
                (xvwin.xvedwin_configure)(xvwin,
                        xved_get_subpart(xvwin, xvedwin_menubar, false),
                        xved_get_subpart(xvwin, xvedwin_scrollbar, false),
                        xved_get_subpart(xvwin, xvedwin_hscrollbar, false),
                    )
            endif
        endif;
        false -> mode
    endif;

    isvector(field)
    and (subscrv(1,field)->field, not(mode) or fast_XtIsManaged(field))
    and field
enddefine;


define xved_make_window_position() /* -> (x, y) */;
    lvars   width, height, x, y, last_win = false, last_x, last_y, last_width,
            last_height, sp, w, h, file, screen_width, screen_height, screen;


    ;;; find most recent file with an open window
    for file in tl(vedbufferlist) do
        quitif((wved_window_of_file(file) ->> last_win)
                and wved_is_open_window(last_win));
        false -> last_win
    endfor;
    returnunless(last_win) (15, 20);

    ;;; do what we can to work out the full width and height of the new window
    ;;; (can't use XptWMShellCoords etc 'cos it's not realised)
    XptWidgetCoords(wvedwindow) -> (, , width, height);
    XptVal[fast] wvedwindow(XtN borderWidth:XptDimension) << 1 -> w;
    width+w -> width, height+w -> height;

    if xved_get_subpart(wvedwindow, xvedwin_scrollbar, true) ->> sp then
        XptWidgetCoords(sp) -> (, , w, );
        width+w -> width
    endif;
    if xved_get_subpart(wvedwindow, xvedwin_menubar, true) ->> sp then
        ;;; width and height for this seem to be OK in Motif but not OLIT
        ;;; -- anyway, use the last one (which is realized)
        if xved_get_subpart(last_win, xvedwin_menubar, false) ->> w then
            w -> sp
        endif;
        XptWidgetCoords(sp) -> (, , w, h);
        max(w, width) -> width;
        height+h -> height;
    endif;
    ;;; add something for WM decoration
    XptWMShellCoords(last_win) -> (last_x, last_y, last_width, last_height);
    XptWidgetCoords(last_win.xvedwin_shell) -> (, , w, h);
    last_width-w -> w;
    width+w -> width;
    last_height-h -> h;
    height+h -> height;

    l_typespec screen :XScreen;
    fast_XtScreen(xveddummyshell) -> screen;
    exacc screen.width  -> screen_width;
    exacc screen.height -> screen_height;

    if last_x + (last_width>>1) < (screen_width>>1) then
        ;;; last to left
        screen_width - width - max(0,last_x-15), last_y
    else
        ;;; last to right
        w>>1 -> w;      ;;; WM border width
        h-w -> h;       ;;; WM title height
        screen_width - last_width - last_x + w + 19, last_y + h + 4
    endif -> (x, y);

    min(y+height, screen_height) - height -> y;
    max(0,x),  max(0,y)
enddefine;


endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 18 1997
        Changes for variable-width mode
--- John Gibson, Apr 16 1996
        Changed xved_max_linewidth to take arg saying whether max width in
        window or whole buffer required.
--- John Gibson, Feb 11 1994
        Added xved_scroll_to_coloffset, and changes to xved_get_subpart
--- John Gibson, Jan 13 1993
        popcom*piler -> subsystem etc
--- John Gibson, Sep  7 1992
        Changed to use XptVal
--- John Gibson, Jun  5 1992
        Made -should_jump- take -xvedvscrollstep- into acccount
--- John Gibson, Oct 26 1991
        Changed xved_scroll_to_lineoffset to use vedscrollvert
--- John Gibson, Oct 22 1991
        Correction to xved_make_window_position
--- John Gibson, Oct 16 1991
        Changed xved_scroll_to_lineoffset to scroll upto half a screen or
        20 lines max
--- John Gibson, Oct  4 1991
        Changes to xved_make_window_position
--- John Gibson, Sep 29 1991
        xved_get_subpart takes 3rd arg to say managed only
--- John Gibson, Sep 27 1991
        Combined xved_make_window_x and xved_make_window_y into
        xved_make_window_position. Now also uses XptWMShellCoords
--- Jonathan Meyer, Sep 25 1991 Added xved_get_subpart
--- Jonathan Meyer, Sep  5 1991
        Added checks for scrollbarMoved in xved_scroll_to_lineoffset
--- John Gibson, Aug 17 1991
        Added xved_scroll_to_lineoffset
--- Jonathan Meyer, Aug  2 1991 ved_mark_range now in auto
--- Adrian Howard, Aug  2 1991 : Stopped -ved_mark_range- doing early autoloads
--- Adrian Howard, Aug  1 1991 : Fixed marked range paragraph selection
--- Jonathan Meyer, Jul  7 1991
        Renamed xved_mark_range to ved_mark_range
--- Jonathan Meyer, Jul  5 1991
        Dynamix X defaulting now can base position on size of window
--- Jonathan Meyer, Jun 18 1991
        Made x and y coords follow placement of last ved window
--- Jonathan Meyer, Jun  6 1991
        Removed "Copyright..." message
--- Robert John Duncan, Jun  4 1991
        Made x and y coords both reset when window goes off edge of screen
--- Jonathan Meyer, Jun  3 1991
        Added xved_make_window_x/y
--- Jonathan Meyer, Jun  3 1991
        Changed to constants (no longer redefinable). Moved into
        XVED_section.
 */
