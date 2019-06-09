/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.x/x/ved/lib/vedxvedmouse.p
 > Purpose:         Basic XVed mouse library
 > Author:          Jonathan Meyer, Apr  6 1991 (see revisions)
 > Documentation:   HELP *XVED
 > Related Files:
 */
compile_mode :pop11 +strict;

include xved_constants.ph;
include ved_do_text_action.ph;

section $-xved =>
                vedmouse__point, vedmouse__drag, vedmouse__end_drag,
                vedmouse__select, vedmouse__adjust, vedmouse__end_select,
                vedmouse__paste, vedmouse__mark_range,
                vedmouse__adjust_range, vedmouse__end_range,
                vedmouse__set_left_margin, vedmouse__set_right_margin,
                vedmouse__scroll, vedmouse__hscroll, vedmouse__end_scroll,
                vedmouse__menu, vedmouse__do_text_action,
                vedxvedmouse
            ;

/************************************************************************
 * Button event handler library
 ************************************************************************/

lvars
        row_drag_timer_id = false, row_drag_timer_speed = false,
        col_drag_timer_id = false, col_drag_timer_speed = false,
        mark_start_line = false,
        saved_line = false, saved_col, saved_status,
        in_mark = false,
        in_select = false,
        in_adjust = false,
        adjust_end = false,
        start_line, start_col, end_line, end_col,
        in_scroll = false,
        ;


define lconstant point_to_screen(set_status);
    lvars   data = vvedmousedata, (row, col) = (data(XVM_ROW), data(XVM_COL)),
            set_status, buffcol;

    if set_status then row == 1 -> ved_on_status endif;
    row fi_- vedwlineoffset -> row;
    fi_max(1, col fi_- vedwcolumnoffset) -> col;
    if 1 fi_<= row and row fi_<= vedwindowlength
    and (ved_buffer_column(row, col, 2:001) ->> buffcol)
    then
        vedline, vedcolumn -> (saved_line, saved_col);
        vedjumpto(row fi_+ vedlineoffset, buffcol);
        true
    else
        false
    endif
enddefine;

define vedmouse__point();
    if (vvedmousedata(XVM_START_ROW) == 1) /== ved_on_status then
        vedstatusswitch()
    endif;
    point_to_screen(false) -> ;
    true -> xvedeventhandled;
enddefine;

    ;;; col can be 0 in this
define lconstant set_coords(line, col);
    lvars line, col, ecol;
    vedjumpto(line, fi_max(1,col));
    if in_select then
        line, col -> if in_adjust and not(adjust_end) then
                        start_line, start_col
                     else
                        end_line, end_col
                     endif;
        start_col -> col; end_col -> ecol;
        if start_line fi_< end_line
        or (start_line == end_line and col fi_<= ecol)
        then
            unless ecol == 0 then ecol fi_+ 1 -> ecol endunless
        else
            col fi_+ 1 -> col
        endif;
        vedselection_adjust(start_line, col, end_line, ecol, false);

    elseif in_mark then
        vedtrimline();
        if mark_start_line > line then
            mark_start_line -> vedline; vedmarkhi();
            line -> vedline; vedmarklo();
        else
            mark_start_line -> vedline; vedmarklo();
            line -> vedline; vedmarkhi();
        endif;
    endif;
enddefine;


define lconstant row_drag_timeout_scroll(isdown, id);
    lvars isdown, id;
    ;;; adds things to Ved's input stream
    if row_drag_timer_speed then
        if (not(isdown)
            or vedline fi_< fi_max(vvedbuffersize fi_+ vedwindowlength fi_- 2,
                               vedlineoffset fi_+ vedwindowlength fi_- 1))
        and (isdown or vedline fi_> 1)
        then
            xved_raise_event(wvedwindow, "rowDragScroll", isdown);
        endif;
    else
        false -> row_drag_timer_id;
    endif;
enddefine;


define lconstant col_drag_timeout_scroll(isright, id);
    lvars isright, id;
    ;;; adds things to Ved's input stream
    if col_drag_timer_speed then
        if vedcolumn fi_> 1 then
            xved_raise_event(wvedwindow, "colDragScroll",isright);
        endif;
    else
        false -> col_drag_timer_id;
    endif;
enddefine;


define lconstant rowDragScroll_event(window, name, isdown)
                                        -> (window, name, isdown);
    lvars window, name, isdown, line;
    returnunless(window == wvedwindow);

    if row_drag_timer_speed then
        XtAppAddTimeOut(xvedappcontext, row_drag_timer_speed,
            row_drag_timeout_scroll, isdown) -> row_drag_timer_id;
    endif;

    if isdown then
        vedlineoffset fi_+ vedwindowlength fi_+ xvedvscrollstep fi_- 1
    else
        fi_max(1, vedlineoffset fi_- xvedvscrollstep fi_+ 1)
    endif -> line;
    set_coords(line, vedcolumn);
enddefine;
;;;
rowDragScroll_event -> xvedeventtable("rowDragScroll");


define lconstant colDragScroll_event(window, name, isright)
                                    -> (window, name, isright);
    lvars window, name, isright, col;
    returnunless(window == wvedwindow);

    if col_drag_timer_speed then
        XtAppAddTimeOut(xvedappcontext, col_drag_timer_speed,
            col_drag_timeout_scroll, isright) -> col_drag_timer_id;
    endif;

    ved_buffer_column(ved_winrel_line(vedline),
                            if isright then 1e6 else 1 endif, 2:110) -> col;
    if isright then
        col fi_+ xvedhscrollstep
    else
        col fi_- xvedhscrollstep
    endif -> col;
    set_coords(vedline, col);
enddefine;
;;;
colDragScroll_event -> xvedeventtable("colDragScroll");


define lconstant remove_row_drag_scroll();
    false -> row_drag_timer_speed;
    if row_drag_timer_id then
        XtRemoveTimeOut(row_drag_timer_id);
        false -> row_drag_timer_id;
    endif;
enddefine;

define lconstant remove_col_drag_scroll();
    false -> col_drag_timer_speed;
    if col_drag_timer_id then
        XtRemoveTimeOut(col_drag_timer_id);
        false -> col_drag_timer_id;
    endif;
enddefine;


define lconstant do_select();
    lvars line, col, srow, scol, data = vvedmousedata;

    data(XVM_ROW), data(XVM_COL) -> (srow, scol);

    ;;; Test for vertical scrolls
    srow fi_- vedwlineoffset -> srow;
    if 1 fi_<= srow and srow fi_< vedwindowlength then
        remove_row_drag_scroll();
        srow fi_+ vedlineoffset
    else
        ;;; Start of scroll
        unless row_drag_timer_id then
            if ved_on_status then 600 else 80 endif -> row_drag_timer_speed;
            XtAppAddTimeOut(xvedappcontext, row_drag_timer_speed,
                row_drag_timeout_scroll, srow fi_>= 1) -> row_drag_timer_id;
        endunless;
        false -> srow;
        vedline
    endif -> line;

    ;;; Test for horizontal scrolls
    scol fi_- vedwcolumnoffset -> scol;
    unless srow then
        vedcolumn
    elseif ved_buffer_column(srow, scol, 2:000) ->> col then
        remove_col_drag_scroll();
        col
    elseif scol fi_<= 1 and vedcolumnoffset == 0 then
        0
    else
        ;;; Start of scroll
        unless col_drag_timer_id then
            80 -> col_drag_timer_speed;
            XtAppAddTimeOut(xvedappcontext, col_drag_timer_speed,
                col_drag_timeout_scroll, scol fi_> 1) -> col_drag_timer_id;
        endunless;
        vedcolumn
    endunless -> col;

    set_coords(line, col);
enddefine;

define vedmouse__drag();
    lvars data = vvedmousedata;
    true -> xvedignorepixelmotion;
    if xvedselectionon == wvedwindow then
        false ->> in_select -> in_adjust;
    endif;
    if (data(XVM_START_ROW) == 1) /== ved_on_status then
        vedstatusswitch()
    endif;
    unless vedscr_input_waiting() then do_select() endunless;
    true -> xvedeventhandled;
enddefine;


define vedmouse__end_drag();
    remove_row_drag_scroll();
    remove_col_drag_scroll();
    point_to_screen(false) -> ;
    true -> xvedeventhandled;
enddefine;


define lconstant save_position();
    true -> xvedignorepixelmotion;
    ved_on_status -> saved_status;
    vvedmousedata(XVM_START_ROW) == 1 -> ved_on_status;
    vedline, vedcolumn -> (saved_line, saved_col)
enddefine;

define lconstant restore_position();
    if saved_line then
        vedjumpto(saved_line, saved_col);
        saved_status -> ved_on_status;
        false -> saved_line;
    endif;
enddefine;

define lconstant input_waiting();
    if vedscr_input_waiting()
    and not(row_drag_timer_speed or col_drag_timer_speed) then
        ;;; compress drag events
        true ->> xvedeventhandled;
    else
        false
    endif
enddefine;

define vedmouse__select();
    lvars data = vvedmousedata, wrline;

    if in_select then
        returnif(input_waiting())
    else
        save_position();
        data(XVM_START_ROW) fi_- vedwlineoffset -> wrline;
        wrline fi_+ vedlineoffset ->> start_line -> end_line;
        if start_line fi_> vvedbuffersize fi_+ 1 then
            vedputmessage('nothing to select');
            restore_position();
            true -> xvedeventhandled;
            return;
        endif;
        ved_buffer_column(wrline,
                        fi_max(0, data(XVM_START_COL) fi_- vedwcolumnoffset),
                        false) ->> start_col -> end_col;
        true -> in_select;
        false -> vedscreencursoron;
    endif;

    do_select();
    true -> xvedeventhandled;
enddefine;


define vedmouse__adjust();
    lvars data = vvedmousedata;

    if in_adjust then
        returnif(input_waiting())
    else
        unless xvedselectionon == wvedwindow
        and xvedselectiononstatus == ved_on_status
        then
            chain(vedmouse__select);
        endunless;

        save_position();
        explode(xvedselectioncoords)
            -> (start_line, start_col, end_line, end_col);
        vedlineoffset fi_+ data(XVM_START_ROW)
            fi_> (start_line fi_+ end_line) fi_div 2 -> adjust_end;
        true ->> in_select -> in_adjust;
        false -> vedscreencursoron;
    endif;

    do_select();
    true -> xvedeventhandled;
enddefine;

define vedmouse__end_select();
    returnunless(in_select);
    do_select();
    false ->> in_select -> in_adjust;
    true -> vedscreencursoron;
    remove_row_drag_scroll();
    remove_col_drag_scroll();
    if xvedselectionon then vedselection_set_primary(true) endif;
    restore_position();
    true -> xvedeventhandled;
enddefine;


define vedmouse__paste();
    vedselection_paste();
    true -> xvedeventhandled;
enddefine;


define vedmouse__mark_range();
    lvars data = vvedmousedata;

    if in_mark then
        returnif(input_waiting())
    else
        save_position();
        data(XVM_START_ROW) fi_+ vedlineoffset - vedwlineoffset
                                    -> mark_start_line;
        if mark_start_line fi_> vvedbuffersize then
            vedputmessage('nothing to mark');
            restore_position();
            true -> xvedeventhandled;
            return;
        endif;

        fi_max(1, mark_start_line) -> mark_start_line;
        true -> in_mark;
    endif;

    do_select();
    true -> xvedeventhandled;
enddefine;


define vedmouse__adjust_range();
    lvars data = vvedmousedata;

    if in_mark then
        returnif(input_waiting())
    else
        save_position();
        if vvedmarkhi == 0 or vvedmarklo > vvedbuffersize then
            ;;; no existing range
            restore_position();
            chain(vedmouse__mark_range);
        endif;

        (((data(XVM_ROW) fi_+ vedlineoffset fi_- vedwlineoffset)
            fi_> (vvedmarklo fi_+ vvedmarkhi) fi_div 2 ) and vvedmarklo)
        or vvedmarkhi
            -> mark_start_line;
        fi_max(1, min(vvedbuffersize, mark_start_line)) -> mark_start_line;
        true -> in_mark;
    endif;

    do_select();
    true -> xvedeventhandled;
enddefine;


define vedmouse__end_range();
    lvars name, data;
    returnunless(in_mark);
    do_select();
    remove_row_drag_scroll();
    remove_col_drag_scroll();
    false ->> mark_start_line -> in_mark;
    restore_position();
    true -> xvedeventhandled;
enddefine;


define vedmouse__set_left_margin();
    dlocal ved_on_status = false;
    lvars col = vedcolumnoffset fi_+ vvedmousedata(XVM_COL);

    if col fi_< 2 then
        2 -> col;
        vedscreenbell();
    elseif col fi_> vedlinemax fi_+ 1 then
        vedlinemax fi_+ 1 -> col;
        vedscreenbell();
    endif;

    col fi_- 2 -> vedleftmargin;
    vedputmessage('LEFTMOST COLUMN IS ' sys_>< (col fi_- 1));

    true -> xvedeventhandled;
enddefine;


define vedmouse__set_right_margin();
    dlocal ved_on_status = false;
    lvars col = vedcolumnoffset fi_+ vvedmousedata(XVM_COL);

    if col fi_< (vedleftmargin fi_+ 2)  then
        vedleftmargin fi_+ 2 -> col;
        vedscreenbell();
    endif;

    col fi_- 1 -> vedlinemax;
    vedputmessage('RIGHTMOST COLUMN IS 'sys_>< (col fi_- 1));

    true -> xvedeventhandled;
enddefine;

define lconstant do_scroll(drag_mode);
    lvars data = vvedmousedata, drag_mode, dist;
    if in_scroll then
        if drag_mode and vedscr_input_waiting() then
            ;;; compress events
            true -> xvedeventhandled;
            return
        endif
    else
        returnunless(drag_mode);
        if (data(XVM_START_ROW) == 1) /== ved_on_status then
            vedstatusswitch()
        endif;
        if xvedselectionon == wvedwindow then
            false ->> in_select -> in_adjust;
        endif;
        if (drag_mode->>in_scroll) == "rowDragScroll" then
            vedlineoffset -> start_line
        else
            vedcolumnoffset -> start_col
        endif
    endif;

    if in_scroll == "rowDragScroll" then
        (data(XVM_Y) fi_- data(XVM_START_Y))
                    fi_div (if ved_on_status then 4 else 2 endif) -> dist;
        xved_scroll_to_lineoffset(start_line fi_+ dist)
    else
        ((data(XVM_X) fi_- data(XVM_START_X)) fi_* vedvscr_average_width(1))
            fi_div (if ved_on_status then 4 else 2 endif) -> dist;
        xved_scroll_to_columnoffset(start_col fi_+ dist)
    endif;
    drag_mode -> in_scroll;
    true -> xvedeventhandled;
enddefine;

define vedmouse__scroll     = do_scroll(%"rowDragScroll"%) enddefine;
define vedmouse__hscroll    = do_scroll(%"colDragScroll"%) enddefine;
define vedmouse__end_scroll = do_scroll(%false%) enddefine;


;;; --- SELECTION CLASSES ----------------------------------------------

uses-now vedselection_new_class;

;;; Closures of this are created by vedselection_new_class
define make_class_selection(classname);
    lvars classname;
    dlocal ved_on_status, vedscreenbell = identfn;
    if point_to_screen(true) then
        vedselection_select(classname);
        vedselection_set_primary(true);
        vedjumpto(saved_line, saved_col);
    endif;
    true -> xvedeventhandled;
enddefine;


;;; Executes the procedure referred to by the identifier -word-
;;; Used to stop early autoloads
define lconstant applyvalof = valof <> apply; enddefine;

define lconstant endchar();
    unless vedcolumn fi_> vvedlinesize then
        vedcharright()
    endunless
enddefine;

define lconstant startword();
    unless vedcolumn == 1 or vedcolumn fi_> vvedlinesize
    or vedatitemstart(vedcolumn,  vedthisline(), vvedlinesize) then
        ;;; not at a word boundary
        vedstartwordleft();     ;;; don't fiddle around autoloading this!
    endunless;
enddefine;

define lconstant endword();
    returnif(vedcolumn fi_> vvedlinesize);
    if vedatitemend(vedcolumn, vedthisline(), vvedlinesize+1) then
        unless vedchartype(vedcurrentchar()) == vedchartype(`\s`) then
            vedcharright()
        endunless
    else
        ;;; not at a word boundary
        vedendwordright();      ;;; don't fiddle around autoloading this!
    endif;
enddefine;

define lconstant startwindow();
    vedjumpto(vedlineoffset+1, 1);
enddefine;

define lconstant endwindow();
    vedscreendown(); vedscreenright();
    vedcolumn+1 -> vedcolumn;
enddefine;

define lconstant endrange();
    applyvalof("vedendrange");
    vedscreenright();
    vedcolumn+1 -> vedcolumn;
enddefine;

vedselection_new_class("character",   identfn, endchar);
vedselection_new_class("word",        startword, endword);
vedselection_new_class(
    "line",
    procedure; dlocal vedleftmargin=0; vedscreenleft(); endprocedure,
    procedure; dlocal vedleftmargin=0; vedscreenleft(); vedchardown(); endprocedure
);
vedselection_new_class("sentence", applyvalof(%"vedcharnext"%) <> applyvalof(%"vedprevsent"%),
                                   applyvalof(%"vednextsentend"%) <> vedcharright);
vedselection_new_class("procedure",applyvalof(%"ved_mcp"%) <> applyvalof(%"vedmarkfind"%),
                                   applyvalof(%"vedendrange"%) <> vedscreenright);
vedselection_new_class("range",    applyvalof(%"vedmarkfind"%), endrange);
vedselection_new_class("paragraph",applyvalof(%"vedcharnext"%) <> applyvalof(%"vedprevpara"%),
                                   applyvalof(%"vednextparaend"%) <> vedcharright);
vedselection_new_class("window",   startwindow, endwindow);
vedselection_new_class("file",        vedtopfile, vedendfile);
vedselection_new_class("toendfile",   identfn, vedendfile);
vedselection_new_class("tostartfile", vedtopfile, identfn);


;;; --- MARKED RANGE CLASSES -------------------------------------------

uses-now vedmarkclasses;

define lconstant mouse_mark_range(type);
    lvars type;
    dlocal ved_on_status, vedscreenbell = identfn;
    if point_to_screen(true) then
        ved_mark_named_range(type);
        vedjumpto(saved_line, saved_col);
    endif;
    true -> xvedeventhandled;
enddefine;
;;;
procedure;
    lvars name, idname;
    dlocal current_section = pop_section;
    for name in vedmarkclasses do
        nextunless(isword(name));
        "vedmouse__mark_" <> name -> idname;
        pop11_define_declare(idname, sysGLOBAL, sysCONSTANT, "procedure");
        sysPASSIGN(mouse_mark_range(%name%), idname);
    endfor
endprocedure();


;;; --- POP-UP MENU ----------------------------------------------------

define vedmouse__menu();
    unless testdef xvedmenubar then
        vederror('XVed: Menus not loaded')
    elseunless islist(xved_value("application", "Menu")) then
        vederror('XVed: "Menu" resource not set to anything')
    else
        if xvedhasinputfocus /== wvedwindow then
            wved_set_input_focus(wvedwindow)
        endif;
        true -> xvedeventhandled;
        xved_button_release();
        ;;; pop it up
        weakref[xvedmenubar] xved_popup_menu(vvedmousedata(XVM_BUTTON))
    endunless
enddefine;


;;; --- OBEYING TEXT ACTIONS -----------------------------------------------

    /*  pdprops being a pair in this procedure allows it to be run when
        xvedblockinput is true
    */
define vedmouse__do_text_action() with_props #_<[vedmouse__do_text_action]>_#;
    lvars data = vvedmousedata, res;
    ved_do_text_action(
        data(XVM_ROW), data(XVM_COL),
        if xvedblockinput then VDTA_TYPE_CHARS
        else #_< VDTA_TYPE_ANY &&~~ (VDTA_TYPE_DOC_REF||VDTA_TYPE_ANY_HELP)>_#
        endif,
        VDTA_MODE_INPUT) -> xvedeventhandled;
    if not(xvedeventhandled) and xvedblockinput then
        true -> xvedeventhandled;
        vedscreenbell();
        vedscr_flush_output()
    endif
enddefine;


;;; --------------------------------------------------------------------

define vars vedxvedmouse;
    if xved_value("application", "UsePwmMouseBindings") then
        ;;; marking ranges goes on the non-modified buttons
        vedset mouse (override)
            point               = click btn1 or btn2 with anyModifier
            drag                = drag btn3 with anyModifier
            end_drag            = dragRelease btn3 with anyModifier

            select              = drag          btn1 with control
            adjust              = drag          btn2 with control
            end_select          = dragRelease   btn1 or btn2 with anyModifier

            select_word         = click btn1 2 times with control
            select_line         = click btn1 3 times with control
            select_procedure    = click btn1 4 times with control

            select_character    = hold btn1 with control

            ;;; marked ranges
            mark_range          = drag          btn1
            adjust_range        = drag          btn2
            end_range           = dragRelease   btn1 or btn2 with anyModifier

            mark_line           = click btn1 2 times
            mark_procedure      = click btn1 3 times

            mark_lo             = click btn1 with shift
            mark_hi             = click btn2 with shift

            ;;; setting margins
            set_left_margin     = hold btn1 with shift
            set_left_margin     = drag btn1 with shift
            set_right_margin    = hold btn3 with shift
            set_right_margin    = drag btn3 with shift

        endvedset;
    elseif xved_value("application", "UseOldMouseBindings") then
        vedset mouse (override)
            point               = click btn1 or btn2 with anyModifier
            do_text_action      = click btn1    ;;; drops thru to point if no action

            scroll              = 1-click drag          btn1
            end_scroll          = 1-click dragRelease   btn1

            drag                = drag btn3 with control
            end_drag            = dragRelease btn3 with control

            select              = drag          btn1
            adjust              = drag          btn2
            adjust <> end_select= click         btn2
            end_select          = dragRelease   btn1 or btn2 with anyModifier

            select_word         = click btn1 2 times
            select_line         = click btn1 3 times
            select_procedure    = click btn1 4 times

            select_character    = hold btn1

            ;;; marked ranges
            mark_range          = drag          btn1 with control
            adjust_range        = drag          btn2 with control
            end_range           = dragRelease   btn1 or btn2 with control

            mark_line           = click btn1 2 times with control
            mark_procedure      = click btn1 3 times with control
            mark_file           = click btn1 4 times with control

            mark_lo             = click btn1 with shift
            mark_hi             = click btn2 with shift

            ;;; margin setting
            set_left_margin     = hold btn1 with shift
            set_left_margin     = drag btn1 with shift
            set_right_margin    = hold btn3 with shift
            set_right_margin    = drag btn3 with shift

        endvedset;
    else
        vedset mouse (override)
            ;;; BUTTON 1
            point               = click btn1
            do_text_action      = click btn1    ;;; drops thru to point if no action

            select_word         = 2-click btn1
            select_line         = 3-click btn1
            select_procedure    = 4-click btn1
            select_character    = hold btn1

            select              = drag          btn1
            end_select          = dragRelease   btn1
            adjust              = drag          btn1 with shift
            adjust <> end_select= click         btn1 with shift
            end_select          = dragRelease   btn1 with shift

            scroll              = 1-click drag          btn1
            end_scroll          = 1-click dragRelease   btn1
            hscroll             = 2-click drag          btn1
            end_scroll          = 2-click dragRelease   btn1

            mark_lo             = click         btn1 with control
            mark_line           = 2-click btn1 with control
            mark_procedure      = 3-click btn1 with control
            mark_file           = 4-click btn1 with control
            mark_range          = drag          btn1 with control
            end_range           = dragRelease   btn1 or btn2 with control

            set_left_margin     = hold btn1 with meta
            set_left_margin     = drag btn1 with meta


            ;;; BUTTON 2
            paste               = click btn2
            mark_hi             = click btn2 with control
            adjust_range        = drag  btn2 with control


            ;;; BUTTON 3
            menu                = hold btn3

            drag                = 1-click drag          btn3
            end_drag            = 1-click dragRelease   btn3

            set_right_margin    = hold btn3 with meta
            set_right_margin    = drag btn3 with meta

        endvedset;
        return
    endif;

    if not(xvedvanilla) and testdef xvedmenubar
    and islist(xved_value("application","Menu")) then
        /* support for menubar is loaded */
        vedset mouse (at front)
            menu        = press btn3
        endvedset
    else
        vedset mouse
            paste       = click btn3
        endvedset
    endif;
enddefine;
;;;
uses-by_name (vedxvedmouse);

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 18 1997
        Changes for variable-width mode
--- John Gibson, Mar  1 1996
        Moved to x/ved/lib
--- John Gibson, Feb 29 1996
        Made vedxvedmouse set new bindings when UseOldMouseBindings is
        false.
--- John Gibson, Feb 23 1996
        Changes to vedmouse__do_text_action
--- John Gibson, Oct 19 1995
        Added vedmouse__do_text_action
--- John Gibson, Feb 12 1994
        Added vedmouse__hscroll
--- John Gibson, Jun  7 1993
        Changes for POPC
--- John Gibson, Jun  4 1993
        Moved renamed vedselection_new_class to x/ved/auto
--- John Gibson, Sep  9 1992
        Got rid of X*VED_section
--- Adrian Howard, Aug 19 1992
        Foxed bug in line selection when -vedleftmargin- was non-zero
--- John Gibson, Jul 24 1992
        Added test for xvedvanilla in vedxvedmouse
--- Jon Meyer, Jul 17 1992
        Prevented menus being bound to right mouse button.
--- John Gibson, Mar 13 1992
        Undid last change. Moved selection class stuff here from
        xvedselections.p, and added new exported procedure
        -vedselection_new_class- to enable new selection classes to be defined
        (i.e. add an entry to -selection_classes- and define a
        corresponding vedmouse__ procedure).
--- James Goodlet, Mar 11 1992
        Removed #_IF DEF blah, and moved the dynamic creation of mark and
        select procedures into vedxvedmouse procedure, to ensure that they
        actually are dynamically created.  Mark procedures are created from
        -vedmarkclasses- list, in sim. fashion to select procedures.
--- John Gibson, Oct 28 1991
        Added assignments to xvedignorepixelmotion, and tests for
        vedscr_input_waiting in drag handlers
--- Jonathan Meyer, Sep 11 1991
        Incorporated vedmouse__set_focus into vedmouse__point directly
--- Jonathan Meyer, Sep  6 1991
        Added vedmouse__set_focus.
--- Jonathan Meyer, Sep  2 1991
        Added #_IF DEF vedmouse__point - stops bits from being reloaded.
--- John Gibson, Aug 26 1991
        Made selections, drags, etc work on status line
--- John Gibson, Aug 17 1991
        Added vedmouse__scroll
--- John Gibson, Aug 12 1991
        vedmouse__ procedures now don't return (name, data) -- this is
        done by the outer wrapper in xvedhandlers.p.
        Event handlers now take window as first arg and must return args.
--- John Gibson, Aug  7 1991
        Removed focus stuff -- all unnecessary
--- Jonathan Meyer, Aug  2 1991 ved_mark_named_range autoloadable
--- Adrian Howard, Jul 29 1991 : Changed PwmButtonBindings to
        UsePwmMouseBindings
--- Adrian Howard, Jul 25 1991
        - removed UserLevel resource
        - added PwmButtonBindings resource
        - altered drag movement of cursor to ctrl + button 3
--- John Gibson, Jul 13 1991
        Added arg to vedselection_set_primary
--- Adrian Howard, Jul 11 1991 : Fixed bug which allowed vert. scrolling
        after end of file reached
--- John Gibson, Jul 10 1991
        Lots of changes
--- John Gibson, Jul  9 1991
        Uses vedscreencursoron
--- Jonathan Meyer, Jul  8 1991
        Changed to use XVED_FLAVOUR
--- Adrian Howard, Jul  2 1991 : Bug fix on selections
--- Adrian Howard, Jul  2 1991
        - Fixed bug in -vedmouse__end_range-
        - Stopped the cursor moving when you marked a range
        - Added proper horizontal scrolling when you drag out of the window
        - Cleaned up some procedures
        - Used fast procedures where appropriate
--- Jonathan Meyer, Jul  1 1991
        Made selections respect ved_on_status
--- Adrian Howard, Jul  1 1991 : Added dragRelease to btn3 so vertical drag
        scrolls work correctly
--- Adrian Howard, Jul  1 1991 : Added margin setting with mouse
--- Adrian Howard, Jul  1 1991 : Bttn 3 paste now click instead of press
--- Jonathan Meyer, Jun 27 1991
        made press set input focus
 */
