/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.x/x/ved/src/xvedscrollbar.p
 > Purpose:         Scrollbar management code for VED
 > Author:          Jonathan Meyer (see revisions)
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

section $-xved => xvedscrollbar;

#_INCLUDE 'gui.ph'
include xpt_xtypes.ph;

lvars
    scrollcheck_enabled = true,
    scrollbar_gui_switch_vec,
;

define lconstant scrollbar_callback(w, client, call, evname);
    lvars w, client, call, evname;
    dlocal pop_asts_enabled = false;
    p_SCROLL_CB_VERIFY(w, call);
    xved_raise_event(client, evname, w);
    if xved == "openlook" then
        ;;; OLIT -- give Ved some priorities
        external_defer_apply(vedprocess_try_input)
    endif
enddefine;

define lconstant init_scrollbar(window, scrollargs, evname, fieldp);
    lvars window, scrollargs, evname, fieldp, scrollbar, fg, bg, menu, callp;

    XptVal[fast] window(XtN scrollbarForeground:XptPixel,
                        XtN scrollbarBackground:XptPixel) -> (fg, bg);
    xved_set_color_args(fg, bg, true);

    ;;; give the scrollbar some basic defaults, set orientation etc
    xved_set_args(scrollargs, false);

    XtCreateManagedWidget('scrollbar', wc_SCROLLBAR, window.XtParent,
                                            xved_arg_list()) -> scrollbar;

    if p_HAS_SCROLLBAR_MENU(scrollbar) ->> menu then
        ;;; If we have a scrollbar menu, make it the same colour as the
        ;;; scrollbar
        xved_set_color_args(fg, bg, true);
        define lconstant set_res(widget);
            lvars widget;
            fast_XtSetValues(widget, xved_arg_list());
            applist(XptChildren(widget), set_res);
        enddefine;
        set_res(menu);
    endif;

    p_SET_SCROLLBAR(scrollbar, 100, 1, 1, 1, 0);

    scrollbar_callback(%evname%) -> callp;

    /* add motion tracking callback */
    XptAddCallback(scrollbar, n_ScrollbarMoved, callp, window, identfn);
    if n_ScrollbarDragged then
        /* add callback for interactive dragging */
        XtAddCallback(scrollbar, n_ScrollbarDragged, callp, window)
    endif;

    /* build up vector of scrollbar info */
    {% scrollbar, 0, 0, 0, 0 %} -> window.fieldp;
enddefine;

define xved_init_scrollbar(window);
    lvars window;
    init_scrollbar(window, l_SCROLLBAR_ARGS, "scrollbarMoved",
                                                    xvedwin_scrollbar)
enddefine;

define xved_init_hscrollbar(window);
    lvars window;
    init_scrollbar(window, l_HSCROLLBAR_ARGS, "hscrollbarMoved",
                                                    xvedwin_hscrollbar)
enddefine;

/* called on each vedprocess loop to check scrollbar's state */

define lconstant check_scrollbars(evname);
    lvars evname, Len, infovec;

    define lconstant check_scrollbar(Max, Len, Value, Incr, infovec);
        lvars (scrollbar, oldMax, oldLen, oldValue, oldIncr)
                = explode(infovec);
        returnunless(XptIsLiveType(scrollbar, "Widget"));
        returnif(Max == oldMax and Len == oldLen and Value == oldValue
                    and Incr == oldIncr);

        p_SET_SCROLLBAR(scrollbar,
            if wvedwindow.xvedwin_isnew or oldMax == 0 then
                ;;; uninitialised scrollbar -- set it up now
                Max, Value, Len, Incr
            else
                Max /== oldMax and Max,
                Value /== oldValue and Value,
                Len /== oldLen and Len,
                Incr /== oldIncr and Incr
            endif, false);

        Max     -> fast_subscrv(2, infovec);
        Len     -> fast_subscrv(3, infovec);
        Value   -> fast_subscrv(4, infovec);
        Incr    -> fast_subscrv(5, infovec);
    enddefine;

    returnunless(scrollcheck_enabled and wved_is_live_window(wvedwindow));

    ;;; need to be off status so that we can get correct buffer variables
    dlocal ved_on_status = false;

    if evname /== "hscrollbarMoved"
    and isvector(wvedwindow.xvedwin_scrollbar ->> infovec) then
        vedwindowlength fi_- 1 -> Len;
        check_scrollbar(
            fi_max(vedlineoffset fi_+ Len, vvedbuffersize),
                        Len, vedlineoffset, 1, infovec)
    endif;
    ;;; must always check the horiz one
    if isvector(wvedwindow.xvedwin_hscrollbar ->> infovec) then
        vedwlinewidth -> Len;
        check_scrollbar(
            fi_max(vedcolumnoffset fi_+ Len, xved_max_linewidth(true)),
                        Len, vedcolumnoffset, vedvscr_average_width(1),
                        infovec)
    endif;
enddefine;

define xved_check_scrollbar = check_scrollbars(%false%) enddefine;

define lconstant scrollbar_moved_handler(w, name, sb, evname) -> (w, name, sb);
    lvars w, name, sb, evname;
    dlocal pop_asts_enabled = false, xvedwarpmouse = false;
    returnif(xved_is_next_event(w, evname));
    xved_select_window(w);
    procedure;
        dlocal ved_on_status = false, scrollcheck_enabled = false;
        lvars offs = p_SCROLLBAR_VALUE(sb);
        if evname == "scrollbarMoved" then
            xved_scroll_to_lineoffset(offs)
        else
            xved_scroll_to_columnoffset(offs)
        endif
    endprocedure();
    check_scrollbars(evname);
enddefine;

scrollbar_moved_handler(%"scrollbarMoved"%)
                        -> xvedeventtable("scrollbarMoved");
scrollbar_moved_handler(%"hscrollbarMoved"%)
                        -> xvedeventtable("hscrollbarMoved");


define xved_scrollbar_children(widget);
    lvars widget, pane;
    if p_HAS_SCROLLBAR_MENU(widget) ->> pane then pane endif
enddefine;


/* ======= Set for Motif or OLIT ====================================== */

#_IF not(DEF POPC_COMPILING)
    #_IF DEF popxlink_motif
        uses $-xved$-scrollbar_xm;
        weak constant scrollbar_xol;
    #_ELSEIF DEF popxlink_openlook
        uses $-xved$-scrollbar_xol;
        weak constant scrollbar_xm;
    #_ENDIF
#_ENDIF

define lconstant set_gui();
    if testdef popxlink_motif then
        weakref[popxlink_motif] scrollbar_xm
    elseif testdef popxlink_openlook then
        weakref[popxlink_openlook] scrollbar_xol
    else
        mishap(0, 'XVed: SYSTEM NOT LINKED WITH MOTIF OR OPENLOOK');
    endif -> scrollbar_gui_switch_vec
enddefine;

#_IF DEF POPC_COMPILING
    sys_runtime_apply(set_gui);
#_ELSE
    set_gui();
#_ENDIF

constant xvedscrollbar = true;      ;;; signature that scrollbar is loaded

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 18 1997
        Changes for variable-width mode
--- John Gibson, May  3 1994
        sys_s*ignals_enabled -> pop_asts_enabled
--- John Gibson, Apr 14 1994
        Xpt*DeferApply -> external_defer_apply
--- John Gibson, Feb 11 1994
        Added horizontal scrollbar
--- John Gibson, Dec 23 1993
        Improved gui interface
--- John Gibson, Jun  3 1993
        Rewritten for POPC etc
--- Adrian Howard, Mar 22 1993
        Set fontColor resource under OLIT
--- John Gibson, Dec 19 1992
        Added xved_scrollbar_children, removed xved_ch*ildren_of_widget
--- Adrian Howard, Sep 14 1992
        Made sure Motif page increment never less than 1
--- John Gibson, Sep  9 1992
        Changed to use XptVal
--- Adrian Howard, Aug 17 1992
        Made the fg/bg colours of any scrollbar menu initialise to
        scrollbarFore/Background
--- Adrian Howard, Aug 14 1992
        Added update of -xved_children_of_widget- so that the BG and FG
        colours of any scrollbar menu are updated properly in xvedresources.p
--- Jonathan Meyer, Aug  4 1992
        Made scrollbar_callback call vedprocess_try_input directly in OLIT,
        so that dragging the scrollbar causes the screen to update directly.
        Added code to set XmN pageIncrement for Motif so that pageup/down
        moves the text by one line less than a window-full.
--- John Gibson, Jul 28 1992
        Got rid of save/setscrollvars (setscrollvars had a bug -- if the
        window didn't have a scrollbar it was corrupting memory).
        Set scrollbar fg/bg from Text widget resources.
--- Jonathan Meyer, Sep 25 1991
        Removed redundant call to PLACE_SCROLL
--- Jonathan Meyer, Sep  5 1991
        Added scrollcheck_enabled
--- Jonathan Meyer, Sep  3 1991
        Added check for xved_is_next_event in xved_check_scrollbar so that
        the scrollbar doesn't jump when its dragged
--- John Gibson, Aug 29 1991
        Uses system vedprocess_try_input instead of Xved_try_input
--- John Gibson, Aug 17 1991
        Made scrollbar_moved_handler use xved_scroll_to_lineoffset
        (xvedgeneral.p)
--- John Gibson, Aug 12 1991
        Event handlers now take window as arg
--- John Gibson, Aug 10 1991
        Made event handlers return their args
--- Jonathan Meyer, Jul 27 1991
        Added xved_before_editor_exit
--- John Gibson, Jul  9 1991
        Uses vedscreencursoron
--- Jonathan Meyer, Jul  8 1991
        scrollbarOn is now a widget resource
--- Jonathan Meyer, Jul  3 1991
        Added scrollbarOn
 */
