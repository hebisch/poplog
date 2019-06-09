/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.x/x/ved/src/xvedbutton.p
 > Purpose:         Mouse button events in XVED
 > Author:          Jonathan Meyer, Oct 1990 (see revisions)
 > Documentation:   REF *XVED
 > Related Files:
 */
compile_mode :pop11 +strict;

include xved_constants.ph;

section $-xved;

/************************************************************************
 * Xved Mouse Button Event Management
 ************************************************************************/

lconstant macro (
    RELEASE = 0,
    PRESS   = 1,
    HOLD    = 2,
    );

lvars
    button_info_freelist = [],
    button_info_freelist_len = 0,

    curr_button = false,
    curr_modifiers,
    curr_nclicks,
    curr_start_col,
    curr_start_row,
    curr_start_x,
    curr_start_y,
    curr_motion_col,
    curr_motion_row,

    curr_isdrag,
    curr_ishold,
;

define lconstant get_button_info(/*col, row, x, y*/) -> info;
    lvars info, l = button_info_freelist;
    if l /== [] then
        sys_grbg_destpair(l) -> button_info_freelist;
        button_info_freelist_len fi_- 1 -> button_info_freelist_len
    else
        initintvec(XVM_SIZE)
    endif -> info;
    curr_button     -> fast_subscrintvec(XVM_BUTTON,    info);
    curr_modifiers  -> fast_subscrintvec(XVM_MODIFIERS, info);
    curr_nclicks    -> fast_subscrintvec(XVM_CLICKS,    info);
    curr_start_col  -> fast_subscrintvec(XVM_START_COL, info);
    curr_start_row  -> fast_subscrintvec(XVM_START_ROW, info);
    curr_start_x    -> fast_subscrintvec(XVM_START_X,   info);
    curr_start_y    -> fast_subscrintvec(XVM_START_Y,   info);
    ()              -> fast_subscrintvec(XVM_Y,         info);
    ()              -> fast_subscrintvec(XVM_X,         info);
    ()              -> fast_subscrintvec(XVM_ROW,       info);
    ()              -> fast_subscrintvec(XVM_COL,       info);
enddefine;

    /*  Called after event processed */
define xved_free_button_info(info);
    lvars info;
    ;;; no point in saving more than a few of these
    if button_info_freelist_len fi_< 8 then
        conspair(info, button_info_freelist) -> button_info_freelist;
        button_info_freelist_len fi_+ 1 -> button_info_freelist_len
    endif
enddefine;

define xved_button_callback(w, client, call);
    lvars w, client, call, button, mode, nclicks, col, row, x, y;
    dlocal pop_asts_enabled = false;
    ;;; get positions
    XptVal[fast] w(XtN mouseColumn:int, XtN mouseRow:int,
                    XtN mouseX:int, XtN mouseY:int) -> (col, row, x, y);
    col fi_+ 1 -> col;  row fi_+ 1 -> row;

    if client == "motion" then
        returnunless(curr_button);
        false -> curr_ishold;
        true -> curr_isdrag;
        unless xvedignorepixelmotion
        and col == curr_motion_col and row == curr_motion_row then
            (col, row) -> (curr_motion_col, curr_motion_row);
            xved_raise_event(w, "buttonDrag", get_button_info(col,row,x,y));
        endunless;
        return
    endif;

    call fi_&& 16:FF -> button;
    call fi_>> 8 -> call;
    call fi_&& 16:FF -> mode;
    call fi_>> 8 -> nclicks;

    ;;; block extraneous button events if we are within a button dialog.
    if curr_button then
        returnif(button /== curr_button)
    else
        ;;; return if -xved_button_release- has been called
        returnunless(mode == PRESS and nclicks == 0)
    endif;

    if mode == PRESS then
        if nclicks == 0 then
            ;;; BUTTON DOWN - start of mouse dialog
            false ->> curr_isdrag ->> curr_ishold -> xvedignorepixelmotion;
            button -> curr_button;
            0 -> curr_nclicks;
            ;;; record location in case of drag
            (col, row) -> (curr_start_col, curr_start_row);
            (x, y) -> (curr_start_x, curr_start_y);
            (col, row) -> (curr_motion_col, curr_motion_row);
            XptVal[fast] w(XtN modifiers:int) fi_&& 16:FF -> curr_modifiers;
            if curr_modifiers == 0 then XVM_NOMODMASK -> curr_modifiers endif;
            ;;; notify ved of event
            xved_raise_event(w, "buttonPress", get_button_info(col,row,x,y));
        else
            ;;; multi click press -- we don't raise this, but wait for
            ;;; it to turn into a Hold, Drag or Release
            nclicks -> curr_nclicks
        endif

    elseif mode == HOLD then
        ;;; BUTTON HOLD
        true -> curr_ishold;
        xved_raise_event(w, "buttonHold", get_button_info(col,row,x,y));

    else
        ;;; BUTTON UP - end of mouse dialog
        xved_raise_event(w,
            if curr_ishold then
                "buttonHoldRelease"
            elseif curr_isdrag then
                "buttonDragRelease"
            else
                if nclicks /== 0 then nclicks else curr_nclicks fi_+ 1 endif
                                    -> curr_nclicks;
                "buttonRelease"
            endif, get_button_info(col,row,x,y));
        false -> curr_button;
    endif;
enddefine;


/*
this is called when a button event is grabbed by eg. a menu, to
tell xved that it is released.
*/

define xved_button_release();
    false -> curr_button;
    true -> xvedeventhandled;
    vedscr_clear_input()
enddefine;


endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 23 1994
        3rd arg to xved_button_callback now the integer value directly, not
        an exptr
--- John Gibson, Sep  7 1992
        Changed to use XptVal
--- John Gibson, Dec  4 1991
        Made -xved_button_release- call -vedscr_clear_input-
--- John Gibson, Oct 28 1991
        Added xvedignorepixelmotion etc.
--- John Gibson, Oct 22 1991
        Removed use of listlength in xved_free_button_info (since this
        has an interrupt check inside it, which means that
        xved_button_callback can get called inside it).
--- John Gibson, Aug 19 1991
        Changes to handling of clicks
--- John Gibson, Aug 17 1991
        Added mouse x,y position to event data
--- John Gibson, Aug  9 1991
        Rewritten for new callbacks from widget, and to generate separate
        intvecs for each event (so a raised but not yet dispatched event
        doesn't get overwritten by another event).
--- Jonathan Meyer, Jun 17 1991
    Added xved_button_release, and also made check for valid buttons a bit
    more rigorous.
 */
