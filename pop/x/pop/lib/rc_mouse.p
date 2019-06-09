/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/rc_mouse.p
 > Purpose:         Facilities for tracking mouse in rc_window
 > Author:          Aaron Sloman, May 21 1990 (see revisions)
 > Documentation:   TEACH * RC_GRAPHIC, HELP * RC_GRAPHIC HELP * XT_CALLBACKS
 > Related Files:   LIB * RC_GRAPHIC, LIB *RC_ROTATE_XY
 */
compile_mode :pop11 +strict;

/*
This package enables line drawing etc to be controlled by button presses
using sensitivity to mouse movements to indicate, with a "rubber
band" where the next line will go. It provides more general mechanisms
in terms of which this is implemented.

In attempting to understand this it is best to work backwards from
the procedure rc_mouse_draw, defined at the end.

To test this out,
(a) load the whole file

(b) do

rc_start(); ;;; ensures that setup is done, create/clear graphic window

(c) do

rc_mouse_draw(false, 3);    ;;; draw a picture click button 1 or 2 for
                            ;;; next line. Stop with button 3


rc_mouse_draw(true, 3) =>   ;;; the same but create a list of coords
                            ;;; of ends of lines (using rc_conspoint)
                            ;;; See HELP * RC_GRAPHIC/rc_conspoint

;;; On a colour display try the following, then do some drawing
XpwSetColor(rc_window, 'red');
XpwSetColor(rc_window, 'green');
XpwSetColor(rc_window, 'black');

*/

uses-now Xpw;

section;
exload_batch;

#_IF DEF POPC_COMPILING
#_INCLUDE 'Xpw/popc_declare.ph'
#_ENDIF

include xpt_xgcvalues.ph;

uses rc_graphic, xt_callback;

vars rc_rubber_function;    ;;; used for rubber banding, below
                            ;;; GXequiv may be useful for colour
unless isinteger(rc_rubber_function) then
    GXxor -> rc_rubber_function
endunless;

;;; Function for transforming between screen co-ordinates and user
;;; co-ordinates. (Compare rc_transxyout in LIB * RC_GRAPHIC

define lconstant rc_mousexyin(/* x, y */) /* -> y -> x */;
    ;;; User definable. Make it identfn to have no effect
    ;;; Must leave numbers on stack in same order
    ;;; Warning - this can produce ratios as results
    ;;; Warning. Must be replaced by rc_rotate_xyin if lib rc_rotate_xy used.
    lvars x,y;
    rc_getxy() -> y -> x;
    ;;; deal with most common case first
    if rc_xscale == 1 or rc_xscale = 1.0 then
        x - rc_xorigin
    else (x - rc_xorigin) / rc_xscale
    endif; /* -> x */

    if rc_yscale == -1 then
        rc_yorigin - y
    elseif rc_yscale == 1 then
        y - rc_yorigin
    elseif rc_yscale = -1.0 then
        rc_yorigin - y
    elseif rc_yscale = 1.0 then
        y - rc_yorigin
    else
        (y - rc_yorigin) / rc_yscale
    endif; /* -> y */
enddefine;


vars procedure rc_transxyin;

if isundef(rc_transxyin) then rc_mousexyin -> rc_transxyin endif;

vars
    rc_button_procedures = [],  ;;; list of procedures to be run on button
    rc_move_procedures = [],    ;;; list of procedures to be run on move
    rc_mousing = false;

define lconstant do_button_actions(widget, item, data);
    ;;; apply all the procedures in rc_button_procedures to
    ;;; information supplied when mouse button event occurs
    lvars proc, widget, item, data;

    if rc_mousing then
        for proc in rc_button_procedures do
            recursive_valof(proc)(widget, item, data);
        endfor;
    endif;
enddefine;

define lconstant do_move_actions(widget, item, data);
    ;;; apply all the procedures in rc_move_procedures to
    ;;; information supplied when mouse move event occurs
    lvars proc, widget, item, data;

    if rc_mousing then
        for proc in rc_move_procedures do
            recursive_valof(proc)(widget, item, data);
        endfor;
    endif;
enddefine;


;;; Property so we know which widgets have callbacks on them. Done like this
;;; rather than using -XtHasCallbacks- so the user can add their own callbacks
;;; to the rc widget.
lconstant rc_mouse_setup_done =
    newanyproperty([], 10, false, false, false, false, "tmparg", false, false);

define rc_mouse_setup(widget);
    ;;; Setup the widget so that the above two procedures are installed
    ;;; see HELP * XT_CALLBACKS

    lvars widget;

    unless rc_mouse_setup_done(widget) do;
        ;;; SET UP BUTTON EVENTS
        XtAddCallback(widget, XtN buttonEvent, do_button_actions, "button");
        ;;; SET UP MOTION EVENTS
        XtAddCallback(widget, XtN motionEvent, do_move_actions, "move");
        true -> rc_mouse_setup_done(widget);
    endunless;

enddefine;

define rc_mouse_disable(widget);
    ;;; Undo the effect of setup
    lvars widget;
    if rc_mouse_setup_done(widget) then;
        XtRemoveCallback(widget, XtN buttonEvent, do_button_actions, "button");
        XtRemoveCallback(widget, XtN motionEvent, do_move_actions, "move");
        false -> rc_mouse_setup_done(widget);
    endif;
enddefine;

define rc_mouse_do(first_input, other_input, test_exit);
    lvars first = true, procedure (first_input, other_input, test_exit);
    lvars rc_mouse_done = false;

    ;;; When mouse button is first pressed call first_input
    ;;; Thereafter call other_input whenever either mouse is moved
    ;;;     or a mouse button is pressed
    ;;; If first_input has been called, then call test_exit after
    ;;; every button event and if it returns true, then stop

    ;;; Run rc_mouse_setup procedure if appropriate
    unless rc_mouse_setup_done(rc_window) then
        rc_mouse_setup(rc_window);
    endunless;

    dlocal
        rc_mousing,
        rc_button_procedures = #_< writeable [0] >_#, ;;; filled below
        rc_move_procedures = #_< writeable [0] >_#    ;;; filled below
        ;

    define lconstant handle_event(widget, item, data);
        ;;; Ignore case where data < 0 (button released). Otherwise
        ;;; call rc_record_mouse on transformed data
        lvars widget, item, data, x, y;
        rc_transxyin(XptVal[fast] rc_window(XtN mouseX, XtN mouseY)) -> (x, y);

        if (exacc ^int data ->> data) >= 0 then
            if first then
                ;;; don't do anything till button pressed
                if item = "button" then
                    first_input(x, y, data, item); false -> first;
                endif
            else
                other_input(x, y, data, item)
            endif;
        endif;
        if not(first) and test_exit(x, y, data, item) then
            true -> rc_mouse_done;
        endif
    enddefine;

    handle_event -> front(rc_button_procedures);
    handle_event -> front(rc_move_procedures);

    ;;; Enable mouse handling
    true -> rc_mousing;

    ;;; Sleep until exitfrom
    until rc_mouse_done do syshibernate(); enduntil;

enddefine;


define rc_mouse_draw(listpoints, stop_button);
    ;;; Draw a picture using the mouse. If listpoints is true, then
    ;;; return a list of points. Use "rubber banding effect"

    lvars listpoints, list = [], stop_button,
        lastx, lasty,   ;;; last actual point
        tempx, tempy;   ;;; last end of "rubber band" line

    dlocal rc_linefunction;

    define lconstant first(x, y, data, item);
        lvars x, y, data, item;
        rc_jumpto(x,y);
        rc_drawpoint(x,y);
        x ->> lastx -> tempx; y->> lasty -> tempy;
        if listpoints then rc_conspoint(x,y) :: list -> list endif
    enddefine;

    define lconstant other(x, y, data, item);
        lvars x,y,data, item;
        returnif(item == "button" and data < 0);
        ;;; clear last line
        rc_jumpto(lastx,lasty);
        rc_rubber_function -> rc_linefunction;
        rc_drawto(tempx, tempy);

        ;;; draw new line (properly if button pressed)
        rc_jumpto(lastx,lasty);
        if item == "button" then GXcopy else rc_rubber_function endif
            -> rc_linefunction;
        rc_drawto(x,y);

        if item == "button" then
            x -> lastx, y-> lasty;
            if listpoints then rc_conspoint(x,y) :: list -> list endif
        else
            x -> tempx; y -> tempy;
        endif;
    enddefine;

    define lconstant exit(/*x, y,*/ data, item);
        lvars data, item;
        -> ->;  ;;; remove x and y from stack
        item == "button" and data == stop_button
    enddefine;

    rc_mouse_do(first, other, exit);
    if listpoints then rev(list) endif

enddefine;

constant rc_mouse = true;    ;;; for "uses"

endexload_batch;
endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun 15 1993
        Added missing include for xpt_xgcvalues.ph
--- John Gibson, Sep 12 1992
        Changed to use XptVal
--- Adrian Howard, Feb 17 1992 : -rc_mouse_do- now calls -rc_mouse_setup- on
        -rc_window- if it has not been previously called
--- Adrian Howard, Jan 19 1992 : Cleaned up callbacks in -rc_mouse_setup- and
        -rc_disable_mouse-
--- Roger Evans, Jun 24 1991 changed to use slow callback routines
--- Roger Evans, Nov 22 1990 changed X*ptValue to use XtN
--- Roger Evans, Nov 22 1990 changed mouse callback back to flag version!
--- Roger Evans, Nov 20 1990 removed use of external_ptr_field,
        changed mouse callback to use exitfrom again
--- Roger Evans, Nov 19 1990 removed uses of - after XtN
--- Roger Evans, Oct 22 1990 reinstalled in masters
--- Jonathan Meyer, Sep 19 1990
    Added rc_mouse_done to mouse callback, instead of using exitfrom.
--- Aaron Sloman, Jun 27 1990
    Replaced GXxor with user-assignable rc_rubber_function
--- Aaron Sloman, Jun 22 1990
    Generalised to produce "rubber-band" option
--- Aaron Sloman, Jun  1 1990
    Fixed comment in rc_mouse_do
--- Aaron Sloman, May 23 1990
    Made it set up rc_transxyin only if not already set up
 */
