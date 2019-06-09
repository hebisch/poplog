/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/rc_context.p
 > Purpose:         Save and restore current rc graphic context
 > Author:          Aaron Sloman, Jun  22 1990 (see revisions)
 > Documentation:   HELP * RC_GRAPHIC/RC_CONTEXT
 > Related Files:   LIB * RC_GRAPHIC and files referred to there
 */
compile_mode :pop11 +strict;

section;
exload_batch;

uses rc_graphic, rc_rotate_xy;

;;; First define some utilities.

;;; Next lot temporary, till rc_mouse sorted out

vars rc_button_procedures, rc_move_procedures, rc_mousing;

;;; Declare a vector class to hold context information for a window
vectorclass constant rc_context full;

;;; We need to know the variables defining the RC window context.
;;; Put them in a list for use by the next two macros

global constant rc_context_vars;

;;; Use the list of variables provided by user, or the default list below
unless islist(rc_context_vars) then
    pop11_compile([global vars rc_props;]);

  [
    ;;; window variables
    rc_window           ;;; this should always be the first item

    rc_window_x
    rc_window_y
    rc_window_xsize
    rc_window_ysize

    ;;; clipping variables
    rc_clipping
    rc_xmin
    rc_ymin
    rc_xmax
    rc_ymax

    ;;; turtle state
    rc_xposition
    rc_yposition
    rc_heading

    ;;; transformation to user co-ordinates
    rc_transxyin
    rc_transxyout
    rc_xscale
    rc_yscale
    rc_xorigin
    rc_yorigin

    ;;; active variables
    rc_frame_angle
    rc_linewidth
    rc_linestyle
    rc_linefunction

    ;;; callback facilities
    rc_button_procedures
    rc_move_procedures
    rc_mousing

    ;;; a field for the user
    rc_props
]

    -> rc_context_vars;

endunless;

lconstant rc_context_size = listlength(rc_context_vars)
;

;;; Some utilities for incorporating the above context variable names
;;; in rc_context and its updater

define lconstant macro PUSH_VALUES;
    ;;; create code to put values of context variables on stack
    ;;; must be in reverse order
    lvars item;
    for item in rev(rc_context_vars) do
        item, ","
    endfor;
enddefine;

define lconstant macro POP_VALUES;
    ;;; create code to assign from stack to variables
    lvars item;
    for item in rc_context_vars do
        "->", item
    endfor
enddefine;

define lconstant check_rc_context(item);
    ;;; check that item is a context vector
    lvars item;
    unless isrc_context(item) and datalength(item) == rc_context_size then
        mishap(item,1, 'RC_CONTEXT VECTOR REQUIRED')
    endunless
enddefine;

define rc_context(context) -> context;
    ;;; Returns a vector containing the global variables defining
    ;;; the current window context.
    ;;; -context- is false or a vector. If a vector then it is filled,
    ;;; otherwise a new one is created. This procedure and its updater must
    ;;; be amended if any new global variables are added.

    lvars context;

    if context then
        check_rc_context(context)
    endif;

    ;;; Put context values on stack
    PUSH_VALUES;

    ;;; store them
    if context then fill(context) -> context
    else
        consrc_context(rc_context_size) -> context
    endif
enddefine;

define updaterof rc_context(context);
    lvars context;
    check_rc_context(context);

    destrc_context(context) ->;

    POP_VALUES;
enddefine;

define rc_destroy_context(context);
    lvars context, window = context(rc_context_size);

    check_rc_context(context);

    if xt_islivewindow(window) then
        XptDestroyWindow(window);
    endif;

    false -> context(1);
enddefine;

endexload_batch;
endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun 15 1993
        Removed redefinition of rc_frame_angle and uses rc_rotate_xy instead
--- Aaron Sloman, Dec  7 1990
    Made it restore rc_window before other things, to fix bug reported
    by David Young.
--- Roger Evans, Oct 22 1990 reinstalled in masters
--- Aaron Sloman, Jun 27 1990 added rc_props
--- Aaron Sloman, Jun 23 1990 removed rc_mouse_procedures
 */
