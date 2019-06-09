/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/fast_xt_grab.p
 > Purpose:         Fast input grabbing routines
 > Author:          Adrian Howard, Jul 17 1990 (see revisions)
 > Documentation:   REF *XT_GRAB
 > Related Files:   C.x/x/pop/lib/xt_grab.p
 */
compile_mode:pop11 +strict;

section;
exload_batch;

include xpt_constants.ph;
include xpt_coretypes.ph;

;;; Load the external "raw" procedures
XptLoadProcedures fast_xt_grab
    lvars
        XtAddGrab
        XtRemoveGrab
        XtGrabKey
        XtUngrabKey
        XtGrabKeyboard
        XtUngrabKeyboard
        XtGrabButton
        XtUngrabButton
        XtGrabPointer
        XtUngrabPointer
        XtSetKeyboardFocus
        XtCallAcceptFocus
        XtLastTimestampProcessed
;

;;; Redirect user input to a modal widget - 17/07/90
;;; Input - <Widget> <BOOL> <BOOL>
define fast_XtAddGrab(widget, exclusive, spring_loaded);
    lvars widget, exclusive, spring_loaded;
    exacc (3) raw_XtAddGrab(
                    widget,
                    -> XptCoerceBoolean(exclusive),
                    -> XptCoerceBoolean(spring_loaded)
                 );
enddefine;


;;; Stops the redirection of user input to a model widget - 17/07/90
;;; Input - <Widget>
define fast_XtRemoveGrab() with_nargs 1;
    exacc (1) raw_XtRemoveGrab();
enddefine;


;;; Redirect keyboard input to a normal descendant of a widget - 17/07/90
;;; Input - <Widget> <Widget>
define fast_XtSetKeyboardFocus() with_nargs 2;
    exacc (2) raw_XtSetKeyboardFocus();
enddefine;


;;; Actively grab the keyboard - 14/08/90
;;; Input - <Widget> <BOOL> <INT> <INT> <Time>, Output - <INT>
define fast_XtGrabKeyboard(widget, bool, ptr_mode, kbd_mode, time);
    lvars widget, bool, ptr_mode, kbd_mode, time;
    exacc (5):int raw_XtGrabKeyboard(
                        widget,
                        -> XptCoerceBoolean(bool),
                        ptr_mode, kbd_mode,
                        time
                      );
enddefine;


;;; Cancel an active keyboard grab - 14/08/90
;;; Input - <Widget> <Time>
define fast_XtUngrabKeyboard() with_nargs 2;
    exacc (2) raw_XtUngrabKeyboard();
enddefine;


;;; Call a widgets accept_focus procedure - 14/08/90
;;; Input - <Widget> <TimePtr>, Output - <BOOL>
define fast_XtCallAcceptFocus() with_nargs 2;
    exacc (2):XptBoolean raw_XtCallAcceptFocus();
enddefine;


;;; Actively grab the pointer - 14/08/90
;;; Input - <Widget> <BOOL> <uint> <INT> <INT> <Window> <Cursor> <Time>
;;; Output - <INT>
define fast_XtGrabPointer(widget, bool, uint, ptr_mode, kbd_mode,
            window, cursor, time);
    lvars widget, bool, uint, ptr_mode, kbd_mode, window, cursor, time;
    exacc (8):int raw_XtGrabPointer(
                        widget,
                        -> XptCoerceBoolean(bool),
                        uint, kbd_mode, ptr_mode,
                        window,
                        cursor,
                        time
                     );
enddefine;


;;; Cancel an active pointer grab - 14/08/90
;;; Input - <Widget> <Time>
define fast_XtUngrabPointer() with_nargs 2;
    exacc (2) raw_XtUngrabPointer();
enddefine;


;;; Passively grab a single key - 14/08/90
;;; Input - <Widget> <KeyCode> <Modifiers> <BOOL> <INT> <INT>
define fast_XtGrabKey(widget, keycode, modifiers, bool, ptr_mode,
            kbd_mode);
    lvars widget, keycode, modifiers, bool, ptr_mode, kbd_mode;
    exacc (6) raw_XtGrabKey(
                    widget,
                    keycode,
                    modifiers,
                    -> XptCoerceBoolean(bool),
                    ptr_mode, kbd_mode
                 );
enddefine;


;;; Cancel a passive key grab - 14/08/90
;;; Input - <Widget> <KeyCode> <Modifiers>
define fast_XtUngrabKey(widget, keycode, modifiers);
    lvars widget, keycode, modifiers;
    exacc (3) raw_XtUngrabKey(
                        widget,
                        keycode,
                        modifiers
                   );
enddefine;


;;; Passively grab a single pointer button - 14/08/90
;;; Input - <Widget> <INT> <Modifiers> <BOOL> <uint> <INT> <INT> <Window>
;;;         <Cursor>
define fast_XtGrabButton(widget, button, modifiers, owner_events,
            event_mask, ptr_mode, kbd_mode, window, cursor);
    lvars widget, button, modifiers, owner_events, event_mask, ptr_mode,
          kbd_mode, window, cursor;
    exacc (9) raw_XtGrabButton(
                        widget, button,
                        modifiers,
                        -> XptCoerceBoolean(owner_events),
                        event_mask, ptr_mode, kbd_mode, window, cursor
                    );
enddefine;


;;; Cancel a passive button grab - 14/08/90
;;; Input - <Widget> <uint> <Modifiers>
define fast_XtUngrabButton() with_nargs 3;
    exacc (3) raw_XtUngrabButton();
enddefine;

;;; Get last Time stamp - JM 8/7/91
;;; Input - <Display>
;;; Output - <TimePtr>

define fast_XtLastTimestampProcessed() with_nargs 1;
    lconstant import_timer_ptr = XptImportAny(%XDT_TIMEPTR%);
    exacc (1):exptr#import_timer_ptr raw_XtLastTimestampProcessed();
enddefine;

lvars ct_desc = false;
;;;
define active XptCurrentTime;
    ct_desc or (consXptDescriptor(null_external_ptr, XDT_TIMEPTR) ->> ct_desc)
enddefine;
;;; constant, so no updater

;;; So uses works OK
constant fast_xt_grab= true;


endexload_batch;
endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 14 1993
        Changed XptCurrentTime to be an active constant (since POPC
        can't handle compile-time XptDescriptors)
--- John Gibson, Nov  3 1991
        includes xpt_coretypes.ph
--- Jonathan Meyer, Jul  8 1991 Added fast_XtLastTimestampProcessed
--- Adrian Howard, Jun 25 1991 : Removed reference to fast_xt_procs
--- Roger Evans, Nov 19 1990 tidied up
--- Roger Evans, Oct 19 1990 changed to use XptLoadProcedures
--- Roger Evans, Oct 11 1990 changed to use exacc
--- Adrian Howard, Sep 13 1990 : Made procs global
--- Adrian Howard, Sep  7 1990 : Added var so uses works
--- Adrian Howard, Sep  6 1990 : Altered calls to XptCoerce/Import to make
        compatable with format of new external conversion/access procs
 */
