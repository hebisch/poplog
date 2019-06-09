/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xt_grab.p
 > Purpose:         Checking input grabbing routines
 > Author:          Adrian Howard, Jul 17 1990 (see revisions)
 > Documentation:   REF *XT_GRAB
 > Related Files:   C.x/x/pop/lib/xpt_typecheck.p,
 >                  C.x/x/pop/lib/fast_xt_grab.p
 */


compile_mode:pop11 +strict;


section;


uses xpt_typecheck.p;        ;;; Get the type checking routines
uses fast_xt_grab;          ;;; Get fast versions of the procedures

include xpt_generaltypes.ph;


;;; Redirect user input to a modal widget - 17/07/90
;;; Input - <Widget> <BOOL> <BOOL>
define global XtAddGrab(widget, exclusive, spring_loaded);
    lvars widget, exclusive, spring_loaded;
    fast_XtAddGrab(XptCheckWidget(widget), exclusive, spring_loaded);
enddefine;


;;; Stops the redirection of user input to a model widget - 17/07/90
;;; Input - <Widget>
define global XtRemoveGrab() with_nargs 1;
    fast_XtRemoveGrab(XptCheckWidget());
enddefine;


;;; Redirect keyboard input to a normal descendant of a widget - 17/07/90
;;; Input - <Widget> <Widget>
define global XtSetKeyboardFocus(subtree, descendant);
    lvars subtree, descendant;
    fast_XtSetKeyboardFocus(    XptCheckWidget(subtree),
                                XptCheckWidget(descendant)
                           );
enddefine;


;;; Actively grab the keyboard - 14/08/90
;;; Input - <Widget> <BOOL> <INT> <INT> <Time>, Output - <INT>
define global XtGrabKeyboard(widget, bool, ptr_mode, kbd_mode, time);
    lvars widget, bool, ptr_mode, kbd_mode, time;
    fast_XtGrabKeyboard(    XptCheckWidget(widget),
                            bool,
                            XptCheckInt(ptr_mode),
                            XptCheckInt(kbd_mode),
                            XptCheckTime(time)
                       );
enddefine;


;;; Cancel an active keyboard grab - 14/08/90
;;; Input - <Widget> <Time>
define global XtUngrabKeyboard(widget, time);
    lvars widget, time;
    fast_XtUngrabKeyboard(  XptCheckWidget(widget),
                            XptCheckTime(time)
                         );
enddefine;


;;; Call a widgets accept_focus procedure - 14/08/90
;;; Input - <Widget> <TimePtr>, Output - <BOOL>
define global XtCallAcceptFocus(widget, timeptr);
    lvars widget, timeptr;
    fast_XtCallAcceptFocus( XptCheckWidget(widget),
                            XptCheckTimePtr(timeptr)
                          );
enddefine;


;;; Actively grab the pointer - 14/08/90
;;; Input - <Widget> <BOOL> <uint> <INT> <INT> <Window> <Cursor> <Time>
;;; Output - <INT>
define global XtGrabPointer(widget, bool, uint, ptr_mode, kbd_mode, window,
            cursor, time);
    lvars widget, bool, uint, ptr_mode, kbd_mode, window, cursor, time;
    fast_XtGrabPointer( XptCheckWidget(widget),
                        bool,
                        XptCheckUnsignedInt(uint),
                        XptCheckInt(kbd_mode), XptCheckInt(ptr_mode),
                        XptCheckWindow(window),
                        XptCheckCursor(cursor),
                        XptCheckTime(time)
                      );
enddefine;


;;; Cancel an active pointer grab - 14/08/90
;;; Input - <Widget> <Time>
define global XtUngrabPointer(widget, time);
    lvars widget, time;
    fast_XtUngrabPointer(   XptCheckWidget(widget),
                            XptCheckTime(time)
                        );
enddefine;


;;; Passively grab a single key - 15/08/90
;;; Input - <Widget> <KeyCode> <Modifiers> <BOOL> <INT> <INT>
define global XtGrabKey(widget, keycode, modifiers, bool, ptr_mode, kbd_mode);
    lvars widget, keycode, modifiers, bool, ptr_mode, kbd_mode;
    fast_XtGrabKey( XptCheckWidget(widget),
                    XptCheckKeyCode(keycode),
                    XptCheckModifiers(modifiers),
                    bool,
                    XptCheckInt(ptr_mode),
                    XptCheckInt(kbd_mode)
                  );
enddefine;


;;; Cancel a passive key grab - 15/08/90
;;; Input - <Widget> <KeyCode> <Modifiers>
define global XtUngrabKey(widget, keycode, modifiers);
    lvars widget, keycode, modifiers;
    fast_XtUngrabKey(   XptCheckWidget(widget),
                        XptCheckKeyCode(keycode),
                        XptCheckModifiers(modifiers)
                    )
enddefine;


;;; Passively grab a single pointer button - 14/08/90
;;; Input - <Widget> <INT> <Modifiers> <BOOL> <uint> <INT> <INT> <Window>
;;;         <Cursor>
define global XtGrabButton(widget, button, modifiers, owner_events,
            event_mask, ptr_mode, kbd_mode, window, cursor);
    lvars widget, button, modifiers, owner_events, event_mask, ptr_mode,
          kbd_mode, window, cursor;
    fast_XtGrabButton(  XptCheckWidget(widget),
                        XptCheckInt(button),
                        XptCheckModifiers(modifiers),
                        owner_events,
                        XptCheckUnsignedInt(event_mask),
                        XptCheckInt(ptr_mode),
                        XptCheckInt(kbd_mode),
                        XptCheckWindow(window),
                        XptCheckCursor(cursor)
                     );
enddefine;


;;; Cancel a passive button grab - 14/08/90
;;; Input - <Widget> <uint> <Modifiers>
define global XtUngrabButton(widget, uint, modifiers);
    lvars widget, uint, modifiers;
    fast_XtUngrabButton(    XptCheckWidget(widget),
                            XptCheckUnsignedInt(uint),
                            XptCheckModifiers(modifiers)
                       );
enddefine;


;;; Get last Time stamp - JM 8/7/91
;;; Input - <Display>
;;; Output - <TimePtr>

define global XtLastTimestampProcessed() with_nargs 1;
    fast_XtLastTimestampProcessed(XptCheckDisplayPtr());
enddefine;


;;; So uses works OK
global vars xt_grab= true;


endsection;

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Jul  8 1991
    Added XtLastTimestampProcessed
--- Jonathan Meyer, Apr  1 1991 Renamed fast_XtUngrabPointer XtUngrabPointer
--- Roger Evans, Nov 19 1990 tidied up
--- Adrian Howard, Sep 13 1990 : Made procs global
--- Adrian Howard, Sep  7 1990 : Added var so uses works
--- Adrian Howard, Sep  5 1990 : xt_typecheck --> xpt_typecheck
 */
