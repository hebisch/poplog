/*  --- Copyright University of Sussex 1993.  All rights reserved. ---------
 >  File:           C.x/x/pop/include/xved_constants.ph
 >  Purpose:        Multi Window X VED - constant definitions
 >  Author:         Jonathan Meyer, 20 July 1990 (see revisions)
 */

#_TERMIN_IF DEF XVED_CONSTANTS_INCLUDED

section;

iconstant macro (
    ;;; mouse event datavec subscripts
    XVM_BUTTON      = 1,
    XVM_MODIFIERS   = 2,
    XVM_CLICKS      = 3,
    XVM_COL         = 4,
    XVM_ROW         = 5,
    XVM_START_COL   = 6,
    XVM_START_ROW   = 7,
    XVM_X           = 8,
    XVM_Y           = 9,
    XVM_START_X     = 10,
    XVM_START_Y     = 11,
    XVM_SIZE        = 11,

    ;;; modifier keys
    XVM_SHIFTMASK   = 2:1e0,
    XVM_LOCKMASK    = 2:1e1,
    XVM_CONTROLMASK = 2:1e2,
    XVM_MOD1MASK    = 2:1e3, XVM_METAMASK = XVM_MOD1MASK,
    XVM_MOD2MASK    = 2:1e4,
    XVM_MOD3MASK    = 2:1e5,
    XVM_MOD4MASK    = 2:1e6,
    XVM_MOD5MASK    = 2:1e7,
    XVM_NOMODMASK   = 2:1e8,
    XVM_ANYMODMASK  = 2:1e9 - 1,

    ;;; buttons
    XVM_BTN1MASK    = 2:1e0,
    XVM_BTN2MASK    = 2:1e1,
    XVM_BTN3MASK    = 2:1e2,
    XVM_BTN4MASK    = 2:1e3,
    XVM_BTN5MASK    = 2:1e4,
    XVM_ANYBTNMASK  = 2:1e5 - 1,


    ;;; key press data
    XVK_BASE_SEQ    = '\^[[',
    XVK_KEY         = 1,
    XVK_MODIFIERS   = 2,

    ;;; Menubar labels
    XVMB_HELP_LABEL     = 'Help',
    XVMB_GOTOBUFF_LABEL = 'Buffer',

    ;;; General
    XV_APPLICATION_NAME = 'xved',
    XV_CLASS_NAME       = 'XVed',

);

iconstant XVED_CONSTANTS_INCLUDED = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun  7 1993
        Made this the proper include file and got rid of the src one
--- John Gibson, Sep  9 1992
        private stuff -> xved_declare.ph
--- John Gibson, Nov 16 1991
        Removed XVS_ macros -- no longer needed.
--- Integral Solutions Ltd, Oct 22 1991 (Julian Clinton)
    Also allows dialogs if -pop_ui_propertytool- is defined.
--- John Gibson, Sep 18 1991
        I*NCLUDE_constant -> iconstant
--- John Gibson, Aug 17 1991
        Added XVM_X, Y, START_X, Y
--- John Gibson, Aug 10 1991
        Added missing modifiers
--- John Gibson, Aug  8 1991
        Made XVM_CLICKS be a separate field
--- John Gibson, Jul  9 1991
        Replaced ved file subscript macros with include vedfile_struct
 */
