/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/include/XolFont.ph
 > Purpose:         Font utilities for Xol
 > Author:          Jonathan Meyer, Sep  2 1990 (see revisions)
 > Documentation:   HELP *OpenLook
 */

#_TERMIN_IF DEF XOLFONT_INCLUDED

section;

iconstant macro (
        NORMAL_LEFT             = 1,
        NORMAL_LINES            = 2,
        NORMAL_RIGHT            = 3,

        HIGHLIGHT_LEFT          = 4,
        HIGHLIGHT_LINES         = 5,
        HIGHLIGHT_RIGHT         = 6,

        DEFAULT_LEFT            = 7,
        DEFAULT_LINES           = 8,
        DEFAULT_RIGHT           = 9,

        STACK_LEFT              = 10,
        STACK_LINES             = 11,
        STACK_RIGHT             = 12,

        STACK_HI_LEFT           = 13,
        STACK_HI_LINES          = 14,
        STACK_HI_RIGHT          = 15,

        STACK_DE_LEFT           = 16,
        STACK_DE_LINES          = 17,
        STACK_DE_RIGHT          = 18,

        HALF_LINES              = 19,
        HALF_HI_LINES           = 20,
        HALF_DE_LINES           = 21,

        HALF_HALF               = 22,
        HALF_HALF_HI            = 23,
        HALF_HALF_DEF           = 24,

        FILL_LEFT               = 25,
        FILL_LINES              = 26,
        FILL_RIGHT              = 27,

        STACK_FILL_LEFT         = 28,
        STACK_FILL_LINES        = 29,
        STACK_FILL_RIGHT        = 30,

        POPUP                   = 31,
        ARROW                   = 32,
        BLANK                   = 33,

        NORMAL_LINES1           = 34,
        HIGHLIGHT_LINES1        = 35,
        DEFAULT_LINES1          = 36,
        STACK_LINES1            = 37,
        STACK_HI_LINES1         = 38,
        STACK_DE_LINES1         = 39,
        HALF_LINES1             = 40,
        HALF_HI_LINES1          = 41,
        HALF_DE_LINES1          = 42,
        FILL_LINES1             = 43,
        STACK_FILL_LINES1       = 44,
        BLANK1                  = 45,

        MENU_PULLRIGHT          = 46,
        MENU_PULLDOWN           = 47,

        MENU_DEFAULT_LEFT       = 48,
        MENU_DEFAULT_LINES      = 49,
        MENU_DEFAULT_RIGHT      = 50,
        MENU_DEFAULT_LINES1     = 51,

        MENU_SET_LEFT           = 52,,
        MENU_SET_LINES          = 53,,
        MENU_SET_RIGHT          = 54,,
        MENU_SET_LINES1         = 55,,

        ABB_MENU_LEFT           = 56,
        ABB_MENU_RIGHT          = 57,
        ABB_MENU_LEFT_SET       = 58,
        ABB_MENU_RIGHT_SET      = 59,

        NONMENU_PULLRIGHT       = 60,
        NONMENU_PULLDOWN        = 61,
        CHECKBOX_LEFT           = 62,
        CHECKBOX_RIGHT          = 63,
        CHECKBOX_LEFT_SET       = 64,
        CHECKBOX_RIGHT_SET      = 65,
);

iconstant XOLFONT_INCLUDED = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  7 1993
        Made an include file
--- Andreas Schoter, Jul 15 1991
    Added global constant XolFont for compatibility with uses
 */
