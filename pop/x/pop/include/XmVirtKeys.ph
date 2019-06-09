/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/include/XmVirtKeys.ph
 > Purpose:         Virtual keysym definitions
 > Author:          Jonathan Meyer, Feb  9 1991 (see revisions)
 > Documentation:   HELP *MOTIF
 */

#_TERMIN_IF DEF XMVIRTKEYS_INCLUDED

section;

iconstant macro (

    osfXK_BackSpace = 16:1004FF08,
    osfXK_Insert    = 16:1004FF63,
    osfXK_Delete    = 16:1004FFFF,
    osfXK_Copy      = 16:1004FF02,
    osfXK_Cut       = 16:1004FF03,
    osfXK_Paste     = 16:1004FF04,

    osfXK_AddMode       = 16:1004FF31,
    osfXK_PrimaryPaste  = 16:1004FF32,
    osfXK_QuickPaste    = 16:1004FF33,

    osfXK_PageUp    = 16:1004FF41,
    osfXK_PageDown  = 16:1004FF42,

    osfXK_EndLine   = 16:1004FF57,
    osfXK_BeginLine = 16:1004FF58,

    osfXK_Activate  = 16:1004FF44,

    osfXK_MenuBar   = 16:1004FF45,

    osfXK_Clear     = 16:1004FF0B,
    osfXK_Cancel    = 16:1004FF69,
    osfXK_Help      = 16:1004FF6A,
    osfXK_Menu      = 16:1004FF67,
    osfXK_Select    = 16:1004FF60,
    osfXK_Undo      = 16:1004FF65,

    osfXK_Left      = 16:1004FF51,
    osfXK_Up        = 16:1004FF52,
    osfXK_Right     = 16:1004FF53,
    osfXK_Down      = 16:1004FF54,

);

iconstant XMVIRTKEYS_INCLUDED = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 14 1993
        Made an include file
--- John Gibson, Nov  1 1991
        Commented out XptLoadClassProc
--- Andreas Schoter, Jul 15 1991
    Added global constant XmVirtKeys for compatibility with uses
 */
