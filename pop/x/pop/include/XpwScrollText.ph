/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.x/x/pop/include/XpwScrollText.ph
 > Purpose:         Provides macros required for lib XpwScrollText
 > Author:          Jon Meyer Jan 1990, Ian Rogers Apr 1990 (see revisions)
 > Documentation:   HELP * XpwScrollText, REF * XpwScrollText
 > Related Files:   LIB * XpwScrollTextMethods, * XpwScrollText
 */

#_TERMIN_IF DEF XPWSCROLLTEXT_INCLUDED

section;

iconstant macro (

        ;;; Methods
        XpwMScroll              = 600,
        XpwMScrollScreenUp      = 601,
        XpwMScrollScreenDown    = 602,
        XpwMScrollScreenLeft    = 603,
        XpwMScrollScreenRight   = 604,
        XpwMScrollLines         = 605,
        XpwMScrollTail          = 606,
        XpwMScrollTails         = 607,
        XpwMScrollTailLeft      = 608,
        XpwMScrollTailRight     = 609,
        XpwMInsertLineAtCursor  = 620,
        XpwMInsertCharAtCursor  = 621,
        XpwMDeleteLineAtCursor  = 622,
        XpwMDeleteCharAtCursor  = 623,
        XpwMDeleteCharsAtCursor = 624,
        XpwMClear               = 630,
        XpwMClearScreen         = 631,
        XpwMClearLine           = 632,
        XpwMClearLines          = 633,
        XpwMClearTail           = 634,
        XpwMClearTails          = 635,
        XpwMClearChar           = 636,
        XpwMClearLineAtCursor   = 637,
        XpwMClearTailAtCursor   = 638,
        XpwMClearCharAtCursor   = 639,
        XpwMInsert              = 650,
        XpwMInsertAtCursor      = 651,
        XpwMWrite               = 652,
        XpwMWriteLine           = 653,
        XpwMWriteLines          = 654,
        XpwMWriteSubstr         = 655,
        XpwMWriteAtCursor       = 656,
        XpwMWriteTrailSpacesAtCursor = 657,
        XpwMCursorTo            = 660,
        XpwMCursorUp            = 661,
        XpwMCursorDown          = 662,
        XpwMCursorLeft          = 663,
        XpwMCursorRight         = 664,
        XpwMBell                = 670,
        XpwMSetWMProtocols      = 671,
        XpwMSetCharAttributes   = 672,
        XpwMGetCharAttributes   = 673,
        XpwMSetTextCursor       = 674,
        XpwMGetTextCursor       = 675,
        XpwMEndTextWidthMode    = 690,
        XpwMBeginTextWidthMode1 = 691,
        XpwMBeginTextWidthMode2 = 692,
        XpwMBeginTextWidthMode3 = 693,
        XpwMSetVarColumnOffset  = 695,
        XpwMGetVarColumnOffset  = 696,
        XpwMGetVarColumnLimOffset = 697,
        XpwMGetVarRowWidth      = 698,

        MAX_INT                 = 1<<28,

        ;;; Character attribute flags for XpwMSet/GetCharAttributes
        ;;; and XpwMSet/GetTextCursor.
        ;;; N.B. XVed assumes these are the same as the VEDCMODE_ flags
        ;;; defined in vedscreendefs.ph, so keep them in step.
        XpwFcolorShift          = 16,
        XpwFcolorNumber         = 2:111e16,     ;;; 16-18
        XpwFunderlineOn         = 2:1e19,
        XpwFboldOn              = 2:1e20,
        XpwFaltFontOn           = 2:1e21,
        XpwFblinkOn             = 2:1e22,
        XpwFactiveOn            = 2:1e23,

        ;;; XtN inputCharMode -- mode for input character strings
        XpwICMChar8             = 0,            ;;; default
        XpwICMISOLatin1         = 1,
        XpwICMUnicode           = 2,
        XpwICMWideChar          = 3,
);

iconstant XPWSCROLLTEXT_INCLUDED = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 15 1997
        Added new methods
--- John Gibson, Jun 13 1997
        Added XpwMDeleteCharsAtCursor
--- John Gibson, Apr 28 1997
        Added XpwICM_ values.
--- John Gibson, Sep  8 1995
        Added XpwFactiveOn
--- John Gibson, Jan 11 1994
        Added XpwMSet/GetTextCursor
--- John Williams, Sep 30 1992
        Fixed missing ( in #_IF at top of file
--- John Gibson, Mar 15 1992
        Attribute bits now start at 16 instead of 8
--- John Gibson, Feb  2 1992
        Added new XpwM- values and XpwF- flags.
--- John Gibson, Sep 14 1991
        Removed XpwMMClearWindow (widget doesn't respond to it)
--- Jonathan Meyer, Jan 29 1991
        Removed redundant XtN* delcarations.
        Reinstated INCLUDE_constant
--- Roger Evans, Oct 11 1990 moved to include directory
--- Andreas Schoter, July 16 1990
    Renamed to XpwScrolltext.ph and changed all variable names from Pop* to
    Xpw*
--- James Goodlet, Jun  6 1990 - split this part off from PopScrollText.p,
        and changed "lconstant macro" to "lvars macros" to allow reinclusion.
 */
