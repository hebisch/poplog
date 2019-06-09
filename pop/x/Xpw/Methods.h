/* --- Copyright University of Sussex 2003. All rights reserved. ----------
 * File:        C.x/x/Xpw/Methods.h
 * Version:         Revision 5.0
 * Purpose:         Public header file for programs that use Xpw methods
 * Author:      Jonathan Meyer (see revisions)
 * Date:        15 January 1990
 * Notes:
 */

#ifndef _XpwMethod_h
#define _XpwMethod_h

#include <X11/Intrinsic.h>

/* procedure used to call methods */

typedef XtArgVal XpwMethodRet;
/* Removed by A.S. Suggested by Andreas Eder */
/* extern  XpwMethodRet XpwCallMethod(); */

#define XtCXpwMethodError "XpwMethodError"

/***********************************************************************
 *
 * Poplog Widget Set Method ID's
 *
 ***********************************************************************/

/*
 * XpwCore Methods
 */

#define XpwMSetFont 100
#define XpwMSetCursor 101
#define XpwMFreeFont 102
#define XpwMFreeCursor 103
#define XpwMSetColor 104
#define XpwMFreeColor 105
#define XpwMLoadPixmap 106
#define XpwMChangeUsersGC 107
/*
 * XpwPixmap/XpwGraphic  methods
 */

#define XpwMDraw 200
#define XpwMDrawFilled 201
#define XpwMDrawPoint 202
#define XpwMDrawLine 203
#define XpwMDrawArc 204
#define XpwMDrawRectangle 205
#define XpwMDrawString 206
#define XpwMDrawImageString 207
#define XpwMClearArea 208
#define XpwMClearWindow 209
#define XpwMDrawPoints 210
#define XpwMDrawLines 211
#define XpwMDrawArcs 212
#define XpwMDrawRectangles 213
#define XpwMDrawSegments 214
#define XpwMFillArc 215
#define XpwMFillArcs 216
#define XpwMFillPolygon 217
#define XpwMFillRectangle 218
#define XpwMFillRectangles 219
#define XpwMCopyFrom 220
#define XpwMCopyTo 221
#define XpwMPutImage 222
#define XpwMGetImage 223
#define XpwMCreatePutImage 224
#define XpwMDrawImage XpwMCreatePutImage
#define XpwMCreateImage 226
#define XpwMDrawRoundedRectangle 228
#define XpwMFillRoundedRectangle 229

/*
 * XpwGraphic only methods
 */

#define XpwMAllocColorRange 400
#define XpwMFreeColorRange 406
#define XpwMAllocStoreColor 401
#define XpwMSetPixelColor 402
#define XpwMCreateColormap 403
#define XpwMFreeColormap 404

#define XpwMAllocReadWriteColor 405
#define XpwMAllocReadOnlyColor 406
#define XpwMQueryPixelColor 407

/*
 * XpwScrollText methods
 */

#define XpwMScroll 600
#define XpwMScrollScreenUp 601
#define XpwMScrollScreenDown 602
#define XpwMScrollScreenLeft 603
#define XpwMScrollScreenRight 604
#define XpwMScrollLines 605
#define XpwMScrollTail 606
#define XpwMScrollTails 607
#define XpwMScrollTailLeft 608
#define XpwMScrollTailRight 609
#define XpwMInsertLineAtCursor 620
#define XpwMInsertCharAtCursor 621
#define XpwMDeleteLineAtCursor 622
#define XpwMDeleteCharAtCursor 623
#define XpwMDeleteCharsAtCursor 624
#define XpwMClear 630
#define XpwMClearScreen 631
#define XpwMClearLine 632
#define XpwMClearLines 633
#define XpwMClearTail 634
#define XpwMClearTails 635
#define XpwMClearChar 636
#define XpwMClearLineAtCursor 637
#define XpwMClearTailAtCursor 638
#define XpwMClearCharAtCursor 639
#define XpwMInsert 650
#define XpwMInsertAtCursor 651
#define XpwMWrite 652
#define XpwMWriteLine 653
#define XpwMWriteLines 654
#define XpwMWriteSubstr 655
#define XpwMWriteAtCursor 656
#define XpwMWriteTrailSpacesAtCursor 657
#define XpwMCursorTo 660
#define XpwMCursorUp 661
#define XpwMCursorDown 662
#define XpwMCursorLeft 663
#define XpwMCursorRight 664
#define XpwMBell 670
#define XpwMSetWMProtocols 671
#define XpwMSetCharAttributes 672
#define XpwMGetCharAttributes 673
#define XpwMSetTextCursor     674
#define XpwMGetTextCursor     675
#define XpwMSetTextSelection 680
#define XpwMGetTextSelection 681

#define XpwMEndTextWidthMode    690
#define XpwMBeginTextWidthMode1 691
#define XpwMBeginTextWidthMode2 692
#define XpwMBeginTextWidthMode3 693

#define XpwMSetVarColumnOffset  695
#define XpwMGetVarColumnOffset  696
#define XpwMGetVarColumnLimOffset 697
#define XpwMGetVarRowWidth      698
#endif

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, 8 Nov 2003
    Converted from varargs to stdargs, as suggested by Andreas Eder
--- John Gibson, Aug 15 1997
    Added new XpwScrollText methods
--- John Gibson, Jun 13 1997
    Added XpwMDeleteCharsAtCursor
--- John Gibson, Jan 11 1994
    Added XpwMSet/GetTextCursor
--- John Gibson, Jan 16 1992
    Added XpwMSetCharAttributes
--- Jonathan Meyer, Dec 17 1991 Removed V*maps
--- Jonathan Meyer, Aug 21 1990
    Added "X11/" prefix again. Renamed Methods.h
--- Andreas Schoter, 16 July 1990
    Replaced all occurances of Pop* variable names with Xpw*
--- Simon Nichols, Jun 19 1990
        Removed "X11/" prefix from #include file names.
 */
