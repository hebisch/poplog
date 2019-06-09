/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 * File:            C.x/x/Xpw/XpwScrText.h
 * Version:         Revision 5.0
 * Purpose:         Public header file for the widget.
 * Author:          Jonathan Meyer (see revisions)
 * Date:            11 February 1989
 * Notes:
 *  this is used for all versions of the widget.
 */


#ifndef _XpwScrollText_h
#define _XpwScrollText_h

/***********************************************************************
 *
 * Poplog Widget - XpwScrollTextWidget
 *
 ***********************************************************************/

#include "XpwCore.h"

/* Parameters:

 Name            Class         RepType      Default Value
 ----            -----         -------      -------------
 hMargin         Margin        Int          2
 vMargin         Margin        Int          2
 fontWidth       Width         Dimension    *
 fontHeight      Height        Dimension    *
 cursorRow       Position      int          0
 cursorColumn    Position      int          0
 numRows         Position      int          0
 numColumns      Position      int          0
*/

/* Resource names used for the poplog graphics widget */
#define XtCScrollText "ScrollText"
#define XtNvisible "visible"
#define XtNvMargin "vMargin"
#define XtNhMargin "hMargin"
#define XtNfontWidth "fontWidth"
#define XtNfontAverageWidth "fontAverageWidth"
#define XtNfontHeight "fontHeight"
#define XtNspaceWidth "spaceWidth"
#define XtNcursorRow "cursorRow"
#define XtNcursorColumn "cursorColumn"
#define XtNnumRows "numRows"
#define XtNnumColumns "numColumns"
#define XtNautoGeometry "autoGeometry"
#define XtNmouseRow "mouseRow"
#define XtNmouseColumn "mouseColumn"
#define XtNmouseX "mouseX"
#define XtNmouseY "mouseY"
#define XtNbuttonEvent "buttonEvent"
#define XtNkeyboardEvent "keyboardEvent"
#define XtNkey "key"
#define XtNmotionEvent "motionEvent"
#define XtNcursorStatus "cursorStatus"
#define XtNcursorChar "cursorChar"
#define XtNhighlightOn "highlightOn"
#define XtNunderlineOn "underlineOn"
#define XtNboldOn "boldOn"
#define XtNaltFontOn "altFontOn"
#define XtNblinkOn "blinkOn"
#define XtNactiveOn "activeOn"
#define XtNcolorNumber "colorNumber"
#define XtNdefaultColorMask "defaultColorMask"
#define XtNselectionColorNum "selectionColorNum"
#define XtNnoGrayScale "noGrayScale"
#define XtNstatusStyle "statusStyle"
#define XtNdrawShadowMask "drawShadowMask"
#define XtNinputCharMode "inputCharMode"
#define XtNnumFixedColumns "numFixedColumns"
#define XtNnumStatusFixedColumns "numStatusFixedColumns"
#define XtNvarWidthMode "varWidthMode"
#define XtNnoDrawMode "noDrawMode"

#define XtNboldFont "boldFont"
#define XtCBoldFont "BoldFont"
#define XtNaltFont "altFont"
#define XtCAltFont "AltFont"
#define XtNboldAltFont "boldAltFont"
#define XtCBoldAltFont "BoldAltFont"

#define XtNboldFontSet "boldFontSet"
#define XtCBoldFontSet "BoldFontSet"
#define XtNaltFontSet "altFontSet"
#define XtCAltFontSet "AltFontSet"
#define XtNboldAltFontSet "boldAltFontSet"
#define XtCBoldAltFontSet "BoldAltFontSet"

#define XtNstatusForeground "statusForeground"
#define XtNstatusBackground "statusBackground"

#define XtNhighlightForeground "highlightForeground"
#define XtNhighlightBackground "highlightBackground"
#define XtNcolor2Foreground "color2Foreground"
#define XtNcolor2Background "color2Background"
#define XtNcolor3Foreground "color3Foreground"
#define XtNcolor3Background "color3Background"
#define XtNcolor4Foreground "color4Foreground"
#define XtNcolor4Background "color4Background"
#define XtNcolor5Foreground "color5Foreground"
#define XtNcolor5Background "color5Background"
#define XtNcolor6Foreground "color6Foreground"
#define XtNcolor6Background "color6Background"
#define XtNcolor7Foreground "color7Foreground"
#define XtNcolor7Background "color7Background"

#define XtNcolor0AForeground "color0AForeground"
#define XtNcolor0ABackground "color0ABackground"
#define XtNcolor1AForeground "color1AForeground"
#define XtNcolor1ABackground "color1ABackground"
#define XtNcolor2AForeground "color2AForeground"
#define XtNcolor2ABackground "color2ABackground"
#define XtNcolor3AForeground "color3AForeground"
#define XtNcolor3ABackground "color3ABackground"
#define XtNcolor4AForeground "color4AForeground"
#define XtNcolor4ABackground "color4ABackground"
#define XtNcolor5AForeground "color5AForeground"
#define XtNcolor5ABackground "color5ABackground"
#define XtNcolor6AForeground "color6AForeground"
#define XtNcolor6ABackground "color6ABackground"
#define XtNcolor7AForeground "color7AForeground"
#define XtNcolor7ABackground "color7ABackground"

#define XtNcursorColor "cursorColor"

#define XtNsynthetic "synthetic"
#define XtNnoBlink "noBlink"
#define XtNscrollbarOn "scrollbarOn"
#define XtNhscrollbarOn "hscrollbarOn"
#define XtCScrollbarOn "ScrollbarOn"
#define XtNmenubarOn "menubarOn"
#define XtCMenubarOn "MenubarOn"
#define XtNmenubarForeground "menubarForeground"
#define XtNmenubarBackground "menubarBackground"
#define XtNscrollbarForeground "scrollbarForeground"
#define XtNscrollbarBackground "scrollbarBackground"
#define XtNfocusChange "focusChange"
#define XtNactiveChange "activeChange"
#define XtNroundSize "roundSize"
#define XtNpointer2Shape "pointer2Shape"

#ifndef XtNgeometry
#define XtNgeometry "geometry"
#define XtCGeometry "Geometry"
#define XtNdrawGraphicChars "drawGraphicChars"
#endif

typedef struct _XpwScrollTextRec *XpwScrollTextWidget;
/* completely defined in .h */
typedef struct _XpwScrollTextClassRec *XpwScrollTextWidgetClass;
/* completely defined in .h */

externalref WidgetClass xpwScrollTextWidgetClass;

/* attribute flags for XpwMSetCharAttributes */
/* (Poplog note: keep these in step with the VEDCMODE_ flags
 *  in vedscreendefs.ph )
 */
#define XpwFcolorShift   16
#define XpwFcolorNumber  (7<<16)    /* i.e. 16-18 */
#define XpwFunderlineOn  (1<<19)
#define XpwFboldOn       (1<<20)
#define XpwFaltFontOn    (1<<21)
#define XpwFblinkOn      (1<<22)
#define XpwFactiveOn     (1<<23)

/*  XtN inputCharMode -- mode for input character strings
 */
#define XpwICMChar8      0    /* default */
#define XpwICMISOLatin1  1
#define XpwICMUnicode    2
#define XpwICMWideChar   3

#endif


/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 15 1997
    Changes to support variable-width chars
--- John Gibson, Apr 29 1997
    Changes to support drawing Unicode & wide-character strings.
--- John Gibson, Apr 16 1996
    Added XtNautoGeometry
--- John Gibson, Oct  6 1995
    Added new resource names
--- John Gibson, Sep  8 1995
    Added XpwFactiveOn
--- John Gibson, Jan  9 1994
    Added some new resource names, got rid of some unnecessary class
    names.
--- John Gibson, Jun 23 1992
    Added XtNnoGrayScale
--- John Gibson, Mar 15 1992
    Attribute bits now start at bit 16 instead of 8
--- John Gibson, Jan 13 1992
    Added altFont and XpwF- flags
--- John Gibson, Dec 23 1991
    Added XtNdrawGraphicChars
--- John Gibson, Dec 18 1991
    Added XtNboldFont, boldOn, underlineOn etc
--- John Gibson, Nov  4 1991
    Added XtNgeometry etc
--- John Gibson, Oct 31 1991
    extern -> externalref for xpwScrollTextWidgetClass
--- Jonathan Meyer, Sep 12 1991
    Added menubarForeground/Background
--- John Gibson, Aug  9 1991
    Removed buttonClickEvent
--- Jonathan Meyer, Aug  2 1991 Added scrollbarForeground, scrollbarBackground
--- John Gibson, Jul 27 1991
    Removed exposed lines declarations
--- Jonathan Meyer, Jul  8 1991
    Added scrollbarOn and menubarOn
--- Jonathan Meyer, Jun  4 1991 Added synthetic
--- Jonathan Meyer, Aug 21 1990
    Added XpwNotInstalled test.
--- Ian Rogers, Jul 20 1990
    Changed popScrollTextWidgetClass to xpwScrollTextWidgetClass
--- Andreas Schoter 16 July 1990
    Replaced all occurances of Pop* variable names with Xpw*
*/
