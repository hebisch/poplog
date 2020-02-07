/* --- Copyright University of Sussex 1999. All rights reserved. ----------
 * File:            C.x/x/Xpw/XpwScrText.c
 * Version:         Revision 5.0
 * Purpose:         C Source for the XpwScrollText widget.
 * Author:          Jonathan Meyer (see revisions)
 * Date:            11 February 1981
 * Documentation:   REF *XpwScrollText, HELP XpwScrollText
*/

/* using Xpw header files locally, not in <X11/Xpw/...> */
#define XpwNotInstalled

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include "XpwScrTextP.h"
#include <ctype.h>
#include <stdio.h>

/****************************************************************
 *
 * Declarations
 *
 ****************************************************************/

/* private procedures */

static void ScrollTextClassInit(), ScrollTextInitialize(),
            ScrollTextRealize(), ScrollTextResize(),
            ScrollTextRedisplay(), ScrollTextSetValuesAlmost(),
            ScrollTextDestroy();
static Boolean ScrollTextSetValues();
static XtGeometryResult QueryGeometry();

extern void
    _XpwTextExpose(),
    _XpwSetBlinkTimer(),
    _XpwTextDestroy(),
    _XpwRecolorPointer(),
    _XpwTextTestActiveChange();

extern int _XpwYCoordToRow();
extern int _XpwXCoordToCol();

extern XpwMethodRet _XpwTextMoveCursorTo();
extern XFontStruct * _XpwFont8OfFontSet();
extern XFontSet _XpwFontSetFromFont();

/* Action procedures */
static void NotifyButtonEvent();
static void NotifyMotionEvent();
static void NotifyKeyboardEvent();
static void BellAction();
static void EventHandler();

/* defined in Text.c */
externalref XpwMethod _xpwScrollTextMethods[];
externalref int _num_xpwScrollTextMethods;

#define NO_PIXEL  0xffffffff
#define IS_NO_PIXEL(pix) (((pix) & NO_PIXEL) == NO_PIXEL)


/****************************************************************
 *
 * Resources
 *
 ****************************************************************/

/* Resource declaration and Initialization of defaults */

static char XtNkey_string[32];

#define offset(field) XtOffset(XpwScrollTextWidget,xpwscrolltext.field)
#define goffset(field) XtOffset(Widget,core.field)

static XtResource resources[] = {
    {XtNhMargin, XtCMargin, XtRShort, sizeof(short),
        offset(h_margin), XtRImmediate, (XtPointer) -50},
    {XtNvMargin, XtCMargin, XtRShort, sizeof(short),
        offset(v_margin), XtRImmediate, (XtPointer) -30},
    {XtNwidth, XtCWidth, XtRDimension, sizeof(Dimension),
        goffset(width), XtRImmediate, 0},
    {XtNheight, XtCHeight, XtRDimension, sizeof(Dimension),
        goffset(height), XtRImmediate, 0},
    {XtNfont, XtCFont, XtRFontStruct, sizeof(XFontStruct *),
        XtOffset(XpwCoreWidget, xpwcore.font), XtRString, "fixed"},

    {XtNboldFont, XtCBoldFont, XtRFontStruct, sizeof(XFontStruct *),
        offset(alt_fonts[0]), XtRImmediate, (XtPointer) NULL},
    {XtNaltFont, XtCAltFont, XtRFontStruct, sizeof(XFontStruct *),
        offset(alt_fonts[1]), XtRImmediate, (XtPointer) NULL},
    {XtNboldAltFont, XtCBoldAltFont, XtRFontStruct, sizeof(XFontStruct *),
        offset(alt_fonts[2]), XtRImmediate, (XtPointer) NULL},

    {XtNboldFontSet, XtCBoldFontSet, XtRFontSet, sizeof(XFontSet),
        offset(alt_font_sets[0]), XtRImmediate, (XtPointer) NULL},
    {XtNaltFontSet, XtCAltFontSet, XtRFontSet, sizeof(XFontSet),
        offset(alt_font_sets[1]), XtRImmediate, (XtPointer) NULL},
    {XtNboldAltFontSet, XtCBoldAltFontSet, XtRFontSet, sizeof(XFontSet),
        offset(alt_font_sets[2]), XtRImmediate, (XtPointer) NULL},

    {XtNfontWidth, XtCWidth, XtRInt, sizeof(int),
        offset(font_width), XtRImmediate, 0},
    {XtNfontAverageWidth, XtCParameter, XtRInt, sizeof(int),
        offset(font_average_width), XtRImmediate, 0},
    {XtNfontHeight, XtCWidth, XtRInt, sizeof(int),
        offset(font_height), XtRImmediate, 0},
    {XtNspaceWidth, XtCWidth, XtRInt, sizeof(int),
        offset(space_width), XtRImmediate, 0},
    {XtNcursorRow, XtCPosition, XtRInt, sizeof(int),
        offset(cursor_row), XtRImmediate, 0},
    {XtNcursorColumn, XtCPosition, XtRInt, sizeof(int),
        offset(cursor_column), XtRImmediate, 0},
    {XtNcursorStatus, XtCParameter, XtRBoolean, sizeof(Boolean),
        offset(cursor_status), XtRImmediate,(XtPointer)1},
    {XtNcursorChar, XtCParameter, XtRInt, sizeof(int),
        offset(cursor_char), XtRImmediate, (XtPointer)'O'},
    {XtNsynthetic, XtCParameter, XtRBoolean, sizeof(Boolean),
        offset(synthetic), XtRImmediate,(XtPointer)0},
    {XtNnumRows, XtCWidth, XtRInt, sizeof(int),
        offset(num_rows), XtRImmediate, (XtPointer)0},
    {XtNnumColumns, XtCWidth, XtRInt, sizeof(int),
        offset(num_columns), XtRImmediate, (XtPointer)0},
    {XtNgeometry, XtCGeometry, XtRString, sizeof(String),
        offset(geometry), XtRString, (XtPointer)NULL},
    {XtNbuttonEvent, XtCCallback, XtRCallback, sizeof(XtPointer),
        offset(button_event), XtRCallback, NULL},
    {XtNmotionEvent, XtCCallback, XtRCallback, sizeof(XtPointer),
        offset(motion_event), XtRCallback, NULL},
    {XtNkeyboardEvent, XtCCallback, XtRCallback, sizeof(XtPointer),
        offset(keyboard_event), XtRCallback, NULL},
    {XtNmouseX, XtCPosition, XtRInt, sizeof(int),
        offset(mouse.x), XtRImmediate, 0},
    {XtNmouseY, XtCPosition, XtRInt, sizeof(int),
        offset(mouse.y), XtRImmediate, 0},
    {XtNmouseRow, XtCPosition, XtRInt, sizeof(int),
        offset(mouse.row), XtRImmediate, 0},
    {XtNmouseColumn, XtCPosition, XtRInt, sizeof(int),
        offset(mouse.column), XtRImmediate, 0},
    {XtNkey, XtCParameter, XtRString, sizeof(String),
        offset(key), XtRString, XtNkey_string},
    {XtNvisible, XtCParameter, XtRBoolean, sizeof(Boolean),
        offset(is_fully_visible), XtRImmediate, 0},
    {XtNstatusForeground, XtCForeground, XtRPixel, sizeof(Pixel),
        offset(status_foreground), XtRString, XtDefaultForeground},
    {XtNstatusBackground, XtCBackground, XtRPixel, sizeof(Pixel),
        offset(status_background), XtRString, XtDefaultBackground},

    {XtNhighlightForeground, XtCParameter, XtRPixel, sizeof(Pixel),
        offset(alt_colors[0].fg), XtRImmediate, (XtPointer) NO_PIXEL},
    {XtNhighlightBackground, XtCParameter, XtRPixel, sizeof(Pixel),
        offset(alt_colors[0].bg), XtRImmediate, (XtPointer) NO_PIXEL},
    {XtNcolor2Foreground, XtCParameter, XtRPixel, sizeof(Pixel),
        offset(alt_colors[1].fg), XtRImmediate, (XtPointer) NO_PIXEL},
    {XtNcolor2Background, XtCParameter, XtRPixel, sizeof(Pixel),
        offset(alt_colors[1].bg), XtRImmediate, (XtPointer) NO_PIXEL},
    {XtNcolor3Foreground, XtCParameter, XtRPixel, sizeof(Pixel),
        offset(alt_colors[2].fg), XtRImmediate, (XtPointer) NO_PIXEL},
    {XtNcolor3Background, XtCParameter, XtRPixel, sizeof(Pixel),
        offset(alt_colors[2].bg), XtRImmediate, (XtPointer) NO_PIXEL},
    {XtNcolor4Foreground, XtCParameter, XtRPixel, sizeof(Pixel),
        offset(alt_colors[3].fg), XtRImmediate, (XtPointer) NO_PIXEL},
    {XtNcolor4Background, XtCParameter, XtRPixel, sizeof(Pixel),
        offset(alt_colors[3].bg), XtRImmediate, (XtPointer) NO_PIXEL},
    {XtNcolor5Foreground, XtCParameter, XtRPixel, sizeof(Pixel),
        offset(alt_colors[4].fg), XtRImmediate, (XtPointer) NO_PIXEL},
    {XtNcolor5Background, XtCParameter, XtRPixel, sizeof(Pixel),
        offset(alt_colors[4].bg), XtRImmediate, (XtPointer) NO_PIXEL},
    {XtNcolor6Foreground, XtCParameter, XtRPixel, sizeof(Pixel),
        offset(alt_colors[5].fg), XtRImmediate, (XtPointer) NO_PIXEL},
    {XtNcolor6Background, XtCParameter, XtRPixel, sizeof(Pixel),
        offset(alt_colors[5].bg), XtRImmediate, (XtPointer) NO_PIXEL},
    {XtNcolor7Foreground, XtCParameter, XtRPixel, sizeof(Pixel),
        offset(alt_colors[6].fg), XtRImmediate, (XtPointer) NO_PIXEL},
    {XtNcolor7Background, XtCParameter, XtRPixel, sizeof(Pixel),
        offset(alt_colors[6].bg), XtRImmediate, (XtPointer) NO_PIXEL},

    {XtNcolor0AForeground, XtCParameter, XtRPixel, sizeof(Pixel),
        offset(alt_colors[7].fg), XtRImmediate, (XtPointer) NO_PIXEL},
    {XtNcolor0ABackground, XtCParameter, XtRPixel, sizeof(Pixel),
        offset(alt_colors[7].bg), XtRImmediate, (XtPointer) NO_PIXEL},
    {XtNcolor1AForeground, XtCParameter, XtRPixel, sizeof(Pixel),
        offset(alt_colors[8].fg), XtRImmediate, (XtPointer) NO_PIXEL},
    {XtNcolor1ABackground, XtCParameter, XtRPixel, sizeof(Pixel),
        offset(alt_colors[8].bg), XtRImmediate, (XtPointer) NO_PIXEL},
    {XtNcolor2AForeground, XtCParameter, XtRPixel, sizeof(Pixel),
        offset(alt_colors[9].fg), XtRImmediate, (XtPointer) NO_PIXEL},
    {XtNcolor2ABackground, XtCParameter, XtRPixel, sizeof(Pixel),
        offset(alt_colors[9].bg), XtRImmediate, (XtPointer) NO_PIXEL},
    {XtNcolor3AForeground, XtCParameter, XtRPixel, sizeof(Pixel),
        offset(alt_colors[10].fg), XtRImmediate, (XtPointer) NO_PIXEL},
    {XtNcolor3ABackground, XtCParameter, XtRPixel, sizeof(Pixel),
        offset(alt_colors[10].bg), XtRImmediate, (XtPointer) NO_PIXEL},
    {XtNcolor4AForeground, XtCParameter, XtRPixel, sizeof(Pixel),
        offset(alt_colors[11].fg), XtRImmediate, (XtPointer) NO_PIXEL},
    {XtNcolor4ABackground, XtCParameter, XtRPixel, sizeof(Pixel),
        offset(alt_colors[11].bg), XtRImmediate, (XtPointer) NO_PIXEL},
    {XtNcolor5AForeground, XtCParameter, XtRPixel, sizeof(Pixel),
        offset(alt_colors[12].fg), XtRImmediate, (XtPointer) NO_PIXEL},
    {XtNcolor5ABackground, XtCParameter, XtRPixel, sizeof(Pixel),
        offset(alt_colors[12].bg), XtRImmediate, (XtPointer) NO_PIXEL},
    {XtNcolor6AForeground, XtCParameter, XtRPixel, sizeof(Pixel),
        offset(alt_colors[13].fg), XtRImmediate, (XtPointer) NO_PIXEL},
    {XtNcolor6ABackground, XtCParameter, XtRPixel, sizeof(Pixel),
        offset(alt_colors[13].bg), XtRImmediate, (XtPointer) NO_PIXEL},
    {XtNcolor7AForeground, XtCParameter, XtRPixel, sizeof(Pixel),
        offset(alt_colors[14].fg), XtRImmediate, (XtPointer) NO_PIXEL},
    {XtNcolor7ABackground, XtCParameter, XtRPixel, sizeof(Pixel),
        offset(alt_colors[14].bg), XtRImmediate, (XtPointer) NO_PIXEL},

    {XtNcursorColor, XtCParameter, XtRPixel, sizeof(Pixel),
        offset(cursor_color), XtRImmediate, (XtPointer) NO_PIXEL},

    {XtNcolorNumber, XtCParameter, XtRShort, sizeof(short),
        offset(curr_attr_resources.colornum), XtRImmediate, 0},
    {XtNdefaultColorMask, XtCParameter, XtRInt, sizeof(int),
        offset(default_color_mask), XtRImmediate, (XtPointer) 2},
    {XtNhighlightOn, XtCParameter, XtRBoolean, sizeof(Boolean),
        offset(highlight_on), XtRImmediate, False},
    {XtNunderlineOn, XtCParameter, XtRBoolean, sizeof(Boolean),
        offset(curr_attr_resources.underline), XtRImmediate, False},
    {XtNboldOn, XtCParameter, XtRBoolean, sizeof(Boolean),
        offset(curr_attr_resources.bold), XtRImmediate, False},
    {XtNaltFontOn, XtCParameter, XtRBoolean, sizeof(Boolean),
        offset(curr_attr_resources.alt_font), XtRImmediate, False},
    {XtNblinkOn, XtCParameter, XtRBoolean, sizeof(Boolean),
        offset(curr_attr_resources.blink), XtRImmediate, False},
    {XtNactiveOn, XtCParameter, XtRBoolean, sizeof(Boolean),
        offset(curr_attr_resources.active), XtRImmediate, False},
    {XtNfocusChange, XtCCallback, XtRCallback, sizeof(XtPointer),
        offset(focus_change), XtRCallback, NULL},
    {XtNactiveChange, XtCCallback, XtRCallback, sizeof(XtPointer),
        offset(active_change), XtRCallback, NULL},
    {XtNroundSize, XtCParameter, XtRBoolean, sizeof(Boolean),
        offset(round_size), XtRImmediate, (XtPointer)1},
    {XtNnoGrayScale, XtCParameter, XtRBoolean, sizeof(Boolean),
        offset(no_gray_scale), XtRImmediate, (XtPointer)0},
    {XtNdrawGraphicChars, XtCParameter, XtRBoolean, sizeof(Boolean),
        offset(draw_graphic_chars), XtRImmediate, 0},
    {XtNnoBlink, XtCParameter, XtRBoolean, sizeof(Boolean),
        offset(no_blink), XtRImmediate, False},
    {XtNstatusStyle, XtCParameter, XtRShort, sizeof(short),
        offset(status_style), XtRImmediate, 0},
    {XtNinputCharMode, XtCParameter, XtRShort, sizeof(short),
        offset(input_char_mode), XtRImmediate, XpwICMChar8},
    {XtNdrawShadowMask, XtCParameter, XtRInt, sizeof(int),
        offset(draw_shadow_mask), XtRImmediate, 0},
    {XtNpointer2Shape, XtCCursor, XtRCursor, sizeof(Cursor),
        offset(pointer2_shape), XtRString, "hand2"},
    {XtNvarWidthMode, XtCParameter, XtRBoolean, sizeof(Boolean),
        offset(var_width_mode), XtRImmediate, (XtPointer)False},
    {XtNnumFixedColumns, XtCParameter, XtRShort, sizeof(short),
        offset(num_fixed_columns), XtRImmediate, 0},
    {XtNnumStatusFixedColumns, XtCParameter, XtRShort, sizeof(short),
        offset(num_status_fixed_columns), XtRImmediate, 0},

    /* resources not used by the widget */
    {XtNmenubarOn, XtCMenubarOn, XtRBoolean, sizeof(Boolean),
        offset(menubar_on), XtRImmediate, (XtPointer)1},
    {XtNscrollbarOn, XtCScrollbarOn, XtRBoolean, sizeof(Boolean),
        offset(scrollbar_on), XtRImmediate, (XtPointer)1},
    {XtNhscrollbarOn, XtCScrollbarOn, XtRBoolean, sizeof(Boolean),
        offset(hscrollbar_on), XtRImmediate, (XtPointer)0},
    {XtNautoGeometry, XtCParameter, XtRString, sizeof(String),
        offset(auto_geometry), XtRString, (XtPointer)NULL},
    {XtNscrollbarForeground, XtCParameter, XtRPixel, sizeof(Pixel),
        offset(scrollbar_foreground), XtRImmediate, (XtPointer) NO_PIXEL},
    {XtNscrollbarBackground, XtCParameter, XtRPixel, sizeof(Pixel),
        offset(scrollbar_background), XtRImmediate, (XtPointer) NO_PIXEL},
    {XtNmenubarForeground, XtCParameter, XtRPixel, sizeof(Pixel),
        offset(menubar_foreground), XtRImmediate, (XtPointer) NO_PIXEL},
    {XtNmenubarBackground, XtCParameter, XtRPixel, sizeof(Pixel),
        offset(menubar_background), XtRImmediate, (XtPointer) NO_PIXEL},
    {XtNselectionColorNum, XtCParameter, XtRShort, sizeof(short),
        offset(selection_color_num), XtRImmediate, (XtPointer)1},
};

#undef offset
#undef goffset


/****************************************************************
 *
 * Actions
 *
 ****************************************************************/


static XtActionsRec actions[] = {
   {"string", NotifyKeyboardEvent},
   {"notify-key-event", NotifyKeyboardEvent},
   {"notify-button-event", NotifyButtonEvent},
   {"notify-motion-event", NotifyMotionEvent},
   {"bell", BellAction},
   {NULL,NULL}
};

/****************************************************************
 *
 * Translations
 *
 ****************************************************************/

static char defaultTranslations[] = "\
    <Motion>: notify-motion-event() \n\
    <BtnDown>: notify-button-event() \n\
    <BtnUp>:    notify-button-event() \n\
    <KeyDown>: notify-key-event() \n\
    <KeyUp>:    notify-key-event() \n\
";


/* Define the top structure */

externaldef(xpwscrolltextclassrec)
    XpwScrollTextClassRec xpwScrollTextClassRec = {
    { /* core fields */
    /* superclass       */  (WidgetClass) &xpwCoreClassRec,
    /* class_name       */  "XpwScrollText",
    /* widget_size      */  sizeof(XpwScrollTextRec),
    /* class_initialize */      ScrollTextClassInit,
    /* class_part_initialize*/      NULL,
    /* class_inited     */  False,
    /* initialize       */  ScrollTextInitialize,
    /* initialize_hook  */      NULL,
    /* realize          */  ScrollTextRealize,
    /* actions          */  actions,
    /* num_actions      */  XtNumber(actions),
    /* resources        */  resources,
    /* resource_count   */  XtNumber(resources),
    /* xrm_class        */  NULLQUARK,
    /* compress_motion  */      True,
    /* compress_exposure*/      XtExposeCompressMultiple, /*False,*/
    /* compress_enterleave*/        False,
    /* visible_interest */      True,
    /* destroy          */  ScrollTextDestroy,
    /* resize           */  ScrollTextResize,
    /* expose           */  ScrollTextRedisplay,
    /* set_values       */  ScrollTextSetValues,
    /* set_values_hook  */  NULL,
    /* set_values_almost*/  ScrollTextSetValuesAlmost,
    /* get_values_hook  */  NULL,
    /* accept_focus     */  NULL,
    /* version          */  XtVersion,
    /* callback_private */      NULL,
    /* tm_table         */  defaultTranslations,
    /* query_geometry   */  (XtGeometryHandler) QueryGeometry,
#if XtVersion > 11002
    /* display accel    */      XtInheritDisplayAccelerator,
    /* extension        */      NULL
#endif
    },
    { /*xpwcore fields */
    /*methods       */  _xpwScrollTextMethods,
    /*num_methods       */  0,   /* done at ClassInit */
    /*apply         */  XtInheritApplyProc,
    /*methods_table     */  NULL,
    }
};

externaldef(xpwscrolltextwidgetclass)
    WidgetClass xpwScrollTextWidgetClass = (WidgetClass) &xpwScrollTextClassRec;

/****************************************************************
 *
 * private procedures
 *
 ****************************************************************/
#ifdef DEBUG
static void debug_msg(m)
char *m;
{
        printf("xpwScrollText: %s\n",m);
}
#else
#define debug_msg(m)
#endif


static void ScrollTextClassInit()
  { xpwScrollTextClassRec.xpwcore_class.num_methods =
        _num_xpwScrollTextMethods;
  }

static void ColorFgBg(w, colornum, isstatus, fgp, bgp, bgpixp, flagsp)
  XpwScrollTextWidget w;
  int colornum;
  Boolean isstatus;
  Pixel *fgp, *bgp;
  Pixmap *bgpixp;
  uint *flagsp;
  { Pixel fg = NO_PIXEL, bg = NO_PIXEL;
    Pixmap bgpix = XtUnspecifiedPixmap;
    uint defnum, flags;

    defnum = (w->xpwscrolltext.default_color_mask
                            >> (colornum&(NALLCOLORS-1))) & 1;
    if (Flags(w) & MONOCHROME)
        /* Monochrome (or don't want GrayScale) -- use default for any colour */
        colornum = defnum;

    if (colornum == CURSOR_COLORNUM)
        /* cursor color */
        fg = w->xpwscrolltext.cursor_color;
    else if (colornum != 0)
      { ColorPair *p = &w->xpwscrolltext.alt_colors[colornum-1];
        fg = p->fg;
        bg = p->bg;
        if (defnum != 0)
          { /* color 1 (inverted) default */
            if (IS_NO_PIXEL(fg))
              { fg = w->xpwscrolltext.alt_colors[0].fg;
                if (IS_NO_PIXEL(fg))
                    fg = isstatus ? w->xpwscrolltext.status_background
                                  : w->core.background_pixel;
              }
            if (IS_NO_PIXEL(bg))
              { bg = w->xpwscrolltext.alt_colors[0].bg;
                if (IS_NO_PIXEL(bg))
                    bg = isstatus ? w->xpwscrolltext.status_foreground
                                  : w->xpwcore.foreground_pixel;
              }
          }
      }

    /* color 0 default */
    if (IS_NO_PIXEL(fg))
        fg = isstatus ? w->xpwscrolltext.status_foreground
                      : w->xpwcore.foreground_pixel;
    if (IS_NO_PIXEL(bg))
      { bg = isstatus ? w->xpwscrolltext.status_background
                      : w->core.background_pixel;
        if (!isstatus) bgpix = w->core.background_pixmap;
      }

    *fgp = fg; *bgp = bg; *bgpixp = bgpix;

    flags = (bg == w->core.background_pixel
                    && bgpix == w->core.background_pixmap)
                    ? GC_HAS_WIN_BG : 0;
    if (bgpix != XtUnspecifiedPixmap && bgpix != None) flags |= GC_HAS_BG_PIX;
    *flagsp = flags;
  }


static void SetShadowBits(w)
  XpwScrollTextWidget w;
  { XColor cdefs[NALLCOLORS], *cd;
    Colormap cmap = w->core.colormap;
    Display *dpy = XtDisplay(w);
    int s, i;
    Pixel fg, *sw;
    Pixmap dummy;
    uint dummyi;
    unsigned short white_bits;

#define SETWHITEVAL(clr) \
        if (clr >= (unsigned)0xf000) clr = (unsigned)0xffff;        \
        else if ((clr += (unsigned)0x1000) < (unsigned)0x8000)      \
            clr = (unsigned)0x8000;

#define LIGHTNESS(xc) ((unsigned)(xc.red+2*xc.green+xc.blue))

    for (s = 0; s < 2; s++)
      { sw = &w->xpwscrolltext.shadow_whites[s];
        if (!IS_NO_PIXEL(*sw))
          { XFreeColors(dpy, cmap, sw, 1, 0);
            *sw = NO_PIXEL;
          }
        if (w->xpwscrolltext.draw_shadow_mask == 0) continue;

        ColorFgBg(w, 0, s==0, &cdefs[0].pixel, &cdefs[1].pixel, &dummy, &dummyi);
        XQueryColors(dpy, cmap, cdefs, 2);
        cd = LIGHTNESS(cdefs[0]) > LIGHTNESS(cdefs[1]) ? &cdefs[0] : &cdefs[1];
        SETWHITEVAL(cd->red); SETWHITEVAL(cd->green); SETWHITEVAL(cd->blue);
        if (!XAllocColor(dpy, cmap, cd))
          { cd->red = cd->green = cd->blue = (unsigned)0xffff;
            XAllocColor(dpy, cmap, cd);
          }
        *sw = cd->pixel;

        for (i = 0; i < NALLCOLORS; i++)
            ColorFgBg(w, i, s==0, &fg, &cdefs[i].pixel, &dummy, &dummyi);

        XQueryColors(dpy, cmap, cdefs, NALLCOLORS);
        white_bits = 0;
        for (i = 0; i < NALLCOLORS; i++)
            if (LIGHTNESS(cdefs[i]) < (unsigned)0x18000)
                /* use a white shadow on a dark background */
                white_bits |= 1 << i;
        w->xpwscrolltext.shadow_white_bits[s] = white_bits;
      }
  }

static Boolean IsFixedFont(font, fwidth)
  XFontStruct *font;
  unsigned fwidth;
  { XCharStruct *pc = font->per_char, *pclim;
    unsigned maxwidth = font->max_bounds.width;

    if (font->min_bounds.width == maxwidth || !pc) return(True);

    /*  Bug in OpenLook means the above test may fail -- some fonts have
        unused chars >= 128 that have zero width, and in OL this makes
        font->min_bounds.width be zero (even though the font is fixed width).
        So we test all chars with non-zero width to be the same.
    */
    pclim = &pc[font->max_char_or_byte2 - font->min_char_or_byte2 + 1];
    for (; pc < pclim; pc++)
        if (pc->width != maxwidth && pc->width != 0) return(False);

    return(True);
  }

static Boolean IsFixedFontSet(fontset, fwidth)
  XFontSet fontset;
  unsigned fwidth;
  { XFontStruct **font_struct_list;
    char **font_name_list;
    int nfonts = XFontsOfFontSet(fontset, &font_struct_list, &font_name_list);
    while (nfonts--)
      { XFontStruct *font = *font_struct_list++;
        if (font->min_bounds.width != font->max_bounds.width
         || font->max_bounds.width != fwidth)
            return(False);
      }
    return(True);
  }

typedef struct { int max_width, av_width, ascent, descent; } FontData;

static void ExtentOfFont(font, datap, aw_atom)
  XFontStruct *font;
  FontData *datap;
  Atom aw_atom;
  { unsigned long value;
    datap->max_width = font->max_bounds.width;
    datap->av_width = (aw_atom != None
                        && XGetFontProperty(font, aw_atom, &value))
                        ? value : datap->max_width * 10;
    datap->ascent = font->ascent;
    datap->descent = font->descent;
  }

static void ExtentOfFontSet(fontset, datap, aw_atom)
  XFontSet fontset;
  FontData *datap;
  Atom aw_atom;
  { FontData d;
    XFontStruct **font_struct_list;
    char **font_name_list;
    int nfonts = XFontsOfFontSet(fontset, &font_struct_list, &font_name_list);
    int max_width = 0, av_width = 0, ascent = 0, descent = 0;
    while (nfonts--)
      { XFontStruct *font = *font_struct_list++;
        ExtentOfFont(font, &d, aw_atom);
        max_width = max(max_width, d.max_width);
        av_width = max(av_width, d.av_width);
        ascent = max(ascent, d.ascent);
        descent = max(descent, d.descent);
      }
    datap->max_width = max_width;
    datap->av_width = av_width;
    datap->ascent = ascent;
    datap->descent = descent;
  }

static void SetupFonts(w)
  XpwScrollTextWidget w;
  {
    unsigned fontnum, draw_modes = 0;
    int width, av_width = 0, ascent = 0, descent = 0;
    FontData data;
    wchar_t sp_char;
    Boolean varwidthmode = VarWidthMode(w);
    Atom aw_atom = varwidthmode ?
                    XInternAtom(XtDisplay(w), "AVERAGE_WIDTH", True) : None;

#define SETUP_FONTS(FIELDPTR, FIELD, GETFONTDATA, ISFIXED)                  \
    /* calculate character cell width, ascent and descent */                \
    for (fontnum = 0; fontnum < NFONTS; fontnum++)                          \
      { font = *(fontp = FIELDPTR(w, fontnum));                             \
        dfont = (fontnum & BOLDFONTBIT) ? FIELD(w, fontnum &~ BOLDFONTBIT)  \
                                        : NULL;                             \
        if (!dfont) dfont = mainfont;                                       \
        if (fontnum != NORMAL_FONT && font == dfont) font = *fontp = NULL;  \
        if (!font) continue;                                                \
                                                                            \
        GETFONTDATA(font, &data, aw_atom);                                  \
        if (varwidthmode)                                                   \
          { if (fontnum == NORMAL_FONT) av_width = data.av_width; }         \
        else                                                                \
            av_width = max(av_width, data.av_width);                        \
        ascent = max(ascent, data.ascent);                                  \
        descent = max(descent, data.descent);                               \
      }                                                                     \
                                                                            \
    FontAverageWidth(w) = av_width;                                         \
    FontWidth(w) = width = (av_width+5) / 10;                               \
    FontHeight(w) = ascent + descent;                                       \
    FontBaseline(w) = ascent;                                               \
                                                                            \
    /* set draw mode for each font(set) */                                  \
    for (fontnum = 0; fontnum < NFONTS; fontnum++)                          \
      { unsigned draw_mode = 0;                                             \
        if (!(font = FIELD(w, fontnum))) continue;                          \
        GETFONTDATA(font, &data, None);                                     \
        if (data.max_width != width || !ISFIXED(font, width))               \
            /* must draw one char at a time */                              \
            draw_mode |= FDM_1BY1;                                          \
        if (data.ascent != ascent || data.descent != descent)               \
            /* can't rely on XDrawImageString to clear bg properly */       \
            draw_mode |= FDM_CLEAR_BG;                                      \
                                                                            \
        draw_modes |= (draw_mode << (fontnum<<1));                          \
      }

    if (CharMode(w) < XpwICMUnicode)
      { XFontStruct *mainfont = FontStruct(w,NORMAL_FONT), *font, *dfont,
                    **fontp;
        XRectangle rect;
        SETUP_FONTS(FontFieldPtr, FontStruct, ExtentOfFont, IsFixedFont);
      }
    else
      { XFontSet mainfont = FontSet(w,NORMAL_FONT), font, dfont, *fontp;
        SETUP_FONTS(FontSetFieldPtr, FontSet, ExtentOfFontSet, IsFixedFontSet);
      }

    w->xpwscrolltext.font_draw_modes = draw_modes;

    sp_char = SPACECHAR;
    w->xpwscrolltext.space_width
        = XwcTextEscapement(FontSet(w,NORMAL_FONT), &sp_char, 1);
  }



static int HMargin(w)
XpwScrollTextWidget w;
  { int hm = w->xpwscrolltext.h_margin;
    return( hm >= 0 ? hm : ((-hm)*FontAverageWidth(w))/1000 );
  }

static int HExcess(w)
XpwScrollTextWidget w;
  { int hm = HMargin(w);
    if (StatusStyle(w) != 0) hm += min(max(6-hm,0), 2);
    return(hm * 2);
  }

static int VMargin(w)
XpwScrollTextWidget w;
  { int vm = w->xpwscrolltext.v_margin;
    return( vm >= 0 ? vm : ((-vm)*FontHeight(w))/100 );
  }

static int StatusVExcess(vm)
  int vm;
  { return( vm + min(max(8-vm,0), 4) );
  }

static int VExcess(w)
XpwScrollTextWidget w;
  { int vm = VMargin(w);
    return( vm*2 + (StatusStyle(w)==0 ? 0 : StatusVExcess(vm)) );
  }

static void TextRecalc(w, set_width, set_height)
  XpwScrollTextWidget w;
  Boolean set_width, set_height;
  { int av_width = FontAverageWidth(w), fheight = FontHeight(w),
        hex = HExcess(w)*10, vex = VExcess(w), width10 = w->core.width*10;
    unsigned short sstyle = StatusStyle(w);

    if (set_width)
      { if (!NumCols(w) && !width10) NumCols(w) = 80;
        if (NumCols(w))
          { width10 = NumCols(w)*av_width + hex;
            w->core.width = (Dimension) ((width10+9)/10);
          }
      }

    if (set_height)
      { if (!NumRows(w) && !w->core.height) NumRows(w) = 24;
        if (NumRows(w))
            w->core.height = (Dimension)(NumRows(w)*fheight) + vex;
      }

    /*  now compute NumRows & NumCols from width & height, then compute
        actual top and left margins */
    NumCols(w) = (int)(width10 - hex) / av_width;
    hex = (int)(width10 - NumCols(w)*av_width + 5) / 10;
    Col0Offset(w) = hex / 2;
    RightTextLim(w) = (int)(w->core.width - (hex-Col0Offset(w)));

    NumRows(w) = (int)(w->core.height - vex) / fheight;
    vex = (int)w->core.height - NumRows(w)*fheight;

    if (sstyle == 0)
      { /* row 0 at top, contiguous with rest */
        Row0Offset(w) = vex/2;
        SplitOffset(w) = Row1Offset(w) = Row0Offset(w) + fheight;
      }
    else
      { int vmar = StatusVExcess(VMargin(w)), sheight = fheight + vmar,
            tmar = (vex-vmar)/2;
        if (sstyle == 1)
          { /* status area at top */
            Row0Offset(w) = vmar-2;
            SplitOffset(w) = sheight;
            Row1Offset(w) = SplitOffset(w) + tmar;
          }
        else
          { /* status area at bottom */
            Row1Offset(w) = tmar;
            SplitOffset(w) = (int)w->core.height - sheight;
            Row0Offset(w) = SplitOffset(w) + 2;
          }
      }
  }

static void GetNewGC(w, gc_index, isstatus)
  XpwScrollTextWidget w;
  unsigned gc_index;
  Boolean isstatus;
  { XGCValues values;
    XFontStruct *font;
    unsigned long
        valuemask = GCFont | GCForeground | GCBackground | GCGraphicsExposures;
    unsigned colornum = gc_index/NFONTS;
    Pixmap bgpix;

    MyGC *fieldp = (colornum == CURSOR_COLORNUM)
                    ? (isstatus ? &w->xpwscrolltext.status_cursor_gc
                                : &w->xpwscrolltext.cursor_gc)
                    : (isstatus ? &w->xpwscrolltext.status_gc_array[gc_index]
                                : &w->xpwscrolltext.gc_array[gc_index]);

    if (fieldp->gc) XtReleaseGC((Widget)w, fieldp->gc);

    font = FontStruct(w, gc_index%NFONTS);
    if (!font)
      { fieldp->gc = NULL;
        return;
      }

    values.font = font->fid;
    ColorFgBg(w, colornum, isstatus, &values.foreground, &values.background,
                                                    &bgpix, &fieldp->flags);
    if (bgpix != XtUnspecifiedPixmap && bgpix != None)
        { values.tile = bgpix; valuemask |= GCTile; }
    values.graphics_exposures = True;

    fieldp->gc = XtGetGC((Widget)w, valuemask, &values);
  }

MyGC _XpwGetWorkTextGC(w, colornum, fontnum, isstatus)
XpwScrollTextWidget w;
unsigned colornum, fontnum;
Boolean isstatus;
  { Display *dpy = XtDisplay(w);
    GC gc = w->xpwscrolltext.work_gc;
    XGCValues values;
    unsigned long valuemask = 0;
    Font fid;
    Pixel fg, bg;
    Pixmap bgpix;
    MyGC mygc;

    fid = (FontStruct(w,fontnum))->fid;
    ColorFgBg(w, colornum, isstatus, &fg, &bg, &bgpix, &mygc.flags);

    if (!gc)
      { values.function = GXcopy;
        gc = w->xpwscrolltext.work_gc =
                    XCreateGC(dpy, w->core.screen->root, GCFunction, &values);
      }

    XGetGCValues(dpy, gc, GCFont|GCForeground|GCBackground|GCTile, &values);

    if (values.font != fid)
      { valuemask |= GCFont; values.font = fid; }
    if (values.foreground != fg)
      { valuemask |= GCForeground; values.foreground = fg; }
    if (values.background != bg)
      { valuemask |= GCBackground; values.background = bg; }
    if (values.tile != bgpix && bgpix != XtUnspecifiedPixmap && bgpix != None)
      { valuemask |= GCTile; values.tile = bgpix; }

    if (valuemask != 0) XChangeGC(dpy, gc, valuemask, &values);
    mygc.gc = gc;
    return(mygc);
  }


static void CvtUserAttributes(arp, attrp, setting)
  AttrResources *arp;
  unsigned *attrp;
  Boolean setting;
  { unsigned int attr;

    if (setting)
      { attr = *attrp;
        arp->colornum   = (attr & XpwFcolorNumber) >> XpwFcolorShift;
        arp->underline  = (attr & XpwFunderlineOn)  != 0;
        arp->bold       = (attr & XpwFboldOn)       != 0;
        arp->alt_font   = (attr & XpwFaltFontOn)    != 0;
        arp->blink      = (attr & XpwFblinkOn)      != 0;
        arp->active     = (attr & XpwFactiveOn)     != 0;
      }
    else
      { attr = arp->colornum << XpwFcolorShift;
        if (arp->underline) attr |= XpwFunderlineOn;
        if (arp->bold)      attr |= XpwFboldOn;
        if (arp->alt_font)  attr |= XpwFaltFontOn;
        if (arp->blink)     attr |= XpwFblinkOn;
        if (arp->active)    attr |= XpwFactiveOn;
        *attrp = attr;
      }
  }

static unsigned WinCharAttributes(arp)
  AttrResources *arp;
  { unsigned fontnum = NORMAL_FONT, flags = 0;
    if (arp->alt_font)  fontnum = ALT_FONT;
    if (arp->bold)      fontnum |= BOLDFONTBIT;
    if (arp->underline) flags |= ATTR_UNDERLINE;
    if (arp->blink)     flags |= ATTR_BLINK;
    if (arp->active)    flags |= ATTR_ACTIVE;

    return( GCIndex(arp->colornum, fontnum) | flags );
  }


static void SetTextAttributes(w)
  XpwScrollTextWidget w;
  { WinCharB wc;
    unsigned attrs, cchar = w->xpwscrolltext.cursor_char;
    AttrResources ar;

    if (!XtIsRealized((Widget)w)) return;

    CvtUserAttributes(&ar, &cchar, True);
    wc.attrs = WinCharAttributes(&ar);
    wc.c0 = cchar;
    if (wc.attrs != w->xpwscrolltext.cursor_winchar.attrs
     || wc.c0 != w->xpwscrolltext.cursor_winchar.c0)
      { _XpwTextCursorOn(w, False);
        w->xpwscrolltext.cursor_winchar = wc;
      }

    attrs = w->xpwscrolltext.curr_attributes
            = WinCharAttributes(&w->xpwscrolltext.curr_attr_resources);

    /* next line for upward compatibility */
    w->xpwscrolltext.highlight_on
            = ((w->xpwscrolltext.curr_attr_resources.colornum&1) != 0);

    _XpwTextCursorOn(w, w->xpwscrolltext.cursor_status && attrs == 0);
  }

/* Special methods for setting/getting boolean character attribute resources
 * quickly from bit flags
 */
void _XpwSetCharAttributes(w, attr)
    XpwScrollTextWidget w;
    unsigned attr;
  { CvtUserAttributes(&w->xpwscrolltext.curr_attr_resources, &attr, True);
    SetTextAttributes(w);
  }

XpwMethodRet _XpwGetCharAttributes(w)
XpwScrollTextWidget w;
  { unsigned attr;
    CvtUserAttributes(&w->xpwscrolltext.curr_attr_resources, &attr, False);
    return(attr);
  }

void _XpwSetTextCursor(w, cchar)
    XpwScrollTextWidget w;
    unsigned cchar;
  { Boolean old_status = w->xpwscrolltext.cursor_status;
    unsigned old_cchar = w->xpwscrolltext.cursor_char;
    if (cchar == 0)
        w->xpwscrolltext.cursor_status = False;
    else
      { w->xpwscrolltext.cursor_status = True;
        if (cchar != 1) w->xpwscrolltext.cursor_char = cchar;
      }
    if (w->xpwscrolltext.cursor_status != old_status
     || w->xpwscrolltext.cursor_char != old_cchar)
        SetTextAttributes(w);
  }

XpwMethodRet _XpwGetTextCursor(w)
XpwScrollTextWidget w;
  { unsigned cchar;
    return(w->xpwscrolltext.cursor_status ? w->xpwscrolltext.cursor_char : 0);
  }


/*  Allocate space for text in window, etc
 */

static void FreeRowArrayRows(w, num_rows, free_whole)
  XpwScrollTextWidget w;
  uint num_rows;
  Boolean free_whole;
  { uint i;
    WinRowB *rowarr = RowArray(w);
    char *lim = w->xpwscrolltext.row_array_lim;
    if (!rowarr) return;
    for (i=0; i < num_rows; i++)
      { char *rp = (char*)rowarr[i];
        if (rp < (char*)rowarr || lim <= rp) XtFree(rp - sizeof(WinVRowHead));
      }
    if (free_whole)
      { XtFree((char*)rowarr);
        RowArray(w) = NULL;
      }
  }

static void AllocRowArray(w, num_cols, num_rows, oldarr)
  XpwScrollTextWidget w;
  uint num_cols, num_rows;
  WinRowB *oldarr;
  { uint i, nbytes, WCsize, headsize, rsize, csz_code, charmode = CharMode(w);
    char *p;
    WinRowB *rowarr;
    Boolean var_width_mode = VarWidthMode(w);

    /* set size code */
    if (charmode < XpwICMUnicode)
        csz_code = CSZ_BYTE;
    else if (charmode == XpwICMUnicode)
        csz_code = CSZ_SHORT;
    else
      { if (sizeof(wchar_t) == sizeof(char))
            csz_code = CSZ_BYTE;
        else if (sizeof(wchar_t) == sizeof(short))
            csz_code = CSZ_SHORT;
        else
            csz_code = CSZ_INT;
      }

    CharSizeCode(w) = csz_code;
    if (csz_code == CSZ_BYTE)
        WCsize = sizeof(WinCharB);
    else if (csz_code == CSZ_SHORT)
        WCsize = sizeof(WinCharS);
    else
        WCsize = sizeof(WinCharI);

    headsize = var_width_mode ? sizeof(WinVRowHead) : sizeof(WinRowHead);
    if (oldarr && var_width_mode)
      { nbytes = 0;
        for (i=0; i < num_rows; i++)
          { rsize = WCsize * NUMCOLS(oldarr[i]);
            rsize = (rsize + 1) & ~1;   /* ensure exact number of shorts */
            nbytes += rsize;
          }
      }
    else
      { oldarr = NULL;
        rsize = WCsize * num_cols;
        rsize = (rsize + 1) & ~1;       /* ensure exact number of shorts */
        nbytes = num_rows * rsize;
      }
    nbytes += num_rows * (sizeof(WinRowB) + headsize);

    p = XtRealloc((char*)RowArray(w), nbytes);

    rowarr = RowArray(w) = (WinRowB*) p;
    p += num_rows * sizeof(WinRowB);

    for (i=0; i < num_rows; i++)
      { p += headsize;
        rowarr[i] = (WinRowB) p;
        NCHARS(p) = 0;                  /* set row empty */
        BLINK_SCOL(p) = MAX_COLS;       /* set blink range empty */
        BLINK_LCOL(p) = 0;
        if (oldarr)
          { num_cols = NUMCOLS(oldarr[i]);
            rsize = WCsize * num_cols;
            rsize = (rsize + 1) & ~1;
          }
        if (var_width_mode)
          { NUMCOLS(p) = num_cols;
            PIXOFFS(p) = COLOFFS(p) = 0;
          }
        p += rsize;
      }

    w->xpwscrolltext.row_array_lim = p;
  }

static void InitWinText(w)
  XpwScrollTextWidget w;
  { AllocRowArray(w, NumCols(w), NumRows(w), NULL);

    w->xpwscrolltext.blink_start_row = MAX_ROWS;
    w->xpwscrolltext.blink_lim_row = 0;

    CursorCol(w) = CursorRow(w) = 0;
    CursorX(w) = CURSOR_X_UNDEF;
  }

static void CopyIL1ToU(old, new)
  XpwScrollTextWidget old, new;
  { uint i, num_cols = NumCols(old), num_rows = NumRows(old);
    WinRowB *oldarr = RowArray(old);
    WinRowS *newarr;

    RowArray(new) = NULL;
    AllocRowArray(new, num_cols, num_rows, oldarr);
    newarr = (WinRowS*) RowArray(new);
    for (i=0; i < num_rows; i++)
      { uint nchars;
        register WinRowB oldrp = oldarr[i], oldlim;
        register WinRowS newrp = newarr[i];
        nchars = NCHARS(newrp) = NCHARS(oldrp);
        BLINK_SCOL(newrp) = BLINK_SCOL(oldrp);
        BLINK_LCOL(newrp) = BLINK_LCOL(oldrp);
        if (VarWidthMode(old))
          { COLOFFS(newrp) = COLOFFS(oldrp);
            PIXOFFS(newrp) = PIXOFFS(oldrp);
          }
        oldlim = oldrp + nchars;
        while (oldrp < oldlim)
          { newrp->c0 = oldrp->c0;
            newrp->c1 = 0;          /* zero-extend Latin1 char to Unicode */
            newrp->attrs = oldrp->attrs;
            oldrp++;
            newrp++;
          }
      }

    FreeRowArrayRows(old, num_rows, True);
  }

static void ResetWinText(w, num_rows)
  XpwScrollTextWidget w;
  uint num_rows;
  { FreeRowArrayRows(w, num_rows, False);
    InitWinText(w);
    _XpwTextCursorOn(w, True);
    XtCallCallbacks((Widget)w, XtNxpwCallback, 0);
  }


static XtGeometryResult QueryGeometry(w, request, preferred)
XpwScrollTextWidget w;
XtWidgetGeometry *request;
XtWidgetGeometry *preferred;
  { XtGeometryResult result = XtGeometryYes;

    preferred->request_mode = request->request_mode;

    if (request->request_mode & CWX) preferred->x = request->x;
    if (request->request_mode & CWY) preferred->y = request->y;
    if (request->request_mode & CWBorderWidth)
        preferred->border_width = request->border_width;

    if (request->request_mode & CWWidth)
      { int av_width = FontAverageWidth(w);
        if (av_width && w->xpwscrolltext.round_size)
          { int hex = HExcess(w)*10,
                n = request->width*10 - hex + av_width - 1;
            preferred->width = ((n/av_width)*av_width + hex) / 10;
            if (preferred->width != request->width) result = XtGeometryAlmost;
          }
        else
            preferred->width = request->width;
      }

    if (request->request_mode & CWHeight)
      { int fheight = FontHeight(w);
        if (fheight && w->xpwscrolltext.round_size)
          { int vex = VExcess(w), n = request->height - vex + fheight-1;
            preferred->height = (n/fheight)*fheight + vex;
            if (preferred->height != request->height)
                result = XtGeometryAlmost;
          }
        else
            preferred->height = request->height;
      }

    return(result);
  }


static void ScrollTextInitialize(request, w)
XpwScrollTextWidget request, w;
  { unsigned i, fontnum, vclass = XDefaultVisualOfScreen(XtScreen(w))->class;
    XGCValues values;

    debug_msg("Initialize start");

    XtAddEventHandler((Widget)w,
        EnterWindowMask|LeaveWindowMask|FocusChangeMask|VisibilityChangeMask,
                            False, EventHandler, False);

    w->xpwscrolltext.shadow_whites[0] = w->xpwscrolltext.shadow_whites[1]
            = NO_PIXEL;

    Flags(w) = 0;
    w->xpwscrolltext.no_draw_mode = -1;

    if (vclass == StaticGray
    || (vclass == GrayScale && w->xpwscrolltext.no_gray_scale))
        /* Monochrome (or don't want GrayScale) */
        Flags(w) |= MONOCHROME;

    for (fontnum = 0; fontnum < NFONTS; fontnum++)
      { XFontSet *fsp = FontSetFieldPtr(w,fontnum);
        XFontStruct **fp = FontFieldPtr(w,fontnum);
        if (*fsp)
            *fp = _XpwFont8OfFontSet(w, *fsp, True);
        else if (*fp)
            *fsp = _XpwFontSetFromFont(w, *fp);
      }
    SetupFonts(w);

    if (w->xpwscrolltext.geometry != NULL)
        /* Interpret geometry string initially, but allow numColumns
           or numRows to override it. Position info is ignored.  */
      { int x, y, res;
        unsigned int ncols, nrows;
        res = XParseGeometry(w->xpwscrolltext.geometry, &x, &y,
                                    &ncols, &nrows);
        if (!NumCols(w) && (res & WidthValue)) NumCols(w) = ncols;
        if (!NumRows(w) && (res & HeightValue)) NumRows(w) = nrows;
      }

    TextRecalc(w, True,True);

    /* this is allocated when realized */
    RowArray(w) = NULL;
    w->xpwscrolltext.next_blink_widget = (XpwScrollTextWidget) -1;

    /* get GCs */
    w->xpwscrolltext.work_gc = NULL;
    for (i = 0; i < NNORMALGCS; i++)
      { w->xpwscrolltext.gc_array[i].gc = NULL;
        GetNewGC(w, i, False);
      }
    w->xpwscrolltext.cursor_gc.gc = NULL;
    GetNewGC(w, CURSOR_GCINDEX, False);

    for (i = 0; i < NSTATUSGCS; i++)
      { w->xpwscrolltext.status_gc_array[i].gc = NULL;
        GetNewGC(w, i, True);
      }
    w->xpwscrolltext.status_cursor_gc.gc = NULL;
    GetNewGC(w, CURSOR_GCINDEX, True);

    w->xpwscrolltext.cursor_winchar.attrs = w->xpwscrolltext.cursor_winchar.c0
                                            = 0;
    SetShadowBits(w);

    debug_msg("Initialize end");
    return;
  }

static unsigned GCSelectMask(n, isfont)
unsigned n;
Boolean isfont;
  { unsigned i, mask = 0;
    if (isfont)
        for (i = 0; i < NCOLORS; i++) mask |= 1 << GCIndex(i,n);
    else if (n < NCOLORS)
        for (i = 0; i < NFONTS; i++) mask |= 1 << GCIndex(n,i);
    return(mask);
  }

static void RecolorPointer2(w)
  XpwScrollTextWidget w;
  { Cursor save = w->xpwcore.pointer_shape;
    w->xpwcore.pointer_shape = w->xpwscrolltext.pointer2_shape;
    _XpwRecolorPointer(w);
    w->xpwcore.pointer_shape = save;
  }

static Boolean ScrollTextSetValues(current, request, new)
XpwScrollTextWidget current, request, new;
  { Boolean
        redisplay       = False,
        recalc_height   = False,
        recalc_width    = False,
        new_cursor_gc   = False,
        new_status_cursor_gc = False,
        font_changes    = False;

    unsigned cw, ch, newcols, newrows;
    register unsigned i, fontnum, gc_mask = 0, status_gc_mask = 0;

#define DIFF(f) (new->f != current->f)

    /* char representation */
    if (DIFF(xpwscrolltext.input_char_mode))
      { if (CharMode(current) != XpwICMISOLatin1
         || CharMode(new) != XpwICMUnicode)
            /* ignore it */
            CharMode(new) = CharMode(current);
        else if (XtIsRealized((Widget)new))
          { /* change from XpwICMISOLatin1 to XpwICMUnicode is allowed */
            CopyIL1ToU(current, new);
            font_changes = True;
          }
      }
    if (DIFF(xpwscrolltext.var_width_mode)) font_changes = True;

    /* blinking */
    if (DIFF(xpwscrolltext.no_blink))
      { if (new->xpwscrolltext.no_blink) redisplay = True;
        else _XpwSetBlinkTimer();
      }

    /* colors */
    if (DIFF(xpwscrolltext.highlight_on))
      { /* for upward compatibility -- interpret as odd/even colours */
        if (new->xpwscrolltext.highlight_on)
            new->xpwscrolltext.curr_attr_resources.colornum |= 1;
        else
            new->xpwscrolltext.curr_attr_resources.colornum &= ~1;
      }

    if (DIFF(xpwscrolltext.selection_color_num)
     || DIFF(core.border_pixel))
        redisplay = True;

    if (DIFF(core.background_pixel) || DIFF(core.background_pixmap))
      { gc_mask |= GCSelectMask(0, False);
        new_cursor_gc = new_status_cursor_gc = redisplay = True;
      }

    if (DIFF(xpwcore.foreground_pixel))
      { gc_mask |= GCSelectMask(0, False);
        new_cursor_gc = redisplay = True;
      }

    if (DIFF(xpwscrolltext.status_foreground)
     || DIFF(xpwscrolltext.status_background))
      { status_gc_mask |= GCSelectMask(0, False);
        new_status_cursor_gc = redisplay = True;
      }

    if (gc_mask != 0 || status_gc_mask != 0
     || DIFF(xpwscrolltext.alt_colors[0].fg)
     || DIFF(xpwscrolltext.alt_colors[0].bg)
     || DIFF(xpwscrolltext.default_color_mask))
        /* colour dependencies may have changed -- check the whole lot properly */
        for (i = 0; i < NALLCOLORS; i++)
          { Pixel fg, bg, nfg, nbg;
            Pixmap bgpix, nbgpix;
            uint flags, nflags;
            unsigned m = GCSelectMask(i, False);
            ColorFgBg(current, i, False, &fg, &bg, &bgpix, &flags);
            ColorFgBg(new, i, False, &nfg, &nbg, &nbgpix, &nflags);
            if (nfg != fg || nbg != bg || nbgpix != bgpix || nflags != flags)
              { gc_mask |= m; redisplay = True; }
            ColorFgBg(current, i, True, &fg, &bg, &bgpix, &flags);
            ColorFgBg(new, i, True, &nfg, &nbg, &nbgpix, &nflags);
            if (nfg != fg || nbg != bg || nbgpix != bgpix || nflags != flags)
              { status_gc_mask |= m; redisplay = True; }
          }
    else
        /* quicker check for remaining colours */
        for (i = 0; i < NALLCOLORS-1; i++)
          { register ColorPair  *np = &new->xpwscrolltext.alt_colors[i],
                                *op = &current->xpwscrolltext.alt_colors[i];
            if (np->fg != op->fg || np->bg != op->bg)
              { unsigned m = GCSelectMask(i+1, False);
                gc_mask |= m;
                status_gc_mask |= m;
                redisplay = True;
              }
          }

    /* check shadow data */
    if (DIFF(xpwscrolltext.draw_shadow_mask)) redisplay = True;
    if (redisplay) SetShadowBits(new);

    if (DIFF(xpwscrolltext.cursor_color))
        new_cursor_gc = new_status_cursor_gc = redisplay = True;


    /* fonts */
    for (fontnum = 0; fontnum < NFONTS; fontnum++)
      { XFontSet    fs, *fsp = FontSetFieldPtr(new,fontnum),
                        *old_fsp = FontSetFieldPtr(current,fontnum);
        XFontStruct *f, **fp = FontFieldPtr(new,fontnum),
                        **old_fp = FontFieldPtr(current,fontnum);
        if ((fs = *fsp) != *old_fsp)
          { *fp = fs ? _XpwFont8OfFontSet(new, fs, True) : NULL;
            font_changes = True;
          }
        else if ((f = *fp) != *old_fp)
          { *fsp = f ? _XpwFontSetFromFont(new, f) : NULL;
            font_changes = True;
          }
      }
    if (font_changes)
      { unsigned m = 0;
        SetupFonts(new);
        if (DIFF(xpwcore.font_set))
          { m |= GCSelectMask(NORMAL_FONT, True);
            new_cursor_gc = new_status_cursor_gc = True;
          }
        for (fontnum = 1; fontnum < NFONTS; fontnum++)
            if (DIFF(xpwscrolltext.alt_font_sets[fontnum-1]))
                m |= GCSelectMask(fontnum, True);
        gc_mask |= m;
        status_gc_mask |= m;
        recalc_width = recalc_height = True;
        redisplay = True;
      }

    /* fetch any new GC's */
    if (gc_mask != 0)
        for (i = 0; i < NNORMALGCS; i++)
            if (gc_mask & (1<<i)) GetNewGC(new, i, False);
    if (new_cursor_gc) GetNewGC(new, CURSOR_GCINDEX, False);
    if (status_gc_mask != 0)
        for (i = 0; i < NSTATUSGCS; i++)
            if (status_gc_mask & (1<<i)) GetNewGC(new, i, True);
    if (new_status_cursor_gc) GetNewGC(new, CURSOR_GCINDEX, True);

    if (DIFF(xpwscrolltext.draw_graphic_chars)) redisplay = True;

    SetTextAttributes(new);

    /* pointer2 */
    if (DIFF(xpwscrolltext.pointer2_shape)
     || DIFF(xpwcore.pointer_foreground) || DIFF(xpwcore.pointer_background))
        RecolorPointer2(new);


    /* check for cursor move */
    if (CursorRow(new) != CursorRow(current)
     || CursorCol(new) != CursorCol(current))
      { int row = CursorRow(new), col = CursorCol(new);
        CursorRow(new) = CursorRow(current);
        CursorCol(new) = CursorCol(current);
        _XpwTextMoveCursorTo(new, col,row);
      }

    /* test for size changes */
    if (NumCols(new) != NumCols(current)) recalc_width = True;
    if (NumRows(new) != NumRows(current)) recalc_height = True;

    /* if they change the margins, also resize widget */
    if (DIFF(xpwscrolltext.h_margin))
      { recalc_width = True; redisplay = True; }
    if (DIFF(xpwscrolltext.v_margin)
     || StatusStyle(new) != StatusStyle(current) )
      { recalc_height = True; redisplay = True; }

    /*  now find new width/height -- if user has specified a width or height,
        stick with that */
    cw = current->core.width; ch = current->core.height;
    if ((recalc_width && new->core.width == cw)
     || (recalc_height && new->core.height == ch))
        TextRecalc(new, recalc_width, recalc_height);

    newcols = NumCols(new);
    newrows = NumRows(new);
    NumCols(new) = NumCols(current);
    NumRows(new) = NumRows(current);

    if (newcols != NumCols(current) || newrows != NumRows(current))
      { if (new->core.width == cw && new->core.height == ch)
            /*  no size change, but changed rows/cols and something else, which
                meant window size stayed the same */
          { ScrollTextResize(new); redisplay = True; }
      }
    else if (DIFF(xpwscrolltext.var_width_mode)
          || DIFF(xpwscrolltext.num_fixed_columns)
          || DIFF(xpwscrolltext.num_status_fixed_columns)
          || (font_changes && VarWidthMode(new)) )
        /* rows/cols stay the same, but must reset win text */
      { ResetWinText(new, newrows); redisplay = True; }

    return (redisplay);

#undef DIFF
}

    /* Called if we failed to change size after SetValues */
static void ScrollTextSetValuesAlmost(current, new, request, reply)
    XpwScrollTextWidget current, new;
    XtWidgetGeometry *request, *reply;
  { request->request_mode = 0;  /* don't try again */
    ScrollTextResize(new);      /* recalc and do callback */
  }

static void ScrollTextRealize(w, valueMask, attrs)
XpwScrollTextWidget w;
XtValueMask *valueMask;
XSetWindowAttributes *attrs;
  { Display *dpy = XtDisplay(w);

    *valueMask |= CWBitGravity;
    attrs->bit_gravity = ForgetGravity;
    if (attrs->cursor = w->xpwcore.pointer_shape)
     { *valueMask |= CWCursor; _XpwRecolorPointer(w); }
    RecolorPointer2(w);

    /* calculate NumRows and NumCols */
    TextRecalc(w, False,False);
    InitWinText(w);

    XtCreateWindow((Widget)w, InputOutput, (Visual *)CopyFromParent,
                                                    *valueMask, attrs);
    /* next line for upward compatibility */
    if (w->xpwscrolltext.highlight_on)
        w->xpwscrolltext.curr_attr_resources.colornum |= 1;
    SetTextAttributes(w);
  }

static void ScrollTextResize(w)
  XpwScrollTextWidget w;
  { int num_rows = NumRows(w), num_cols = NumCols(w);

    /* do nothing if window hasn't been realized yet. */
    if (!XtIsRealized((Widget)w)) return;

    CursorX(w) = CURSOR_X_UNDEF;
    /* recalculate NumRows and NumCols */
    TextRecalc(w, False,False);
    if (NumRows(w) != num_rows || NumCols(w) != num_cols)
        ResetWinText(w, num_rows);
  }


static void ScrollTextRedisplay(w, event, region)
XpwScrollTextWidget w;
XEvent *event;
Region region;
  { Position x, y, xl, yl;
    int split = SplitOffset(w), count;
    Boolean sattop = (StatusStyle(w) != 2);

    if (event->type == GraphicsExpose)
      { xl = (x = event->xgraphicsexpose.x) + event->xgraphicsexpose.width;
        yl = (y = event->xgraphicsexpose.y) + event->xgraphicsexpose.height;
        count = event->xgraphicsexpose.count;
      }
    else if (event->type == Expose)
      { xl = (x = event->xexpose.x) + event->xexpose.width;
        yl = (y = event->xexpose.y) + event->xexpose.height;
        count = event->xexpose.count;
      }
    else
        return;

    if (y < split)  _XpwTextExpose(w, x, y,     xl, min(yl,split), sattop);
    if (split < yl) _XpwTextExpose(w, x, split, xl, yl,            !sattop);
  }


static void ScrollTextDestroy(w)
XpwScrollTextWidget w;
  { unsigned i, s;

    _XpwTextDestroy(w);

    for (s = 0; s < 2; s++)
      { Pixel *sw = &w->xpwscrolltext.shadow_whites[s];
        if (!IS_NO_PIXEL(*sw))
            XFreeColors(XtDisplay(w), w->core.colormap, sw, 1, 0);
      }

    if (w->xpwscrolltext.work_gc)
        XFreeGC(XtDisplay(w), w->xpwscrolltext.work_gc);
    for (i = 0; i < NNORMALGCS; i++)
      { GC gc = w->xpwscrolltext.gc_array[i].gc;
        if (gc) XtReleaseGC((Widget)w, gc);
      }
    XtReleaseGC((Widget)w, w->xpwscrolltext.cursor_gc.gc);
    for (i = 0; i < NSTATUSGCS; i++)
      { GC gc = w->xpwscrolltext.status_gc_array[i].gc;
        if (gc) XtReleaseGC((Widget)w, gc);
      }
    XtReleaseGC((Widget)w, w->xpwscrolltext.status_cursor_gc.gc);

    FreeRowArrayRows(w, NumRows(w), True);

    if (Flags(w) & HAVE_FOCUS)
        XtCallCallbacks((Widget)w, XtNfocusChange, (XtPointer) False);
  }



/* ACTIONS */

#define ButtonMask (Button1Mask|Button2Mask|Button3Mask|Button4Mask|Button5Mask)

#define RELEASE (0<<8)
#define PRESS   (1<<8)
#define HOLD    (2<<8)

static int
    buff_button, buff_nclicks, buff_mode,
    buff_expire_time, timer_expire_time,
    last_button_row;

static XtIntervalId buff_timerid = (XtIntervalId)NULL;

static void button_timeout();

/* takes an event, and fills in the mouse locations */

static void ExtractPosition(w, event, mouse)
  XpwScrollTextWidget w;
  XEvent *event;
  MousePosition *mouse;
  { int x, y, row;
    Boolean row_relative = False;
    unsigned evtype = event->type;

    switch(evtype)
      {
        case MotionNotify:
            row_relative = (event->xmotion.state & ButtonMask) != 0;
            x = event->xmotion.x;   y = event->xmotion.y;   break;
        case ButtonRelease:
            row_relative = True;
        case ButtonPress:
            x = event->xbutton.x;   y = event->xbutton.y;   break;
        case KeyPress:
        case KeyRelease:
            x = event->xkey.x;      y = event->xkey.y;      break;
        case EnterNotify:
        case LeaveNotify:
            x = event->xcrossing.x; y = event->xcrossing.y; break;
        default:
            x = 0; y = 0;
      }

    mouse->x = x;
    mouse->y = y;
    row = _XpwYCoordToRow(w, y);
    mouse->column = _XpwXCoordToCol(w, x, row);

    if (row_relative)
      { /* make row of event relative to last ButtonPress row */
        int fheight = FontHeight(w);
        y -= RowToYCoord(w, last_button_row);
        if (y < 0) y -= fheight-1;
        row = y/fheight + last_button_row;
      }
    else
        if (evtype == ButtonPress) last_button_row = row;

    mouse->row = row;
  }

static void flush_button(w, timed_out)
XpwScrollTextWidget w;
Boolean timed_out;
  { long button = buff_button, nclicks = buff_nclicks, mode = buff_mode;
    buff_button = buff_nclicks = 0;
    buff_mode = RELEASE;
    if (button == 0) return;

    if (buff_timerid)
      { XtRemoveTimeOut(buff_timerid); buff_timerid = (XtIntervalId)NULL; }
    if (nclicks != 0)
      { /* button multiClick Press/Release callback */
        XtCallCallbacks((Widget)w, XtNbuttonEvent, (XtPointer)((nclicks<<16)|mode|button));
        if (mode == PRESS && timed_out)
          { /* try for hold */
            buff_button = button;
            buff_mode = PRESS;
            button_timeout(w, NULL);
          }
      }
    else if (timed_out && mode == HOLD)
        /* button Hold callback */
        XtCallCallbacks((Widget)w, XtNbuttonEvent, (XtPointer)(HOLD|button));
  }

static void button_timeout(w, timeridp)
XpwScrollTextWidget w;
XtIntervalId *timeridp;
  { Display *dpy = XtDisplay(w);
    int interval;
    if (buff_button == 0) return;
    interval = buff_expire_time - timer_expire_time;
    if (interval <= 0 && buff_nclicks == 0 && buff_mode != HOLD)
      { buff_mode = HOLD;
        interval += XtGetMultiClickTime(dpy);
      }
    if (interval > 0)
      { buff_timerid = XtAppAddTimeOut(XtDisplayToApplicationContext(dpy),
                                    interval, button_timeout, w);
        timer_expire_time += interval;
      }
    else
      { buff_timerid = (XtIntervalId)NULL;
        flush_button(w, True);
      }
  }

static void NotifyButtonEvent(w, event, params, num_params)
XpwScrollTextWidget w;
XEvent *event;
String *params;
Cardinal *num_params;
{   long button = event->xbutton.button;

    if (!w->core.visible) return;
    w->xpwcore.modifiers = event->xbutton.state;
    buff_expire_time = event->xbutton.time + XtGetMultiClickTime(XtDisplay(w));

    if (event->type == ButtonPress)
      { if (button == buff_button)
            buff_mode = PRESS;
        else
          { flush_button(w, False);
            ExtractPosition(w, event, &w->xpwscrolltext.mouse);
            /* button Press callback */
            XtCallCallbacks((Widget)w, XtNbuttonEvent, (XtPointer)(PRESS|button));
            buff_button = button;
            buff_mode = PRESS;
            timer_expire_time = event->xbutton.time;
            button_timeout(w, NULL);
          }
      }
    else
      { if (button == buff_button && buff_mode != HOLD)
          { buff_nclicks++;
            buff_mode = RELEASE;
          }
        else
          { flush_button(w, False);
            ExtractPosition(w, event, &w->xpwscrolltext.mouse);
            /* button Release callback */
            XtCallCallbacks((Widget)w, XtNbuttonEvent, (XtPointer)(RELEASE|button));
          }
      }
  }


static void NotifyMotionEvent(w, event, params, num_params)
XpwScrollTextWidget w;
XEvent *event;
String *params;
Cardinal *num_params;
  { long call_data;
    MousePosition mouse;

    if (!w->core.visible) return;
    ExtractPosition(w, event, &mouse);
    call_data = event->xmotion.state;

    if (call_data & ButtonMask)
      { if (buff_button != 0)
          { if (buff_mode == HOLD
             && (mouse.column != w->xpwscrolltext.mouse.column
                 || mouse.row != w->xpwscrolltext.mouse.row))
                /* abort HOLD if sufficient movement to change col/row */
                flush_button(w, False);
            else
                /* ignore motion otherwise */
                return;
          }

        /* notify the clients of the motion event */
        w->xpwscrolltext.mouse = mouse;
        w->xpwcore.modifiers = call_data;
        XtCallCallbacks((Widget)w, XtNmotionEvent, (XtPointer)call_data);
      }
    else if (buff_button == 0)
      { w->xpwscrolltext.mouse = mouse;
        w->xpwcore.modifiers = call_data;
        _XpwTextTestActiveChange(w);
      }
  }

static void process_string(src, dst, len)
String src, dst;
int len;
{
    if (src[0] == 0) dst[0] = 0;
    else if (src[0] == '0' && src[1] == 'x' && src[2] != '\0') {
        /* turn 0x?? to a string containing that hex character */
        char c, *p;
        dst[0] = dst[1] = dst[2] = 0;
        for (p = src+2; (c = *p); p++) {
            dst[0] *= 16;
            if (isupper(c)) c = tolower(c);
            if (c >= '0' && c <= '9')
                dst[0] += c - '0';
            else if (c >= 'a' && c <= 'f')
                dst[0] += c - 'a' + 10;
            else break;
        }
    } else {
        /* scan string looking for \? sequences */
        char *s, *d, c = -1, *dst_lim = dst+len;
        for (s = src, d = dst; c != 0 && d < dst_lim; ) {
            c = *s++;
            if (c == '\\') {
                c = *s++;
                if (islower(c)) c ^= 32;
                if (c >= '@' && c <= '[') c -= '@';
            };
            *d++ = c;
        }
    }
}

static void NotifyKeyboardEvent(gw, event, params, num_params)
Widget gw;
XEvent *event;
String *params;
Cardinal *num_params;
  {
    KeySym key; unsigned int modifiers_return, count = 14;
    XpwScrollTextWidget w = (XpwScrollTextWidget) gw;
    if (!gw->core.visible) return;

    flush_button(w, False);
    ExtractPosition(w, event, &w->xpwscrolltext.mouse);

    if (*num_params == 1)
      { /* string action (N.B. can't assume event is a keyboard
            event, or that the keycode is valid even if it is) */
        w->xpwscrolltext.synthetic = True;
        process_string(params[0], w->xpwscrolltext.key, 15);
        key = 0;    /* unused */
        w->xpwcore.modifiers = 0;
      }
    else
      { /* convert keycode to keysym and string */
        w->xpwscrolltext.synthetic = False;
        w->xpwcore.modifiers = event->xkey.state;
        /* stop XLookupString interpreting Mod1 */
        event->xkey.state &= ~Mod1Mask;
        count = XLookupString((XKeyEvent *)event, w->xpwscrolltext.key, 15,
                                                        &key, NULL);
        /* ensure it is null terminated */
        w->xpwscrolltext.key[count] = 0;
        if (key == 0) return;
        if (event->type == KeyRelease) key *=(int)-1;
      }

    XtCallCallbacks(gw, XtNkeyboardEvent, (XtPointer)key);
}

/* Bell action */

static void BellAction(gw, event, params, num_params)
Widget gw;
XEvent *event;
String *params;
Cardinal *num_params;
  {
    XBell(XtDisplay(gw), 100);
  }

static void EventHandler(w, client, event, continue_to_dispatch)
XpwScrollTextWidget w;
Opaque client;
XEvent *event;
Boolean *continue_to_dispatch;
  { unsigned flags = Flags(w), oldflags = flags, f;
    Boolean in, have_focus;

    if (w->core.being_destroyed) return;

    switch (event->type)
      {
        case FocusIn:
        case FocusOut:
            f = event->xfocus.detail == NotifyPointer ? PTR_FOCUS
                : event->xfocus.send_event ? SYN_FOCUS      /* synthetic */
                : ACT_FOCUS;
            in = (event->type == FocusIn);
            break;

        case EnterNotify:
        case LeaveNotify:
            in = (event->type == EnterNotify);
            if (in)
                ExtractPosition(w, event, &w->xpwscrolltext.mouse);
            else
              { /* ensure any current active segment cleared */
                w->xpwscrolltext.mouse.column = w->xpwscrolltext.mouse.row = -1;
                _XpwTextTestActiveChange(w);
              }
            if (event->xcrossing.detail == NotifyInferior
             || !event->xcrossing.focus)
                return;
            f = PTR_FOCUS;
            break;

        case VisibilityNotify:
            w->xpwscrolltext.is_fully_visible =
                (event->xvisibility.state == VisibilityUnobscured);
            if (!(flags & HAVE_FOCUS) || (flags & CALLBACK_YES)) return;
            /* have focus, but client not notified due to previously
               unviewable state -- must now be viewable */
            f = 0;
            in = True;
            break;

        default:
            return;
      }

    if (in) flags |= f; else flags &= ~f;
    have_focus = (flags & HAVE_FOCUS) != 0;

    if (have_focus != ((oldflags & HAVE_FOCUS)!=0))
      { /* focus change -- reverse cursor if on and do callback */
        Boolean save_cursor = _XpwTextCursorOn(w, False);
        Flags(w) = flags & ~CURSOR_ON;
        _XpwTextCursorOn(w, save_cursor);

        if (have_focus)
          { /* don't say we have focus if we're not viewable */
            XWindowAttributes attr;
            XGetWindowAttributes(XtDisplay(w), XtWindow(w), &attr);
            if (attr.map_state != IsViewable) return;
          }
      }
    else
      { Flags(w) = flags;
        if (!in || (flags & CALLBACK_YES)) return;
        /* have focus, but client not notified due to previously
           unviewable state */
        if (event->type != VisibilityNotify)
          { /* this state is probably due to a bug in twm ... */
            if (flags & SYN_FOCUS) Flags(w) &= ~PTR_FOCUS;
            return;
          }
      }

    if (have_focus)
        Flags(w) |= CALLBACK_YES;
    else
        Flags(w) &= ~CALLBACK_YES;
    XtCallCallbacks((Widget)w, XtNfocusChange, (XtPointer)(long) have_focus);
  }


/* _XpwTextCopyWait(w) - waits for an exposure or a noexpose event */
void _XpwTextCopyWait(w)
XpwScrollTextWidget w;
  {
    Display *dpy = XtDisplay(w);
    Window win = XtWindow(w);
    XEvent event;
    if (w->xpwscrolltext.is_fully_visible) return;
    while (1)
      { XWindowEvent(dpy, win, ExposureMask, &event);
        if (event.type == NoExpose)
            return;
        else if (event.type == GraphicsExpose)
          { ScrollTextRedisplay(w, &event, NULL);
            if (event.xgraphicsexpose.count == 0) return;
          }
        else if (event.type == Expose)
          { ScrollTextRedisplay(w, &event, NULL);
          }
      }
  }


/* --- Revision History ---------------------------------------------------
--- John Gibson, May 24 1999
        Added XtNfontAverageWidth resource to widget
--- John Gibson, Aug 15 1997
        Changes to support variable-width chars
--- John Gibson, Apr 29 1997
        Changes to support drawing Unicode & wide-character strings.
--- John Gibson, Dec 18 1996
        Stopped NotifyKeyboardEvent calling the callback when keysym is 0.
--- John Gibson, Nov 22 1996
        Changed to uses MyGC structures instead of GC.
--- John Gibson, May 21 1996
        Fixed processing of motion events so that they don't corrupt the
        position of a button event.
--- John Gibson, Apr 16 1996
        Added XtNautoGeometry resource
--- John Gibson, Feb 26 1996
        In EventHandler, stopped an EnterNotify calling
        _XpwTextTestActiveChange.
--- John Gibson, Oct  9 1995
        Added extra active colours, stuff for drawing shadowed chars etc.
        Now allows any fonts at all (all drawn fixed-width).
--- John Gibson, Jan 25 1994
        Changed SetupFonts so that specifying the same font for bold etc
        is the same as specifying NULL
--- John Gibson, Jan  9 1994
        Added support for multiple cursors etc (XtN cursorChar resource),
        and different positions of status line (XtN statusStyle).
--- John Gibson, Dec 19 1993
        Fixed ANSI warnings
--- John Gibson, Aug 15 1992
        Made InitWinText ensure that CursorCol/Row are within window
        after a resize
--- Adrian Howard, Aug 12 1992
        Altered (menubar|scrollbar)(foreground|background) so that foregrounds
        default to XtNDefaultForeground, not XtNDefaultBackground (and vice
        versa)
--- John Gibson (for Jonathan Meyer), Aug  8 1992
        Made NotifyKeyboardEvent clear the Mod1Mask (Meta) so that
        it does not effect the lookup of the key string.
--- John Gibson, Jul 25 1992
        Stopped NotifyKeyboardEvent calling XLookupString for a "string"
        action (causes bugs in Openwin 3)
--- John Gibson, Jun 23 1992
        Made ColorFgBg default all colours for GrayScale visuals when
        XtNnoGrayScale is true
--- John Gibson, Mar 26 1992
        Fixed ScrollTextSetValues to detect colour dependency changes properly
--- John Gibson, Mar  9 1992
        VMS externalref mods
--- John Gibson, Feb  2 1992
        Changes to support 8 per-character colours.
--- John Gibson, Jan 16 1992
        Added _XpwSetCharAttributes
--- John Gibson, Jan 15 1992
        Added support for mark fg & bg colours in col 0
--- John Gibson, Jan 13 1992
        Changes to support alternative fonts
--- John Gibson, Dec 23 1991
        Add XtNdrawGraphicChars resource
--- John Gibson, Dec 18 1991
        Lots of changes to support bold & underline modes
--- Simon Nichols, Nov 27 1991
        Cast initializing value of query_geometry field of
        xpwScrollTextClassRec to type XtGeometryHandler.
--- John Gibson, Nov  4 1991
        Added geometry resource and code to process it in ScrollTextInitialize
--- John Gibson, Oct 25 1991
        VMS mods
--- John Gibson, Oct 10 1991
        Changed XlookupString -compose- arg to NULL in NotifyKeyboardEvent
--- John Gibson, Sep 29 1991
        Added roundSize resource and rewrote QueryGeometry (again)
--- Jonathan Meyer, Sep 11 1991
        Added menubarForeground/Background
--- John Gibson, Aug 22 1991
        Fixed QueryGeometry
--- John Gibson, Aug 16 1991
        Changes for correct handling of size changes etc.
--- John Gibson, Aug  9 1991
        Rewrote button handling
--- John Gibson, Aug  6 1991
        Now uses internal margins which may be different from user-specified
        ones.
--- John Gibson, Aug  5 1991
        EventHandler now deals with FocusIn/Out
--- Jonathan Meyer, Aug  2 1991 Added scrollbarForeground, scrollbarBackground
--- Jonathan Meyer, Jul 29 1991
        Added needs_new_{status/cursor}_gc variables to SetValues
        so that changing font and colours at the same time
        doesn't generate wasted GC's.
--- Jonathan Meyer, Jul 29 1991
        Added test for change to highlight_foreground/background in
        SetValues procedure (such changes were ignored).
--- John Gibson, Jul 27 1991
        Now represents text window contents in row array in widget.
        Large number of changes for this and to fix other bugs.
--- John Gibson, Jul 14 1991
        Changes to cursor state handling
--- John Gibson, Jul 11 1991
        Added initialisation for cursor_state in Initialize.
        Uncommented out call to highlight cursor in AcceptFocus
--- Jonathan Meyer, Jul  9 1991
    Changed default numRows to 24
--- John Gibson, Jul  7 1991
    Fixed InitExposedLines to allocate correct amount of space for
    exposed lines array.
--- Jonathan Meyer, Jul  1 1991
    Added test for fixedwidth font
--- Jonathan Meyer, Jun 27 1991
    Added QueryGeometry to return prefered geom.
    Made Resize not change the width/height values (passes Recalc False)
--- Jonathan Meyer, Jun 18 1991
    Made XtNvisible is_fully_visible
--- Jonathan Meyer, Jun  17 19XChangeGC91
    Removed test for visibility from exposure handler.
--- Jonathan Meyer, Jun  13 1991
    Added redefinition of XpwCore's font resource with a default
    value of 'fixed'.
    Made NotifyKeyboardEvent call XLookupString before the
    process_string so that the KeySym key is set correctly
--- Jonathan Meyer, Jun  13 1991
    Made exposed_lines a per-widget slot.
--- John Gibson, Jun 10 1991
        Fixed process_string
--- Jonathan Meyer, Jun  4 1991
    Added synthetic, also extra testing for visibility.
--- Jonathan Meyer, Jun  3 1991
    Forces change of status foreground/background to redisplay.
--- Jonathan Meyer, May 31 1991
    Made XtNstatusForeground and XtNstatusBackground default to
    having the same colours as the foreground/background of the widget.
--- Jonathan Meyer, Apr  4 1991
    Made SetValues proc set redisplay on font/margin change
--- Jonathan Meyer, Mar 29 1991
    Changed XtDestroyGC to XtReleaseGC - a bug in XtDestroyGC stops
    it working with multiple displays.
--- Jonathan Meyer, Mar 29 1991
    Added Bell action
--- Jonathan Meyer, Aug 24 1990
    Made use of revised code in UsersGC.c - Changed occurances of
        xpwCoreGCValueMask to _xpwGCvaluemask,
        xpwCoreGCValues to _xpwGCvalues,
        UpdateUsersGC to _XpwCondUpdateUsersGC
        XpwTextRecalc -> TextRecalc,
        XpwTextCursor -> _XpwTextCursor,
--- Jonathan Meyer, Aug 21 1990
    Added "X11/" prefix again. Defined XpwNotInstalled. Removed some
    redundant #include's.
--- Jonathan Meyer, Aug  8 1990
    Added process_string, and made a new action, called "string",
    which takes one argument and passes this to Poplog as the
    string for the key that was hit. See HELP xpop_news, and
    REF XpwScroll
--- Andreas Schoter, Jul 24 1990
    Changed NotifyPopCallback to NotifyXpwCallback and
    notify-pop-callback to notify-xpw-callback
--- Andreas Schoter, Jul 23 1990 Changed all pop* identifiers to xpw*
--- Ian Rogers, Jul 20 1990
    Changed popScrollTextWidgetClass to xpwScrollTextWidgetClass
--- Andreas Schoter 16 July 1990
    Replaced all occurances of Pop* variable names with Xpw*
--- Simon Nichols, Jun 19 1990
        Removed "X11/" prefix from #include file names.
 */
