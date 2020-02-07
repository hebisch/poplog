/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 * File:        C.x/x/Xpw/Text.c
 * Version:     Revision 5.0
 * Purpose:     Contains XpwScrollTextWidget utility functions.
 * Author:      Jonathan Meyer (see revisions)
 * Date:        11 Nov 1989
 *
 */

/* using Xpw header files locally, not in <X11/Xpw/...> */
#define XpwNotInstalled

#include <stdlib.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xatom.h>

#include "XpwScrTextP.h"

/* declarations */

extern void
    _XpwTextCopyWait(),
    _XpwSetCharAttributes(),
    _XpwGetCharAttributes(),
    _XpwSetTextCursor(),
    _XpwGetTextCursor();

extern MyGC _XpwGetWorkTextGC();

XpwMethodRet
    _XpwTextMoveCursorTo();

static void Clear();

externaldef (xpwscrolltext) uint (*XpwUcToMb)();


/* The XpwScrollText widget has three core methods:
    Scroll - scrolls any rectangle of the screen in any direction.
    Clear  - clears a rectangle of the screen.
    Write  - writes an array of strings onto the screen in a rectangle.

All of the other methods are just specialisations of these four
*/

static void ClearRectangle(dpy, win, mygc, xcliprect, x, y, width, height)
  Display *dpy;
  Window win;
  MyGC mygc;
  XRectangle *xcliprect;
  Coord x, y;
  int width, height;
  { GC gc = mygc.gc;
    XGCValues gcv, newgcv;
    unsigned long valuemask;

    if (xcliprect)
      { Coord xl = x + width, xcl = xcliprect->x + xcliprect->width;
        x = max(x, xcliprect->x);
        width = min(xl, xcl) - x;
        if (width <= 0) return;
      }

    if (mygc.flags & GC_HAS_WIN_BG)
      { XClearArea(dpy, win, x, y, width, height, FALSE);
        return;
      }
    XGetGCValues(dpy, gc, GCForeground|GCBackground|GCFillStyle, &gcv);
    if (mygc.flags & GC_HAS_BG_PIX)
      { newgcv.fill_style = FillTiled;
        valuemask = GCFillStyle;
      }
    else
      { newgcv.foreground = gcv.background;
        newgcv.fill_style = FillSolid;
        valuemask = GCForeground|GCFillStyle;
      }
    XChangeGC(dpy, gc, valuemask, &newgcv);
    XFillRectangle(dpy, win, gc, x, y, width, height);
    XChangeGC(dpy, gc, valuemask, &gcv);
  }


/*  Drawing a string
 */

static XpwScrollTextWidget blink_widget_chain = NULL;
static Boolean blink_on_phase = True, skip_cursor_blink = False;
static unsigned blink_row_start_col, blink_row_lim_col;
static XtIntervalId blink_timerid = (XtIntervalId)NULL;

static XpwScrollTextWidget onactive_widget = NULL;
static uint onactive_mode, onactive_row, onactive_start_x,
    onactive_start_col, onactive_lim_col;
static int draw_lim_x, draw_lim_col, last_text_width;



#define XTEXTWIDTH(str,n)   XTextWidth(font,(char*)str,n)

#define XDRAWSTRING(x,y,str,n)                                          \
    if (fontmode & FDM_1BY1)                                            \
        DrawString1By1(dpy,win,gc,x,y,(char*)str,n,font,fwidth);        \
    else XDrawString(dpy,win,gc,x,y,(char*)str,n);

#define XDRAWIMAGESTRING(x,y,str,n)                                     \
    XDrawImageString(dpy,win,gc,x,y,(char*)str,n);


#define XucTEXTWIDTH(str,n) XucTextEscapement((XFontSet)font,str,n)

#define XucDRAWSTRING(x,y,str,n)                                        \
    if (fontmode & FDM_1BY1)                                            \
        ucDrawString1By1(dpy,win,gc,x,y,str,n,(XFontSet)font,fwidth);   \
    else XucDrawString(dpy,win,(XFontSet)font,gc,x,y,str,n);

#define XucDRAWIMAGESTRING(x,y,str,n)                                   \
    XucDrawImageString(dpy,win,(XFontSet)font,gc,x,y,str,n);


#define XwcTEXTWIDTH(str,n) XwcTextEscapement((XFontSet)font,str,n)

#define XwcDRAWSTRING(x,y,str,n)                                        \
    if (fontmode & FDM_1BY1)                                            \
        wcDrawString1By1(dpy,win,gc,x,y,str,n,(XFontSet)font,fwidth);   \
    else XwcDrawString(dpy,win,(XFontSet)font,gc,x,y,str,n);

#define XwcDRAWIMAGESTRING(x,y,str,n)                                   \
    XwcDrawImageString(dpy,win,(XFontSet)font,gc,x,y,str,n);


#define XDRAWLINE(x1,y1,x2,y2)  XDrawLine(dpy,win,gc,x1,y1,x2,y2)

#define SETLSEG(X1,Y1,X2,Y2) \
    { lsegp->x1=X1; lsegp->y1=Y1; lsegp->x2=X2; lsegp->y2=Y2; lsegp++; }


/*  Draw a string when the font width is proportional or less
    than FontWidth(w)
*/
static void
DrawString1By1(dpy, win, gc, x, y, string, nchars, font, fwidth)
  Display *dpy;
  Window win;
  GC gc;
  int x, y;
  uchar *string;
  int nchars;
  XFontStruct *font;
  int fwidth;
  { XTextItem items[MAX_COLS], *it = items, *itlim = it+MAX_COLS;
    XCharStruct *perchar = font->per_char, *pc = &font->max_bounds;
    int xoffs = 0, diff, leftsp, rightsp = 0;
    unsigned c, min_char = font->min_char_or_byte2,
                max_char = font->max_char_or_byte2;

    while (nchars--)
      { c = *string;
        if (perchar)
          { if (c > max_char || c < min_char) c = font->default_char;
            pc = &perchar[c-min_char];
          }

        diff = pc->rbearing - pc->lbearing;     /* ink.width */
        if (diff == 0
        /* some fonts seem to have the wrong rbearing for space */
         || (c == SPACECHAR && diff == 1))
            /* space */
            rightsp += fwidth;
        else
          { diff = fwidth - pc->width;
            leftsp = diff >> 1;
            it->delta = rightsp + leftsp;
            rightsp = diff - leftsp;
            if (it->delta != 0 || it == items)
              { it->chars = (char *)string;
                it->nchars = 1;
                it->font = None;
                it++;
              }
            else
                it[-1].nchars += 1;
          }

        string++;
        xoffs += fwidth;

        if (it == itlim || nchars == 0)
          { diff = it-items;
            if (diff != 0) XDrawText(dpy, win, gc, x, y, items, diff);
            it = items;
            x += xoffs;
            xoffs = rightsp = 0;
          }
      }
  }

static void
wcDrawString1By1(dpy, win, gc, x, y, string, nchars, fontset, fwidth)
  Display *dpy;
  Window win;
  GC gc;
  int x, y;
  wchar_t *string;
  int nchars;
  XFontSet fontset;
  int fwidth;
  { XwcTextItem items[MAX_COLS], *it = items, *itlim = it+MAX_COLS;
    XRectangle ink, logical;
    int xoffs = 0, diff, leftsp, rightsp = 0;
    uint c;

    while (nchars--)
      { XwcTextExtents(fontset, string, 1, &ink, &logical);
        if (ink.width == 0
        /* some fonts seem to have the wrong rbearing for space */
         || (*string == SPACECHAR && ink.width == 1))
            /* space */
            rightsp += fwidth;
        else
          { diff = fwidth - logical.width;
            leftsp = diff >> 1;
            it->delta = rightsp + leftsp;
            rightsp = diff - leftsp;
            if (it->delta != 0 || it == items)
              { it->chars = string;
                it->nchars = 1;
                it->font_set = (it == items) ? fontset : None;
                it++;
              }
            else
                it[-1].nchars += 1;
          }

        string++;
        xoffs += fwidth;

#ifndef __NUTC__    /* NUTC XwcDrawText doesn't work with multiple items */
        if (it == itlim || nchars == 0)
#endif
          { diff = it-items;
            if (diff != 0) XwcDrawText(dpy, win, gc, x, y, items, diff);
            it = items;
            x += xoffs;
            xoffs = rightsp = 0;
          }
      }
  }

static void
ucDrawString1By1(dpy, win, gc, x, y, string, nchars, fontset, fwidth)
  Display *dpy;
  Window win;
  GC gc;
  int x, y;
  ushort *string;
  int nchars;
  XFontSet fontset;
  int fwidth;
  { char mbout[MAX_COLS*4+1];
    wchar_t wcout[MAX_COLS+1];
    uint nbytes = (*XpwUcToMb)(string, nchars, mbout, MAX_COLS*4);
    mbout[nbytes] = 0;
    nchars = mbstowcs(wcout, mbout, MAX_COLS);
    wcDrawString1By1(dpy, win, gc, x, y, wcout, nchars, fontset, fwidth);
  }

static int XucTextEscapement(fontset, string, nchars)
  XFontSet fontset;
  ushort *string;
  uint nchars;
  { uchar mbout[MAX_COLS*4];
    uint nbytes = (*XpwUcToMb)(string, nchars, mbout, MAX_COLS*4);
    return(XmbTextEscapement(fontset, mbout, nbytes));
  }


#define UC_DRAWSTRING(Name, Draw)                               \
static void Name(dpy, win, fontset, gc, x, y, string, nchars)   \
  Display *dpy;                                                 \
  Window win;                                                   \
  XFontSet fontset;                                             \
  GC gc;                                                        \
  int x, y;                                                     \
  ushort *string;                                               \
  uint nchars;                                                  \
  { uchar mbout[MAX_COLS*4];                                    \
    uint nbytes = (*XpwUcToMb)(string, nchars, mbout, MAX_COLS*4);  \
    Draw(dpy, win, fontset, gc, x, y, mbout, nbytes);           \
  }
UC_DRAWSTRING(XucDrawString, XmbDrawString)
UC_DRAWSTRING(XucDrawImageString, XmbDrawImageString)


/*
 *  Draw Cursors
 */
static void
DrawCursor(w, gc, xleft, ybase, cchar, cwidth, bold)
  XpwScrollTextWidget w;
  GC gc;
  int xleft, ybase;
  uchar cchar;
  int cwidth;
  Boolean bold;
  {
    Display *dpy = XtDisplay(w);
    Window win = XtWindow(w);
    int fheight = FontHeight(w),
        fbaseline = FontBaseline(w), ytop = ybase - fbaseline,
        xmid = xleft+(cwidth-1)/2, ymid = ytop+fheight/2,
        xright = xleft+cwidth-1, ybot = ytop+fheight-1, h
        ;
    XSegment lseg[6], *lsegp;
    XPoint pt[5];

    switch (cchar)
      { case ' ':               /* Transparent */
            break;

        case 'O':               /* Rect */
        default:
            XDrawRectangle(dpy, win, gc, xleft, ytop, cwidth-1, fheight-1);
            if (bold)
                XFillRectangle(dpy, win, gc, xleft, ytop, cwidth, fheight);
            break;

        case 'I':               /* I beam */
            lsegp = lseg;
            SETLSEG(xleft, ytop, xright, ytop);
            SETLSEG(xmid, ytop+1, xmid, ybot-1);
            SETLSEG(xleft, ybot, xright, ybot);
            if (bold)
              { SETLSEG(xleft, ytop+1, xright, ytop+1);
                SETLSEG(xmid+1, ytop+2, xmid+1, ybot-2);
                SETLSEG(xleft, ybot-1, xright, ybot-1);
              }
            XDrawSegments(dpy, win, gc, lseg, lsegp-lseg);
            break;

        case '^':               /* carat */
            pt[0].x = xright; pt[2].x = xleft;
            pt[0].y = pt[2].y = ybot;

            pt[1].x = xmid;
            pt[1].y = ybase-(fbaseline/3);

            pt[3].x = pt[0].x; pt[3].y = pt[0].y;
            XDrawLines(dpy, win, gc, pt, 4, CoordModeOrigin);
            if (bold)
                XFillPolygon(dpy, win, gc, pt, 3, Convex, CoordModeOrigin);
            break;

        case '_':               /* underscore */
            h = ybot-ybase+1;
            XDrawRectangle(dpy, win, gc, xleft, ybase, cwidth-1, h-1);
            if (bold)
                XFillRectangle(dpy, win, gc, xleft, ybase, cwidth, h);
            break;
      }
  }

/*  Draw graphics characters as codes 0-31 (vt100 encoding). Only the
    line-drawing ones and a few others are currently implemented.
*/
static int
DrawGraphString(w, gc, xleft, ybase, string, nchars, bold, font, fontmode,
                                        var_width_mode)
  XpwScrollTextWidget w;
  GC gc;
  int xleft, ybase;
  uchar *string;
  uint nchars;
  Boolean bold, var_width_mode;
  XFontStruct *font;
  int fontmode;
  {
    Display *dpy = XtDisplay(w);
    Window win = XtWindow(w);
    int fwidth = FontWidth(w), fheight = FontHeight(w),
        fbaseline = FontBaseline(w), ytop = ybase - fbaseline,
        xmid = xleft+(fwidth-1)/2, ymid = ytop+fheight/2,
        xright = xleft+fwidth-1, ybot = ytop+fheight-1,
        d, r, h, v, start_xleft = xleft;
    register uint charmode = CharMode(w);
    XSegment lseg[6], *lsegp;
    XPoint pt[5];


    while (nchars--)
      { int xstep = fwidth;
        uint c;
        if (charmode < XpwICMUnicode)
            c = *string++;
        else if (charmode == XpwICMUnicode)
          { c = *(ushort*)string; string = (uchar*)(((ushort*)string)+1); }
        else
          { c = *(wchar_t*)string; string = (uchar*)(((wchar_t*)string)+1); }

        switch (c)
          { case 0:
                break;

            case 0x01:              /* diamond */
                if (var_width_mode) xstep += xstep/2;
                if (fontmode < 0) break;
                h = (fbaseline*5)/8; if (!(h&1)) h++;

                pt[0].x = pt[2].x = xleft+(xstep-1)/2;
                pt[0].y = ybase-1;  pt[2].y = ybase-h;

                pt[3].x = xleft+(xstep&1);
                pt[1].x = xleft+xstep-2;
                pt[1].y = pt[3].y = ybase-1-h/2;

                pt[4].x = pt[0].x; pt[4].y = pt[0].y;
                XDrawLines(dpy, win, gc, pt, 5, CoordModeOrigin);
                XFillPolygon(dpy, win, gc, pt, 4, Convex, CoordModeOrigin);
                break;

            case 0x02:              /* checkerboard */
            case 0x03:              /* HT */
            case 0x04:              /* FF */
            case 0x05:              /* CR */
            case 0x06:              /* LF */
                break;

            case 0x07:              /* degree */
                if (fontmode < 0) break;
                d = (fwidth*5)/8;  r = d/2;
                v = xmid-r;
                h = fwidth; if (bold) h += 8;
                for (; h>0; h -= 8)
                  { XDrawArc(dpy, win, gc, v, ytop+fbaseline/3-r, d, d, 0, 360*64);
                    v++;
                  }
                break;

            case 0x08:              /* plus or minus */
                /* used for 1-pixel space in var width mode */
                if (var_width_mode) xstep = 1;
                break;

            case 0x09:              /* NL */
            case 0x0a:              /* VT */
                break;

            case 0x0b:              /* bottom-right ell */
                h = 1; v = 1; goto hvlines;

            case 0x0c:              /* top-right ell */
                h = 1; v = 2; goto hvlines;

            case 0x0d:              /* top-left ell */
                h = 2; v = 2; goto hvlines;

            case 0x0e:              /* bottom-left ell */
                h = 2; v = 1; goto hvlines;

            case 0x0f:              /* cross */
                h = 3; v = 3; goto hvlines;

            case 0x10:              /* horizontal line 1 */
                d = -2;  goto hline;

            case 0x11:              /* horizontal line 2 */
                d = -1;  goto hline;

            case 0x12:              /* horizontal line 3 (middle) */
                d = 0;  goto hline;

            case 0x13:              /* horizontal line 4 */
                d = 1;  goto hline;

            case 0x14:              /* horizontal line 5 */
                d = 2;  goto hline;

            case 0x15:              /* tee left */
                h = 2; v = 3; goto hvlines;

            case 0x16:              /* tee right */
                h = 1; v = 3; goto hvlines;

            case 0x17:              /* tee down */
                h = 3; v = 1; goto hvlines;

            case 0x18:              /* tee up */
                h = 3; v = 2; goto hvlines;

            case 0x19:              /* vertical line */
                h = 0; v = 3; goto hvlines;

            case 0x1a:              /* <= */
            case 0x1b:              /* >= */
            case 0x1c:              /* pi */
            case 0x1d:              /* /= */
            case 0x1e:              /* pound sign */
                break;

            case 0x1f:              /* centred dot */
                if (fontmode < 0) break;
                d = (fwidth+2)/4 - 1;
                d = max(1, d);
                h = xmid - d/2;
                if (!((fwidth|d) & 1)) h++;
                v = ybase-fbaseline/2+1;
                XDrawArc(dpy, win, gc, h, v, d, d, 0, 360*64);
                XFillArc(dpy, win, gc, h, v, d, d, 0, 360*64);
                break;

            default:
                break;

            hline:
                if (fontmode < 0) break;
                lsegp = lseg;
                d = ymid + d*((fheight+2)/5);
                SETLSEG(xleft, d, xright, d);
                if (bold) SETLSEG(xleft, d-1, xright, d-1);
                XDrawSegments(dpy, win, gc, lseg, lsegp-lseg);
                break;

            hvlines:
                if (fontmode < 0) break;
                lsegp = lseg;
                if (h != 0)
                  { int x1 = h==2?xmid:xleft, x2 = h==1?xmid:xright;
                    SETLSEG(x1, ymid, x2, ymid);
                    if (bold) SETLSEG(x1, ymid-1, x2, ymid-1);
                  }
                if (v != 0)
                  { int y1 = v==2?ymid:ytop, y2 = v==1?ymid:ybot;
                    SETLSEG(xmid, y1, xmid, y2);
                    if (bold) SETLSEG(xmid+1, y1, xmid+1, y2);
                  }
                XDrawSegments(dpy, win, gc, lseg, lsegp-lseg);
                break;
          }

        xleft += xstep; xmid += xstep; xright += xstep;
      }

    return(xleft - start_xleft);    /* return width of chars */
  }

#define DS_INVERT       1
#define DS_ISCURSOR     2
#define DS_NO_DRAW      4
#define DS_X_LIM        8

#define ACT_LEAVE_NOTIFY    1
#define ACT_INVERT          2
#define ACT_BOX             4
#define ACT_POINTER2        8

static Coord
DrawString(w, string, x, col, row, nchars, attrs, flags, cursorchar)
  XpwScrollTextWidget w;
  uchar *string;
  Coord x;
  uint col, row, nchars, attrs, flags, cursorchar;
  {
    Display
        *dpy = XtDisplay(w);
    Window
        win = XtWindow(w);
    int
        y = RowToYCoord(w,row),
        ybase = y + FontBaseline(w),
        start_x = x, indexlim, gcv_mask,
        colornum, startcol = col, fontmode;
    XRectangle
        clip_rect, *clip = NULL;
    uint
        gc_index = attrs & ATTR_GC_MASK, fontnum = gc_index%NFONTS,
        actmode = 0, charmode, fwidth = FontWidth(w), fheight = FontHeight(w);
    Boolean
        bold = False, overstrike = False,
        draw_graph = w->xpwscrolltext.draw_graphic_chars,
        invert = flags & DS_INVERT,
        draw_shadow = False, var_width_mode;
    MyGC
        *gc_array = w->xpwscrolltext.gc_array, mygc;
    GC
        gc;
    XGCValues
        gcv;
    Pixel
        shad_fg, shad_bg;
    XFontStruct
        *font;

    if (nchars == 0) return(x);

    if (attrs & ATTR_ACTIVE)
      { if (w == onactive_widget && row == onactive_row
         && onactive_start_col <= startcol && startcol < onactive_lim_col)
            actmode = onactive_mode;
        /* treat as colors 8 - 15 except when cursor color */
        if (!(flags & DS_ISCURSOR)) gc_index |= ATTR_ACTIVE;
      }

    colornum = gc_index/NFONTS;

    if (w->xpwscrolltext.draw_shadow_mask & (1<<colornum))
      { if (Flags(w) & MONOCHROME)
            /* use underline instead */
            attrs |= ATTR_UNDERLINE;
        else
          { unsigned s = row==0 ? 0 : 1;
            draw_shadow = True;
            gc_index -= fontnum; fontnum &= ~BOLDFONTBIT; gc_index += fontnum;
            shad_fg = BlackPixelOfScreen(XtScreen(w));
            shad_bg = w->xpwscrolltext.shadow_whites[s];
            if (w->xpwscrolltext.shadow_white_bits[s] & (1<<colornum))
              { Pixel fg = shad_fg; shad_fg = shad_bg; shad_bg = fg; }
          }
      }

    mygc = gc_array[fontnum];

    if (fontnum & BOLDFONTBIT)
      { bold = True;
        if (!mygc.gc)
          { /* bold mode with no bold font -- use non-bold font and
                overstrike */
            overstrike = True;
            gc_index -= fontnum;
            fontnum &= ~BOLDFONTBIT;
            gc_index += fontnum;
            mygc = gc_array[fontnum];
          }
      }

    if (!mygc.gc || cursorchar)
      { /* alt font mode with no alt font -- underline instead */
        if (cursorchar) attrs &= ~ATTR_UNDERLINE;
        else if (!draw_shadow) attrs |= ATTR_UNDERLINE;
        gc_index -= fontnum;
        fontnum = NORMAL_FONT;
        gc_index += NORMAL_FONT;
      }

    /* font established */
    fontmode = (w->xpwscrolltext.font_draw_modes >> (fontnum<<1)) & 3;

    var_width_mode = VarWidthMode(w);
    if (var_width_mode)
      { /* variable width */
        if (col < NumFixedCols(w,row))
            /* but fixed width for leading fixed cols */
            var_width_mode = False;
        else
          { fontmode &= ~FDM_1BY1;  /* draw var width chars for any font */
            clip_rect.x = LeftVarTextStart(w,row);
            clip_rect.width = RightTextLim(w) - clip_rect.x;
            clip = &clip_rect;
          }
      }

    if (flags & DS_NO_DRAW)
        /* want next x only */
        fontmode = (flags & DS_X_LIM) ? FDM_NO_DRAW1 : FDM_NO_DRAW;

    else
      {
        /* get gc proper */
        if ((flags & DS_ISCURSOR) && colornum == 0)
          { /* drawing cursor in colour 0 -- substitute cursor colour */
            gc_array = (row == 0) ? &w->xpwscrolltext.status_cursor_gc
                                  : &w->xpwscrolltext.cursor_gc;
            indexlim = 1;
            colornum = CURSOR_COLORNUM;
          }
        else
          { if (row == 0)
              { gc_array = w->xpwscrolltext.status_gc_array;
                indexlim = NSTATUSGCS;
              }
            else
                indexlim = NNORMALGCS;
          }

        /* This expression is too complicated for NCR's C compiler...
            mygc = gc_index < indexlim ? gc_array[gc_index]
                                : _XpwGetWorkTextGC(w, colornum, fontnum, row==0);
        */
        if (gc_index < indexlim)
            mygc = gc_array[gc_index];
        else
            /* use work_gc and alter it */
            mygc = _XpwGetWorkTextGC(w, colornum, fontnum, row==0);

        gc = mygc.gc;

        if (actmode & ACT_INVERT) invert = !invert;
        if (invert) mygc.flags &= ~(GC_HAS_WIN_BG|GC_HAS_BG_PIX);
        else if (mygc.flags & GC_HAS_BG_PIX) fontmode |= FDM_CLEAR_BG;

        if ((attrs & ATTR_BLINK) && !w->xpwscrolltext.no_blink)
          { /* set these values for DoBlink */
            blink_row_start_col = min(col, blink_row_start_col);
            blink_row_lim_col = max(col+nchars, blink_row_lim_col);
            if (!blink_on_phase)
                /* blinking chars and currently off -- just clear them */
                fontmode = FDM_CLEAR_ONLY;
          }

        if (invert || draw_shadow)
          { Pixel fg;
            gcv_mask = GCForeground | GCBackground;
            XGetGCValues(dpy, gc, gcv_mask, &gcv);
            if (invert)
              { fg = gcv.foreground;
                gcv.foreground = gcv.background; gcv.background = fg;
                XChangeGC(dpy, gc, gcv_mask, &gcv);
                fg = shad_fg; shad_fg = shad_bg; shad_bg = fg;
              }
          }

        if (!string) fontmode = FDM_CLEAR_ONLY;

        if (var_width_mode && fontmode >= 0)
          { /* set clip mask on gc */
            clip_rect.y = y;
            clip_rect.height = fheight;
            XSetClipRectangles(dpy, gc, 0, 0, clip, 1, YXBanded);
          }
      }

    /* draw the characters */

#define CLEARRECT(width)    \
        ClearRectangle(dpy, win, mygc, clip, x, y, width, fheight)

#define DRAWCURSOR          \
        DrawCursor(w, gc, x, ybase, cursorchar, twidth, bold)

#define DRAWGRAPHSTRING(xleft, ybase, fontmode) \
        DrawGraphString(w, gc, xleft, ybase, (uchar*)pstart, n, bold, font, \
                                            fontmode, var_width_mode)

#define DRAWLOOP(TEXTWIDTH, DRAWSTRING, DRAWIMAGESTRING)                    \
    while (p < plim)                                                        \
      { int n, twidth;                                                      \
        tmplim = plim;                                                      \
        if (fontmode == FDM_NO_DRAW1)                                       \
          { if (x >= draw_lim_x)                                            \
              { draw_lim_col = (x == draw_lim_x) ? col : col-1;             \
                return(x);                                                  \
              }                                                             \
            tmplim = p+1;   /* do 1 char only */                            \
          }                                                                 \
                                                                            \
        pstart = p;                                                         \
        if (draw_graph)                                                     \
            do if (*p++ < 0x20) { --p; break; } while (p < tmplim);         \
        else                                                                \
            p = tmplim;                                                     \
                                                                            \
        if (n = p - pstart)                                                 \
          { /* some non-graphics chars */                                   \
            twidth = var_width_mode ? TEXTWIDTH(pstart,n) : n*fwidth;       \
            if (cursorchar)                                                 \
                DRAWCURSOR;                                                 \
            else if (fontmode >= 0)                                         \
              { Boolean draw = True;                                        \
                if (draw_shadow)                                            \
                  { CLEARRECT(twidth);                                      \
                    XSetForeground(dpy, gc, shad_fg);                       \
                    DRAWSTRING(x+1, ybase+1, pstart, n);                    \
                    XSetForeground(dpy, gc, gcv.foreground);                \
                  }                                                         \
                else                                                        \
                  { if (fontmode != 0)                                      \
                        CLEARRECT(twidth);                                  \
                    else                                                    \
                      { DRAWIMAGESTRING(x, ybase, pstart, n);               \
                        draw = False;                                       \
                      }                                                     \
                  }                                                         \
                if (draw) { DRAWSTRING(x, ybase, pstart, n); }              \
                if (overstrike) { DRAWSTRING(x+1, ybase, pstart, n); }      \
              }                                                             \
            x += twidth;                                                    \
            last_text_width = twidth;                                       \
            col += n;                                                       \
          }                                                                 \
                                                                            \
        if (p < tmplim)                                                     \
          { /* some graphics chars */                                       \
            pstart = p++;                                                   \
            while (p < tmplim) if (*p++ >= 0x20) { --p; break; };           \
            n = p - pstart;                                                 \
            twidth = var_width_mode ? DRAWGRAPHSTRING(x, ybase, FDM_NO_DRAW)\
                                    : n*fwidth;                             \
            if (cursorchar)                                                 \
                DRAWCURSOR;                                                 \
            else if (fontmode >= 0)                                         \
              { CLEARRECT(twidth);                                          \
                if (draw_shadow)                                            \
                  { XSetForeground(dpy, gc, shad_fg);                       \
                    DRAWGRAPHSTRING(x+1, ybase+1, fontmode);                \
                    XSetForeground(dpy, gc, gcv.foreground);                \
                  }                                                         \
                DRAWGRAPHSTRING(x, ybase, fontmode);                        \
              }                                                             \
            x += twidth;                                                    \
            last_text_width = twidth;                                       \
            col += n;                                                       \
          }                                                                 \
      }

#define SVGCFONT_DRAWLOOP(TEXTWIDTH, DRAWSTRING, DRAWIMAGESTRING)           \
    if (fontmode >= 0 && !cursorchar)                                       \
      { /* save and restore the gc font member */                           \
        XGetGCValues(dpy, gc, GCFont, &gcv);                                \
        fid = gcv.font;                                                     \
      }                                                                     \
    DRAWLOOP(TEXTWIDTH, DRAWSTRING, DRAWIMAGESTRING);                       \
    if (fontmode >= 0 && !cursorchar)                                       \
      { XGetGCValues(dpy, gc, GCFont, &gcv);                                \
        if (gcv.font != fid) XSetFont(dpy, gc, fid);                        \
      }

    if (string)
      { charmode = CharMode(w);
        if (charmode < XpwICMUnicode)
          { register uchar *p = string, *pstart, *tmplim;
            uchar *plim = p+nchars;
            font = FontStruct(w,fontnum);
            DRAWLOOP(XTEXTWIDTH, XDRAWSTRING, XDRAWIMAGESTRING)
          }
        else if (charmode == XpwICMUnicode)
          { register ushort *p = (ushort *)string, *pstart, *tmplim;
            ushort *plim = p+nchars;
            Font fid;
            font = (XFontStruct*) FontSet(w,fontnum);
            SVGCFONT_DRAWLOOP(XucTEXTWIDTH, XucDRAWSTRING, XucDRAWIMAGESTRING);
          }
        else
          { register wchar_t *p = (wchar_t *)string, *pstart, *tmplim;
            wchar_t *plim = p+nchars;
            Font fid;
            font = (XFontStruct*) FontSet(w,fontnum);
            SVGCFONT_DRAWLOOP(XwcTEXTWIDTH, XwcDRAWSTRING, XwcDRAWIMAGESTRING);
          }
      }
    else
      { int cols, swidth = var_width_mode ? SpaceWidth(w) : fwidth;
        last_text_width = swidth;
        if (fontmode == FDM_NO_DRAW1
         && nchars > (cols = (draw_lim_x-x) / swidth))
          { draw_lim_col = col + cols;
            x += cols * swidth;
            return(x == draw_lim_x ? x : x+swidth);
          }
        else
            x += nchars * swidth;
      }


    if (fontmode <= FDM_NO_DRAW)
      { if (fontmode == FDM_NO_DRAW1 && x > draw_lim_x) draw_lim_col = col-1;
        return(x);  /* want next x only */
      }

    if (fontmode == FDM_CLEAR_ONLY)
        ClearRectangle(dpy, win, mygc, clip, start_x, y, x-start_x, fheight);
    else if (attrs & ATTR_UNDERLINE)
        /* underline */
        XDRAWLINE(start_x, ybase+1, x-1, ybase+1);

    if (actmode & ACT_BOX)
      { int ybot = y+fheight-1, xright = x-1;
        XSegment lseg[4], *lsegp = lseg;
        SETLSEG(start_x, y,    xright, y);
        SETLSEG(start_x, ybot, xright, ybot);
        if (startcol == onactive_start_col) SETLSEG(start_x, y, start_x, ybot);
        if (col == onactive_lim_col)        SETLSEG(xright,  y, xright,  ybot);
        XDrawSegments(dpy, win, gc, lseg, lsegp-lseg);
      }

    if (invert)
      { Pixel fg = gcv.background;
        gcv.background = gcv.foreground; gcv.foreground = fg;
        XChangeGC(dpy, gc, gcv_mask, &gcv);
      }

    if (var_width_mode && fontmode >= 0) XSetClipMask(dpy, gc, None);

    /* return next x */
    return(x);
  }         /* DrawString */

static Coord DrawSpaces(w, x, col, row, nchars)
  XpwScrollTextWidget w;
  Coord x;
  uint col, row, nchars;
  {
    Display *dpy = XtDisplay(w);
    Window win = XtWindow(w);
    int start_x = x;
    XRectangle clip_rect, *clip = NULL;
    uint swidth = FontWidth(w);
    MyGC mygc;

    if (nchars == 0) return(x);

    if (VarWidthMode(w) && col >= NumFixedCols(w,row))
      { /* variable width */
        swidth = SpaceWidth(w);
        clip_rect.x = LeftVarTextStart(w,row);
        clip_rect.width = RightTextLim(w) - clip_rect.x;
        clip = &clip_rect;
      }
    x += nchars * swidth;

    mygc = (row == 0) ? Font0StatusMyGC(w) : Font0NormalMyGC(w);
    ClearRectangle(dpy, win, mygc, clip, start_x, RowToYCoord(w,row),
                                                x-start_x, FontHeight(w));
    /* return next x */
    return(x);
  }         /* DrawSpaces */


static Coord DrawRowSequences(w, x, scol, lcol, row, attr_mask, ds_flags)
  XpwScrollTextWidget w;
  int x;
  uint scol, lcol, row, attr_mask, ds_flags;
  {
    WinRowB rptr = RowArray(w)[row];
    register uint wc, attrs = 0, clcol;
    register int len = 0;
    uint csz_code = CharSizeCode(w), sp_width = FontWidth(w),
         fixed_cols = NumFixedCols(w,row), col;

    if (VarWidthMode(w))
      { if (scol < fixed_cols && fixed_cols < lcol)
          { x = DrawRowSequences(w, x, scol, fixed_cols, row, attr_mask,
                                                                ds_flags);
            if (ds_flags&DS_X_LIM && draw_lim_col >= 0) return(x);
            scol = fixed_cols;
          }
        if (scol >= fixed_cols) sp_width = SpaceWidth(w);
      }

    col = scol;
    clcol = min(NCHARS(rptr), lcol);


#define DRAW_ROW_SEQS(RPCHAR)                                               \
    while (rp <= rplim)                                                     \
      { if (rp >= rpclim || (wc = RPCHAR) == 0 || rp->attrs != attrs)       \
          { if (len != 0)                                                   \
              { uint dsf = ds_flags;                                        \
                if (attr_mask!=0 && !(attrs&attr_mask)) dsf |= DS_NO_DRAW;  \
                x = DrawString(w, (uchar *)tmpstring, x, col, row, len,     \
                                                        attrs, dsf, 0);     \
                if (ds_flags&DS_X_LIM && draw_lim_col >= 0) return(x);      \
                col += len;                                                 \
                outp = tmpstring;                                           \
                len = 0;                                                    \
              }                                                             \
            if (rp >= rpclim)                                               \
              { if (rp == rplim) break;                                     \
                wc = 0;                                                     \
              }                                                             \
            else                                                            \
                attrs = rp->attrs;                                          \
            if (wc == 0)                                                    \
              { if (ds_flags&DS_X_LIM && x >= draw_lim_x)                   \
                  { draw_lim_col = (x == draw_lim_x) ? col : col-1;         \
                    return(x);                                              \
                  }                                                         \
                col++; rp++; x += sp_width;                                 \
                last_text_width = sp_width;                                 \
                continue;                                                   \
              }                                                             \
          }                                                                 \
                                                                            \
        *outp++ = wc; len++; rp++;                                          \
      }

    if (csz_code == CSZ_BYTE)
      { uchar tmpstring[MAX_COLS];
        register uchar *outp = tmpstring;
        register WinRowB rp = rptr+scol, rpclim = rptr+clcol,
                    rplim = rptr+lcol;
        DRAW_ROW_SEQS(rp->c0)
      }
    else if (csz_code == CSZ_SHORT)
      { ushort tmpstring[MAX_COLS];
        register ushort *outp = tmpstring;
        register WinRowS rp = ((WinRowS)rptr)+scol,
            rpclim = ((WinRowS)rptr)+clcol, rplim = ((WinRowS)rptr)+lcol;
        DRAW_ROW_SEQS(rp->c0 + (rp->c1<<8))
      }
    else
      { uint tmpstring[MAX_COLS];
        register uint *outp = tmpstring;
        register WinRowI rp = ((WinRowI)rptr)+scol,
            rpclim = ((WinRowI)rptr)+clcol, rplim = ((WinRowI)rptr)+lcol;
        DRAW_ROW_SEQS(rp->s0 + (rp->s1<<16))
      }

    if (ds_flags&DS_X_LIM && x > draw_lim_x)
        draw_lim_col = col-1;
    else if (VarWidthMode(w) && lcol == fixed_cols)
        x += PIXOFFS(rptr);

    return(x);
  }

static Coord ColToXCoord(w, col, row)
  XpwScrollTextWidget w;
  uint col, row;
  { if (VarWidthMode(w))
        return(DrawRowSequences(w, Col0Offset(w), 0, col, row, 0, DS_NO_DRAW));
    else
        return(Col0Offset(w) + col*FontWidth(w));
  }

static Coord GetCursorX(w)
  XpwScrollTextWidget w;
  { Coord x;
    if ((x = CursorX(w)) == CURSOR_X_UNDEF)
        x = CursorX(w) = ColToXCoord(w, CursorCol(w), CursorRow(w));
    return(x);
  }

int _XpwXCoordToCol(w, x, row)
  XpwScrollTextWidget w;
  Coord x;
  uint row;
  { Coord col0offs = Col0Offset(w);
    if (!VarWidthMode(w) || x < col0offs)
      { int fwidth = FontWidth(w);
        x -= Col0Offset(w);
        if (x < 0) x -= fwidth-1;
        return(x / fwidth);
      }
    else if (0 <= row && row < NumRows(w))
      { uint nchars = NCHARS(RowArray(w)[row]);
        Coord endx;
        draw_lim_x = x;
        draw_lim_col = -1;
        endx = DrawRowSequences(w, col0offs, 0, nchars, row, 0,
                                                    DS_NO_DRAW|DS_X_LIM);
        return(draw_lim_col >= 0 ? draw_lim_col
                                 : nchars + (x-endx)/SpaceWidth(w));
      }
    else
        return((x-col0offs) / SpaceWidth(w));
  }

static int AreaCoordToRow(w, y, in_status)
  XpwScrollTextWidget w;
  Coord y;
  Boolean in_status;
  { int fheight = FontHeight(w), row;
    if (in_status)
      { y -= Row0Offset(w);
        if (y < 0) y -= fheight-1;
        row = y/fheight;
        return(min(1,row));
      }
    else
      { row = (y-Row1Offset(w))/fheight;
        return(max(0,row)+1);
      }
  }

int _XpwYCoordToRow(w, y)
  XpwScrollTextWidget w;
  Coord y;
  { int row;
    Boolean in_status = (y < SplitOffset(w));
    if (StatusStyle(w) == 2) in_status = !in_status;
    row = AreaCoordToRow(w, y, in_status);
    return(in_status ? min(0,row) : row);
  }

/*  Deal with expose events -- used by ScrollTextRedisplay
 */
void _XpwTextExpose(w, x, y, xl, yl, in_status)
  XpwScrollTextWidget w;
  Coord x, y, xl, yl;
  Boolean in_status;
  {
    int fwidth = FontWidth(w), fheight = FontHeight(w),
        cursor_row = CursorRow(w), cursor_col = CursorCol(w),
        srow, lrow, scol, lcol;

    if (in_status)
      { /* ensure status row cleared to correct background */
        GC gc = Font0StatusMyGC(w).gc;
        Display *dpy = XtDisplay(w);
        Window win = XtWindow(w);
        XRectangle expose;
        XGCValues gcv;
        Coord col0offs = Col0Offset(w);
        int bx = col0offs-2, by = Row0Offset(w)-2,
            bwidth = RightTextLim(w)-col0offs+4, bheight = fheight+4;

        expose.x = x; expose.width = xl-x;
        expose.y = y; expose.height = yl-y;
        XSetClipRectangles(dpy, gc, 0, 0, &expose, 1, Unsorted);
        XGetGCValues(dpy, gc, GCForeground|GCBackground, &gcv);
        XSetForeground(dpy, gc, gcv.background);

        if (StatusStyle(w) == 0)
            XFillRectangle(dpy, win, gc, bx+2, by+2, bwidth-4, bheight-4);
        else
          { XFillRectangle(dpy, win, gc, bx+1, by+1, bwidth-2, bheight-2);
            XSetForeground(dpy, gc, w->core.border_pixel);
            XDrawRectangle(dpy, win, gc, bx, by, bwidth-1, bheight-1);
          }

        XSetForeground(dpy, gc, gcv.foreground);
        XSetClipMask(dpy, gc, None);
      }

    srow = AreaCoordToRow(w, y, in_status);
    srow = max(0, srow);
    lrow = AreaCoordToRow(w, yl+fheight-1, in_status);
    lrow = min(NumRows(w), lrow);

    while (srow < lrow)
      { scol = _XpwXCoordToCol(w, x, srow);
        scol = max(0, scol);
        lcol = _XpwXCoordToCol(w, xl+fwidth-1, srow);

        if (scol < lcol)
          { Boolean DoCursor = False, save_cursor;
            if (srow == cursor_row && scol <= cursor_col && cursor_col < lcol)
              { DoCursor = True; save_cursor = _XpwTextCursorOn(w, False); }
            DrawRowSequences(w, ColToXCoord(w,scol,srow), scol, lcol, srow,
                                                                0, 0);
            if (DoCursor) _XpwTextCursorOn(w, save_cursor);
          }
        srow++;
      }
  }

static void DrawRowChars(w, x, scol, lcol, row)
  XpwScrollTextWidget w;
  int x;
  uint scol, lcol, row;
  { int cursor_col = CursorCol(w);
    if (row == CursorRow(w) && scol <= cursor_col && cursor_col < lcol)
      { Boolean save_cursor = _XpwTextCursorOn(w, False);
        DrawRowSequences(w, x, scol, lcol, row, 0, 0);
        _XpwTextCursorOn(w, save_cursor);
      }
    else
        DrawRowSequences(w, x, scol, lcol, row, 0, 0);
  }

static void ClearOnActive(w)
  XpwScrollTextWidget w;
  { onactive_widget = NULL;
    if (onactive_mode & (ACT_INVERT|ACT_BOX))
        DrawRowChars(w, onactive_start_x, onactive_start_col, onactive_lim_col,
                                            onactive_row);
    if (onactive_mode & ACT_POINTER2)
      { Display *dpy = XtDisplay(w); Window win = XtWindow(w);
        if (w->xpwcore.pointer_shape)
            XDefineCursor(dpy, win, w->xpwcore.pointer_shape);
        else
            XUndefineCursor(dpy, win);
      }
  }

void _XpwTextTestActiveChange(w)
  XpwScrollTextWidget w;
  {
    int mcol = w->xpwscrolltext.mouse.column,
        mrow = w->xpwscrolltext.mouse.row,
        nchars, num_rows = NumRows(w);
    Boolean set_new = False;
    register WinRowB rp; register WinRowS rpS; register WinRowI rpI;
    XpwScrollTextWidget onact_w = onactive_widget;
    uint csz_code = CharSizeCode(w), attrs;

    if (0 <= mrow && mrow < num_rows)
      { rp = RowArray(w)[mrow];
        nchars = NCHARS(rp);
        if (0 <= mcol && mcol < nchars)
          { if (csz_code == CSZ_BYTE)
                attrs = rp[mcol].attrs;
            else if (csz_code == CSZ_SHORT)
                attrs = (rpS = (WinRowS)rp)[mcol].attrs;
            else
                attrs = (rpI = (WinRowI)rp)[mcol].attrs;

            if (attrs & ATTR_ACTIVE)
              { /* on active char */
                if (w == onact_w && mrow == onactive_row
                 && onactive_start_col <= mcol && mcol < onactive_lim_col)
                    /* within current on-active segment */
                    return;
                else
                    set_new = True;
              }
          }
      }

    onactive_widget = NULL;

    if (w == onact_w && onactive_row < num_rows)
      { /* clear current on-active segment */
        ClearOnActive(w);
        if (onactive_mode & ACT_LEAVE_NOTIFY)
          { w->xpwscrolltext.mouse.column = onactive_start_col;
            w->xpwscrolltext.mouse.row = onactive_row;
            XtCallCallbacks((Widget)w, XtNactiveChange, (caddr_t)NULL);
          }
      }

    if (set_new)
      { /* set new on-active segment -- determine its extent */
        register int col = mcol, lcol = col+1;
        if (csz_code == CSZ_BYTE)
          { do col--; while (0 <= col && (rp[col].attrs & ATTR_ACTIVE));
            while (lcol < nchars && (rp[lcol].attrs & ATTR_ACTIVE)) lcol++;
          }
        else if (csz_code == CSZ_SHORT)
          { do col--; while (0 <= col && (rpS[col].attrs & ATTR_ACTIVE));
            while (lcol < nchars && (rpS[lcol].attrs & ATTR_ACTIVE)) lcol++;
          }
        else
          { do col--; while (0 <= col && (rpI[col].attrs & ATTR_ACTIVE));
            while (lcol < nchars && (rpI[lcol].attrs & ATTR_ACTIVE)) lcol++;
          }
        w->xpwscrolltext.mouse.column = mcol;
        w->xpwscrolltext.mouse.row = mrow;
        onactive_mode = 0;
        XtCallCallbacks((Widget)w, XtNactiveChange, &onactive_mode);
        if (onactive_mode != 0)
          { onactive_start_col = col = col+1;
            onactive_lim_col = lcol;
            onactive_row = mrow;
            onactive_widget = w;
            onactive_start_x = ColToXCoord(w,col,mrow);
            if (onactive_mode & (ACT_INVERT|ACT_BOX))
                DrawRowChars(w, onactive_start_x, col, lcol, mrow);
            if (onactive_mode & ACT_POINTER2)
                XDefineCursor(XtDisplay(w), XtWindow(w),
                                        w->xpwscrolltext.pointer2_shape);
          }
      }
  }


static void DoBlink();

void _XpwSetBlinkTimer()
  { XpwScrollTextWidget w = blink_widget_chain;
    if (blink_timerid || !w) return;
    blink_timerid =
        XtAppAddTimeOut(XtDisplayToApplicationContext(XtDisplay(w)),
                                                    600, DoBlink, NULL);
  }

static void DoBlink()
  { XpwScrollTextWidget w = blink_widget_chain, nextw,
                        *lastp = &blink_widget_chain;
    Boolean some_blinking = False;

    blink_timerid = (XtIntervalId)NULL;

    /* toggle blink phase */
    blink_on_phase = !blink_on_phase;

    while (w)
      { WinRowB *rowarr = RowArray(w);
        unsigned row, lim_row, new_start_row = MAX_ROWS, new_lim_row = 0;
        Boolean did_cursor = False, save_cursor, cursor_blinking, cursor_blink;

        if (w->xpwscrolltext.no_blink)
          { *lastp = w;
            w = *(lastp = &w->xpwscrolltext.next_blink_widget);
            continue;
          }

        lim_row = min(NumRows(w), w->xpwscrolltext.blink_lim_row);
        for (row = w->xpwscrolltext.blink_start_row; row < lim_row; row++)
          { WinRowB rp = rowarr[row];
            unsigned scol, lcol = BLINK_LCOL(rp);
            if (lcol == 0) continue;

            if (did_cursor = (row == CursorRow(w)))
                save_cursor = _XpwTextCursorOn(w, False);

            blink_row_start_col = MAX_COLS; blink_row_lim_col = 0;
            scol = BLINK_SCOL(rp);
            DrawRowSequences(w, ColToXCoord(w,scol,row), scol, lcol, row,
                                                            ATTR_BLINK, 0);
            BLINK_SCOL(rp) = blink_row_start_col;
            if ((BLINK_LCOL(rp) = blink_row_lim_col) != 0)
              { if (new_lim_row == 0) new_start_row = row;
                new_lim_row = row+1;
              }
          }

        cursor_blinking = ( (w->xpwscrolltext.cursor_winchar.attrs & ATTR_BLINK)
                            && (Flags(w) & HAVE_FOCUS) );
        cursor_blink = (cursor_blinking && !skip_cursor_blink);

        if (!did_cursor && (did_cursor = cursor_blink))
            save_cursor = _XpwTextCursorOn(w, False);

        if (did_cursor)
          { if (cursor_blink && save_cursor && !blink_on_phase)
                /* cursor on but don't draw it */
                Flags(w) |= CURSOR_ON;
            else
                _XpwTextCursorOn(w, save_cursor);
          }

        nextw = w->xpwscrolltext.next_blink_widget;
        w->xpwscrolltext.blink_start_row = new_start_row;
        if ((w->xpwscrolltext.blink_lim_row = new_lim_row) != 0
            || cursor_blinking)
          { *lastp = w;
            lastp = &w->xpwscrolltext.next_blink_widget;
            some_blinking = True;
          }
        else
            /* no blink rows -- says no longer in the chain */
            w->xpwscrolltext.next_blink_widget = (XpwScrollTextWidget) -1;

        w = nextw;
      }

    *lastp = NULL;
    if (some_blinking) _XpwSetBlinkTimer();
    skip_cursor_blink = False;
  }

static void EnsureBlinkWidget(w)
  XpwScrollTextWidget w;
  { if (w->xpwscrolltext.next_blink_widget == (XpwScrollTextWidget)-1)
      { w->xpwscrolltext.next_blink_widget = blink_widget_chain;
        blink_widget_chain = w;
        if (!w->xpwscrolltext.no_blink) _XpwSetBlinkTimer();
      }
  }

/* Called from XpwScrollTextDestroy */
void _XpwTextDestroy(w)
  XpwScrollTextWidget w;
  { XpwScrollTextWidget *thisp, *lastp = &blink_widget_chain, wc;

    /* remove if on-active widget */
    if (onactive_widget == w) onactive_widget = NULL;

    /* remove if blink widget */
    if (w->xpwscrolltext.next_blink_widget == (XpwScrollTextWidget)-1) return;
    while (wc = *lastp)
      { thisp = &wc->xpwscrolltext.next_blink_widget;
        if (wc == w)
          { *lastp = *thisp;
            *thisp = (XpwScrollTextWidget) -1;
            return;
          }
        else
            lastp = thisp;
      }
  }



/*  Routines for manipulating the representation of text on the window
 */

static int write_string();

/* modes for write_string */
#define WS_WRITE    0
#define WS_INSERT   1
#define WS_DELETE   2


#define WINCHAR_COPY(Name, WinChar_t)           \
static void Name(sp, dp, len)                   \
  register WinChar_t *sp, *dp;                  \
  int len;                                      \
  { register WinChar_t *slimp = sp+len;         \
    if (dp < sp)                                \
        while (sp < slimp) *dp++ = *sp++;       \
    else                                        \
      { register WinChar_t *dlimp = dp+len;     \
        while (slimp > sp) *--dlimp = *--slimp; \
      }                                         \
  }
WINCHAR_COPY(wincharB_copy, WinCharB)
WINCHAR_COPY(wincharS_copy, WinCharS)
WINCHAR_COPY(wincharI_copy, WinCharI)

#define WINCHAR_ZERO(Name, WinChar_t, Char_expr)\
static void Name(dp, len)                       \
  register WinChar_t *dp;                       \
  int len;                                      \
  { register WinChar_t *dlimp = dp+len;         \
    while (dp < dlimp)                          \
      { Char_expr = dp->attrs = 0; dp++; }      \
  }
WINCHAR_ZERO(wincharB_zero, WinCharB, dp->c0)
WINCHAR_ZERO(wincharS_zero, WinCharS, dp->c0 = dp->c1)
WINCHAR_ZERO(wincharI_zero, WinCharI, dp->s0 = dp->s1)

static void winrow_copy(sp, dp, len)
  register WinRowB *sp, *dp;
  int len;
  { register WinRowB *slimp = sp+len;
    if (dp < sp)
        while (sp < slimp) *dp++ = *sp++;
    else
      { register WinRowB *dlimp = dp+len;
        while (slimp > sp) *--dlimp = *--slimp;
      }
  }

static WinRowB expand_row(w, row, want_cols)
  XpwScrollTextWidget w;
  uint row, want_cols;
  { uint csz_code = CharSizeCode(w), WCsize, nchars;
    WinRowB *rowarr = RowArray(w), rptr = rowarr[row], new_rptr;
    char *free = (char*)rptr;
    if ((char*)rowarr <= free && free < w->xpwscrolltext.row_array_lim)
        free = NULL;
    else
        free -= sizeof(WinVRowHead);

    if (csz_code == CSZ_BYTE)
        WCsize = sizeof(WinCharB);
    else if (csz_code == CSZ_SHORT)
        WCsize = sizeof(WinCharS);
    else
        WCsize = sizeof(WinCharI);

    want_cols += want_cols/2;
    new_rptr = rowarr[row] =
        (WinRowB) (XtRealloc(free, sizeof(WinVRowHead) + WCsize*want_cols)
                                                + sizeof(WinVRowHead));
    NUMCOLS(new_rptr) = want_cols;
    if (!free)
      { /* copy it */
        nchars = NCHARS(new_rptr) = NCHARS(rptr);
        BLINK_SCOL(new_rptr) = BLINK_SCOL(rptr);
        BLINK_LCOL(new_rptr) = BLINK_LCOL(rptr);
        PIXOFFS(new_rptr) = PIXOFFS(rptr);
        COLOFFS(new_rptr) = COLOFFS(rptr);
        if (csz_code == CSZ_BYTE)
            wincharB_copy(rptr, new_rptr, nchars);
        else if (csz_code == CSZ_SHORT)
            wincharS_copy((WinRowS)rptr, (WinRowS)new_rptr, nchars);
        else
            wincharI_copy((WinRowS)rptr, (WinRowS)new_rptr, nchars);
      }
    return(new_rptr);
  }

static void clear_chars(w, scol, srow, ncols, nrows, set_end)
  XpwScrollTextWidget w;
  int scol, srow, ncols, nrows;
  Boolean set_end;
  { WinRowB *rowarr = RowArray(w), rptr;
    int nchars, lcol = scol + ncols;
    uint csz_code = CharSizeCode(w);

    while (nrows--)
      { rptr = rowarr[srow++];
        nchars = NCHARS(rptr);
        if (set_end && lcol >= nchars)
          { NCHARS(rptr) = min(scol, nchars);
            continue;
          }
        if (csz_code == CSZ_BYTE)
          { WinRowB rpB = rptr + scol;
            if (ncols == 1) rpB->c0 = rpB->attrs = 0;
            else wincharB_zero(rpB, ncols);
          }
        else if (csz_code == CSZ_SHORT)
          { WinRowS rpS = (WinRowS)rptr + scol;
            if (ncols == 1) rpS->c0 = rpS->c1 = rpS->attrs = 0;
            else wincharS_zero(rpS, ncols);
          }
        else
          { WinRowI rpI = (WinRowI)rptr + scol;
            if (ncols == 1) rpI->s0 = rpI->s1 = rpI->attrs = 0;
            else wincharI_zero(rpI, ncols);
          }
      }
  }

static void clear_text(w, scol, srow, ncols, nrows)
  XpwScrollTextWidget w;
  register int scol, srow, ncols, nrows;
  { Boolean whole_rows;

    if (ncols == 0) ncols = MAX_COLS;

    if (w == onactive_widget && srow <= onactive_row
     && onactive_row < srow+nrows)
        ClearOnActive(w);

    whole_rows = (scol == 0 && ncols == MAX_COLS);
    if (whole_rows || !VarWidthMode(w))
      { int x, dx, fheight = FontHeight(w);
        Display *dpy = XtDisplay(w);
        Window win = XtWindow(w);

        if (whole_rows)
          { x = Col0Offset(w);
            dx = RightTextLim(w);
          }
        else
          { int lcol = scol+ncols;
            x = ColToXCoord(w, scol, 0);    /* row doesn't matter */
            dx = ColToXCoord(w, min(lcol,NumCols(w)), 0);
          }
        dx -= x;

        clear_chars(w, scol, srow, ncols, nrows, True);

        if (srow == 0)
          { ClearRectangle(dpy, win, Font0StatusMyGC(w), NULL,
                                    x, Row0Offset(w), dx, fheight);
            nrows--;
            srow++;
          }
        if (nrows != 0)
            XClearArea(dpy, win, x, MainRowToYCoord(w,srow), dx, nrows*fheight,
                                                                    FALSE);
      }
    else
      { WinRowB *rowarr = RowArray(w), rptr;
        while (nrows--)
          { int len;
            rptr = rowarr[srow];
            len = min(ncols, NCHARS(rptr));
            write_string(w, ColToXCoord(w,scol,srow), scol, srow, 0,
                                        len, NULL, False, WS_WRITE);
            srow++;
          }
      }
  }

static void copy_row_chars(w, scol, srow, dcol, drow, ncols)
  XpwScrollTextWidget w;
  int scol, srow, dcol, drow, ncols;
  {
    WinRowB *rowarr = RowArray(w);
    register WinRowB srp = rowarr[srow], drp =  rowarr[drow];
    uint csz_code = CharSizeCode(w);
    register int dlcol, trail0, nchars;

    trail0 = scol+ncols-NCHARS(srp);
    if (trail0 > 0)
      { ncols -= trail0; ncols = max(0, ncols); }
    else
        trail0 = 0;

    dlcol = dcol+ncols;
    if (VarWidthMode(w) && dlcol > NUMCOLS(drp))
        drp = expand_row(w, drow, dlcol);
    nchars = NCHARS(drp);
    if (dcol > nchars)
        clear_chars(w, nchars, drow, dcol-nchars, 1, False);
    if (dlcol+trail0 >= NCHARS(drp))
      { NCHARS(drp) = dlcol;
        trail0 = 0;
      }

    if (BLINK_LCOL(drp) != 0 || BLINK_LCOL(srp) != 0)
      { BLINK_SCOL(drp) = 0; BLINK_LCOL(drp) = NCHARS(drp); }

    if (csz_code == CSZ_BYTE)
      { srp += scol;
        drp += dcol;
        wincharB_copy(srp, drp, ncols);
        if (trail0 != 0) wincharB_zero(drp+ncols, trail0);
      }
    else if (csz_code == CSZ_SHORT)
      { register WinRowS srpS = (WinRowS)srp + scol,
                         drpS = (WinRowS)drp + dcol;
        wincharS_copy(srpS, drpS, ncols);
        if (trail0 != 0) wincharS_zero(drpS+ncols, trail0);
      }
    else
      { register WinRowI srpI = (WinRowI)srp + scol,
                         drpI = (WinRowI)drp + dcol;
        wincharI_copy(srpI, drpI, ncols);
        if (trail0 != 0) wincharI_zero(drpI+ncols, trail0);
      }
  }

static void copy_text(w, scol, srow, ncols, nrows, c_dis, r_dis)
  XpwScrollTextWidget w;
  register int scol, srow, ncols, nrows;
  int c_dis, r_dis;
  {
    register int dcol = scol+c_dis, lrow = srow+nrows, drow = srow+r_dis;

    if (w == onactive_widget && drow <= onactive_row
     && onactive_row < drow+nrows)
        ClearOnActive(w);

    if (w->xpwscrolltext.blink_lim_row != 0 && r_dis != 0)
      { w->xpwscrolltext.blink_start_row = 0;
        w->xpwscrolltext.blink_lim_row = NumRows(w);
      }

    if (!VarWidthMode(w) && ncols == NumCols(w)) ncols = MAX_COLS;
    if (c_dis == 0 && scol == 0 && ncols == MAX_COLS)
      { /* Scrolling whole rows up/down */
        WinRowB *rowarr = RowArray(w);
        register int savesrc, savedst = 0, savelim = r_dis+nrows, savelen;
        /* work out which rows need saving */
        rowarr += srow;
        if (r_dis < 0)
          { savesrc = r_dis;
            if (savelim > 0) { savedst = savelim; savelim = 0; }
          }
        else
            savesrc = max(r_dis, nrows);

        if ((savelen = savelim-savesrc) == 1)
          { WinRowB rp = rowarr[savesrc];
            winrow_copy(rowarr, rowarr+r_dis, nrows);
            rowarr[savedst] = rp;
          }
        else
          { WinRowB savearr[MAX_ROWS/2];
            winrow_copy(rowarr+savesrc, savearr, savelen);
            winrow_copy(rowarr, rowarr+r_dis, nrows);
            winrow_copy(savearr, rowarr+savedst, savelen);
          }
      }

    else
      { /* General case */
        if (r_dis < 0)
          { /* moving rows down */
            while (srow < lrow)
                copy_row_chars(w, scol, srow++, dcol, drow++, ncols);
          }
        else
          { /* moving rows up */
            int ldrow = drow+nrows;
            while (lrow > srow)
                copy_row_chars(w, scol, --lrow, dcol, --ldrow, ncols);
          }
      }
  }

static Boolean draw_text_write_spaces = False;

static Coord draw_text(w, string, dx, dcol, drow, len)
  XpwScrollTextWidget w;
  register uchar *string;
  int dx, dcol, drow;
  register int len;
  { int dlcol = dcol+len, attrs = w->xpwscrolltext.curr_attributes, nchars;
    uint csz_code = CharSizeCode(w);
    register WinRowB rptr = RowArray(w)[drow];

    if (w == onactive_widget && drow == onactive_row) ClearOnActive(w);

    if (!string && !draw_text_write_spaces)
      { /* clearing */
        dx = DrawSpaces(w, dx, dcol, drow, len);
        clear_chars(w, dcol, drow, len, 1, True);
        return(dx);
      }

    if (attrs & ATTR_BLINK)
      { EnsureBlinkWidget(w);
        BLINK_SCOL(rptr) = min(BLINK_SCOL(rptr), dcol);
        BLINK_LCOL(rptr) = max(BLINK_LCOL(rptr), dlcol);
        w->xpwscrolltext.blink_start_row =
            min(drow, w->xpwscrolltext.blink_start_row);
        w->xpwscrolltext.blink_lim_row =
            max(drow+1, w->xpwscrolltext.blink_lim_row);
      }

    dx = DrawString(w, string, dx, dcol, drow, len, attrs, 0, 0);
    if (VarWidthMode(w))
      { if (dlcol == NumFixedCols(w,drow)) dx += PIXOFFS(rptr);
        if (dlcol > NUMCOLS(rptr)) rptr = expand_row(w, drow, dlcol);
      }
    nchars = NCHARS(rptr);
    if (dcol > nchars) clear_chars(w, nchars, drow, dcol-nchars, 1, False);
    NCHARS(rptr) = max(nchars, dlcol);

    if (csz_code == CSZ_BYTE)
      { register WinRowB rpB = rptr + dcol;
        while (len--)
          { rpB->c0 = string ? *string++ : SPACECHAR;
            rpB->attrs = attrs;
            rpB++;
          }
      }
    else if (csz_code == CSZ_SHORT)
      { register ushort us, *us_string = (ushort *) string;
        register WinRowS rpS = (WinRowS)rptr + dcol;
        while (len--)
          { us = us_string ? *us_string++ : SPACECHAR;
            rpS->c0 = us; rpS->c1 = us>>8;
            rpS->attrs = attrs;
            rpS++;
          }
      }
    else
      { register uint ui, *ui_string = (uint *) string;
        register WinRowI rpI = (WinRowI)rptr + dcol;
        while (len--)
          { ui = ui_string ? *ui_string++ : SPACECHAR;
            rpI->s0 = ui; rpI->s1 = ui>>16;
            rpI->attrs = attrs;
            rpI++;
          }
      }

    return(dx);
  }

static uint winchar_at(w, col, row, attr_p)
  XpwScrollTextWidget w;
  int col, row;
  uchar *attr_p;
  { uint csz_code = CharSizeCode(w), wc, attrs;
    WinRowB rptr = RowArray(w)[row];
    if (col >= NCHARS(rptr))
      { attrs = 0;
        wc = 0;
      }
    else if (csz_code == CSZ_BYTE)
      { register WinRowB rpB = rptr + col;
        attrs = rpB->attrs;
        wc = rpB->c0;
      }
    else if (csz_code == CSZ_SHORT)
      { register WinRowS rpS = (WinRowS)rptr + col;
        attrs = rpS->attrs;
        wc = rpS->c0 + (rpS->c1<<8);
      }
    else
      { register WinRowI rpI = (WinRowI)rptr + col;
        attrs = rpI->attrs;
        wc = rpI->s0 + (rpI->s1<<16);
      }
    if (attr_p) *attr_p = attrs;
    /* return SPACECHAR for nulls, but 0 for dummy chars inserted
        after a right scroll */
    return( wc == 0 && attrs == 0 ? SPACECHAR : wc );
  }


/* CLEAR and associated methods */

static void Clear(w, col, row, ncols, nrows)
  XpwScrollTextWidget w;
  int row, col, nrows, ncols;
  { int num_rows = NumRows(w), nc;
    Boolean DoCursor, save_cursor;

    if (row >= num_rows) return;
    nrows = min(nrows, num_rows - row);
    if (nrows == 0) nrows = num_rows - row;

    if ((nc = ncols) == 0) nc = MAX_COLS;
    if (DoCursor = (col <= CursorCol(w) && CursorCol(w) < col+nc
                && row <= CursorRow(w) && CursorRow(w) < row+nrows))
        save_cursor = _XpwTextCursorOn(w, False);

    clear_text(w, col, row, ncols, nrows);

    if (DoCursor)
      { CursorX(w) = CURSOR_X_UNDEF; _XpwTextCursorOn(w, save_cursor); }
  }

/* related methods */

static void ClearScreen(w)
  XpwScrollTextWidget w;
  {  Clear(w, 0, 0, 0, 0); }

static void ClearLine(w, start_line)
  XpwScrollTextWidget w;
  int start_line;
  { Clear(w, 0, start_line, 0, 1); }


static void ClearLines(w, start_line, num_lines)
  XpwScrollTextWidget w;
  int start_line, num_lines;
  { Clear(w, 0, start_line, 0, num_lines); }

static void ClearTail(w, col, row)
  XpwScrollTextWidget w;
  int col, row;
  { Clear(w, col, row, 0, 1); }

static void ClearTails(w, col, row, num_lines)
  XpwScrollTextWidget w;
  int col, row, num_lines;
  { Clear(w, col, row, 0, num_lines); }

static void ClearChar(w, col, row)
  XpwScrollTextWidget w;
  int col, row;
  { Clear(w, col, row, 1, 1); }

static void ClearLineAtCursor(w)
  XpwScrollTextWidget w;
  { Clear(w, 0, CursorRow(w), 0, 1); }

static void ClearTailAtCursor(w)
  XpwScrollTextWidget w;
  { Clear(w, CursorCol(w), CursorRow(w), 0, 1); }

static void ClearCharAtCursor(w)
  XpwScrollTextWidget w;
  { Clear(w, CursorCol(w), CursorRow(w), 1, 1); }


/* SCROLL and associated methods */

static void VarTextScroll(w, row, nrows, c_dis, save_cursor)
  XpwScrollTextWidget w;
  int row, nrows, c_dis;
  Boolean save_cursor;
  {
    Coord lstart = LeftVarTextStart(w,row), y = RowToYCoord(w,row), sx, dx;
    int width = RightTextLim(w) - lstart, height = nrows*FontHeight(w);
    WinRowB *rowarr = RowArray(w);
    int fixed_cols = NumFixedCols(w,row);
    Display *dpy = XtDisplay(w);
    Window win = XtWindow(w);
    MyGC mygc = Font0MyGC(w,row);

    if (c_dis < 0)
      { /* scrolling left */
        sx = lstart - c_dis;
        dx = lstart;
        width += c_dis;
      }
    else
      { /* scrolling right */
        sx = lstart;
        dx = lstart + c_dis;
        width -= c_dis;
      }

    XCopyArea(dpy, win, win, mygc.gc, sx, y, width, height, dx, y);
    if (c_dis < 0)
        ClearRectangle(dpy, win, mygc, NULL, dx+width, y, -c_dis, height);

    do
      { WinRowB rptr = rowarr[row];
        Coord limx;
        draw_lim_x = (c_dis < 0) ? sx : sx+width-1;
        draw_lim_col = -1;
        limx = DrawRowSequences(w, lstart+PIXOFFS(rptr), fixed_cols,
                                    MAX_COLS, row, 0, DS_NO_DRAW|DS_X_LIM);
        if (c_dis < 0)
          { int col = draw_lim_col, dcols = col - fixed_cols;
            COLOFFS(rptr) += dcols;
            PIXOFFS(rptr) = (limx == sx) ? 0 : (limx-sx) - last_text_width;
            if (dcols != 0)
                copy_text(w, col, row, NCHARS(rptr), 1, -dcols, 0);
          }
        else
          { int nchars = draw_lim_col+1;
            NCHARS(rptr) = min(NCHARS(rptr), nchars);
            PIXOFFS(rptr) += c_dis;
          }
        row++;
      }
    while (--nrows);

    CursorX(w) = CURSOR_X_UNDEF;
    _XpwTextCursorOn(w, save_cursor);

    _XpwTextCopyWait(w);
  }


static void Scroll(w, col, row, ncols, nrows, c_dis, r_dis)
  XpwScrollTextWidget w;
  register int col, row, ncols, nrows, c_dis, r_dis;
  {
    int fwidth = FontWidth(w), fheight = FontHeight(w), tmp,
        num_rows = NumRows(w), num_cols = NumCols(w), copywait = 0;
    Boolean save_cursor, var_width_mode = VarWidthMode(w);

    /* two complicated to faff around trying to work out if cursor needs
       replotting, so just do it for every scroll
    */

    save_cursor = _XpwTextCursorOn(w, False);

    row = min(row, num_rows); row = max(row, 0);
    if ((tmp=row+r_dis) < 0) row -= tmp;
    if (nrows == 0) nrows = num_rows - row;
    else nrows = min(nrows, num_rows - row);
    if (row+nrows+r_dis >= num_rows) nrows = num_rows - (row+r_dis);

    if (var_width_mode && c_dis != 0)
      { VarTextScroll(w, row, nrows, c_dis, save_cursor);
        return;
      }

    if (var_width_mode) num_cols = MAX_COLS;
    col = min(col, num_cols); col = max(col, 0);
    if ((tmp=col+c_dis) < 0) col -= tmp;
    if (ncols == 0) ncols = num_cols - col;
    else ncols = min(ncols, num_cols - col);
    if (col+ncols+c_dis >= num_cols) ncols = num_cols - (col+c_dis);

    if (ncols != 0 && nrows != 0)
      { /* Copy the text */
        Display *dpy = XtDisplay(w);
        Window win = XtWindow(w);
        int srcx = ColToXCoord(w,col,0), dstx = srcx+(c_dis*fwidth),
            width, Srow = row, Drow = row+r_dis, Nrows = nrows;
        GC gc = Font0NormalMyGC(w).gc;
        width = var_width_mode ? RightTextLim(w)-srcx : ncols*fwidth;

        copy_text(w, col, row, ncols, nrows, c_dis, r_dis);

        /* need a separate 1-row copy to/from row 0 if not contiguous */
        do
          { int n = (Srow == 0 || Drow == 0)
                     && Row1Offset(w) != Row0Offset(w)+fheight ? 1 : Nrows;
            XCopyArea(dpy, win, win, gc, srcx, RowToYCoord(w,Srow),
                                width, n*fheight, dstx, RowToYCoord(w,Drow));
            copywait++;
            Nrows -= n; Srow += n; Drow += n;
          }
        while (Nrows != 0);
      }

    /* Clear the area made by any c_dis move, and then clear the area made
        by any r_dis move.
    */
    if (c_dis > 0 && nrows != 0)
        clear_text(w, col, row, c_dis, nrows);
    else if (c_dis < 0 && nrows != 0)
        clear_text(w, col+ncols+c_dis, row, -c_dis, nrows);

    if (r_dis > 0 && ncols != 0)
        clear_text(w, col, row, ncols, r_dis);
    else if (r_dis < 0 && ncols != 0)
        clear_text(w, col, row+nrows+r_dis, ncols, -r_dis);

    CursorX(w) = CURSOR_X_UNDEF;
    _XpwTextCursorOn(w, save_cursor);

    while (copywait--) _XpwTextCopyWait(w);
  }

/* associated methods */

static void ScrollScreenUp(w)
 XpwScrollTextWidget w;
  { Scroll(w, 0, 1, 0, 0, 0, -1); }

static void ScrollScreenDown(w)
  XpwScrollTextWidget w;
  { Scroll(w, 0, 0, 0, 0, 0, 1); }

static void ScrollScreenLeft(w)
  XpwScrollTextWidget w;
  { Scroll(w, 1, 0, 0, 0, -1, 0); }

static void ScrollScreenRight(w)
  XpwScrollTextWidget w;
  { Scroll(w, 0, 0, 0, 0, 1, 0); }

static void ScrollLines(w, start_line, num_lines, dist)
  XpwScrollTextWidget w;
  int start_line, num_lines, dist;
  { Scroll(w, 0, start_line, 0, num_lines, 0, dist); }

static void ScrollTail(w, col, row, dist)
  XpwScrollTextWidget w;
  int row, col, dist;
  { Scroll(w, col, row, 0, 1, dist, 0); }

static void ScrollTails(w, col, row, num_rows, dist)
  XpwScrollTextWidget w;
  int col, row, num_rows, dist;
  { Scroll(w, col, row, 0, num_rows, dist, 0); }

static void ScrollTailLeft(w, col, row)
  XpwScrollTextWidget w;
  int col, row;
  { Scroll(w, col, row, 0, 1, -1, 0); }

static void ScrollTailRight(w, col, row)
  XpwScrollTextWidget w;
  int col, row;
  { Scroll(w, col, row, 0, 1, 1, 0); }

static void InsertLineAtCursor(w)
  XpwScrollTextWidget w;
  { Scroll(w, 0, CursorRow(w), 0, 0, 0, 1); }

static void DeleteLineAtCursor(w)
  XpwScrollTextWidget w;
  { int row = CursorRow(w);
    if (row == NumRows(w)-1)
        Clear(w, 0, row, 0, 1);
    else
        Scroll(w, 0, CursorRow(w)+1, 0, 0, 0, -1);
  }


/* Write */

/* primitive text drawing routines */

/* Strings are first pre-processed to determine how much should be painted.

   If len is < 0, the string is assumed to be a poplog string, with its length
   stored 2 longwords before the string pointer. If len is 0, the whole string
   is painted. Otherwise strlen is used to calculate the maximum length of the
   string.

   The function returns the num of chars that need to be painted.
*/


#define POP_SIZE_OFFSET -2


static uchar *increment_string(w, string, offset)
  XpwScrollTextWidget w;
  uchar *string;
  int offset;
  { int csz_code = CharSizeCode(w);
    if (csz_code == CSZ_BYTE)
        return(string + offset);
    else if (csz_code == CSZ_SHORT)
        return((uchar*) ((ushort *)string + offset));
    else
        return((uchar*) ((uint *)string + offset));
  }


static int process_string(w, offset, len, stringp)
  XpwScrollTextWidget w;
  int offset, len;
  uchar **stringp;
  { uchar *string = *stringp;
    long max_len;

    /* work out maximum size of string */
    if (len < 0)
      { /* POPLOG STRING - length is in (ptr-2) */
        caddr_t *ptr = (caddr_t *)string;
        max_len = (long) ptr[POP_SIZE_OFFSET];
        len = abs(len);
      }
    else
      { int csz_code = CharSizeCode(w);
        if (csz_code == CSZ_BYTE)
            max_len = strlen((char *)string);
        else if (csz_code == CSZ_SHORT)
          { register ushort *usp = (ushort *)string;
            while (*usp++ != 0);
            max_len = (usp - (ushort *)string) - 1;
          }
        else
          { register uint *uip = (uint *)string;
            while (*uip++ != 0);
            max_len = (uip - (uint *)string) - 1;
          }
        if (len == 0) len = MAX_COLS;
      }

    if (offset < 0) offset = 0;
    if (offset >= max_len) return(0);
    if (offset+len > max_len) len = max_len - offset;

    *stringp = increment_string(w, string, offset);
    return(len);
  }

static int write_string(w, x, col, row, offset, len, string, at_cursor, mode)
  XpwScrollTextWidget w;
  Coord x;
  uint col, row;
  int offset, len;
  uchar *string;
  Boolean at_cursor;
  int mode;
  {
    Display *dpy = XtDisplay(w);
    Window win = XtWindow(w);
    Boolean copywait = False;
    MyGC mygc = Font0MyGC(w,row);
    WinRowB rptr = RowArray(w)[row];
    int nchars = NCHARS(rptr);

    if (x >= RightTextLim(w)) return(0);
    if (string) len = process_string(w, offset, len, &string);

    if (!VarWidthMode(w))
      { int num_cols = NumCols(w);
        len = min(len, num_cols-col);
        len = max(0, len);
        if (mode == WS_INSERT)
          { int nafter = nchars-col, ncols = num_cols-(col+len);
            if (ncols > nafter) ncols = nafter;
            if (ncols > 0)
              { int fwidth = FontWidth(w);
                Coord y = RowToYCoord(w,row);
                XCopyArea(dpy, win, win, mygc.gc, x, y,
                                ncols*fwidth, FontHeight(w), x+len*fwidth, y);
                copywait = True;
                copy_text(w, col, row, ncols, 1, len, 0);
              }
          }
        x = draw_text(w, string, x, col, row, len);
      }
    else
      { int fixed_cols = NumFixedCols(w,row), flen = fixed_cols - col;
        flen = min(len, flen);
        if (flen > 0)
          { /* insertion/deletion treated as write for this bit */
            x = draw_text(w, string, x, col, row, flen);
            col += flen;
            len -= flen;
            if (len > 0 && string) string = increment_string(w, string, flen);
          }
        if (len > 0)
          { Coord endx;
            int nextcol = (mode == WS_INSERT) ? col : col+len;
            /* calculate number of cols that fit */
            draw_lim_x = RightTextLim(w)-1;
            draw_lim_col = -1;
            if (mode == WS_DELETE)
              { endx = x; len = -len; }
            else if (string)
                endx = DrawString(w, string, x, col, row, len,
                    w->xpwscrolltext.curr_attributes, DS_NO_DRAW|DS_X_LIM, 0);
            else
              { int swidth = SpaceWidth(w), cols = (draw_lim_x-x) / swidth;
                if (len > cols)
                  { draw_lim_col = col+cols;
                    endx = x + cols*swidth;
                    if (endx != draw_lim_x) endx += swidth;
                  }
                else
                    endx = x + len*swidth;
              }
            if (draw_lim_col >= 0)
              { /* fills whole line */
                int new_nchars = draw_lim_col+1;
                len = new_nchars-col;
                nchars = NCHARS(rptr) = min(new_nchars, nchars);
                mode = WS_WRITE;
              }
            else if (winchar_at(w, col, row, NULL) != 0)
              { Coord oldend = 0, clearx = endx, y = RowToYCoord(w,row);
                if (nextcol >= nchars)
                  { /* replaces all old cols */
                    if (nchars > col)
                      oldend = DrawRowSequences(w, x, col, nchars, row, 0,
                                                                DS_NO_DRAW);
                  }
                else
                  { /* some old cols remain on end */
                    oldend = DrawRowSequences(w, x, col, nextcol, row, 0,
                                                                DS_NO_DRAW);
                    if (oldend > endx)
                      { /* old cols shift left */
                        Coord Limx, limx;
                        uint width;
                        Limx = DrawRowSequences(w, oldend, nextcol, nchars,
                                                    row, 0, DS_NO_DRAW);
                        limx = min(Limx, RightTextLim(w));
                        width = limx-oldend;
                        XCopyArea(dpy, win, win, mygc.gc, oldend, y,
                                    width, FontHeight(w), endx, y);
                        copywait = True;
                        clearx = endx+width;
/*
                        if (limx != Limx)
                            _XpwTextExpose(w, x, y, xl, y+FontHeight(w), row==0);
*/
                      }
                    else if (oldend < endx)
                      { /* old cols shift right */
                        /* calc how many old cols now fit on */
                        Coord limx;
                        draw_lim_x = RightTextLim(w)-1;
                        draw_lim_col = -1;
                        limx = DrawRowSequences(w, endx, nextcol, nchars, row,
                                                    0, DS_NO_DRAW|DS_X_LIM);
                        if (draw_lim_col >= 0)
                          { nchars = NCHARS(rptr) = draw_lim_col+1;
                            limx = RightTextLim(w);
                          }
                        XCopyArea(dpy, win, win, mygc.gc, oldend, y,
                                    limx-endx, FontHeight(w), endx, y);
                        copywait = True;
                      }
                  }
                if (oldend > endx)
                  { XRectangle r;
                    r.x = LeftVarTextStart(w,row);
                    r.width = RightTextLim(w) - r.x;
                    ClearRectangle(dpy, win, mygc, &r, clearx, y,
                                                oldend-endx, FontHeight(w));
                  }
              }

            if (mode != WS_WRITE)
              { int ncols = nchars-col;
                if (ncols > 0) copy_text(w, nextcol, row, ncols, 1, len, 0);
              }
            if (mode != WS_DELETE)
                x = draw_text(w, string, x, col, row, len);
          }
      }

    if (at_cursor) { CursorCol(w) = col+len; CursorX(w) = x; }
    if (copywait) _XpwTextCopyWait(w);
    return(len);
  }


/* INSERT -writes string at row, col - after moving text in that location right
   by the correct number of chars.
*/

static XpwMethodRet Insert(w, col, row, string, len)
  XpwScrollTextWidget w;
  int row, col, len;
  uchar *string;
  { Boolean DoCursor, save_cursor;

    if (row >= NumRows(w) || row < 0 || col < 0) return(0);

    if (DoCursor = (row == CursorRow(w)))
        save_cursor = _XpwTextCursorOn(w, False);

    len = write_string(w, ColToXCoord(w,col,row), col, row, 0, len, string,
                                                        False, WS_INSERT);

    if (DoCursor)
      { CursorX(w) = CURSOR_X_UNDEF; _XpwTextCursorOn(w, save_cursor); }

    return(len);
  }

static void InsertAtCursor(w, string, len)
  XpwScrollTextWidget w;
  uchar *string;
  int len;
  { Boolean save_cursor = _XpwTextCursorOn(w, False);
    write_string(w, GetCursorX(w), CursorCol(w), CursorRow(w), 0, len, string,
                                                        True, WS_INSERT);
    _XpwTextCursorOn(w, save_cursor);
  }

static void InsertCharAtCursor(w)
  XpwScrollTextWidget w;
  { Boolean save_cursor = _XpwTextCursorOn(w, False);
    write_string(w, GetCursorX(w), CursorCol(w), CursorRow(w), 0, 1, NULL,
                                                            False, WS_INSERT);
    _XpwTextCursorOn(w, save_cursor);
  }


/* WRITE - most general case text overwrite procedure */

/* takes an array of strings. For each string, it draws the substring
   starting at offset, of length len, at row,col. If clear != 0, the line is
   cleared first.
*/

static void Write(w, col, row, start_line, nlines, strings, offset, len, clear)
  XpwScrollTextWidget w;
  int col,row,offset,len,start_line, nlines,clear;
  uchar *strings[];
  {
    Boolean DoCursor = False, save_cursor;
    int i, cursor_row = CursorRow(w), num_rows = NumRows(w);

    if (row >= num_rows || row < 0 || col < 0) return;
    if (row + nlines > num_rows) nlines = num_rows - row;

    if (DoCursor = (cursor_row >= row && cursor_row <= (row+nlines)))
        save_cursor = _XpwTextCursorOn(w, False);

    if (clear && !VarWidthMode(w))
      { Clear(w, col, row, 0, nlines);
        clear = False;
      }

    for (i=start_line; i < start_line+nlines; i++)
      { if (clear) clear_text(w, col, row, 0, 1);
        write_string(w, ColToXCoord(w,col,row), col, row, offset, len,
                                            strings[i], False, WS_WRITE);
        row++;
      }

    if (DoCursor)
      { CursorX(w) = CURSOR_X_UNDEF; _XpwTextCursorOn(w, save_cursor); }
  }

/* writes str at row,col. If clear != 0, the whole line is cleared first.
*/

static void WriteLine(w, col, row, string, len, clear)
  XpwScrollTextWidget w;
  int row, col, len, clear;
  uchar *string;
  { uchar *strings[1];
    strings[0] = string;
    Write(w, col, row, 0, 1, strings, 0, len, clear);
  }

/* same as above, for multiple lines */

static void WriteLines(w, col, row, start_line, num_lines, strings, len, clear)
  XpwScrollTextWidget w;
  int row, col, clear, len, start_line, num_lines;
  uchar *strings[];
  { Write(w, col, row, start_line, num_lines, strings, 0, len, clear); }


/* takes a string, and writes substring starting at offset, of length len, at
   row,col.
*/

static void WriteSubstr(w, col, row, string, offset, len, clear)
  XpwScrollTextWidget w;
  int col, row, offset, len, clear;
  uchar *string;
  { uchar *strings[1];
    strings[0] = string;
    Write(w, col, row, 0, 1, strings, offset, len, clear);
  }

static XpwMethodRet WriteTrailSpacesAtCursor(w)
  XpwScrollTextWidget w;
  { Boolean save_cursor = _XpwTextCursorOn(w, False);
    draw_text_write_spaces = True;
    write_string(w, GetCursorX(w), CursorCol(w), CursorRow(w), 0, MAX_COLS,
                                            NULL, False, WS_WRITE);
    draw_text_write_spaces = False;
    _XpwTextCursorOn(w, save_cursor);
  }

static XpwMethodRet
WriteAtCursor(XpwScrollTextWidget w, uchar * string, int len)
{
    int no_draw_mode = w->xpwscrolltext.no_draw_mode, col;
    uint attrs;
    Coord x;

    if (no_draw_mode < 0) {
        Boolean save_cursor = _XpwTextCursorOn(w, False);
        draw_text_write_spaces = True;
        write_string(w, GetCursorX(w), CursorCol(w), CursorRow(w), 0, len,
                                                string, True, WS_WRITE);
        draw_text_write_spaces = False;
        _XpwTextCursorOn(w, save_cursor);
        return;
    }

    /* computing width of chars etc */
    if (no_draw_mode & 1) return(0);    /* already passed limit */

    if (string) len = process_string(w, 0, len, &string);
    x = w->xpwscrolltext.alt_cursor_x;
    col = w->xpwscrolltext.alt_cursor_column;
    attrs = w->xpwscrolltext.curr_attributes;

    if (no_draw_mode != 0) {
        /* calculating number of cols that fit */
        draw_lim_x = no_draw_mode >> 2;
        draw_lim_col = -1;
        x = DrawString(w, string, x, col, 1, len, attrs,
                                                DS_NO_DRAW|DS_X_LIM, 0);
        if (draw_lim_col >= 0) {
            int new_mode = draw_lim_col - NumFixedCols(w,1);
            if (x > draw_lim_x) {
                int rem = x - draw_lim_x;
                if (!(no_draw_mode & 2)) rem = last_text_width - rem;
                new_mode |= rem << 16;
            }
            w->xpwscrolltext.no_draw_mode = (new_mode<<2) | 1;
            return(draw_lim_col - col);
        }
    } else {
        x = DrawString(w, string, x, col, 1, len, attrs, DS_NO_DRAW, 0);
    }

    w->xpwscrolltext.alt_cursor_x = x;
    w->xpwscrolltext.alt_cursor_column = col+len;
    return(len);
}

static XpwMethodRet BeginTextWidthMode1(w)
  XpwScrollTextWidget w;
  { w->xpwscrolltext.no_draw_mode = 0;
    w->xpwscrolltext.alt_cursor_column = NumFixedCols(w,1);
    w->xpwscrolltext.alt_cursor_x = 0;
  }

static XpwMethodRet BeginTextWidthMode2(w, width)
  XpwScrollTextWidget w;
  int width;
  { BeginTextWidthMode1(w);
    w->xpwscrolltext.no_draw_mode = (width <= 0) ? 1 : width << 2;
  }

static XpwMethodRet BeginTextWidthMode3(w, col, row)
  XpwScrollTextWidget w;
  int col, row;
  { BeginTextWidthMode2(w, RightTextLim(w) - ColToXCoord(w,col,row));
    w->xpwscrolltext.no_draw_mode |= 2;
  }

static XpwMethodRet EndTextWidthMode(w)
  XpwScrollTextWidget w;
  { int no_draw_mode = w->xpwscrolltext.no_draw_mode,
        x = w->xpwscrolltext.alt_cursor_x;
    w->xpwscrolltext.no_draw_mode = -1;
    if (no_draw_mode == 0)
        return(x);
    else if (no_draw_mode & 1)
        return(no_draw_mode >> 2);
    else
      { /* not reached limit -- extend with spaces */
        int swidth = VarWidthMode(w) ? SpaceWidth(w) : FontWidth(w),
            lim_x = no_draw_mode >> 2, spcols = (lim_x-x) / swidth,
            ncols = w->xpwscrolltext.alt_cursor_column + spcols
                            - NumFixedCols(w,1);
        x += spcols * swidth;
        if (x < lim_x)
          { int rem = lim_x - x;
            if (no_draw_mode & 2) rem = swidth - rem;
            ncols |= rem << 16;
          }
        return(ncols);
      }
  }

static XpwMethodRet SetVarColumnOffset(w, row, ncols, pixrem)
  XpwScrollTextWidget w;
  int row, ncols, pixrem;
  { WinRowB rptr = RowArray(w)[row];
    int diff = COLOFFS(rptr)-ncols, num_fixed = NumFixedCols(w,row),
        nchars = NCHARS(rptr)-num_fixed,
        pixdiff = PIXOFFS(rptr)-pixrem;
    Boolean save_cursor;

    if (row == CursorRow(w)) save_cursor = _XpwTextCursorOn(w, False);

    if (PIXOFFS(rptr) > 0 && diff > 0 && nchars > 0)
      { /* insert dummy chars (0 with nonzero attr) following a right scroll
        */
        uint csz_code = CharSizeCode(w), len = diff;
        copy_text(w, num_fixed, row, nchars, 1, len, 0);

        if (csz_code == CSZ_BYTE)
          { register WinRowB rpB = rptr + num_fixed;
            while (len--)
              { rpB->c0 = 0; rpB->attrs = 1; rpB++; }
          }
        else if (csz_code == CSZ_SHORT)
          { register WinRowS rpS = (WinRowS)rptr + num_fixed;
            while (len--)
              { rpS->c0 = rpS->c1 = 0; rpS->attrs = 1; rpS++; }
          }
        else
          { register WinRowI rpI = (WinRowI)rptr + num_fixed;
            while (len--)
              { rpI->s0 = rpI->s1 = 0; rpI->attrs = 1; rpI++; }
          }
      }

    COLOFFS(rptr) = ncols;
    PIXOFFS(rptr) = -pixrem;

    if (row == CursorRow(w))
      { CursorX(w) = CURSOR_X_UNDEF;
        _XpwTextCursorOn(w, save_cursor);
      }

    return( (pixdiff << 16) | (diff & 0xffff));
  }

static XpwMethodRet GetVarColumnOffset(w, row, pixrem_p)
  XpwScrollTextWidget w;
  int row, *pixrem_p;
  { WinRowB rptr = RowArray(w)[row];
    *pixrem_p = -PIXOFFS(rptr);
    return(COLOFFS(rptr));
  }

static XpwMethodRet GetVarColumnLimOffset(w, row, pixrem_p)
  XpwScrollTextWidget w;
  int row, *pixrem_p;
  { WinRowB rptr = RowArray(w)[row];
    int col = NCHARS(rptr), diff = ColToXCoord(w, col, row) - RightTextLim(w);
    if (diff < 0)
      { int swidth = SpaceWidth(w), spcols;
        diff = -diff;
        spcols = diff / swidth;
        diff -= spcols * swidth;
        if (diff > 0) { spcols++; diff = swidth - diff; }
        col += spcols;
      }
    *pixrem_p = diff;
    return( COLOFFS(rptr) + (col - NumFixedCols(w,row)) );
  }

static XpwMethodRet GetVarRowWidth(w, row)
  XpwScrollTextWidget w;
  int row;
  { return( RightTextLim(w) - Col0Offset(w)
                    - NumFixedCols(w,row)*FontWidth(w) );
  }




static void DeleteCharsAtCursor(w, nchars)
  XpwScrollTextWidget w;
  int nchars;
  { if (VarWidthMode(w))
      { Boolean save_cursor = _XpwTextCursorOn(w, False);
        write_string(w, GetCursorX(w), CursorCol(w), CursorRow(w), 0, nchars,
                                        NULL, False, WS_DELETE);
        _XpwTextCursorOn(w, save_cursor);
      }
    else
        Scroll(w, CursorCol(w)+nchars, CursorRow(w),0,1, -nchars,0);
  }

static void DeleteCharAtCursor(w)
  XpwScrollTextWidget w;
  { DeleteCharsAtCursor(w, 1); }


/* CURSOR CONTROL */

Boolean _XpwTextCursorOn(w, on)
  XpwScrollTextWidget w;
  Boolean on;
  { uint flags = Flags(w), wc, dsflags;
    Boolean wason = flags & CURSOR_ON, hasfocus = flags & HAVE_FOCUS;
    uchar cc = w->xpwscrolltext.cursor_winchar.c0,
          cattrs = w->xpwscrolltext.cursor_winchar.attrs,
          attrs;
    int col = CursorCol(w), row = CursorRow(w);
    Coord x;
    uint tmp[1], csz_code;

    if (w->xpwscrolltext.no_draw_mode >= 0) return(wason);

    if (hasfocus && (cattrs & ATTR_BLINK))
      { EnsureBlinkWidget(w);
        if (on) skip_cursor_blink = True;
      }

    if (wason)
      { /* cursor is already on */
        if (on) return(True);
        /* turn it off */
        wc = winchar_at(w, col, row, &attrs);
        dsflags = cc = 0;
      }

    else
      { /* cursor is currently off */
        int cgc_index;
        if (!on) return(False);
        /* turn it on */
        cgc_index = cattrs & ATTR_GC_MASK;
        wc = winchar_at(w, col, row, &attrs);
        dsflags = DS_ISCURSOR;

        if (hasfocus)
          { cattrs &= ~cgc_index;
            cgc_index = GCIndex(cgc_index/NFONTS,
                                (cgc_index%NFONTS)|BOLDFONTBIT);
            cattrs |= cgc_index;
          }

        if (cc == 'O' && ((cgc_index%NFONTS) & BOLDFONTBIT)
         && (!(attrs & ATTR_BLINK)
                || w->xpwscrolltext.no_blink || blink_on_phase) )
          { /* Draw the underlying char in inverse */
            int gc_index = attrs & ATTR_GC_MASK;
            cattrs = (attrs & ~gc_index)
                        | GCIndex(cgc_index/NFONTS, gc_index%NFONTS);
            dsflags = DS_ISCURSOR|DS_INVERT;
            cc = 0;
          }
        attrs = cattrs & ~ATTR_BLINK;
      }

    csz_code = CharSizeCode(w);
    if (csz_code == CSZ_BYTE)
        ((uchar *)tmp)[0] = wc;
    else if (csz_code == CSZ_SHORT)
        ((ushort *)tmp)[0] = wc;
    else
        tmp[0] = wc;

    if ((x = GetCursorX(w)) < RightTextLim(w))
        DrawString(w, (uchar*)tmp, x, col, row, 1, attrs, dsflags, cc);

    Flags(w) = (on ? flags | CURSOR_ON : flags & ~CURSOR_ON);
    return(wason);
  }


XpwMethodRet _XpwTextMoveCursorTo(w, col, row)
  XpwScrollTextWidget w;
  int col, row;
  { int cursor_col = CursorCol(w), cursor_row = CursorRow(w);
    int num_rows = NumRows(w);
    Boolean save_cursor;

    if (w->xpwscrolltext.no_draw_mode >= 0) return(0);

    row = min(num_rows-1, row);
    col = max(0, col);
    row = max(0, row);

    if (row == cursor_row && col == cursor_col) return(0);

    save_cursor = _XpwTextCursorOn(w, False);
    CursorCol(w) = col;
    CursorRow(w) = row;
    if (VarWidthMode(w) && row == cursor_row && col > cursor_col
     && CursorX(w) != CURSOR_X_UNDEF)
        CursorX(w) = DrawRowSequences(w, CursorX(w), cursor_col, col, row, 0,
                                                        DS_NO_DRAW);
    else
        CursorX(w) = CURSOR_X_UNDEF;
    _XpwTextCursorOn(w, save_cursor);
    return(0);
  }

static void MoveCursorUp(w)
  XpwScrollTextWidget w;
  { _XpwTextMoveCursorTo(w, CursorCol(w), CursorRow(w)-1); }

static void MoveCursorDown(w)
  XpwScrollTextWidget w;
  { _XpwTextMoveCursorTo(w, CursorCol(w), CursorRow(w)+1); }

static void MoveCursorLeft(w)
  XpwScrollTextWidget w;
  { _XpwTextMoveCursorTo(w, CursorCol(w)-1, CursorRow(w)); }

static void MoveCursorRight(w)
  XpwScrollTextWidget w;
  { _XpwTextMoveCursorTo(w, CursorCol(w)+1, CursorRow(w)); }


static void Bell(w, volume)
  Widget w;
  int volume;
  { XBell(XtDisplay(w), volume); }


/*******************************************************************
 * Text Selections
 *******************************************************************/

static XpwAssocTable *setsel_table = NULL;

typedef struct setsel_context {
    struct setsel_context *next_widget;
    Widget          widget;
    String          string;
    int             length;
    Time            time;
    XtCallbackProc  lose_p;
    Opaque          client;
} setsel_context;

static setsel_context *GetSelnContext(w, selection, create)
  Widget w;
  XpwAssocID selection;
  Boolean create;
  { setsel_context *c, *context;

    /* build table first time */
    if (!setsel_table) setsel_table = XpwCreateAssocTable(4);

    context = c = (setsel_context*)XpwLookupAssoc(setsel_table, selection);

    while (c != NULL)
        if (c->widget == w || c->widget == NULL)
          { if (c->next_widget != NULL)
              { /* there can only be one more */
                XtFree(c->next_widget->string);
                XtFree((char *)c->next_widget);
                c->next_widget = NULL;
              }
            return((c->widget == w || create) ? c : NULL);
          }
        else
            c = c->next_widget;

    /* build new context for selection */
    if (create)
      { c = XtNew(setsel_context);
        c->next_widget = context;
        c->widget   = (Widget)NULL;
        c->string   = (String)NULL;
        c->length   = 0;
        c->time     = 0;
        c->lose_p   = (XtCallbackProc)NULL;
        XpwMakeAssoc(setsel_table, (XpwAssocID)selection, (caddr_t)c);
      }
    return(c);
}

static Boolean SelectionConvert(w, selectionp, target, type_return,
                                value_return, length_return, format_return)
  Widget w;
  XpwAssocID *selectionp;
  Atom *target;
  Atom *type_return;
  Opaque *value_return;
  unsigned long *length_return;
  int *format_return;
  {
    Display *dpy = XtDisplay(w);
    Atom targets = XInternAtom(dpy, "TARGETS", False);
    Boolean result = False;
    setsel_context *context = GetSelnContext(w, *selectionp, False);

    if (context == NULL)
        return(False);
    else if (*target == XA_STRING ||
            *target == XInternAtom(dpy,"TEXT",False))
      { /* Return the string */
        *value_return = (Opaque)XtNewString(context->string);
        *length_return = context->length;
        *type_return = XA_STRING;
        *format_return = 8;
        result = True;
      }
    else if (*target == XInternAtom(dpy, "LENGTH", False))
      { /* Return the length of the string */
        *value_return = (Opaque)&context->length;
        *type_return = XA_INTEGER;
        *length_return = 1;
        *format_return = sizeof(int) * 8;
        result = True;
      }
    else if (*target == XInternAtom(dpy, "LIST_LENGTH", False))
      { /* Return number of elements, = 1 */
        static int len = 1;
        *value_return = (Opaque)&len;
        *type_return = XA_INTEGER;
        *length_return = 1;
        *format_return = sizeof(int) * 8;
        result = True;
      }
    else if (*target == XInternAtom(dpy, "TIMESTAMP", False))
      { /* Return time of selection */
        static int len = 1;
        *value_return = (Opaque)&context->time;
        *type_return = XA_INTEGER;
        *length_return = 1;
        *format_return = sizeof(int) * 8;
        result = True;
      }
    else if (*target == targets)
      { /* Return the list of things I can return */
        Atom *atomPtr;
        *value_return = (Opaque)(XtMalloc(sizeof(Atom)*7));
        *length_return = 7;
        atomPtr = *(Atom**)value_return;
        *atomPtr++ = XA_STRING;
        *atomPtr++ = targets;
        *atomPtr++ = XInternAtom(dpy, "TEXT", False);
        *atomPtr++ = XInternAtom(dpy, "MULTIPLE", False);
        *atomPtr++ = XInternAtom(dpy, "LENGTH", False);
        *atomPtr++ = XInternAtom(dpy, "LIST_LENGTH", False);
        *atomPtr++ = XInternAtom(dpy, "TIMESTAMP", False);
        *type_return = XA_ATOM;
        *format_return = sizeof(Atom) * 8;
        result = True;
      }

    return(result);
  }

/* Losing selections */
static void LoseSelection(w, selectionp)
  Widget w;
  XpwAssocID *selectionp;
  {     setsel_context *context = GetSelnContext(w, *selectionp, False);
    XtCallbackProc lose_p = context->lose_p;
    XtFree(context->string);
    context->widget = (Widget)NULL;
    context->string = (String)NULL;
    context->length = 0;
    context->time   = 0;
    context->lose_p = (XtCallbackProc)NULL;
    if (lose_p != NULL)
        (*lose_p)(w, context->client, selectionp);
  }

XpwMethodRet XpwSetSelection(w, location, string, nbytes, lose_p, client)
  Widget w;
  String location;
  String string;
  int nbytes;
  XtCallbackProc lose_p;
  Opaque client;
  {
    Display *dpy = XtDisplay(w);
    Atom selection;
    setsel_context *context;

    /* convert selection location - 1 and 2 are left alone, but rest
       are treated as strings
    */
    selection = (Atom)location;
    if (!selection)
        selection = XA_PRIMARY;
    else if (selection > 2)
        selection = XInternAtom(dpy, location, FALSE);


    if (string && nbytes)
      { /* specified a new string to set the selection as */
        context = GetSelnContext(w, (XpwAssocID)selection, True);
        XtFree(context->string);    /* free any old string */
        /* copy string, and set length */
        context->string = XtNewString(string);
        context->length = nbytes;
        context->time = XtLastTimestampProcessed(dpy);
        context->lose_p = lose_p;
        context->client = client;

        if (context->widget != NULL) return; /* already own the selection */

        /* n.b. the selection data must be set up before calling
         * XtOwnSelection, because xclipboard does nasty things like
         * having its lose procedure get the new selection and then
         * immediately taking back ownership (so that our call to
         * XtOwnSelection returns true even though we've lost it again!)
         */
        context->widget = w;
        if (!XtOwnSelection(w, selection, context->time,
                            SelectionConvert, LoseSelection, NULL))
            /* failed */
            LoseSelection(w, &selection);
      }

    else
        /* clearing selection */
      { context = GetSelnContext(w, (XpwAssocID)selection, False);
        if (context != NULL)
          { XtDisownSelection(w, selection, context->time);
            /* just to make sure */
            XtFree(context->string); /* free old string */
            context->widget = (Widget)NULL;
            context->string = (String)NULL;
            context->length = 0;
            context->time   = 0;
            context->lose_p = (XtCallbackProc)NULL;
          }
      }
 }


/* Receiving selections */
typedef struct {XtCallbackProc p; Opaque val;} closure;

static void SelectionCB(w, clos, selectionp, typep, value, length, format)
  Widget w;
  closure *clos;
  Atom *selectionp;
  Atom *typep;
  Opaque value;
  unsigned long *length;
  int *format;
  { if (*typep != XA_STRING) value = NULL;
    (*clos->p)(w, clos->val, &value);
    XtFree(value);
    XtFree((char *)clos);
  }

XpwMethodRet XpwGetSelection(w, location, receive_p, client)
  Widget w;
  String location;
  XtCallbackProc receive_p;
  Opaque client;
  { Display *dpy = XtDisplay(w);
    Time time = XtLastTimestampProcessed(XtDisplay(w));
    Atom selection = (Atom)location;
    setsel_context *context;

    if (!selection)
        selection = XA_PRIMARY;
    else if (selection > 2)
        selection = XInternAtom(dpy, location, FALSE);

    context = GetSelnContext(w, (XpwAssocID)selection, False);

    if (context != NULL)
        /* call this directly */
        (*receive_p)(w, client, &context->string);
    else
      { closure *clos = XtNew(closure);
        clos->p = receive_p; clos->val = client;
        XtGetSelectionValue(w, selection, XA_STRING, SelectionCB, clos, time);
      }
    return(0);      /* dummy */
  }


/****************************************************************
 *
 * Methods
 *
 ****************************************************************/

externaldef (xpwscrolltext)
XpwMethod _xpwScrollTextMethods[] = {
/*  {id,                proc,           num_args,   flags    }*/
#define M METHOD_STRUCT
M   (XpwMScroll,        Scroll,                 6, RequiresWidget|Cut),
M   (XpwMScrollScreenUp,    ScrollScreenUp,     0, RequiresWidget|Cut),
M   (XpwMScrollScreenDown,  ScrollScreenDown,   0, RequiresWidget|Cut),
M   (XpwMScrollScreenLeft,  ScrollScreenLeft,   0, RequiresWidget|Cut),
M   (XpwMScrollScreenRight, ScrollScreenRight,  0, RequiresWidget|Cut),
M   (XpwMScrollLines,   ScrollLines,        3, RequiresWidget|Cut),
M   (XpwMScrollTail,    ScrollTail,     3, RequiresWidget|Cut),
M   (XpwMScrollTails,   ScrollTails,        4, RequiresWidget|Cut),
M   (XpwMScrollTailLeft,    ScrollTailLeft,     2, RequiresWidget|Cut),
M   (XpwMScrollTailRight,   ScrollTailRight,    2, RequiresWidget|Cut),
M   (XpwMInsertLineAtCursor,InsertLineAtCursor, 0, RequiresWidget|Cut),
M   (XpwMInsertCharAtCursor,InsertCharAtCursor, 0, RequiresWidget|Cut),
M   (XpwMDeleteLineAtCursor,DeleteLineAtCursor, 0, RequiresWidget|Cut),
M   (XpwMDeleteCharAtCursor,DeleteCharAtCursor, 0, RequiresWidget|Cut),
M   (XpwMDeleteCharsAtCursor,DeleteCharsAtCursor,   1, RequiresWidget|Cut),
M   (XpwMClear,         Clear,          4, RequiresWidget|Cut),
M   (XpwMClearScreen,   ClearScreen,        0, RequiresWidget|Cut),
M   (XpwMClearLine,     ClearLine,      1, RequiresWidget|Cut),
M   (XpwMClearLines,    ClearLines,     2, RequiresWidget|Cut),
M   (XpwMClearTail,     ClearTail,      2, RequiresWidget|Cut),
M   (XpwMClearTails,    ClearTails,     3, RequiresWidget|Cut),
M   (XpwMClearChar,     ClearChar,      2, RequiresWidget|Cut),
M   (XpwMClearLineAtCursor, ClearLineAtCursor,  0, RequiresWidget|Cut),
M   (XpwMClearTailAtCursor, ClearTailAtCursor,  0, RequiresWidget|Cut),
M   (XpwMClearCharAtCursor, ClearCharAtCursor,  0, RequiresWidget|Cut),
M   (XpwMInsert,        Insert,         4, RequiresWidget|Cut),
M   (XpwMInsertAtCursor,    InsertAtCursor,     2, RequiresWidget|Cut),
M   (XpwMWrite,         Write,          8, RequiresWidget|Cut),
M   (XpwMWriteLine,     WriteLine,      5, RequiresWidget|Cut),
M   (XpwMWriteLines,    WriteLines,     7, RequiresWidget|Cut),
M   (XpwMWriteSubstr,   WriteSubstr,        6, RequiresWidget|Cut),
M   (XpwMWriteAtCursor, WriteAtCursor,      2, RequiresWidget|Cut),
M   (XpwMWriteTrailSpacesAtCursor,  WriteTrailSpacesAtCursor,0, RequiresWidget|Cut),
M   (XpwMCursorTo,      _XpwTextMoveCursorTo,   2, RequiresWidget|Cut),
M   (XpwMCursorUp,      MoveCursorUp,       0, RequiresWidget|Cut),
M   (XpwMCursorDown,    MoveCursorDown,     0, RequiresWidget|Cut),
M   (XpwMCursorLeft,    MoveCursorLeft,     0, RequiresWidget|Cut),
M   (XpwMCursorRight,   MoveCursorRight,    0, RequiresWidget|Cut),
M   (XpwMBell,              Bell,                   1, RequiresWidget|Cut),
M   (XpwMSetCharAttributes, _XpwSetCharAttributes,  1, RequiresWidget|Cut),
M   (XpwMGetCharAttributes, _XpwGetCharAttributes,  0, RequiresWidget|Cut),
M   (XpwMSetTextCursor,     _XpwSetTextCursor,      1, RequiresWidget|Cut),
M   (XpwMGetTextCursor,     _XpwGetTextCursor,      0, RequiresWidget|Cut),

M   (XpwMEndTextWidthMode,  EndTextWidthMode,       0, RequiresWidget|Cut),
M   (XpwMBeginTextWidthMode1,   BeginTextWidthMode1,0, RequiresWidget|Cut),
M   (XpwMBeginTextWidthMode2,   BeginTextWidthMode2,1, RequiresWidget|Cut),
M   (XpwMBeginTextWidthMode3,   BeginTextWidthMode3,2, RequiresWidget|Cut),
M   (XpwMSetVarColumnOffset,    SetVarColumnOffset, 3, RequiresWidget|Cut),
M   (XpwMGetVarColumnOffset,    GetVarColumnOffset, 2, RequiresWidget|Cut),
M   (XpwMGetVarColumnLimOffset, GetVarColumnLimOffset,  2, RequiresWidget|Cut),
M   (XpwMGetVarRowWidth,        GetVarRowWidth, 1, RequiresWidget|Cut),

M   (XpwMSetTextSelection,  XpwSetSelection,        3, RequiresWidget),
M   (XpwMGetTextSelection,  XpwGetSelection,        1, RequiresWidget),
#undef M
};

externaldef (xpwscrolltext)
int _num_xpwScrollTextMethods = XtNumber(_xpwScrollTextMethods);



/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 15 1997
        Changes to support variable-width chars
--- Robert Duncan, Jul 31 1997
        Workaround for bug in NCR C compiler
--- John Gibson, Apr 29 1997
        Changes to support drawing Unicode & wide-character strings.
--- John Gibson, Nov 22 1996
        Changed to uses MyGC structures instead of GC.
--- John Gibson, Oct  9 1995
        Added active attribute stuff and drawing shadowed chars etc.
        Also added fixed-width drawing of proportional fonts or fonts
        less than FontWidth in width using XDrawText.
--- John Gibson, Jan  9 1994
        Added support for different cursors/blinking cursor etc.
--- John Gibson, Dec 19 1993
        Fixed ANSI warnings
--- Julian Clinton, Mar 29 1993
        Modified ClearRectangle so that it doesn't grope into the internals
        of a GC.
--- John Gibson, Jul 28 1992
        Changed Scroll so that it deals sensibly with the case where
        row = num_rows and nrows = 0, although r_dis is non-zero
        (i.e. there is no text to copy, but just an area to clear).
--- John Gibson, Mar  9 1992
        VMS externaldef mods
--- John Gibson, Feb 12 1992
        Added drawing of diamond in DrawGraphString
--- John Gibson, Feb  2 1992
        Changes to support 8 per-char colours
--- John Gibson, Jan 25 1992
        Added blinking attribute
--- John Gibson, Jan 15 1992
        Added support for mark fg & bg colours in col 0
--- John Gibson, Jan 13 1992
        Add alt font support
--- John Gibson, Dec 24 1991
        Added support for built-in graphics chars
--- John Gibson, Dec 18 1991
        Added DrawString to support bold & underline modes
--- Simon Nichols, Nov 27 1991
        Changed semicolon to comma in middle of declaration starting at
        line 38.
--- John Gibson, Nov 21 1991
        Moved _xpwScrollTextMethods and _num_xpwScrollTextMethods to
        end of file
--- John Gibson, Oct 31 1991
        VMS mods
--- John Gibson, Oct 23 1991
        Changed declaration of -string- in -draw_text- to uchar
--- John Gibson, Sep  4 1991
        Fixed bug in Insert where _XpwTextCopyWait was being called when
        no XCopyArea had been done (causing it to hang up).
--- John Gibson, Aug 21 1991
        Rewrote selections (again)
--- John Gibson, Aug 14 1991
        ClearScreen now indexed on XpwMClearScreen not XpwMClearWindow
--- John Gibson, Aug  5 1991
        Cursor highlighting now dependent on flags field
--- John Gibson, Jul 27 1991
        Now represents text window contents in row array in widget.
        Large number of changes for this and to fix other bugs.
--- John Gibson, Jul 16 1991
        Rewrote selection code
--- John Gibson, Jul 14 1991
        Changes to cursor state handling
--- John Gibson, Jul 11 1991
        Added initialisation of context fields in RestoreSelectionVars
        (it helps not to call XtFree on any old junk).
--- Jonathan Meyer, Jul  9 1991
    Fixed call to LoseSelection inside SetSelection
--- Jonathan Meyer, Jul  8 1991
    Added setsel_context to allow multiple selections to be maintained
    at once
--- Jonathan Meyer, Jul  3 1991
    Called LoseSelection if SetSelection fails. Made SetSelection
    with a NULL string disown the selection if necessary
--- Jonathan Meyer, Jul  2 1991
    Wrote _XpwTextSetColors
--- Jonathan Meyer, Jun 27 1991
    Added set_geom arg to _XpwTextRecalc
--- Jonathan Meyer, Jun 17 1991
    Renamed Xpt* Xpw
--- Jonathan Meyer, Jun  4 1991
    Added SetColors, now sensitive to cursor position
--- Jonathan Meyer, Aug 21 1990
    Renamed Text.c, added "X11" prefix to #include's.
    Added XpwNotInstalled.
    XpwTextRecalc -> _XpwTextRecalc,
    XpwTextCursor -> _XpwTextCursor,
--- Andreas Schoter, Jul 23 1990
    Changed all pop* identifiers to xpw*
--- Ian Rogers, Jul 20 1990 - changed Pop* to Xpw*
 */
