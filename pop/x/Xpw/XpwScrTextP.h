/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 * File:            C.x/x/Xpw/XpwScrTextP.h
 * Version:         Revision 5.0
 * Purpose:         Private header file for the XpwScrollText widget.
 * Author:          Jonathan Meyer (see revisions)
 * Date:            11 February 1989
 * Documentation:   REF *XpwScrText, HELP *XpwScrText
 * Related Files:   XpwScrText.h XpwScrText.c xpwgraphics.p
 */

/*
 * Notes:
 *
 *  This contains all of the private declarations.
 *  It is organised as follows:
 *      o  new fields for instances of the XpwComposite - a structure.
 *      o  declaration of the full instance record
 *      o  new fields for the class XpwComposite - a structure.
 *      o  declaration of the full class record.
 *  o  private macro definitions used in widget source.
 */

#ifndef _XpwScrollTextP_h
#define _XpwScrollTextP_h

#include "XpwScrText.h"
#include "XpwCoreP.h"

#define uchar unsigned char
#define ushort unsigned short
#define uint unsigned int
typedef int Coord;

/* Window rows */
typedef struct {ushort nchars, blink_scol, blink_lcol;} WinRowHead;
typedef struct {ushort num_cols, col_offs; short cscroll; WinRowHead head;}
                            WinVRowHead;

typedef struct {uchar c0, attrs;}            WinCharB, *WinRowB;
typedef struct {uchar c0, c1, attrs;}        WinCharS, *WinRowS;
typedef struct {ushort s0, s1; uchar attrs;} WinCharI, *WinRowI;

#define NCHARS(rp)  ((WinRowHead*)(rp))[-1].nchars
#define BLINK_SCOL(rp)  ((WinRowHead*)(rp))[-1].blink_scol
#define BLINK_LCOL(rp)  ((WinRowHead*)(rp))[-1].blink_lcol
#define NUMCOLS(rp) ((WinVRowHead*)(rp))[-1].num_cols
#define COLOFFS(rp) ((WinVRowHead*)(rp))[-1].col_offs
#define PIXOFFS(rp) ((WinVRowHead*)(rp))[-1].cscroll

/* Character GC index */
#define BOLDFONTBIT       1
#define NORMAL_FONT       (0<<1)
#define ALT_FONT          (1<<1)
#define BOLD_NORMAL_FONT  (NORMAL_FONT|BOLDFONTBIT)
#define BOLD_ALT_FONT     (ALT_FONT|BOLDFONTBIT)
#define NFONTS 4

/* Normal colours range from 0 - NCOLORS-1 (normal colour is 0) */
#define NCOLORS 8
/* ... then colours 8 - 15 represent colours with the active bit set */
#define NALLCOLORS  16
#define CURSOR_COLORNUM NALLCOLORS

#define GCIndex(colornum, fontnum)  ((colornum)*NFONTS+(fontnum))

/* Allow shared gcs for 3 colours in all fonts for main text, and 1 colour
 * in all fonts for status text
 */
#define NNORMALGCS      GCIndex(3, 0)
#define NSTATUSGCS      GCIndex(1, 0)
#define CURSOR_GCINDEX  GCIndex(CURSOR_COLORNUM, 0)

/* Character GC index + other attributes */
#define ATTR_GC_MASK   0x1f      /* 5 bits to hold < NCOLORS*NFONTS */
#define ATTR_ACTIVE    (1<<5)
#define ATTR_UNDERLINE (1<<6)
#define ATTR_BLINK     (1<<7)

/* Font Draw Flags */
#define FDM_CLEAR_BG    1        /* Clear bg first then use XDrawString */
#define FDM_1BY1        2        /* Must draw one char at a time */
#define FDM_CLEAR_ONLY -1        /* Special temporary mode */
#define FDM_NO_DRAW    -2        /* Special temporary mode */
#define FDM_NO_DRAW1   -3        /* Special temporary mode */

/* Char Sizes */
#define CSZ_BYTE       0
#define CSZ_SHORT      1
#define CSZ_INT        2

#define MAX_ROWS       512
#define MAX_COLS       1024
#define CURSOR_X_UNDEF -1000


/* flags field */
#define CURSOR_ON    (1<<0)   /* Cursor is currently displayed */
#define ACT_FOCUS    (1<<1)   /* we are the actual focus window */
#define PTR_FOCUS    (1<<2)   /* we inherit the focus by having the pointer */
#define SYN_FOCUS    (1<<3)   /* synthetic focus courtesy of XtSetKeyboardFocus */
#define CALLBACK_YES (1<<4)   /* Last callback said client had focus */
#define HAVE_FOCUS (ACT_FOCUS | PTR_FOCUS | SYN_FOCUS)
#define MONOCHROME   (1<<5)   /* Monochrome screen */

typedef struct {
    Pixel fg, bg;
} ColorPair;

typedef struct {
    ushort colornum;
    Boolean underline, bold, alt_font, blink, active;
} AttrResources;

typedef struct {
    int x, y, column, row;
} MousePosition;

typedef struct {
    GC gc;
    unsigned int flags;
} MyGC;

#define GC_HAS_WIN_BG   1
#define GC_HAS_BG_PIX   2

#define SPACECHAR ' '

/* this is directly below the XpwCore in widget heirarchy */

/* New fields for the poplog scrolltext widget instance record */

typedef struct {
    MyGC cursor_gc, status_cursor_gc;
    MyGC gc_array[NNORMALGCS];
    MyGC status_gc_array[NSTATUSGCS];
    GC work_gc;
    short h_margin, v_margin;
    Position col0_offset, row0_offset, row1_offset, split_offset,
            font_baseline, right_text_lim;
    ushort num_fixed_columns, num_status_fixed_columns;
    unsigned int font_width, font_average_width, font_height, space_width;
    int num_columns, num_rows, cursor_column, cursor_row, cursor_x,
            alt_cursor_column, alt_cursor_x;
    MousePosition mouse;
    uchar flags;
    Boolean cursor_status, is_fully_visible, synthetic,
            draw_graphic_chars, var_width_mode;
    ushort font_draw_modes;
    String key;
    XtCallbackList keyboard_event;
    XtCallbackList button_event;
    XtCallbackList motion_event;
    Pixel status_foreground, status_background, cursor_color;
    ColorPair alt_colors[NALLCOLORS-1];
    ushort curr_attributes;
    AttrResources curr_attr_resources;
    Boolean highlight_on;
    Boolean round_size;
    Boolean no_blink;
    Boolean no_gray_scale;
    ushort blink_start_row, blink_lim_row;
    XpwScrollTextWidget next_blink_widget;
    WinRowB *row_array;
    char *row_array_lim;
    XtCallbackList focus_change;
    String geometry;
    XFontStruct *alt_fonts[3];
    XFontSet alt_font_sets[3];

    /* Blank resources - these resources are dummies filled by XVed */
    Boolean menubar_on, scrollbar_on, hscrollbar_on;
    String auto_geometry;
    Pixel scrollbar_foreground, scrollbar_background;
    Pixel menubar_foreground, menubar_background;

    ushort selection_color_num, status_style, input_char_mode;
    uchar char_size_code;
    WinCharB cursor_winchar;
    unsigned int cursor_char;
    unsigned int default_color_mask;
    XtCallbackList active_change;
    Cursor pointer2_shape;
    Pixel shadow_whites[2];
    ushort shadow_white_bits[2];
    unsigned int draw_shadow_mask;
    int no_draw_mode;
} XpwScrollTextPart;

/* Full instance record declaration */
typedef struct _XpwScrollTextRec {
    CorePart core;
    XpwCorePart xpwcore;
    XpwScrollTextPart xpwscrolltext;
} XpwScrollTextRec;

/* New fields for the class record - there are none.*/
typedef struct {int dummy;} XpwScrollTextClassPart;

/* Full class record declaration. */
typedef struct _XpwScrollTextClassRec {
    CoreClassPart core_class;
    XpwCoreClassPart xpwcore_class;
    XpwScrollTextClassPart xpwscrolltext_class;
} XpwScrollTextClassRec;

/* Class pointer. */
externalref XpwScrollTextClassRec xpwScrollTextClassRec;

/* Useful MACROS */

#define Col0Offset(w)   ((w)->xpwscrolltext.col0_offset)
#define Row0Offset(w)   ((w)->xpwscrolltext.row0_offset)
#define Row1Offset(w)   ((w)->xpwscrolltext.row1_offset)
#define SplitOffset(w)  ((w)->xpwscrolltext.split_offset)
#define RightTextLim(w) ((w)->xpwscrolltext.right_text_lim)
#define NumRows(w)      ((w)->xpwscrolltext.num_rows)
#define NumCols(w)      ((w)->xpwscrolltext.num_columns)
#define CursorRow(w)    ((w)->xpwscrolltext.cursor_row)
#define CursorCol(w)    ((w)->xpwscrolltext.cursor_column)
#define CursorX(w)      ((w)->xpwscrolltext.cursor_x)
#define FontWidth(w)    ((w)->xpwscrolltext.font_width)
#define FontAverageWidth(w)    ((w)->xpwscrolltext.font_average_width)
#define FontHeight(w)   ((w)->xpwscrolltext.font_height)
#define FontBaseline(w)  ((w)->xpwscrolltext.font_baseline)
#define RowArray(w)     ((w)->xpwscrolltext.row_array)
#define Flags(w)        ((w)->xpwscrolltext.flags)
#define StatusStyle(w)  ((w)->xpwscrolltext.status_style)
#define CharMode(w)     ((w)->xpwscrolltext.input_char_mode)
#define CharSizeCode(w) ((w)->xpwscrolltext.char_size_code)
#define VarWidthMode(w)  ((w)->xpwscrolltext.var_width_mode)
#define SpaceWidth(w)   ((w)->xpwscrolltext.space_width)
#define NumFixedCols(w,row)  \
     ((row)==0 ? w->xpwscrolltext.num_status_fixed_columns \
           : w->xpwscrolltext.num_fixed_columns)

#define MainRowToYCoord(w,row) (FontHeight(w)*((row)-1) + Row1Offset(w))
#define RowToYCoord(w,row)    ((row)==0 ? Row0Offset(w) : MainRowToYCoord(w,row))

#define FontStruct(w,fontnum) ((fontnum)==NORMAL_FONT ? w->xpwcore.font \
                : w->xpwscrolltext.alt_fonts[(fontnum)-1])
#define FontSet(w,fontnum)    ((fontnum)==NORMAL_FONT ? w->xpwcore.font_set \
                : w->xpwscrolltext.alt_font_sets[(fontnum)-1])

#define FontFieldPtr(w,fontnum) ((fontnum)==NORMAL_FONT ? &w->xpwcore.font \
                : &w->xpwscrolltext.alt_fonts[(fontnum)-1])
#define FontSetFieldPtr(w,fontnum) ((fontnum)==NORMAL_FONT ? &w->xpwcore.font_set \
                : &w->xpwscrolltext.alt_font_sets[(fontnum)-1])

#define Font0NormalMyGC(w) w->xpwscrolltext.gc_array[0]
#define Font0StatusMyGC(w) w->xpwscrolltext.status_gc_array[0]
#define Font0MyGC(w,row) ((row)==0 ? Font0StatusMyGC(w) : Font0NormalMyGC(w))

#define LeftVarTextStart(w,row)  \
            (Col0Offset(w) + NumFixedCols(w,row)*FontWidth(w))


#define max(a, b) ((int)(a) > (int)(b) ? (a) : (b))
#define min(a, b) ((int)(a) < (int)(b) ? (a) : (b))

extern Boolean _XpwTextCursorOn();

#endif /* _XpwScrollTextP_h */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 15 1997
    Changes to support variable-width chars
--- John Gibson, Apr 29 1997
    Changes to support drawing Unicode & wide-character strings.
--- John Gibson, Nov 22 1996
    Changed GC structures to MyGC.
--- John Gibson, Apr 16 1996
    Added auto_geometry field
--- John Gibson, Oct  9 1995
    Various changes.
--- John Gibson, Feb 11 1994
    Added hscrollbar_on field
--- John Gibson, Jan  9 1994
    Changes for multiple cursors, different status line positions etc
--- John Gibson, Dec 19 1993
    Cast min & max params as (int)
--- John Gibson, Jun 23 1992
    Added no_gray_scale field
--- John Gibson, Feb  2 1992
    Changes to support 8 per-char colours
--- John Gibson, Jan 27 1992
    Added blink stuff
--- John Gibson, Jan 13 1992
    Added alt font stuff and regions
--- John Gibson, Dec 23 1991
    Added draw_graphic_chars field
--- John Gibson, Dec 18 1991
    Changes to support bold & underline modes
--- John Gibson, Nov  4 1991
    Added geometry field
--- John Gibson, Oct 31 1991
    extern -> externalref for XpwScrollTextClassRec
--- John Gibson, Oct 24 1991
    Removed casts on field macros (not needed)
--- John Gibson, Sep 29 1991
    Added round_size
--- Jonathan Meyer, Sep 11 1991 Added menubar_foreground/background
--- John Gibson, Aug  6 1991
    Added h_margin and v_margin as resource margin fields -- col0_offset
    and rowX_offset are actual margins used.
--- John Gibson, Aug  6 1991
    Added focus_change field
--- John Gibson, Aug  5 1991
    Replaced cursor_state field with flags
--- Jonathan Meyer, Aug  2 1991
    Added scrollbar_foreground, scrollbar_background
--- John Gibson, Jul 27 1991
    Added row_array and new GC fields to widget, removed exposed_lines,
    etc.
--- John Gibson, Jul 14 1991
    Changes to cursor state handling
--- Jonathan Meyer, Jul  8 1991
    Added scrollbar_on, menubar_on (dummy entries used by XVed)
--- Jonathan Meyer, Jun  4 1991
    Added synthetic
--- Jonathan Meyer, Aug 21 1990
    Added test for XpwNotInstalled.
--- Andreas Schoter, Jul 23 1990
    Changed all pop* identifiers to xpw*
--- Andreas Schoter 16 July 1990
    Replaced all occurances of Pop* variable names with Xpw*
*/
