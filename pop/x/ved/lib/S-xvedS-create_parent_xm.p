/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.x/x/ved/lib/S-xvedS-create_parent_xm.p
 > Purpose:         Create textwidget parent for Motif XVed
 > Author:          John Gibson, Jun  1 1993 (see revisions)
 */
compile_mode :pop11 +strict;

uses-now Xm;

section $-xved;

include xpt_xtypes.ph;
include xpt_generaltypes.ph;

uses
    fast_xt_converter,
    xmMainWindowWidget,
;
    ;;; Configure parent window with menubar/scrollbar/hscrollbar if present
define lconstant configure(text, menu, scroll, hscroll);
    lvars text, menu, scroll, hscroll;
    menu, scroll, hscroll -> XptVal[fast] (XtParent(text)) (
                                XtN menuBar:XptWidget,
                                XtN verticalScrollBar:XptWidget,
                                XtN horizontalScrollBar:XptWidget)
enddefine;

define create_parent_xm(shell, name, create_text) -> text;
    lvars shell, name, procedure create_text, parent, text;
    xved_set_args([% XtN spacing, 0 %], true);
    XmCreateMainWindow(shell, name, xved_arg_list()) -> parent;
    XtManageChild(parent);

    ;;; create text widget
    create_text(parent, []) ->> text
                            -> XptVal[fast] parent(XtN workWindow:XptWidget);
    configure -> text.xvedwin_configure;

    ;;; Make the bg colour of the Form same as Text widget (if no colour
    ;;; is specified for them, scrollbars will derive their colour
    ;;; from this)
    XptVal[fast] text(XtN background:XptPixel)
                            -> XptVal[fast] parent(XtN background:XptPixel);
enddefine;


    ;;; Used by xved_value when updating the background of a subpart
define get_xm_colours(value, win);
    lvars value, win;

    lconstant
        src_value = EXPTRINITSTR(:XptXrmValue),
        dest_value = EXPTRINITSTR(:XptXrmValue),
        foreground_ptr = EXPTRINITSTR(:XptPixel),
        top_shadow_ptr = EXPTRINITSTR(:XptPixel),
        bottom_shadow_ptr = EXPTRINITSTR(:XptPixel),
        select_ptr = EXPTRINITSTR(:XptPixel);

    ;;; IF THE BACKGROUND'S NOT A NUMBER, CONVERT IT
    unless isnumber(value) then
        0 ->> exacc :XptXrmValue dest_value.XptXVSize
            -> exacc :XptXrmValue dest_value.XptXVAddr;
        XtNLookup(value, "N") ->> exacc :XptXrmValue src_value.XptXVAddr;
        datalength() -> exacc :XptXrmValue src_value.XptXVSize;
        if fast_XtConvertAndStore(win, XtR String, src_value, XtR Pixel, dest_value) then
            exacc :XptPixel (exacc :XptXrmValue dest_value.XptXVAddr) -> value;
        else
            ;;; COULDN'T CONVERT COLOUR NAME TO PIXEL
            fast_XtDisplayStringConversionWarning(xveddisplay, XtR String, XtR Pixel);
            return();
        endif;
    endunless;

    ;;; GET THE NEW SHADOW AND SELECT COLORS
    XmGetColors(
        XptVal [fast] (fast_XptWidgetOfObject(win)) (
            XtN screen:XptScreenPtr,
            XtN colormap:XptColormap),
        value,
        foreground_ptr,
        top_shadow_ptr,
        bottom_shadow_ptr,
        select_ptr
    );
    exacc :XptPixel foreground_ptr,
    exacc :XptPixel top_shadow_ptr,
    exacc :XptPixel bottom_shadow_ptr,
    exacc :XptPixel select_ptr
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 15 1994
        Changed get_xm_colours to return foreground color as well
--- John Gibson, Feb 11 1994
        Reorganised create procedure plus horiz scrollbar
 */
