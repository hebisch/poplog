/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.x/x/ved/lib/S-xvedS-create_parent_xol.p
 > Purpose:         Create textwidget parent for OpenLook XVed
 > Author:          John Gibson, Jun  1 1993 (see revisions)
 */
compile_mode :pop11 +strict;

uses-now Xol;

section $-xved;

include xpt_xtypes.ph;

uses xolFormWidget;

lconstant
    gui_text_args = [
            ^XtN xAddWidth      ^true
            ^XtN yAddHeight     ^true
            ^XtN xResizable     ^true
            ^XtN yResizable     ^true
            ^XtN xAttachRight   ^true
            ^XtN yAttachBottom  ^true
    ];

    ;;; configure parent window with menubar/scrollbar/hscrollbar if present
define lconstant configure(text, menu, scroll, hscroll);
    lvars text, menu, scroll, hscroll;

    scroll, menu -> XptVal[fast] text(XtN xRefWidget:XptWidget,
                                      XtN yRefWidget:XptWidget);

    if scroll then
        menu -> XptVal[fast] scroll(XtN yRefWidget:XptWidget)
    endif;

    if hscroll then
        scroll, text -> XptVal[fast] hscroll(XtN xRefWidget:XptWidget,
                                             XtN yRefWidget:XptWidget)
    endif;

    if menu then
        ;;; Make the bg colour of the Form that of the Menubar (see
        ;;; xvedresources.p for reason)
        XptVal[fast] menu(XtN background:XptPixel)
                    -> XptVal[fast] (XtParent(text))(XtN background:XptPixel)
    endif
enddefine;

define create_parent_xol(shell, name, create_text) -> text;
    lvars shell, name, procedure create_text, parent, text;

    fast_XtVaCreateManagedWidget(name, xolFormWidget, shell, 0) -> parent;
    ;;; create text widget
    create_text(parent, gui_text_args) -> text;
    configure -> text.xvedwin_configure;

    ;;; Initially, make the bg colour of the Form same as Text widget
    XptVal[fast] text(XtN background:XptPixel)
                            -> XptVal[fast] parent(XtN background:XptPixel);
enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 11 1994
        Removing setting of yAttachBottom false for text when hscroll true
--- John Gibson, Feb 11 1994
        Reorganised create procedure plus horiz scrollbar
 */
