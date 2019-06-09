/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.x/x/ved/src/xvedarglist.p
 > Purpose:         XVed arglist management routines
 > Author:          Jonathan Meyer, Aug  2 1991 (see revisions)
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

section $-xved;

include xt_constants.ph;

constant procedure xved_get_icon_pixmap;

/* All of XVed uses a single 100 element ArgList for its work */
lvars
    arglist         = false,
    arglist_count   = 0,
;

define xved_set_args(args, init);
    lvars arg, val, args, init;
    lconstant wklist = writeable [0 0];

    if init then
        0 -> arglist_count;
        unless arglist then initXptArgList(100) -> arglist endunless
    endif;
    returnif(args == []);
    unless ispair(args) then
        (), args -> dl(wklist);
        wklist -> args
    endunless;
    repeat
        destpair(destpair(args)) -> (arg, val, args);
        if isstring(val) then
            if arg = XtN borderPixmap or arg = XtN backgroundPixmap then
                xved_get_icon_pixmap(val, true) -> val
            elseif arg = XtN iconPixmap then
                xved_get_icon_pixmap(val, false) -> val
            endif
        endif;
        arglist_count fi_+ 1 -> arglist_count;
        arg, val -> nc_subscrXptArgList(arglist_count, arglist);
        quitif(args == [])
    endrepeat
enddefine;

define xved_arg_list();
    arglist, arglist_count;
enddefine;

define xved_va_arg_list();
    lvars arg, val, i;
    #|  fast_for i to arglist_count do
            nc_subscrXptArgList(i, arglist) -> (arg, val);
            if isstring(val) and arg /= XtN autoGeometry then
                XtVaTypedArg, arg, XtR String, val, datalength(val)
            else
                arg, val
            endif
        endfor
    |#
enddefine;

define xved_set_color_args(fg, bg, init);
    lvars fg, bg, init;

    define lconstant undef_color(color);
        lvars color;
        lconstant NO_PIXEL = 16:FFFFFFFF;   ;;; careful, bigint ...
        color == -1 or color = NO_PIXEL or color == "undef"
    enddefine;

    xved_set_args([], init);            ;;; clear first arglist if init true
    unless undef_color(fg) then
        xved_set_args(XtN foreground, fg, false);
        if xved == "openlook" then
            xved_set_args(XtN fontColor, fg, false)
        endif
    endunless;
    unless undef_color(bg) then
        xved_set_args(XtN background, bg, false)
    endunless
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov  5 1996
        Made xved_set_args call xved_get_icon_pixmap with 2nd arg false
        for XtN iconPixmap.
--- John Gibson, Aug 31 1995
        Fixed xved_set_color_args to test for colour being "undef"
--- John Gibson, Feb 24 1994
        Made xved_set_args call xved_get_icon_pixmap on the value when
        appropriate
--- John Gibson, Feb 14 1994
        Added xved_set_color_args
--- John Gibson, Jun  3 1993
        Made arglist be initialised at run-time
--- John Gibson, Aug 22 1991
        Removed totally inappropriate active declarations for
        xved_arg_list etc, and added second arg to xved_set_args to
        say whether to init.
 */
