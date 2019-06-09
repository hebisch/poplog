/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.x/x/ved/src/xvedbitmaps.p
 > Purpose:         Ved icons
 > Author:          Jonathan Meyer, May 30 1991 (see revisions)
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

uses-now Xpw;

section $-xved => xved_get_icon_filename, xved_create_icon_window;

#_INCLUDE 'xved_declare.ph';
include xpt_xscreen.ph;
include xpt_xevent.ph;
include XpwCore;

uses xpwCoreWidget;

/* XVed uses iconWindow resources of shells to specify icons, which are in
fact instances of the Toolkit OverrideShellWidget. Icon windows are used
instead of (much simpler) pixmaps because:

    (1) they allow us to use colours in icons
    (2) they work with more window managers
    (3) we can change the picture and text associated with a window easily
    (4) icons are more likely to appear with the correct dimensions
    (5) we can specify the font to use to draw the icon label
*/


XptLoadProcedures xvedbitmaps [^^XPW_EXLIBS]
lvars
    XGetIconSizes(dpy,win,size_list,count) :XptLongBoolean,
    XFree,
    XCopyArea,
    XDrawImageString,
    XFlush,
    XpwCallMethod,
    XClearArea(dpy,w,x,y,width,height,exposures),
;

define xved_get_icon_filename(widget, name);
    lvars widget, name, icon_size, XIconSize;
    lconstant
            icon_sizes  = EXPTRINITSTR(:exptr),
            num         = EXPTRINITSTR(:int);

    l_typespec XIconSize {
        min_width  :int,
        min_height :int,
        max_width  :int,
        max_height :int,
    };

    48 -> icon_size;        ;;; default

    ;;; code to find what the size the window manager wants the icons to be.
    if exacc raw_XGetIconSizes(XtDisplay(widget),
                                exacc :XScreen (XtScreen(widget)).root,
                                icon_sizes, num)
    then
        exacc :exptr icon_sizes -> XIconSize;
        if exacc :int num == 0 or exacc XIconSize.max_height fi_>= 64 then
            64 -> icon_size     ;;; big - for OpenLook
        endif;
        exacc (1) raw_XFree(XIconSize)
    endif;

    name sys_>< '_' sys_>< icon_size sys_>< '.xbm';
enddefine;


;;; General purpose procedure for getting a Ved icon

lvars
    icon_widget = false,
    icon_fg,
    icon_bg,
    icon_depth,
    icon_gc,
    icons = newmapping([], 8, false, false),
;

;;; returns a bitmap with the correct colours
define xved_get_icon_pixmap(filename, use_colours) -> pixmap;
    lvars   filename, use_colours, name, pixmap, len, w, n, string,
            fg, bg, depth, col_list = [];
    lconstant XtUnspecifiedPixmap = 2;

    returnif(filename = 'None')(XtUnspecifiedPixmap -> pixmap);

    ;;; Create an XpwCore widget so that we can load the pixmaps
    unless icon_widget ->> w then
        fast_XtVaCreateWidget('Icon', xpwCoreWidget, xveddummyshell, 0)
                                            ->> w -> icon_widget;
        XptVal[fast] w(XtN foreground:XptPixel, XtN background:XptPixel,
                        XtN usersGC:XptGC, XtN depth:int)
                -> (icon_fg, icon_bg, icon_gc, icon_depth);
    endunless;
    (icon_fg, icon_bg, icon_depth) -> (fg, bg, depth);

    if strmember(`(`, filename) ->> n then
        ;;; fg/bg colours specified
        allbutfirst(n, filename) -> string;
        substring(1, n-1, filename) -> filename;
        if isendstring(')', string) then allbutlast(1, string) -> string endif;
        if strmember(`,`, string) ->> n then `\s` -> subscrs(n,string) endif;
        if use_colours and (sysparse_string(string) ->> col_list) /== [] then
            dest(col_list) -> col_list,
            if col_list /== [] then hd(col_list) else icon_bg endif
                -> XptVal[fast] w(XtN foreground:XptPixel <TYPE=string>,
                                XtN background:XptPixel <TYPE=string> <OPT>);
            XptVal[fast] w(XtN foreground:XptPixel, XtN background:XptPixel)
                                -> (fg, bg)
        endif
    endif;
    unless use_colours then
        lvars screen = fast_XtScreen(w);
        l_typespec screen :XScreen;
        exacc[fast] screen.white_pixel, exacc[fast] screen.black_pixel,
            exacc[fast] screen.root_depth -> (fg, bg, depth)
    endunless;

    sysfileok(filename) -> filename;
    consvector(sys_fname_name(filename), fg, bg, 3) -> name;
    returnif(icons(name) ->> pixmap);

    exacc[fast] (6):XptPixmap raw_XpwCallMethod(w, XpwMLoadPixmap, filename,
                                fg, bg, depth) -> pixmap;
    if pixmap == 0 then
        sys_raise_exception(filename, 1, 'PIXMAP FILE NOT FOUND', `W`);
        XtUnspecifiedPixmap -> pixmap
    else
        pixmap -> icons(name)
    endif
enddefine;


/* Code to create icon windows */

lconstant macro (
    IW_TYPE     = 1,
    IW_SHELL    = 2,
    IW_PIXMAP   = 3,
    IW_TEXT     = 4,
);

define lconstant isIconWindow(w);
    lvars w, data;
    XptIsType(w, "Window") and isvector(fast_XptDataProps(w) ->> data)
    and datalength(data) == 4 and data(IW_TYPE) == "IconWindow"
    and data
enddefine;

define xved_create_icon_window(pixmap, title) -> window;
    lvars pixmap, shell, xpw, info, title, window;

    ;;; Handle exposure events in icon windows */
    define lconstant expose_event(w, client, event);
        lvars   w, client, event, window = fast_XtWindow(w),
                data = XptDataProps(window), text;
        lconstant blank = '                            ';
        exacc (7) raw_XDrawImageString(xveddisplay, window,
                                icon_gc, 0, 63, blank, datalength(blank));
        exacc (10) raw_XCopyArea(xveddisplay, data(IW_PIXMAP),
                                window, icon_gc, 0,0, 65,65, 0,0);
        if data(IW_TEXT) ->> text then
            exacc (7) raw_XDrawImageString(xveddisplay, window,
                                icon_gc, 1, 62, text, datalength(text));
        endif;
        exacc (1) raw_XFlush(xveddisplay);
    enddefine;

    if pixmap.isstring then
        xved_get_icon_pixmap(pixmap, true) -> pixmap
    endif;
    dlocal XptWMProtocols = false;
    xved_set_args(#_<[
        ^XtN mappedWhenManaged ^false
        ^XtN width 65
        ^XtN height 65
    ]>_#, true);
    fast_XtAppCreateShell(title, title, xtOverrideShellWidget,
                        xveddisplay, xved_arg_list()) -> shell;
    fast_XtAddEventHandler(shell, ExposureMask, false,
                    XptExportEventHandlerCached(expose_event, false, false));
    fast_XtRealizeWidget(shell);
    fast_XtWindow(shell) -> window;
    consXptDescriptor(window, "Window") ->> XptRegister(window) -> window;
    {IconWindow ^shell ^pixmap ^title} -> XptDataProps(window);

    dlocal sys_destroy_dependent = true;
    procedure(w);
        lvars w, dest_proc;
        XptDataProps(w)(IW_SHELL) -> w;
        if sys_process_destroy_action(w) ->> dest_proc then dest_proc(w) endif
    endprocedure -> sys_process_destroy_action(window)
enddefine;

define :XVED_FOR wved_icon_label() with_nargs 1;
    XptVal (XptShellOfObject()) (XtN iconName:XptString)
enddefine;
;;;
define :XVED_FOR updaterof wved_icon_label(label, w);
    lvars label, w = XptShellOfObject(w), data;
    XptCheckString(label) -> XptVal w(XtN iconName:XptString);
    if XptVal w(XtN iconWindow:XptWindow) ->> w then
        if isIconWindow(w) ->> data then
            label -> data(IW_TEXT)
        endif;
        ;;; expose the window
        exacc raw_XClearArea(xveddisplay, w, 0, 0, 0, 0, true)
    endif
enddefine;
;;;
constant procedure xved_icon_name  = XVED_FOR_wved_icon_label;

define lconstant icon_window_data(w) -> (w, data);
    lvars   w = XptVal (XptShellOfObject(w))(XtN iconWindow:XptWindow),
            data = isIconWindow(w);
enddefine;

define xved_icon_window_pixmap(w);
    lvars (w, data) = icon_window_data(w);
    if data then data(IW_PIXMAP) else "undef" endif
enddefine;
;;;
define updaterof xved_icon_window_pixmap(pixmap, w);
    lvars pixmap, (w, data) = icon_window_data(w);
    returnif(pixmap == "undef" or not(data));
    if isstring(pixmap) then
        xved_get_icon_pixmap(pixmap, true) -> pixmap
    elseunless pixmap then
        0 -> pixmap
    endif;
    unless pixmap.isinteger or pixmap.isexternal_ptr_class then
        mishap(pixmap, 1, 'PIXMAP NEEDED');
    endunless;
    pixmap -> data(IW_PIXMAP);
    ;;; expose the window
    exacc raw_XClearArea(xveddisplay, w, 0, 0, 0, 0, true)
enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov 25 1996
        Extended xved_get_icon_pixmap to allow colour specs.
--- John Gibson, Nov  5 1996
        Gave xved_get_icon_pixmap a 2nd arg to say whether to colour pixmap
        or not.
--- John Gibson, Apr  1 1995
        Replaced use of int*vecs with EXPTRINITSTRs for correct types
--- John Gibson, Feb 22 1994
        # Got rid of iconwin record class, used an "IconWindow" descriptor with
          an XptDataProps vector instead
        # Replaced xved_de*stroy_icon_window with destroy action on iconwin
          structure
        # Made pixmaps be created as needed, instead of all at once.
--- Adrian Howard, Jun 18 1993
        xved_de*stroy_icon_window altered to take into account only shell
        widgets have destroy actions
--- John Gibson, Sep  7 1992
        Changed to use XptVal
--- Adrian Howard, Jun 12 1992
        -xved_destroy_icon_window- now only destroys widgets if we are in
        the same process XVed was invoked in
--- John Gibson, Mar 28 1992
        Made icon window exposures be dealt with by an event handler
--- John Gibson, Mar 26 1992
        Restored procedures xved_create_icon_window and xved_get_icon_filename
        and exported them
--- John Gibson, Mar  5 1992
        Made a number of procedures lconstants.
        Removed descriptor field from iconwindow structure (unnecessary
        now bug in xpt_descriptor is fixed)
--- John Gibson, Nov 21 1991
        Changed -get_icon_filename- to use `_` as separator
        character in bitmap filenames instead of `.`
--- Jonathan Meyer, Sep  3 1991 Added get_icon_filename
--- John Gibson, Aug 10 1991
        Made -pixmaps- writeable
--- Adrian Howard, Aug  2 1991 : Changed to use -newanyproperty- rather than
        assoc
--- Jonathan Meyer, Jul 31 1991 Renamed XptWm -> XptWM
--- Jonathan Meyer, Jun 24 1991 added xved_icon_window_pixmap
--- Jonathan Meyer, Jun  13 1991 added test for icon sizes
--- Jonathan Meyer, Jun  13 1991 converted to use iconWindows
--- Jonathan Meyer, Jun  4 1991 added tmp and src pixmaps
--- Jonathan Meyer, Jun  2 1991
        Added xved_get_icon_pixmap
--- Jonathan Meyer, May 30 1991 Changed XVED_include xveddefs to be
        just include xveddefs.ph
 */
