/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptNewWindow.p
 > Purpose:         Simple interface to make and destroy windows
 > Author:          Andreas Schoter,  Sept 6 1990 (see revisions)
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

include xpt_constants.ph;

section;
exload_batch;

uses
    xt_widget,
;

define XptNewWindow(name, size, args, class) -> widget;
    lvars   name, size, class, args, shellargs = [], argstring, shell, l,
            shellname, widget;

    ;;; look for optional shell args
    if islist(class) then
        ((), name, size, args, class) -> (name, size, args, class, shellargs);
    endif;

    name sys_>< '_shell' -> shellname;

    unless XptDefaultDisplay then XptDefaultSetup(); endunless;

    [%  if size then {geometry ^(XptGeometrySpec(size))} endif,
        {title ^name},
        {iconName ^name},
        {allowShellResize ^true}
    %] nc_<> shellargs -> shellargs;

    XtAppCreateShell(shellname, XT_POPLOG_CLASSNAME,
        xtApplicationShellWidget,XptDefaultDisplay,
        XptArgList(shellargs)
    ) -> shell;

    XtCreateManagedWidget(name, class, shell,
        XptArgList([% if size then
                        {height ^(size(1))}, {width ^(size(2))}
                      endif
                    %] nc_<> args)
    ) -> widget;

    XtRealizeWidget(shell);
enddefine;

endexload_batch;
endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  3 1993
        Uses xtApplicationShellWidget
--- Adrian Howard, Feb 17 1992 : Moved -XptDestroyWindow- into it's own file
        so it can autoload seperately
--- John Gibson, Aug 15 1991 Added allowShellResize true, tidied up
--- Jonathan Meyer, Jan 17 1991 Moved to auto
--- Adrian Howard, Dec  3 1990 : Changed library name to XptNewWindow.p
--- Roger Evans, Nov 20 1990 corrected initialisation of shell_name
--- Roger Evans, Nov 11 1990 tidied up
--- Jonathan Meyer, Sep 19 1990 Renamed XpwAppShell AppShell. Used XptArgList
--- Adrian Howard, Sep 12 1990   Made procedures global
--- Andreas Schoter, Sep  6 1990 Mended a missing closing bracket
--- Andreas Schoter, Sep  6 1990 Changed to XptWindow for name compatabilty
    and modified calls to consArgList to use correct format
 */
