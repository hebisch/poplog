/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xm/xmShellExtObject.p
 > Purpose:         Motif widgetclass
 > Author:          John Gibson, Apr 14 1993
 > Documentation:   HELP * MOTIF
 > Related Files:   Xm/xm*Widget.p etc
 */
compile_mode :pop11 +strict;

include sysdefs.ph;

#_IF DEF VMS
    #_TERMIN_IF DEF POPC_COMPILING
    mishap('ShellExtObject', 1, 'WIDGETCLASS NOT SUPPORTED IN VMS');
#_ENDIF

section;
exload_batch;

include xpt_coretypes.ph;

uses Xmgeneral, xmExtObject;

XptLoadWidgetClass xmShellExtObject [^^XM_EXLIBS]
    xmShellExtObject    <- xmShellExtObjectClass,
;

define XmIsShellExtObject =
    XtIsSubclass(% xmShellExtObject %)
enddefine;

endexload_batch;
endsection;
