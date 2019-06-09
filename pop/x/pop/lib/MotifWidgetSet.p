/*--- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/MotifWidgetSet.p
 > Purpose:         Motif Widget Set Interface
 > Author:          Jonathan Meyer, Jan 8 1990 (see revisions)
 > Documentation:   HELP *MOTIF
 > Related Files:   LIB * XptWidgetSet
 */
compile_mode :pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING

uses-now popxlib, Xm;

section;

include xdefs.ph;
include xpt_constants.ph;
include xpt_coretypes;
include xpt_generaltypes.ph;
include xpt_xtypes.ph;

uses XptWidgetSet;
uses xpt_general;

loadinclude XmConstants.ph;
loadinclude XmMisc.ph;
loadinclude XmMwmUtil.ph;
loadinclude XmVirtKeys.ph;

lconstant ws = XptNewWidgetSet("Motif");
XM_EXLIBS -> ws("WidgetSetLibList");

lconstant wsmembers =
[
        ;;; Motif classes
        PrimitiveWidget
        Gadget
        ManagerWidget
        ArrowButtonWidget
        ArrowButtonGadget
        BulletinBoardWidget
        CascadeButtonWidget
        CascadeButtonGadget
        CommandWidget
        DialogShellWidget
        DrawingAreaWidget
        DrawnButtonWidget
;;;     ExtObject
        FileSelectionBoxWidget
        FormWidget
        FrameWidget
        LabelWidget
        LabelGadget
        ListWidget
        MainWindowWidget
        MenuShellWidget
        MessageBoxWidget
        PanedWindowWidget
        PushButtonWidget
        PushButtonGadget
        RowColumnWidget
        ScaleWidget
        ScrollBarWidget
        ScrolledWindowWidget
        SelectionBoxWidget
        SeparatorGadget
        SeparatorWidget
        TextWidget
        TextFieldWidget
        ToggleButtonWidget
        ToggleButtonGadget
;;;     DesktopObject
;;;     WorldObject
;;;     DisplayObject
;;;     ScreenObject
;;;     ShellExtObject
;;;     VendorShellExtObject
];

wsmembers -> ws("WidgetSetMembers");


procedure(wcname);
    lvars wcname;
    consword('xm' sys_>< wcname sys_>< 'Class');
endprocedure -> ws("WidgetSetNameMapping");


;;; The following property is used to determine what libraries and
;;; external procedures to load for any givent widget class.

lconstant MotifDependencies = newproperty([

[global [ ['XmVirtKeys.ph' 'XmConstants.ph' 'Xmgeneral'] ^false]]

], 4, false, "perm");


;;; MOTIF DEPENDENCY TREE MANAGEMENT
define lconstant xm_widget_to_library_mapping(wsprefix, wcname) -> l;
    lvars wsprefix, wcname, l;
    XptGetWidgetDependencies(wsprefix, wcname,
                                    MotifDependencies, "Motif") -> l;
    if lmember(wcname, wsmembers) then
        consword('xm' sys_>< wcname) :: l -> l
    endif
enddefine;

xm_widget_to_library_mapping -> ws("WidgetSetFileMapping");
"Xm" -> ws("WidgetSetPrefix");

;;; MOTIF STARTUP
;;; load general procedures and id's and do startup
applist(xm_widget_to_library_mapping("Xm","global"), useslib);


global constant MotifWidgetSet = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- Julian Clinton, Jan 17 1994
        Added some loadincludes so code using XptWidgetSet compiles okay.
--- John Gibson, Apr 15 1993
        All widgets now in Xm/xm*.p files, and WidgetSetFileMapping
        procedure loads them. General code -> Xm/Xmgeneral.p.
--- John Gibson, Sep 10 1992
        Files included instead of used.
--- Robert John Duncan, Dec  5 1991
        Disabled loading of -XmGetDestination- on MIPS RISC/os because it's
        not provided in their Motif library
--- Simon Nichols, Dec  3 1991
        Added -lPW to XMOTIF_LIBS for HP-UX 8.0.
--- John Gibson, Nov  1 1991
        VMS mods
--- Robert John Duncan, Sep 20 1991
        Removed incorrect test for DECstation
--- Jonathan Meyer, Sep  6 1991
        Reinstalled new strategy for determining WidgetSetLibList
--- Andreas Schoter, Jul 18 1991
        Removed general widget dependency tree code to LIB *XptWidgetSet
        Added global constant for compatibility with uses.
--- Jonathan Meyer, Jul  3 1991
        Moved string coerce routines to XmConstants.p
        Made this file do uses Xm/XmConstants.p;
--- Robert John Duncan, Jun 11 1991
        New strategy for determining WidgetSetLibList
--- Jonathan Meyer, May 25 1991
        Added string coerce routine as well
--- Jonathan Meyer, May 25 1991
        Added import updater for XmStrings
--- Jonathan Meyer, Apr 24 1991
        Fixed dependency tree lookup for gadgets.
--- Robert John Duncan, Mar 15 1991
        No "WidgetSetLibList" for DECstation because everything is
        statically linked.
 */
