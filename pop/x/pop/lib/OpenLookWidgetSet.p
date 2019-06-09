/*--- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.x/x/pop/lib/OpenLookWidgetSet.p
 > Purpose:         Open Look Widget Set Interface
 > Author:          Tom Khabaza, Jonathan Meyer, Aug 14 1990 (see revisions)
 > Documentation:   HELP * OPENLOOK
 > Related Files:   LIB * XptWidgetSet
 */

#_TERMIN_IF DEF POPC_COMPILING

uses-now popxlib, Xol;

section;

include xpt_constants;
include xpt_coretypes.ph;
include xpt_generaltypes.ph;
uses xpt_general;
uses XptWidgetSet;

loadinclude XolClients.ph;
loadinclude XolConstants.ph;
loadinclude XolDynamic.ph;
loadinclude XolFlat.ph;
loadinclude XolFont.ph;
loadinclude XolScroll.ph;
loadinclude XolTextEdit.ph;
loadinclude XolTextField.ph;
loadinclude Xolbuffer.ph;

if popmemlim - popmemused < 50000 then
    popmemlim + 50000 -> popmemlim; ;;; is this safe?
endif;

lconstant ws = XptNewWidgetSet("OpenLook");
XOL_EXLIBS -> ws("WidgetSetLibList");

lconstant wsmembers =
[   ;;; Xol Widget Classes
    AbbrevMenuButtonWidget
    ArrowWidget
    BaseWindowShellWidget
    BulletinBoardWidget
    ButtonWidget
    ButtonGadget
    CaptionWidget
    CheckBoxWidget
    ControlAreaWidget
    EventObj
    ExclusivesWidget
    FlatCheckBoxWidget
    FlatExclusivesWidget
    FlatNonexclusivesWidget
    FlatWidget
    FooterPanelWidget
    FormWidget
    HelpWidget
    ListPaneWidget
    MagWidget
    ManagerWidget
    MenuShellWidget
    MenuButtonWidget
    MenuButtonGadget
    NonexclusivesWidget
    NoticeShellWidget
    OblongButtonWidget
    OblongButtonGadget
    PopupWindowShellWidget
    PushpinWidget
    RectButtonWidget
    ScrollbarWidget
    ScrolledWindowWidget
    ScrollingListWidget
    SliderWidget
    StaticTextWidget
    StubWidget
    TextEditWidget
    TextFieldWidget
% #_IF XOL_VERSION > 2000
    ;;; new in OLIT version 2.5
    "GaugeWidget",
#_ENDIF
#_IF XOL_VERSION >= 3000
    ;;; new in OLIT version 3.0
    "DrawAreaWidget",
    "DropTargetWidget",
    "RubberTileWidget",
#_ENDIF
#_IF XOL_VERSION <= 3000
    ;;; dropped from OLIT version 3.0.1
    "TextWidget",
    "TextPaneWidget",
#_ENDIF %
];

wsmembers -> ws("WidgetSetMembers");

;;; The following property is used to determine what libraries and
;;; external procedures to load for any givent widget class.

lconstant OpenLookDependencies = newproperty(
[   [global [ ['Xolgeneral' 'XolConstants.ph'] ^false]]
    [Dynamic [ ['XolDynamic' 'XolDynamic.ph'] ^false]]
    [OlCursor ['XolOlCursor' ^false]]
    [Font ['XolFont.ph' ^false]]   ;;; no references to this?
    [Form [^false ^false]]   ;;; Added, JM, 12/3/91
    [Manager ['XolManager' ^false]]
    [buffer [['Xolbuffer' 'Xolbuffer.ph'] ^false]]
    [TextEdit [['XolTextEdit.ph' Dynamic buffer] ^false]]
    [TextField ['XolTextField.ph' ^false]]
    [ScrolledWindow ['XolScroll.ph' ^false]]
    [Arrow ['XolScroll.ph' ^false]]
    [Flat [ ['XolFlat' 'XolFlat.ph'] ^false]]
    [Scrollbar ['XolScroll.ph' ^false]]
    [ScrollingList ['XolScroll.ph' ^false]]

    [BulletinBoard [Manager ^false]]
    [Caption [Manager ^false]]
    [CheckBox [Manager ^false]]
    [ControlArea [Manager ^false]]
    [Exclusives [Manager ^false]]
    [FlatExclusives [Flat ^false]]
    [FlatNonExlusives [Flat ^false]]
    [FooterPane [Manager ^false]]
    [Help [Manager ^false]]
    [MenuButton ['XolMenu' ^false]]
    [MenuShell ['XolMenu' ^false]]
    [Nonexclusives [Manager ^false]]
    [Text [Manager ^false]]

], 60, false, "perm");


;;; OPEN LOOK DEPENDENCY TREE MANAGEMENT
define lconstant ol_widget_to_library_mapping(wsprefix, wcname) -> l;
    lvars wsprefix, wcname, l;
    XptGetWidgetDependencies(wsprefix, wcname,
                                    OpenLookDependencies, "OpenLook") -> l;
    if lmember(wcname, wsmembers) then
        consword('xol' sys_>< wcname) :: l -> l
    endif
enddefine;

ol_widget_to_library_mapping -> ws("WidgetSetFileMapping");
"Xol" -> ws("WidgetSetPrefix");

;;; load general procedures and id's and do startup
applist(ol_widget_to_library_mapping("Xol","global"), useslib);
global constant OpenLookWidgetSet = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- Integral Solutions Ltd (Julian Clinton), Mar  4 1994
        Modified #_IF test for GaugeWidget to be > 2000.
--- Julian Clinton, Jan 17 1994
        Added some loadincludes so code using XptWidgetSet compiles okay.
--- John Gibson, Apr  9 1993
        All widgets now in Xol/xol*.p files, and WidgetSetFileMapping
        procedure loads them. Startup code -> Xol/Xolgeneral.p.
--- John Gibson, Sep  9 1992
        Made it include xpt_generaltypes.ph and use xpt_general.p instead of
        using xpt_generaltypes.p (so perm typespecs aren't loaded)
--- Robert John Duncan, Aug  7 1992
        TextWidget and TextPaneWidget dropped from OLIT 3.0.1
--- Jonathan Meyer, Apr 21 1991
        Added functions and widgets new to OLIT 3.0. Changed the startup
        code to call OlToolkitInitialize rather than OlInitialize.
--- Adrian Howard, Feb 20 1992 : LookupOlColors and OlGetApplicationResources
        are not loaded in post-version 2.0 OLIT (no longer supported by Sun)
--- Adrian Howard, Sep 27 1991
        -XptSetUnprocessedArgs- --> -fast_XptSetUnprocessedArgs-
--- Simon Nichols, Sep 18 1991
        Fixed argument processing in -XpolInitialize-.
--- Jonathan Meyer, Sep 12 1991
        Removed setting of sysxsetup.
--- Andreas Schoter, Sep  9 1991
        Changed coersion of returned value for OlGet{50/75}PercentGrey from
        exptr to ulong. Changed coercion of returned value for
        LastTextBufferLocation from int to exptr.
--- Ian Rogers, Aug 27 1991
        Changed applist(..., loadlib) to use -useslib-
--- Jonathan Meyer, Aug 19 1991
        Now assigns XpolDefaultSetup to XptDefaultSetup
--- Andreas Schoter, Jul 18 1991
        Removed general widget set dependency code to XptWidgetSet
        Added global constant for compatibility with uses.
--- Jonathan Meyer, Jul  1 1991
        Added use of XolMenu.p
--- Jonathan Meyer, Jun 27 1991
        Added loud warning before calling XpolDefaultSetup directly
--- Jonathan Meyer, Jun 27 1991
        Corrected XpolDefaultSetup to work if the user has already called
        XptDefaultSetup - though this is still not a good thing to do.
--- Robert John Duncan, Jun 11 1991
        New strategy for determining WidgetSetLibList
--- Roger Evans, May 25 1991
        Added code to set default appcontext async.
--- Jonathan Meyer, Mar 12 1991
        Added load of XolForm, which uses XptSpecifyResourceInfo to correct
        some wrongly typed resource
--- Jonathan Meyer, Feb 13 1991
        Added support for gadgets, which now work properly
--- Jonathan Meyer, Feb  6 1991
        Renamed all XptOl variables Xpol.

        modified XpolDefaultSetup, splitting it into two parts,
        XpolInitialize and XpolDefaultSetup.

        Added conditional to call XpolInitialize if a setup has already
        been performed, so that OpenLook widgets will work even after a
        Xpt toolkit initialize has been performed.
--- Roger Evans, Feb  6 1991 cleaned up OlInitialize in light of new
        system features
--- Roger Evans, Jan 27 1991  OlInitialize bug now fixed properly!
--- Jonathan Meyer, Jan 18 199
        Put OlInitialize back to previous definition. XptImportWidget
        not fixed yet !
--- Jonathan Meyer, Jan 17 1991
        Modified OlInitialize so that it returns a widget. Previously
        a bug in XptImportWidget meant that OlInitialize had to return an
        external_ptr.
--- Jonathan Meyer, Dec  5 1990 Added BulletinBoardWidget
--- Jonathan Meyer, Dec  5 1990 Modified guard on sysxsetup
--- Jonathan Meyer, Nov 30 1990
        Commented out Gadgets, since they can't be used anyway.
        Added redefine of sysxsetup. Set application context name.
        Added guard to increase popmemlim.
--- Jonathan Meyer, Nov 21 1990
        Fixed lconstant declaration bug
--- Roger Evans, Nov 19 1990 changed xpt_coerce to xpt_generaltypes
 */
