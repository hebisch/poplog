/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xm/Xmgeneral.p
 > Purpose:         Xm general procedures
 > Author:          John Gibson, Apr 14 1993 (see revisions)
 > Documentation:   HELP * MOTIF
 */
compile_mode :pop11 +strict;

section;
exload_batch;

include sysdefs.ph;
include xpt_coretypes.ph;
include XmConstants.ph;

XptLoadWidgetClass Xmgeneral [^^XM_EXLIBS]
    xmPrimitiveWidget   <- xmPrimitiveWidgetClass,
    xmGadget            <- xmGadgetClass,
    xmManagerWidget     <- xmManagerWidgetClass
;

define XmIsPrimitive =
    XtIsSubclass(% xmPrimitiveWidget %);
enddefine;

define XmIsGadget =
    XtIsSubclass(% xmGadget %);
enddefine;

define XmIsManager =
    XtIsSubclass(% xmManagerWidget %);
enddefine;

;;; N.B. Some other procedures are loaded by XpmImportString.p

XptPopLoadProcedures ''
    ;;; Color code
    XmSetColorCalculation(x) :void,
    XmGetColorCalculation() :XptProcedure,
    XmGetColors(t,u,v,w,x,y,z) :void,
    XmCvtStringToUnitType(w,x,y,z) :void,

    XmSetFontUnit(x,y) :void,
    XmSetFontUnits(x,y,z) :void,
    XmStringDirectionCreate(x) :XmString,
    XmStringSeparatorCreate() :XmString,

    XmStringLtoRCreate(x,y) :XmString,

    XmStringGetNextComponent(t,u,v,w,x,y,z) :XmStringComponentType,
    XmStringPeekNextComponent(x) :XmStringComponentType,
    XmStringGetLtoR(x,y,z) :XptBoolean,
    XmFontListCreate(x,y) :XmFontList,
    XmStringCreateFontList(x,y) :XmFontList,
    XmFontListFree (x) :void,
    XmFontListAdd(x,y,z) :XmFontList,
    XmFontListCopy (x) :XmFontList,
    XmFontListInitFontContext(x,y) :XptBoolean,
    XmFontListGetNextFont(x,y,z) :XptBoolean,
    XmFontListFreeFontContext (x) :void,
    XmFontListNextEntry(x) :XmFontListEntry,
    XmFontListEntryGetTag(x) :exptr,
    XmFontListEntryGetFont(x,y) :exptr,
    XmStringConcat (x,y) :XmString,
    XmStringNConcat(x,y,z) :XmString,
    XmStringCopy (x) :XmString,
    XmStringNCopy (x,y) :XmString,
    XmStringByteCompare (x,y) :XptBoolean,
    XmStringCompare(x,y) :XptBoolean,
    XmStringLength (x) :int,
    XmStringEmpty (x) :XptBoolean,
    XmStringHasSubstring(x,y) :XptBoolean,
    XmStringBaseline(x,y) :ushort,
    XmStringWidth (x,y) :ushort,
    XmStringHeight (x,y) :ushort,
    XmStringExtent(w,x,y,z) :void,
    XmStringLineCount (x) :int,
    XmStringDraw(p,q,r,s,t,u,v,w,x,y,z) :void,
    XmStringDrawImage(p,q,r,s,t,u,v,w,x,y,z) :void,
    XmStringDrawUnderline(o,p,q,r,s,t,u,v,w,x,y,z) :void,

    ;;; Misc
    XmGetSecondaryResourceData(x,y) :uint,
    XmTrackingLocate(x,y,z) :XptWidget,
    XmConvertUnits(v,w,x,y,z) :int,
;;; XmCvtFromHorizontalPixels(x,y,z) :int,
;;; XmCvtFromVerticalPixels(x,y,z) :int,
;;; XmCvtToHorizontalPixels(x,y,z) :int,
;;; XmCvtToVerticalPixels(x,y,z) :int,
    XmCvtCTToXmString(x) :XmString,
    XmCvtXmStringToCT(x) :exptr.exacc_ntstring,
    XmCvtTextToXmString(u,v,w,x,y,z) :XptBoolean,
    XmCvtXmStringToText(u,v,w,x,y,z) :XptBoolean,

    XmAddTabGroup(x) :void,
    XmRemoveTabGroup(x) :void,
    XmProcessTraversal(x,y) :XptBoolean,
    XmUninstallImage(x) :XptBoolean,
    XmDestroyPixmap(x,y) :XptBoolean,
    XmInstallImage(x,y) :XptBoolean,
    XmGetPixmap(w,x,y,z) :ulong,        ;;; Pixmap

    XmGetMenuCursor(x) :ulong,          ;;; Cursor
    XmSetMenuCursor(x,y) :void,

    XmUpdateDisplay(x) :void,
    XmResolvePartOffsets(x,y) :void,
    XmResolveAllPartOffsets(x,y,z) :void,

    XmIsMotifWMRunning(w) :XptBoolean,  ;;; defined in Vendor.o

    XmGetDestination(x) :XptWidget,
;

/* Do we need the following?

;;; extern int xmUseVersion;
exload ''
    xmUseVersion: int
endexload;
*/


shadowclass XpmStringTable [props "XmStringTable"] [nc, prefix nc_]
        :XmString[];

shadowclass XpmAnyCallbackStructPtr [props "XmAnyCallbackStructPtr"]
    {:XpmAnyCallbackStruct};


constant macro MOTIF = true;

constant Xmgeneral = true;

endexload_batch;
endsection;


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Jun 26 1997
        Added some extra FontList procedures
--- Robert Duncan, Mar 12 1997
        More XmString procedures moved to XpmImportString.p
--- Robert John Duncan, Mar  3 1995
        Removed reference to RIS*COS
 */
