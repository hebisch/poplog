/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptPopValue.p
 > Purpose:         High-level resource access/update facility
 > Author:          Jonathan Meyer, Mar  9 1991 (see revisions)
 > Documentation:   REF *XPT_RESOURCE
 > Related Files:   LIB *XptResourceInfo, LIB *XptSpecifyPopValueTypes
 */
compile_mode :pop11 +strict;

/********************************************************************
 *              HIGH LEVEL RESOURCE ACCESS/UPDATE LIBRARY
 ********************************************************************/

section $-Xpt => XptPopValueError, XptPopValue;

include xt_constants.ph;
include xpt_constants.ph;
include xpt_xtypes.ph;
include xpt_coretypes.ph;

uses XptResourceInfo;
uses-now XptSpecifyPopValueTypes;

constant procedure PopValueTypes;
declare_incremental property PopValueTypes = newassoc([]);

/* DEFINE DEFAULT SETUP OF TYPES */

lconstant
    ;;; Descriptor types that we import, but which aren't in
    ;;; generaltypes or xtypes

    ;;; these don't event have an XDT defined, but should do...
    XptImportAcceleratorTable = XptImportAny(%"AcceleratorTable"%),
    XptImportFontStruct = XptImportAny(%"FontStruct"%),
    XptImportStringList = XptImportAny(%"StringList"%),

    ;;; these are defined, but not in generaltypes or xtypes
    XptImportTranslationTable = XptImportAny(%XDT_TRANSLATIONS%),
    XptImportCallbackList = XptImportAny(%XDT_CALLBACKLIST%),
    XptImportWindow = XptImportAny(%XDT_WINDOW%),
    XptImportWidgetList = XptImportAny(%XDT_WIDGETLIST%),
    XptImportScreenPtr = XptImportAny(%XDT_SCREENPTR%),
;

XptSpecifyPopValueTypes([
    ;;; Representation Types <-> Xpt types

    ;;; All the basic toolkit types
    [AcceleratorTable :exptr#XptImportAcceleratorTable]
    [Atom :XptAtom]
    [Bitmap :XptXID]
    [Bool :XptLongBoolean]  ;;; long bool
    [Boolean :XptBoolean]
    [CallProc :XptProcedure]
    [Callback :exptr#XptImportCallbackList]
    [Cardinal :XptCardinal]
    [Color :XptColor]
    [Colormap :XptColormap]
    [Cursor :XptCursor]
    [Dimension :XptDimension]
    [Display :XptDisplayPtr]
    [Enum :XptEnum]
    [File :XptFile]
    [Float :XptFloat]
    [Font :XptFont]
    [FontStruct :exptr#XptImportFontStruct]
    [Function :XptProcedure]
    [GCCapStyle :uint]
    [GCFunction :uint]
    [GCJoinStyle :uint]
    [GCLineStyle :uint]
    [GCSubwindowMode :uint]
    [Geometry :XptString]
    [Int :XptInt]
    [LongBoolean :XptLongBoolean]   ;;; long bool
    [Object :XptWidget]
    [Pixel :XptPixel]
    [Pixmap :XptPixmap]
    [Pointer :XptPointer]
    [Position :XptPosition]
    [Screen :exptr#XptImportScreenPtr]
    [Short :XptShort]
    [String :XptString]
    [StringArray :exptr#XptImportStringList]
    [StringTable :exptr#XptImportStringList]
    [TranslationTable :exptr#XptImportTranslationTable]
    [UnsignedChar :XptUnsignedChar]
    [Visual :XptVisual]
    [Widget :XptWidget]
    [WidgetClass :XptWidgetClass]
    [WidgetList :exptr#XptImportWidgetList]
    [Window :exptr#XptImportWindow]

    ;;; Representation sizes <-> pop-11 types
    [^SIZEOFTYPE(:XptInt,:1)   :XptInt]
    [^SIZEOFTYPE(:XptShort,:1) :XptShort]
    [^SIZEOFTYPE(:XptChar,:1)  :XptChar]

    ;;; Default type - XptPointer - keyed on "DefaultRepresentation"
    [DefaultRepresentation :XptPointer]

    ;;; Conversion types - keys -> Reperesentation types/sizes
    ;;; (we currently specify strings and integers as typed args)
    [^string_key {String ^false}]
    [^integer_key {Int ^SIZEOFTYPE(:XptInt,:1)}]
]);


vars procedure XptPopValueError = mishap;

lconstant
    ;;; size of a value buffer in BYTES
    ;;; (NB: no checking for overflow, but all existing ones seem to be
    ;;; 1 word anyway ...)
    RESOURCE_BYTE_SIZE = SIZEOFTYPE(:dfloat),

    ;;; default coercion type - XptPointer type
    DEFAULT_REP     = "DefaultRepresentation",
;

lvars
    free_value_ptrs = pdtolist(initexptr_mem(%RESOURCE_BYTE_SIZE%)),
;

;;; get resource info, with error if we can't find it
define lconstant Resource_info(widget, name) /* -> (size, rep) */;
    lvars widget, name, res_info;
    unless (XptResourceInfo(widget, name) ->> res_info) then
        XptPopValueError(widget,name,2,'NO SUCH RESOURCE');
        false, false
    else
        res_info(2), res_info(1);
    endunless;
enddefine;

;;; get information about coercion for resource
define lconstant Coerce_info(name, rep, size) -> coerce;
    lvars name, rep, coerce, size;
    unless (PopValueTypes(rep) or PopValueTypes(size)) ->> coerce then
        XptPopValueError(name,rep,size,3,'UNKNOWN RESOURCE TYPE/SIZE');
        PopValueTypes(DEFAULT_REP) -> coerce;
    endunless
enddefine;

;;; Generate args for resource fetch
define lconstant Do_get_arg(widget, name, ptr) -> (name, ptr, coerce);
    lvars widget, name, ptr, coerce, rep, size;
    Resource_info(widget, name) -> (size, rep);
    Coerce_info(name, rep, size) -> coerce;
enddefine;

;;; Generate args for resource set
define lconstant Do_set_arg(widget, name, value);
    lvars widget, name, value, coerce, rep, size, typed=false;
    Resource_info(widget, name) -> (size, rep);

    ;;; See if the object should be specified as typed
    if (PopValueTypes(datakey(value)) ->> coerce) and coerce(1) /== rep then
        coerce -> typed; coerce(1) -> rep; coerce(2) -> size;
    endif;

    Coerce_info(name, rep, size) -> coerce;

    ;;; coerce data with access procedure whose updater just returns the
    ;;; converted value
    value -> coerce() -> value;

    if typed then
        ;;; We've come across a typed arg
        XtVaTypedArg, name, typed(3), value, typed(2) or datalength(value)+1
    else
        name, value
    endif;
enddefine;

define lconstant Check_args() -> num_args;
    lvars sl = stacklength(), i, arg, widget, num_args;
    ;;; Check items on stack and return number of resource name arguments
    ;;; need at least 2 arguments
    if sl fi_< 2 then clearstack() -> endif;

    fast_for i from 1 to sl do
        ;;; descend down stack
        subscr_stack(i) -> arg;
        if i /== 1 and XptIsType(arg, "Widget") then
            ;;; hit a widget - check its live
            XptCheckWidget(arg)->widget;
            i fi_- 1 -> num_args;
            quitloop;
        elseunless arg.isstring then
            ;;; hit a resource name - check its a string
            mishap(arg,1,'STRING NEEDED');
        endif;
    endfast_for;
    unless widget then mishap(0,'WIDGET NEEDED'); endunless;
enddefine;

define XptPopValue();
    lvars widget, num_args, names, name,
            value_ptrs=free_value_ptrs, value_ptr,
            types=[], procedure type;

    ;;; get resource names
    conslist(Check_args() ->> num_args) -> names; -> widget;

    fast_XtVaGetValues(widget,
        #| fast_for name in names do
                ;;; get new value pointer, remember it, and stack argument
                dest(free_value_ptrs) -> (value_ptr, free_value_ptrs);
                Do_get_arg(widget, name, value_ptr) :: types -> types;
                endfor
        |# );
    ;;; convert all resources to correct types
    fast_for value_ptr,type in value_ptrs, fast_ncrev(types) do
        fast_apply(value_ptr, type);
    endfast_for;

    ;;; reclaim lists
    sys_grbg_list(types); sys_grbg_list(names);
    value_ptrs -> free_value_ptrs;
enddefine;


define updaterof XptPopValue();
    lvars widget, num_args, names, name, values, value;

    ;;; get resource names, widget, and resource values
    conslist(Check_args() ->> num_args) -> names; -> widget;
    conslist(num_args) -> values;

    ;;; no limit - this is simple:
    fast_XtVaSetValues(widget,
        #| fast_for name,value in names,values do
                Do_set_arg(widget, name, value)
            endfast_for
        |# );
    ;;; reclaim lists.
    sys_grbg_list(names);
    sys_grbg_list(values);
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, May 12 1995
        Type info from PopValueTypes is now an access procedure whose
        updater just returns the converted value
--- John Gibson, Mar 25 1993
        Moved initialisation of PopValueTypes into this file from
        XptSpecifyPopValueTypes.p
--- John Gibson, Mar 22 1993
        Removed MAX_EXT_CALL_ARGS. CHanged to use initexptr_mem for
        free_value_ptrs.
--- Jonathan Meyer, Mar 12 1991 Changed definition of MAX_EXT_CALL_ARGS
    since JohnG fixed it for 14.01 Poplog.
 */
