/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.x/x/src/xt_impl.ph
 > Purpose:         Xtoolkit - implementation dependent declarations
 > Author:          Roger Evans, Sep  9 1988 (see revisions)
 > Documentation:
 > Related Files:   xt_declare.ph
 */

/*  XtVersion is an implementation specific identifier - we derive
    a default from XtSpecificationRelease (defined in xt_declare.ph)  */

#_IF not(DEF _XtVersion)
    lconstant macro _XtVersion = 11000 + _XtSpecificationRelease;
#_ENDIF


/* Macros dependent on X toolkit implementation */

lconstant macro (XtFalse = 0, XtTrue = 1);

lconstant macro (
    ;;; XtCallbackStatus enum values
    _XtCallbacksNoList = _0,
    _XtCallbacksHasNone = _1,
    _XtCallbacksHasSome = _2,
);


/*  Pick up external widgetclass and widget structure. Include enough fields
    to cover our needs - derived from CoreP.h and CompositeP.h

    there should be a better way to do this ...
*/

/*  first, some mappings from C data-types to pop ones */

deftype
    XtPointer       = <byte>,

    Position        = -short,
    Dimension       = short,
    XID             = long,

    String          = <byte>,
    Widget          = XtPointer,
    WidgetList      = <Widget>,
    WidgetClass     = XtPointer,
    CompositeWidget = Widget,
    XtActionList    = XtPointer,
    XtEventTable    = XtPointer,
    XtBoundAccActions= XtPointer,
    Cardinal        = int,
    ShortCard       = short,
    Boolean         = byte,
    XtAppContext    = XtPointer,
    XtValueMask     = long,
    XtIntervalId    = long,
    XtInputId       = long,
    XtWorkProcId    = long,
    XtGeometryMask  = int,
    XtGCMask        = long,
    Pixel           = long,
    XtArgVal        = long,
    XrmQuark        = int,
    XrmName         = XrmQuark,
    XtCallbackList  = XtPointer,
    XtTranslations  = XtPointer,
    XtOrderProc     = XtPointer,
    Screen_*        = XtPointer,
    Pixmap          = XID,
    Colormap        = XID,
    Window          = XID
;


;;; raw widget structure
struct XRAWWIDGET {
    ;;; core fields
    Widget          XRW_self;
    WidgetClass     XRW_widget_class;
    Widget          XRW_parent;
    XrmName         XRW_xrm_name;
    Boolean         XRW_being_destroyed;
    XtCallbackList  XRW_destroy_callbacks;
    XtPointer       XRW_constraints;
    Position        XRW_x, XRW_y;
    Dimension       XRW_width, XRW_height;
    Dimension       XRW_border_width;
    Boolean         XRW_managed;
    Boolean         XRW_sensitive;
    Boolean         XRW_ancestor_sensitive;
    XtEventTable    XRW_event_table;
    ;;; should be an embedded TMRec struct here...
    XtPointer       XtTMRec[4];
    XtTranslations  XRW_accelerators;
    Pixel           XRW_border_pixel;
    Pixmap          XRW_border_pixmap;
    WidgetList      XRW_popup_list;
    Cardinal        XRW_num_popups;
    String          XRW_name;
    Screen_*        XRW_screen;
    Colormap        XRW_colormap;
    Window          XRW_window;
    Cardinal        XRW_depth;
    Pixel           XRW_background_pixel;
    Pixmap          XRW_background_pixmap;
    Boolean         XRW_visible;
    Boolean         XRW_mapped_when_managed;

    ;;; composite fields
    WidgetList      XRW_children;
    Cardinal        XRW_num_children;
    Cardinal        XRW_num_slots;
    XtOrderProc     XRW_insert_position;
};

;;; raw widget class structure
struct XRAWWIDGETCLASS {
    WidgetClass     XRWC_superclass;
    String          XRWC_class_name;
    ;;; ... and loads more we don't need
};



/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 29 1995
        Revised all the types and changed to use deftype
--- Roger Evans, Jan 18 1991 removed r2(!) support
 */
