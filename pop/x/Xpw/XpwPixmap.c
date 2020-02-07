/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 * File:    C.x/x/Xpw/XpwPixmap.c
 * Version: Revision 5.0
 * Purpose: C code for XpwPixmap widget.
 * Author:  Jonathan Meyer (see revisions)
 * Date:    9 April 1989
 * Notes:       This widget is not a widget in the usual sense. Creating
 *              a XpwPixmap widget does not make a window on the screen,
 *              but instead creates a pixmap. See Graphics.c.
 */

/* using Xpw header files locally, not in <X11/Xpw/...> */
#define XpwNotInstalled

#include <ctype.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include "XpwPixmapP.h"

/****************************************************************
 *
 * Declarations
 *
 ****************************************************************/

/* Declare local widget class procedures. */

/* private procedures*/
static void ClassInit(), Initialize(), Realize(), Destroy(), Resize();
static Boolean SetValues(), SetSubpartValues();
static void InitSubpartValues(), GetSubpartValues();
static XpwMethodRet ApplyMethod();

/* external declarations */
/*
#ifdef XtSpecificationRelease
extern XmuCvtStringToBitmap();
#else
extern XmuCvtStringToPixmap();
#endif
*/
extern void _XpwCondUpdateUsersGC();

externalref XtGCMask _xpwGCvaluemask;
externalref XGCValues _xpwGCvalues;

/* macros */
#define GCsubpart(w) w->xpwcore.users_gc->values

/* GCsubpartSet - sets _xpwGCvalues slots to mirror relevant
   fields of the values argument. Must not interfere with unused members of
   the _xpwGCvalues structure.
   Is it quicker to copy whole structure and unset fields that we aren't
   interested in?
*/

#define GCsubpartSet(values) \
    _xpwGCvalues.function = values.function ; \
    _xpwGCvalues.plane_mask = values.plane_mask ; \
    _xpwGCvalues.line_width = values.line_width ; \
    _xpwGCvalues.line_style = values.line_style ; \
    _xpwGCvalues.cap_style = values.cap_style ; \
    _xpwGCvalues.join_style = values.join_style ; \
    _xpwGCvalues.fill_style = values.fill_style ; \
    _xpwGCvalues.fill_rule = values.fill_rule; \
    _xpwGCvalues.arc_mode = values.arc_mode ; \
    _xpwGCvalues.tile = values.tile ; \
    _xpwGCvalues.stipple = values.stipple ; \
    _xpwGCvalues.ts_x_origin = values.ts_x_origin ; \
    _xpwGCvalues.ts_y_origin = values.ts_y_origin ; \
    _xpwGCvalues.subwindow_mode = values.subwindow_mode ; \
    _xpwGCvalues.clip_x_origin = values.clip_x_origin ; \
    _xpwGCvalues.clip_y_origin = values.clip_y_origin ; \
    _xpwGCvalues.clip_mask = values.clip_mask ; \
    _xpwGCvalues.dash_offset = values.dash_offset ; \
    _xpwGCvalues.dashes = values.dashes ;

#define GC_FIELD(values, field, mask) ((values.field == _xpwGCvalues.field) ? 0 : mask)

static XtGCMask GCsubpartMask(values)
XGCValues values;
{
    XtGCMask valuemask;

    /* used to find which relevant members of values differ from the
       global XGCValues structure (_xpwGCvalues), setting the relevant
       fields of the mask to reflect these differences. Could use a for
       loop with a structure-style array, but still think this version
       is faster.
    */
    valuemask = (
        GC_FIELD(values, function, GCFunction) |
        GC_FIELD(values, plane_mask, GCPlaneMask) |
        GC_FIELD(values, line_width, GCLineWidth) |
        GC_FIELD(values, line_style, GCLineStyle) |
        GC_FIELD(values, cap_style, GCCapStyle) |
        GC_FIELD(values, join_style, GCJoinStyle) |
        GC_FIELD(values, fill_style, GCFillStyle) |
        GC_FIELD(values, fill_rule, GCFillRule) |
        GC_FIELD(values, arc_mode, GCArcMode) |
        GC_FIELD(values, tile, GCTile) |
        GC_FIELD(values, stipple, GCStipple) |
        GC_FIELD(values, ts_x_origin, GCTileStipXOrigin) |
        GC_FIELD(values, ts_y_origin, GCTileStipYOrigin) |
        GC_FIELD(values, subwindow_mode, GCSubwindowMode) |
        GC_FIELD(values, clip_x_origin, GCClipXOrigin) |
        GC_FIELD(values, clip_y_origin, GCClipYOrigin) |
        GC_FIELD(values, clip_mask, GCClipMask) |
        GC_FIELD(values, dash_offset, GCDashOffset) |
        GC_FIELD(values, dashes, GCDashList)
    );
    return(valuemask);
}


/****************************************************************
 *
 * Resources
 *
 ****************************************************************/

/* Resource declaration and Initialization of defaults */

#define offset(field) XtOffset(XpwPixmapWidget,xpwpixmap.field)
#define coffset(field) XtOffset(XpwCoreWidget,xpwcore.field)
#define goffset(field) XtOffset(Widget,core.field)

static XtResource resources[] = {
    {XtNpixmap, XtCPixmap, XtRPixmap, sizeof(Pixmap),
        offset(pixmap), XtRPointer, NULL},
    {XtNprivateGC, XtCGC, XtRPointer, sizeof(caddr_t),
        offset(private_gc), XtRPointer, NULL},
    {XtNpixmapStatus, XtCPixmapStatus, XtRPixmapStatus, sizeof(PixmapStatus),
        offset(pixmap_status), XtRString, XtDefaultPixmapStatus},
};


#undef offset
#undef goffset

/* This structure contains values mirrored by the _xpwPixmapGCResources structure below, and is
   used to see which values differ from their defaults when a new GC is initialized. It is based
   on the default graphics context.
*/

static XGCValues default_gc_values = {
    GXcopy,         /* function */
    AllPlanes,      /* plane_mask */
    0,          /* (unused) foreground */
    0,          /* (unused) background */
    0,          /* line_width */
    LineSolid,      /* line_style */
    CapButt,        /* cap_style */
    JoinMiter,      /* join_style */
    FillSolid,      /* fill_stlye */
    EvenOddRule,        /* fill_rule */
    ArcPieSlice,        /* arc_mode */
    0,          /* tile */
    0,          /* stipple */
    0,          /* ts_x_origin */
    0,          /* ts_y_origin */
    0,          /* (unused) font */
    ClipByChildren,     /* subwindow_mode */
    True,           /* (unused) graphics_exposures */
    0,          /* clip_x_origin */
    0,          /* clip_y_origin */
    0,          /* clip_mask */
    0,          /* dash_offset */
    4,          /* dashes */
};


typedef XGCValues *XGCValPtr;
#define offset(field) XtOffset(XGCValPtr,field)

XtResource _xpwPixmapGCResources[] = {
    {XtNfunction, XtCFunction, XtRFunction, sizeof(int),
        offset(function), XtRImmediate, (caddr_t)GXcopy},
    {XtNplaneMask, XtCPlaneMask, XtRInt, sizeof(int),
        offset(plane_mask), XtRImmediate, (caddr_t)AllPlanes},
    {XtNlineWidth, XtCLineWidth, XtRInt, sizeof(int),
        offset(line_width), XtRImmediate, 0},
    {XtNlineStyle, XtCLineStyle, XtRLineStyle, sizeof(int),
        offset(line_style), XtRImmediate, (caddr_t)LineSolid},
    {XtNcapStyle, XtCCapStyle, XtRCapStyle, sizeof(int),
        offset(cap_style), XtRImmediate, (caddr_t)CapButt},
    {XtNjoinStyle, XtCJoinStyle, XtRJoinStyle, sizeof(int),
        offset(join_style), XtRImmediate, (caddr_t)JoinMiter},
    {XtNfillStyle, XtCFillStyle, XtRFillStyle, sizeof(int),
        offset(fill_style), XtRImmediate, (caddr_t)FillSolid},
    {XtNfillRule, XtCFillRule, XtRFillRule, sizeof(int),
        offset(fill_rule), XtRImmediate, (caddr_t)EvenOddRule},
    {XtNarcMode, XtCArcMode, XtRArcMode, sizeof(int),
        offset(arc_mode), XtRImmediate, (caddr_t)ArcPieSlice},
    {XtNtile, XtCTile, XtRPixmap, sizeof(Pixmap),
        offset(tile), XtRPointer, NULL},
    {XtNstipple, XtCStipple, XtRBitmap, sizeof(Pixmap),
        offset(stipple), XtRPointer, NULL},
    {XtNtileStipXOrigin, XtCTileStipXOrigin, XtRInt, sizeof(int),
        offset(ts_x_origin), XtRImmediate, 0},
    {XtNtileStipYOrigin, XtCTileStipYOrigin, XtRInt, sizeof(int),
        offset(ts_y_origin), XtRImmediate, 0},
    {XtNsubwindowMode, XtCSubwindowMode, XtRSubwindowMode, sizeof(int),
        offset(subwindow_mode), XtRImmediate, (caddr_t)ClipByChildren},
    {XtNclipXOrigin, XtCClipXOrigin, XtRInt, sizeof(int),
        offset(clip_x_origin), XtRImmediate, 0},
    {XtNclipYOrigin, XtCClipYOrigin, XtRInt, sizeof(int),
        offset(clip_y_origin), XtRImmediate, 0},
    {XtNclipMask, XtCClipMask, XtRBitmap, sizeof(Pixmap),
        offset(clip_mask), XtRPointer, NULL},
    {XtNdashOffset, XtCDashOffset, XtRInt, sizeof(int),
        offset(dash_offset), XtRImmediate, 0},
    {XtNdashes, XtCDashes, XtRInt, sizeof(char),
        offset(dashes), XtRImmediate, (caddr_t)4},
};
int _xpwPixmapGCResourceCount = XtNumber(_xpwPixmapGCResources);

#undef offset

/****************************************************************
 *
 * Methods
 *
 ****************************************************************/

/* define methods for pixmap widgets */

/* see Graphics.c */

externalref XpwMethod _xpwPixmapMethods[];
externalref int _num_xpwPixmapMethods;



/****************************************************************
 *
 * Quark Lists
 *
 ****************************************************************/

/*  Each GC value slot resource has resource converters. This requires
    a list of the names for the different values in each slot of the GC,
    a default value for the slot, and a hash table for the list
*/

typedef struct {
    String name;
    XrmQuark quark;
    Cardinal value;
} QuarkList;

#define VAL_OFFSET 128
/* this is added to all QuarkTable values so that value entries of 0 are
   handled properly.
*/

/* Function */

static void CvtStringToFunction();
static XrmQuark XrmQExtdefaultfunction;
static XpwAssocTable *functionQuarkTable = NULL;

static QuarkList functionQuarks [] = {
    {XtDefaultFunction, NULLQUARK, GXcopy},
        {"clear",NULLQUARK,GXclear},
        {"and",NULLQUARK,GXand},
        {"andreverse",NULLQUARK,GXandReverse},
        {"copy",NULLQUARK,GXcopy},
        {"andinverted",NULLQUARK,GXandInverted},
        {"noop",NULLQUARK,GXnoop},
        {"xor",NULLQUARK,GXxor},
        {"or",NULLQUARK,GXor},
        {"nor",NULLQUARK,GXnor},
        {"equiv",NULLQUARK,GXequiv},
        {"invert",NULLQUARK,GXinvert},
        {"orreverse",NULLQUARK,GXorReverse},
        {"copyinverted",NULLQUARK,GXcopyInverted},
        {"orinverted",NULLQUARK,GXorInverted},
        {"nand",NULLQUARK,GXnand},
        {"set",NULLQUARK,GXset},
};

/* Cap Style */
static void CvtStringToCapStyle();
static XrmQuark XrmQExtdefaultcapstyle;
static XpwAssocTable *capStyleQuarkTable = NULL;
static QuarkList capStyleQuarks [] = {
        {XtDefaultCapStyle,NULLQUARK,CapButt},
        {"notlast",NULLQUARK,CapNotLast},
        {"butt",NULLQUARK,CapButt},
        {"round",NULLQUARK,CapRound},
        {"projecting",NULLQUARK,CapProjecting},
};

/* JoinStyle */
static void CvtStringToJoinStyle();
static XrmQuark XrmQExtdefaultjoinstyle;
static XpwAssocTable *joinStyleQuarkTable = NULL;
static QuarkList joinStyleQuarks [] = {
        {XtDefaultJoinStyle, NULLQUARK, JoinMiter},
        {"miter",NULLQUARK,JoinMiter},
        {"round",NULLQUARK,JoinRound},
        {"bevel",NULLQUARK,JoinBevel},
};

/* LineStyle */
static void CvtStringToLineStyle();
static XrmQuark XrmQExtdefaultlinestyle;
static XpwAssocTable *lineStyleQuarkTable = NULL;
static QuarkList lineStyleQuarks [] = {
        {XtDefaultLineStyle, NULLQUARK, LineSolid},
        {"solid",NULLQUARK,LineSolid},
        {"onoffdash",NULLQUARK,LineOnOffDash},
        {"doubledash",NULLQUARK,LineDoubleDash},
};

/* FillStyle */
static void CvtStringToFillStyle();
static XrmQuark XrmQExtdefaultfillstyle;
static XpwAssocTable *fillStyleQuarkTable = NULL;
static QuarkList fillStyleQuarks [] = {
        {XtDefaultFillStyle, NULLQUARK, FillSolid},
        {"solid",NULLQUARK,FillSolid},
        {"tiled",NULLQUARK,FillTiled},
        {"stippled",NULLQUARK,FillStippled},
        {"opaquestippled", NULLQUARK, FillOpaqueStippled},
};

/* FillRule */
static void CvtStringToFillRule();
static XrmQuark XrmQExtdefaultfillrule;
static XpwAssocTable *fillRuleQuarkTable = NULL;
static QuarkList fillRuleQuarks [] = {
        {XtDefaultFillRule, NULLQUARK, EvenOddRule},
        {"evenoddrule",NULLQUARK,EvenOddRule},
        {"windingrule",NULLQUARK,WindingRule},
};

/* Subwindow Mode */
static void CvtStringToSubwindow();
static XrmQuark XrmQExtdefaultsubwindow;
static XpwAssocTable *subwindowQuarkTable = NULL;
static QuarkList subwindowQuarks [] = {
        {XtDefaultSubwindow, NULLQUARK, ClipByChildren},
        {"clipbychildren", NULLQUARK, ClipByChildren},
        {"includeinferiors",NULLQUARK, IncludeInferiors},
};

/* ArcMode */
static void CvtStringToArcMode();
static XrmQuark XrmQExtdefaultarcmode;
static XpwAssocTable *arcModeQuarkTable = NULL;
static QuarkList arcModeQuarks [] = {
        {XtDefaultArcMode, NULLQUARK, ArcChord},
        {"chord",NULLQUARK,ArcChord},
        {"pieslice",NULLQUARK,ArcPieSlice},
};

/* pixmap Status */

static void CvtStringToPixmapStatus();
static XrmQuark XrmQExtdefaultpixmapstatus;
static XpwAssocTable *pixmapStatusQuarkTable = NULL;
static QuarkList pixmapStatusQuarks [] = {
        {XtDefaultPixmapStatus, NULLQUARK, (Cardinal)PixmapOn},
        {"on", NULLQUARK, (Cardinal)PixmapOn},
        {"off",NULLQUARK, (Cardinal)PixmapOff},
        {"hasnone",NULLQUARK, (Cardinal)PixmapHasNone},
        {"only",NULLQUARK, (Cardinal)PixmapOnly},
};


/****************************************************************
 *
 * Class structure
 *
 ****************************************************************/

/* Define the top structure */

externaldef(xpwpixmapclassrec)
    XpwPixmapClassRec xpwPixmapClassRec = {
    { /* core fields */
    /* superclass       */      (WidgetClass) &xpwCoreClassRec,
    /* class_name       */      "XpwPixmap",
    /* widget_size      */      sizeof(XpwPixmapRec),
    /* class_initialize */      ClassInit,
    /* class_part_initialize*/  NULL,
    /* class_inited     */      FALSE,
    /* initialize       */      Initialize,
    /* initialize_hook  */      InitSubpartValues,
    /* realize          */      Realize,
    /* actions          */      NULL,
    /* num_actions      */      0,
    /* resources        */      resources,
    /* resource_count   */      XtNumber(resources),
    /* xrm_class        */      NULLQUARK,
    /* compress_motion  */      TRUE,
    /* compress_exposure*/      TRUE,
    /* compress_enterleave*/    TRUE,
    /* visible_interest */      FALSE,
    /* destroy          */      Destroy,
    /* resize           */      Resize,
    /* expose           */      NULL,
    /* set_values       */      SetValues,
    /* set_values_hook  */      SetSubpartValues,
    /* set_values_almost*/      XtInheritSetValuesAlmost,
    /* get_values_hook  */      GetSubpartValues,
    /* accept_focus     */      NULL,
    /* version          */      XtVersion,
    /* callback_private */      NULL,
    /* tm_table         */      NULL,
    /* query_geometry   */      NULL,
#if XtVersion > 11002
    /* display accel    */      XtInheritDisplayAccelerator,
    /* extension        */      NULL,
#endif
    },
    { /*xpwcore fields */
    /*methods           */      _xpwPixmapMethods,
    /*num_methods       */      0, /* see class_init */
    /*apply             */      ApplyMethod,
    /*methods_table     */      NULL,
    }
};

externaldef(xpwpixmapwidgetclass)
    WidgetClass xpwPixmapWidgetClass = (WidgetClass) &xpwPixmapClassRec;


/****************************************************************
 *
 * private procedures
 *
 ****************************************************************/
#ifdef DEBUG
static void debug_msg(m)
char *m;
{
    printf("xpwpixmap: %s\n",m);
}
#else
#define debug_msg(a)
#endif

/* ClassInit */
/* At class initialize, all of the Quark Lists are converted into
   Assoc Tables. This way, given any quark, it is easy to look up that
   quark, and the data associated with the quark. This is only useful because
   there are so many values for each of the graphics context resources.
   NB. The results of conversions are also cached, so each field will only
   be lookup'd once - perhaps the gains aren't so obvious.
*/



/* take a QuarkList, and convert it into a hashed Assoc table */
static XpwAssocTable *MakeQuarkTable(list,num_quarks,default_quark)
QuarkList list[];
XrmQuark *default_quark;
int num_quarks;
{
    int i;
    int table_size;
    XpwAssocTable *t;

    if (num_quarks < 4) table_size = 4;
    else if (num_quarks < 8) table_size = 8;
    else if (num_quarks < 16) table_size = 16;
    else table_size = 32;

    t = XpwCreateAssocTable(table_size);

    for (i=0; i < num_quarks; i++) {
        list[i].quark = XrmStringToQuark(list[i].name);
        XpwMakeAssoc(t,(XpwAssocID)list[i].quark,
                    (caddr_t)(long) (list[i].value + VAL_OFFSET));
    }
    *default_quark = list[0].quark;
    return(t);
}


/* CLASS INITIALIZE - Add new resource converters */
static void ClassInit ()
{
    static XtConvertArgRec screenConvertArg[] = {
        {XtBaseOffset, (caddr_t) XtOffset(Widget, core.screen),
            sizeof(Screen *)}
    };

    /* make lookup tables for GC resource converters */
    functionQuarkTable = MakeQuarkTable(functionQuarks,
        XtNumber(functionQuarks),&XrmQExtdefaultfunction);
    capStyleQuarkTable = MakeQuarkTable(capStyleQuarks,
        XtNumber(capStyleQuarks),&XrmQExtdefaultcapstyle);
    joinStyleQuarkTable = MakeQuarkTable(joinStyleQuarks,
        XtNumber(joinStyleQuarks),&XrmQExtdefaultjoinstyle);
    lineStyleQuarkTable = MakeQuarkTable(lineStyleQuarks,
        XtNumber(lineStyleQuarks),&XrmQExtdefaultlinestyle);
    subwindowQuarkTable = MakeQuarkTable(subwindowQuarks,
        XtNumber(subwindowQuarks),&XrmQExtdefaultsubwindow);
    pixmapStatusQuarkTable = MakeQuarkTable(pixmapStatusQuarks,
        XtNumber(pixmapStatusQuarks), &XrmQExtdefaultpixmapstatus);
    fillStyleQuarkTable = MakeQuarkTable(fillStyleQuarks,
        XtNumber(fillStyleQuarks),&XrmQExtdefaultfillstyle);
    arcModeQuarkTable = MakeQuarkTable(arcModeQuarks,
        XtNumber(arcModeQuarks), &XrmQExtdefaultarcmode);

    XtAddConverter( XtRString, XtRFunction, CvtStringToFunction, NULL, 0 );
    XtAddConverter( XtRString, XtRCapStyle, CvtStringToCapStyle, NULL, 0 );
    XtAddConverter( XtRString, XtRJoinStyle, CvtStringToJoinStyle, NULL,0 );
    XtAddConverter( XtRString, XtRLineStyle, CvtStringToLineStyle, NULL,0 );
    XtAddConverter( XtRString, XtRSubwindowMode, CvtStringToSubwindow, NULL,0 );
    XtAddConverter( XtRString, XtRPixmapStatus, CvtStringToPixmapStatus, NULL,0 );
    XtAddConverter( XtRString, XtRFillStyle, CvtStringToFillStyle, NULL,0 );
    XtAddConverter( XtRString, XtRFillRule, CvtStringToFillRule, NULL,0 );
    XtAddConverter( XtRString, XtRArcMode, CvtStringToArcMode, NULL,0 );

    xpwPixmapClassRec.xpwcore_class.num_methods = _num_xpwPixmapMethods;
}


/* INITIALIZE - create pixmap and private_gc, check for background_pixmap */
static void Initialize (request, new)
    Widget request, new;
{
    XpwPixmapWidget w = (XpwPixmapWidget)new;
    Display *dpy = XtDisplay(new);
    Pixmap pixmap;
    GC gc;
    XGCValues values;
    XtGCMask valuemask = GCForeground | GCBackground;

    debug_msg("Initialize start");

    pixmap = w->core.background_pixmap;
    if (pixmap != XtUnspecifiedPixmap) {
        debug_msg("bg");
        w->core.background_pixmap = XtUnspecifiedPixmap;
        values.fill_style = FillTiled;
        values.tile = pixmap;
        valuemask |= GCTile | GCFillStyle;
        values.foreground = w->xpwcore.foreground_pixel;
        values.background = w->core.background_pixel;
    } else {
        values.foreground  = w->core.background_pixel;
        values.background = w->xpwcore.foreground_pixel;
    }

    gc = w->xpwpixmap.private_gc = XCreateGC(dpy, w->core.screen->root,
            valuemask, &values);

    w->core.mapped_when_managed = FALSE;
    if (w->xpwpixmap.pixmap_status == PixmapHasNone) return;

    debug_msg("create pixmap");
    pixmap = XCreatePixmap(dpy, w->core.screen->root,
                w->core.width,w->core.height, w->core.depth);

    debug_msg("fill pixmap");
    XFillRectangle(dpy, pixmap, gc, 0,0, w->core.width, w->core.height);

    if (w->xpwpixmap.pixmap && w->xpwpixmap.pixmap != XtUnspecifiedPixmap) {
        debug_msg("set bg");
        XCopyArea(dpy, w->xpwpixmap.pixmap, pixmap, gc,
            0,0, w->core.width, w->core.height, 0,0);
        XFreePixmap(dpy, w->xpwpixmap.pixmap);
    }
    w->xpwpixmap.pixmap = pixmap;

    debug_msg("Initialize end");
}


/* SUBPART INITIALIZE - setup GC resources */
/* The subparts of the widget - ie. the graphics context resources - must
   be initialized and set using the initialize_hook and setvalues_hook. */

static void InitSubpartValues(gw, args, num_args)
    Widget gw;
    ArgList args;
    Cardinal *num_args;
{
    XpwCoreWidget w=(XpwCoreWidget)gw;

    /* get resources for the graphics context values subpart */

    debug_msg("InitSubpart Start");

    /* fill in subpart resource slots */

    GCsubpartSet(default_gc_values);

    XtGetSubresources((Widget)w, &_xpwGCvalues, XtNusersGCValues,
                XtCUsersGCValues, _xpwPixmapGCResources,
                XtNumber(_xpwPixmapGCResources), args, *num_args);

    /* find out what resources are not the same as the default vals -
       these must be changed, so set valuemask correctly.
    */
    _xpwGCvaluemask |= GCsubpartMask(default_gc_values);

    /* create/update the GC of the widget */

    _XpwCondUpdateUsersGC(xpwPixmapWidgetClass,w);

    debug_msg("InitSubpart End");
}

static Boolean SetSubpartValues(gw, args, num_args)
    Widget gw;
    ArgList args;
    Cardinal *num_args;
{
    XpwCoreWidget w=(XpwCoreWidget)gw;
    Display *dpy = XtDisplay(gw);
    XGCValues gcv;
    XtGCMask valuemask =
        GCFunction | GCPlaneMask |
        GCLineWidth | GCLineStyle | GCCapStyle | GCJoinStyle |
        GCFillStyle | GCFillRule | GCTile | GCStipple |
        GCTileStipXOrigin | GCTileStipYOrigin |
        GCSubwindowMode | GCClipXOrigin |
        GCClipYOrigin | /* GCClipMask |*/
        GCDashOffset | /* GCDashList | */
        GCArcMode;
    Boolean redisplay = FALSE;

    /* first blank out the slots in _xpwGCvalues that are resources */
    /* note, this does not affect resources that are set from SetValues */

    debug_msg("SetSubpartValues Start");

    XGetGCValues(dpy, w->xpwcore.users_gc, valuemask, &gcv);
    GCsubpartSet(gcv);

    debug_msg("GC init done");
    /*then fill in the slots in myXGCV with the resources being specified */

    XtSetSubvalues(&_xpwGCvalues, _xpwPixmapGCResources, XtNumber(_xpwPixmapGCResources),
                        args, *num_args);

    debug_msg("XtSet  done");
    /* now look for the resources that have changed */

    _xpwGCvaluemask |= GCsubpartMask(gcv);

    _XpwCondUpdateUsersGC(xpwPixmapWidgetClass, w);
    debug_msg("SetSubpartValues End");
    return(redisplay);
}

static void GetSubpartValues(gw, args, num_args)
    Widget gw;
    ArgList args;
    Cardinal *num_args;
{
    XpwCoreWidget w=(XpwCoreWidget)gw;
    Display *dpy = XtDisplay(gw);
    XGCValues gcv;
    XtGCMask valuemask =
        GCFunction | GCPlaneMask |
        GCLineWidth | GCLineStyle | GCCapStyle | GCJoinStyle |
        GCFillStyle | GCFillRule | GCTile | GCStipple |
        GCTileStipXOrigin | GCTileStipYOrigin |
        GCSubwindowMode | GCClipXOrigin |
        GCClipYOrigin | /* GCClipMask |*/
        GCDashOffset | /* GCDashList | */
        GCArcMode;
    XGetGCValues(dpy, w->xpwcore.users_gc, valuemask, &gcv);
    XtGetSubvalues(&gcv, _xpwPixmapGCResources, XtNumber(_xpwPixmapGCResources),
            args, *num_args);
}

static Boolean SetValues (gcurrent, grequest, gnew)
    Widget gcurrent, grequest, gnew;
{
    XpwPixmapWidget current = (XpwPixmapWidget) gcurrent;
    XpwPixmapWidget new = (XpwPixmapWidget) gnew;
    Display *dpy = XtDisplay(gcurrent);
    Boolean redisplay = FALSE;

    /* check for requests that affect our pixmap :*/

    if (new->core.background_pixmap != current->core.background_pixmap) {
        XGCValues values; XtGCMask valuemask = GCFillStyle;
        if (new->core.background_pixmap != XtUnspecifiedPixmap) {
            values.fill_style = FillTiled;
            values.tile = new->core.background_pixmap;
            valuemask |= GCTile;
            XChangeGC(dpy, new->xpwpixmap.private_gc,
                    valuemask, &values);
        } else {
            values.fill_style = FillSolid;
            XChangeGC(dpy, new->xpwpixmap.private_gc,
                    valuemask, &values);
        }
        redisplay = TRUE;
    }

    if (new->xpwpixmap.pixmap != current->xpwpixmap.pixmap) {
        if (new->xpwpixmap.pixmap == XtUnspecifiedPixmap)
            new->xpwpixmap.pixmap_status = PixmapHasNone;
        else
            redisplay = TRUE;
        if (current->xpwpixmap.pixmap)
            XFreePixmap(dpy,current->xpwpixmap.pixmap);
    }

    if (redisplay) Resize(new);

    if (new->xpwpixmap.pixmap_status != current->xpwpixmap.pixmap_status) {
        PixmapStatus new_status = new->xpwpixmap.pixmap_status;
        Pixmap new_pixmap = new->xpwpixmap.pixmap;
        debug_msg("SetValues: status change");
        if (new_status == PixmapHasNone) {
            if (new_pixmap && new_pixmap != XtUnspecifiedPixmap)
                XFreePixmap(dpy, new_pixmap);
            new->xpwpixmap.pixmap = new_pixmap = XtUnspecifiedPixmap;
        }
        else {
            if (!new_pixmap || new_pixmap == XtUnspecifiedPixmap) {
                debug_msg("SetValues: creating new pixmap");
                new_pixmap = XCreatePixmap(XtDisplay(new),
                    new->core.screen->root, new->core.width, new->core.height,
                    new->core.depth);
                XFillRectangle(XtDisplay(new), new_pixmap,
                    new->xpwpixmap.private_gc, 0, 0, new->core.width,
                    new->core.height);
                new->xpwpixmap.pixmap = new_pixmap;
            }
            if (new_status == PixmapOn) redisplay = TRUE;
        }
    }

    if (redisplay) {
        /* this stops the widgets window from flickering - the toolkit
           uses XClearArea if redisplay is TRUE; this causes
           an unpleasant flicker */
        if (XtIsRealized((Widget)new) && (XtClass(new)->core_class.expose))
            /* just call the expose routine directly */
            XtClass(new)->core_class.expose((Widget)new, NULL, NULL);
        redisplay = FALSE;
    }

    return(redisplay);
}


static void Realize ()
{
    XtWarning("XpwPixmap: cannot realize XpwPixmap widget");
    /* do nothing */
}


static void Resize (gw)
    Widget gw;
{
    XpwPixmapWidget w = (XpwPixmapWidget) gw;
    Pixmap old, new;
    register Display *dpy = XtDisplay(gw);
    register GC gc = w->xpwpixmap.private_gc;
    /* need a check for the pixmap's size */
    if (w->xpwpixmap.pixmap_status == PixmapHasNone) return;
    old = w->xpwpixmap.pixmap;
    new = XCreatePixmap(dpy, old, gw->core.width,gw->core.height,
                        (unsigned) gw->core.depth );
    w->xpwpixmap.pixmap = new;

    XFillRectangle(dpy, new, gc, 0,0, w->core.width, w->core.height);
    XCopyArea(dpy, old, new, gc, 0,0, w->core.width, w->core.height, 0,0);
    XFreePixmap(dpy,old);
}


static void Destroy (gw)
     Widget gw;
{
    XpwPixmapWidget w = (XpwPixmapWidget) gw;
    register Display *dpy = XtDisplay(w);
    if (w->xpwpixmap.pixmap_status != PixmapHasNone)
        XFreePixmap(dpy,w->xpwpixmap.pixmap);
    XFreeGC(dpy, w->xpwpixmap.private_gc);
    return;
}



/* Resource Converters */

static char lowercase_string[1000];

static void
LowerCase(source, dest)
register char *source, *dest;
{
    register char ch;
    int i;

    for (i = 0; (ch = *source) != 0 && i < 999; source++, dest++, i++)
        *dest = tolower(ch);
    *dest = 0;
}

/* generic version for all GC resources */

static void CvtString(fromVal, toVal, name, table, def)
XrmValuePtr fromVal, toVal;
String name;
XpwAssocTable *table;
XrmQuark def;
{   static long ret_val;
    caddr_t lookup_result;
    XrmQuark q;
    LowerCase((char *) fromVal->addr, lowercase_string);
    q = XrmStringToQuark(lowercase_string);
    lookup_result = XpwLookupAssoc(table,(XpwAssocID)q);
    if (!((long)lookup_result & VAL_OFFSET)) {
        /* can't get requested setting. Give warning and use default*/
        XtStringConversionWarning(fromVal->addr, name);
        lookup_result = XpwLookupAssoc(table,(XpwAssocID)def);
    }
    ret_val = (long)lookup_result - VAL_OFFSET;
    toVal->addr = (caddr_t)&ret_val; toVal->size = sizeof(caddr_t);
}

static void CvtStringToFunction( args, num_args, fromVal, toVal)
XrmValuePtr *args;
Cardinal    *num_args;
XrmValuePtr fromVal, toVal;
{
    CvtString(fromVal, toVal, "Function",functionQuarkTable,
        XrmQExtdefaultfunction);
}

static void CvtStringToCapStyle( args, num_args, fromVal, toVal)
XrmValuePtr *args;
Cardinal    *num_args;
XrmValuePtr fromVal, toVal;
{
    CvtString(fromVal, toVal, "CapStyle",capStyleQuarkTable,
        XrmQExtdefaultcapstyle);
}

static void CvtStringToLineStyle( args, num_args, fromVal, toVal)
XrmValuePtr *args;
Cardinal    *num_args;
XrmValuePtr fromVal, toVal;
{
        CvtString(fromVal, toVal, "LineStyle",lineStyleQuarkTable,
                XrmQExtdefaultlinestyle);

}

static void CvtStringToJoinStyle( args, num_args, fromVal, toVal)
XrmValuePtr *args;
Cardinal    *num_args;
XrmValuePtr fromVal, toVal;
{
    CvtString(fromVal, toVal, "JoinStyle",joinStyleQuarkTable,
                XrmQExtdefaultjoinstyle);

}

static void CvtStringToSubwindow( args, num_args, fromVal, toVal)
XrmValuePtr *args;
Cardinal    *num_args;
XrmValuePtr fromVal, toVal;
{
    CvtString(fromVal, toVal, "Subwindow",subwindowQuarkTable,
                    XrmQExtdefaultsubwindow);
}

static void CvtStringToPixmapStatus( args, num_args, fromVal, toVal)
XrmValuePtr *args;
Cardinal    *num_args;
XrmValuePtr fromVal, toVal;
{
    CvtString(fromVal, toVal, "PixmapStatus", pixmapStatusQuarkTable,
                    XrmQExtdefaultpixmapstatus);
}

static void CvtStringToFillStyle( args, num_args, fromVal, toVal)
XrmValuePtr *args;
Cardinal    *num_args;
XrmValuePtr fromVal, toVal;
{
    CvtString(fromVal, toVal, "FillStyle",fillStyleQuarkTable,
                XrmQExtdefaultfillstyle);

}

static void CvtStringToFillRule( args, num_args, fromVal, toVal)
XrmValuePtr *args;
Cardinal    *num_args;
XrmValuePtr fromVal, toVal;
{
    CvtString(fromVal, toVal, "FillRule", fillRuleQuarkTable,
                XrmQExtdefaultfillrule);

}

static void CvtStringToArcMode( args, num_args, fromVal, toVal)
XrmValuePtr *args;
Cardinal    *num_args;
XrmValuePtr fromVal, toVal;
{
    CvtString(fromVal, toVal, "ArcMode",arcModeQuarkTable,
                XrmQExtdefaultarcmode);

}
/* Methods */

#define done(type, value) \
    {                           \
        if (toVal->addr != NULL) {              \
        if (toVal->size < sizeof(type)) {       \
            toVal->size = sizeof(type);         \
            return False;               \
        }                       \
        *(type*)(toVal->addr) = (value);        \
        }                           \
        else {                      \
        static type static_val;             \
        static_val = (value);               \
        toVal->addr = (Opaque)&static_val;      \
        }                           \
        toVal->size = sizeof(type);             \
        return True;                    \
    }

static Boolean CvtFromInt(dpy, args, num_args, fromVal, toVal, closure_ret)
    Display*    dpy;
    XrmValuePtr args;
    Cardinal    *num_args;
    XrmValuePtr fromVal;
    XrmValuePtr toVal;
    Opaque  *closure_ret;
{
    if (*num_args != 0)
    XtAppWarningMsg(XtDisplayToApplicationContext(dpy),
          "wrongParameters","cvtFromInt","XpwError",
          "Integer conversion needs no extra arguments",
           (String *) NULL, (Cardinal *)NULL);
    done(int, *(int*)fromVal->addr);
}


static XpwMethodRet ApplyMethod(w, method, args)
XpwPixmapWidget w;
XpwMethod *method;
va_list args;
{   int i, num_args = method->num_args;
    XpwMethodProc proc=method->proc;
    XpwMethodArg arg_list[MAX_ARGS];
    XpwMethodRet ret_val = 0;
    Cardinal flags = method->flags;
    PixmapStatus status = w->xpwpixmap.pixmap_status;
    int count=0;
    if (status == PixmapOff || status == PixmapHasNone)
        return(ret_val);

    if (flags & RequiresWidget) arg_list[count++]=(XpwMethodArg)w;
    else arg_list[count++]=(XpwMethodArg) XtDisplay(w);

    if (flags & RequiresDrawable)
        arg_list[count++]=(XpwMethodArg)w->xpwpixmap.pixmap;

    if (flags & RequiresFontSet)
        arg_list[count++]=(XpwMethodArg)w->xpwcore.font_set;

    if (flags & RequiresGC)
        arg_list[count++]=(XpwMethodArg)w->xpwcore.users_gc;

    for (i=count; i<count+num_args; i++)
        arg_list[i] = (XpwMethodArg)va_arg(args, caddr_t);

    ret_val = (XpwMethodRet)_XpwMakeCall(proc, arg_list);
    return(ret_val);
}


/* MISC STUFF: */

/* ClearPixmap removed, JM, Aug 14 1990 */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  7 1997
        Added test for RequiresFontSet in ApplyMethod.
--- Robert Duncan, Oct 23 1996
        Fixed the way that SetValues behaves when the pixmapStatus changes:
        it creates a new pixmap when the status changes from PixmapHasNone
        and leaves well alone otherwise.
--- Robert John Duncan, Jun  3 1993
        Changed to use local version of LowerCase on all systems, since
        it's not guaranteed to be pre-defined on any
--- Adrian Howard, Jun  1 1993
        Installed fixes from Jon Meyer to SetValues and Realize
--- Julian Clinton, Apr  7 1993
        Included definition of LowerCase for __hpux
--- Julian Clinton, Mar 29 1993
        Modified SetSubpartValues and GetSubpartValues so they don't look
        into the internals of a GC.
        Changed GC_FIELD macro so that the holding structure name is a
        parameter rather than assumed to be "values". Modified GCsubpartMask
        accordingly.
--- John Gibson, Mar  9 1992
        VMS externalref mods
--- John Gibson, Jan 13 1992
        Corrected last change to _XpwSyntheticExposeEvent
--- Jonathan Meyer, Jan 12 1992
    Changed to use _XpwSyntheticExpose, which stops widget from
    flickering when the pixmapStatus is turned on/off.
--- John Gibson, Oct 31 1991
        VMS mods
--- Jonathan Meyer, Jul 29 1991
    Added XtNclipMask, XtNclip?Origin, XtNfillRule.
    Fixed handling of extra fields in users_gc
--- Jonathan Meyer, Mar 13 1991
    Added XtRInt to XtRGC* resource converters
--- Jonathan Meyer, Mar 13 1991
    Changed representation classes of subpart resources
--- Jonathan Meyer, Jan 18 1991
    Changed so it checks pixmap_status before resizing and
    destroying pixmap
--- Jonathan Meyer, Sep 24 1990
    Fixed bug in resize code.
--- Jonathan Meyer, Aug 24 1990
    Modified to use UsersGC.c stuff: renamed occurances of
        xpwCoreGCValueMask to _xpwGCvaluemask,
        xpwCoreGCValues to _xpwGCvalues,
        UpdateUsersGC to _XpwCondUpdateUsersGC,
    Made GCresources _xpwPixmapGCResources - global.
--- Jonathan Meyer, Aug 21 1990
    Added X11 prefix again. Defined XpwNotInstalled. Removed some
    redundant #include's.
--- Jonathan Meyer, Aug 20 1990
    Added arcMode, dashes, dashOffset, tile, stipple, fillStyle,
    tsXorigin, tsYorigin and planeMask resources.
--- Jonathan Meyer, Aug 14 1990
    Made widget work with new String to Pixmap convertor, and also
    made it sensitive to backgroundPixmap tiling, by modifying
    Initialize and SetValues. Improved Resize.
--- Jonathan Meyer, Jul 24 1990
    Added PixmapOnly to XtNpixmapStatus
--- Andreas Schoter, Jul 23 1990 Changed all pop* identifiers to xpw*
--- Andreas Schoter 16 July 1990
    Replaced all occurances of Pop* variable names with Xpw*
--- Simon Nichols, Jun 19 1990
        Removed "X11/" prefix from #include file names.
 */
