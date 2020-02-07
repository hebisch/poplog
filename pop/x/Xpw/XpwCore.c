/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 * File:            C.x/x/Xpw/XpwCore.c
 * Version:         Revision 5.0
 * Purpose:         Poplog Widget Set, XpwCore widget class.
 * Author:          Jonathan Meyer, 11 Feb 1989 (see revisions)
 * Documentation:   HELP *Xpw, REF *XpwCore
 * Related Files:   XpwCoreP.h XpwCore.h LIB *Xpw/XpwCore.p
 */

/* using Xpw header files locally, not in <X11/Xpw/...> */
#define XpwNotInstalled

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xatom.h>
#include "XpwCoreP.h"

#include <ctype.h>

/****************************************************************
 *
 * Declarations
 *
 ****************************************************************/

/* The following procs are part of the class structure */

static void Initialize(), Realize(), Resize(), Redisplay(), Destroy();
static void ClassInit(), ClassPartInit();
static Boolean SetValues();
static XpwMethodRet ApplyMethod();

/* Action procedures - only one for the Core Widget: */

static void NotifyEvent();


/* procs for handling UsersGC - see UsersGC.c */
externalref XGCValues _xpwGCvalues;
externalref XtGCMask  _xpwGCvaluemask;

extern void _XpwCondUpdateUsersGC();
extern void _XpwUpdateUsersGC();
extern void XpwCopyAssoc();
extern Pixmap XpwLocateBitmapFile();

/* converting bitmap files to pixmaps - see CvtStrToPmap.c and RecolorPmap.c*/
extern Pixmap XpwRecolorPixmap();
/*
extern XpwCvtStringToPixmap();
*/

/****************************************************************
 *
 * Resources
 *
 ****************************************************************/

/* Resource declaration and Initialization of defaults */

#define offset(field) XtOffset(XpwCoreWidget,xpwcore.field)
#define goffset(field) XtOffset(Widget,core.field)

static int setvalues_done;

static XtResource resources[] = {
#if XtVersion > 11002
    {XtNwidth, XtCWidth, XtRDimension, sizeof(Dimension),
        goffset(width), XtRString, "200"},
    {XtNheight, XtCHeight, XtRDimension, sizeof(Dimension),
        goffset(height), XtRString, "100"},
#else
    {XtNwidth, XtCWidth, XtRInt, sizeof(int),
        goffset(width), XtRString, "200"},
    {XtNheight, XtCHeight, XtRInt, sizeof(int),
        goffset(height), XtRString, "100"},
#endif
    {XtNpointerShape, XtCCursor, XtRCursor, sizeof(Cursor),
        offset(pointer_shape), XtRImmediate, NULL},
    {XtNpointerForeground, XtCForeground, XtRPixel, sizeof(Pixel),
        offset(pointer_foreground), XtRString, XtDefaultForeground},
    {XtNpointerBackground, XtCBackground, XtRPixel, sizeof(Pixel),
        offset(pointer_background), XtRString, XtDefaultBackground},
    {XtNfont, XtCFont, XtRFontStruct, sizeof(XFontStruct *),
        offset(font), XtRString, XtDefaultFont},
    {XtNfontSet, XtCFontSet, XtRFontSet, sizeof(XFontSet),
        offset(font_set), XtRImmediate, (caddr_t) NULL},
    {XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
        offset(foreground_pixel), XtRString, XtDefaultForeground},
    {XtNxpwCallback, XtCCallback, XtRCallback, sizeof(caddr_t),
        offset(xpw_callback), XtRCallback, NULL},
    {XtNusersGC, XtCParameter, XtRPointer, sizeof(GC),
        offset(users_gc), XtRPointer, NULL},
    {XtNautoFlush, XtCAutoFlush, XtRBoolean, sizeof(Boolean),
        offset(auto_flush), XtRImmediate, (caddr_t)True},
    {XtNmodifiers, XtCParameter, XtRInt, sizeof(int),
        offset(modifiers), XtRImmediate, 0},
};


#undef offset
#undef goffset

/****************************************************************
 *
 * Methods
 *
 ****************************************************************/

/* The XpwCore widget has the following methods:
    SetFont    FreeFont
    SetCursor  FreeCursor
    SetColour  FreeColour
    SetBackgroundTile
    ChangeUsersGC

   SetFont takes a string. SetCursor takes a cursor number. They
   both keep caches of fetched information. FreeFont and FreeCursor remove
   entries from cache, and free associated memory.
   JM - added SetColor and FreeColor
*/

static XpwMethodRet LoadFont(), LoadColor(), SetCursor(), LoadPixmap();
static void FreeFont(), FreeColor(), FreeCursor();
static XpwMethod methods[] = {
/*  {id,                    proc,       num_args,       flags     }*/
#define M METHOD_STRUCT
M   (XpwMSetFont,       LoadFont,   1,      RequiresWidget),
M   (XpwMSetCursor,     SetCursor,  1,      RequiresWidget),
M   (XpwMFreeFont,      FreeFont,   1,  RequiresWidget),
M   (XpwMFreeCursor,    FreeCursor, 1,  RequiresWidget),
M   (XpwMSetColor,      LoadColor,  1,  RequiresWidget),
M   (XpwMFreeColor,     FreeColor,  1,  RequiresWidget),
M   (XpwMLoadPixmap,    LoadPixmap, 4,  RequiresWidget),
M   (XpwMChangeUsersGC, _XpwUpdateUsersGC, 2, RequiresWidget),
#undef M
};


/****************************************************************
 *
 * Actions
 *
 ****************************************************************/

/* Action procedures Record for the Core widget */
static XtActionsRec actions[] =
{ {"notify-event", NotifyEvent},
{NULL,NULL},
};

/****************************************************************
 *
 * Translations
 *
 ****************************************************************/

/* Translations for the Core widget -
    These were removed because they interfered with subclasses
static char defaultTranslations [] = NULL;
*/


/****************************************************************
 *
 * Class structure
 *
 ****************************************************************/


externaldef(xpwCoreClassRec)
    XpwCoreClassRec xpwCoreClassRec = {
    { /* core fields */
    /* superclass       */      &widgetClassRec,
    /* class_name       */      "XpwCore",
    /* widget_size      */      sizeof(XpwCoreRec),
    /* class_initialize */      ClassInit,
    /* class_part_initialize*/  ClassPartInit,
    /* class_inited     */      FALSE,
    /* initialize       */      Initialize,
    /* initialize_hook  */      NULL,
    /* realize          */      Realize,
    /* actions          */      actions,
    /* num_actions      */      XtNumber(actions),
    /* resources        */      resources,
    /* resource_count   */      XtNumber(resources),
    /* xrm_class        */      NULLQUARK,
    /* compress_motion  */      TRUE,
    /* compress_exposure*/      TRUE,
    /* compress_enterleave*/    TRUE,
    /* visible_interest */      FALSE,
    /* destroy          */      Destroy,
    /* resize           */      Resize,
    /* expose           */      Redisplay,
    /* set_values       */      SetValues,
    /* set_values_hook  */      NULL,
    /* set_values_almost*/      XtInheritSetValuesAlmost,
    /* get_values_hook  */      NULL,
    /* accept_focus     */      NULL,
    /* version          */      XtVersion,
    /* callback_private */      NULL,
    /* tm_table         */      NULL, /* defaultTranslations */
    /* query_geometry*/         NULL,
#if XtVersion > 11002
    /* display accel    */      XtInheritDisplayAccelerator,
    /* extension        */      NULL
#endif
    },
    { /*xpwcore fields */
    /* methods      */ methods,
    /* num_methods      */ XtNumber(methods),
    /* apply_proc       */ ApplyMethod,
    /* methods_table    */ NULL,
    }
};

externaldef(xpwcorewidgetclass)
    WidgetClass xpwCoreWidgetClass = (WidgetClass) &xpwCoreClassRec;

/****************************************************************
 *
 * private Class procedures
 *
 ****************************************************************/

#ifdef DEBUG
static void debug_msg(m)
char *m;
{
    printf("xpwCore: %s\n",m);
}
#else
#define debug_msg(a)
#endif

static XtConvertArgRec strToPixmapArgs[] = {
    {XtBaseOffset, (caddr_t) XtOffset(Widget, core.screen), sizeof(Screen *)},
    {XtBaseOffset, (caddr_t) XtOffset(Widget, core.depth), sizeof(Cardinal)},
    {XtBaseOffset, (caddr_t) XtOffset(Widget, core.background_pixel),
                    sizeof(Pixel)}
};

/* Class Initialization */
static void ClassInit()
{
/*  XtAddConverter(XtRString, XtRPixmap, XpwCvtStringToPixmap,
        strToPixmapArgs, XtNumber(strToPixmapArgs));
*/
}


/* Class part initialization - does two things: allows for method
    or apply_proc inheritance, and initializes the methods_table
*/

static void ClassPartInit(gwc)
WidgetClass gwc;
{
    register XpwCoreWidgetClass wc = (XpwCoreWidgetClass)gwc,
                super = (XpwCoreWidgetClass)gwc->core_class.superclass;
    int num_methods, table_size=0;
    Boolean add_supers_methods = FALSE;
    XpwMethod *method;
    XpwMethodList methods = wc->xpwcore_class.methods;
    XpwAssocTable *t, *supers_t = NULL;

    debug_msg("ClassPart Initialize Start");

    if (wc->xpwcore_class.apply_proc == XtInheritApplyProc)
        wc->xpwcore_class.apply_proc = super->xpwcore_class.apply_proc;

    /*  if the user specifies XtInheritMethods in the methods_table slot,
        we must add all of the methods from the superclass's methods_table.
        Then we must add all of the methods from this classes methods slot.
    */

    if (wc->xpwcore_class.methods_table == XtInheritMethods) {
        add_supers_methods = TRUE;
        supers_t = super->xpwcore_class.methods_table;
    }

    /* make and initialize the hash table for the methods */

    if (methods || add_supers_methods) {
        num_methods = wc->xpwcore_class.num_methods;
        /*  For fastest access, there is more than one bucket per method.
            Since the number of methods is unlikely to be greater than 30
            for any specific widget class, this is not a problem.
        */

        /* best size is power of 2 */
        if (num_methods < 8) table_size = 8;
        else if (num_methods < 16) table_size = 16;
        else if (num_methods < 32) table_size = 32;
        else if (num_methods < 64) table_size = 64;
        else table_size = 128;

        if (add_supers_methods && supers_t && supers_t != XtInheritMethods)
            table_size += supers_t->size;
            /* superclass methods_table has been constructed - so add its
               size to the size of this classes table */

        /* create the Assoc Table */

        t = (XpwAssocTable *)XpwCreateAssocTable(table_size);

        /* add entries to the table */

        /* if we are inheriting supers methods, add them to the table */
        if (add_supers_methods && supers_t && supers_t != XtInheritMethods)
            XpwCopyAssoc(supers_t, t);

        /* if we have additional methods for this class, add them as well */
        if (methods)
            for (method=methods; method < methods+num_methods; method++ )
                XpwMakeAssoc(t, (XpwAssocID)method->id,(caddr_t)method);
    wc->xpwcore_class.methods_table = t;
    }
    debug_msg("ClassPart Initialize End");
}

void _XpwRecolorPointer(w)
XpwCoreWidget w;
 {
    Display *dpy = XtDisplay(w);
    XColor colordefs[2];        /* 0 is foreground, 1 is background */

    colordefs[0].pixel = w->xpwcore.pointer_foreground;
    colordefs[1].pixel = w->xpwcore.pointer_background;
    XQueryColors(dpy, DefaultColormapOfScreen(XtScreen(w)), colordefs, 2);
    XRecolorCursor(dpy, w->xpwcore.pointer_shape, colordefs, colordefs+1);
 }

    /*  This tedium is necessary because not all systems offer
        strcasecmp() as standard
    */
static int is_iso_latin_1_font(font_name)
char* font_name;
{
    char *p1, *p2;
    int n = strlen(font_name) - 9;
    if (n < 0) return 0;
    for (p1 = "ISO8859-1", p2 = font_name + n; *p1; ++p1, ++p2) {
        char c = *p2;
        if (islower(c)) c = toupper(c);
        if (c != *p1) return 0;
    }
    return 1;
}

XFontSet _XpwFontSetFromFont(w, font)
  XpwCoreWidget w;
  XFontStruct *font;
  { Atom a;
    XrmValue from_val, to_val;
    XFontSet font_set = (XFontSet)NULL;
    char *name, *fname = (char *)NULL;

    if (XGetFontProperty(font, XA_FONT, &a))
        name = fname = XGetAtomName(XtDisplay(w), a);
    else
      { XtAppWarningMsg(XtWidgetToApplicationContext((Widget)w),
            "noFontName", "fontSetFromFont", "XpwError",
            "Cannot get font name to make font set - using fixed instead",
            (String *) NULL, (Cardinal *)NULL);
        name = "fixed";
      }

    from_val.size = strlen(name);
    from_val.addr = (XPointer)name;
    to_val.size = sizeof(XFontSet);
    to_val.addr = (XPointer)&font_set;
    XtConvertAndStore((Widget)w, XtRString, &from_val, XtRFontSet, &to_val);
    if (fname) XFree(fname);
    return(font_set);
  }

XFontStruct * _XpwFont8OfFontSet(w, font_set, iso_latin_1)
  XpwCoreWidget w;
  XFontSet font_set;
  Boolean iso_latin_1;
  { XFontStruct **font_struct_list, **lim, *font;
    char **font_name_list, *name;
    int nfonts = XFontsOfFontSet(font_set, &font_struct_list, &font_name_list);
    Display *dpy;
    XrmValue from_val, to_val;

    name = *font_name_list;
    lim = &font_struct_list[nfonts];
    while (font_struct_list < lim)
      { char *nm = *font_name_list++;
        XFontStruct *f = *font_struct_list++;
        int n;
        if (f->min_byte1 != 0 || f->max_byte1 != 0) continue;
        name = nm;
        if (!iso_latin_1 || is_iso_latin_1_font(nm))
            break;
      }

    from_val.size = strlen(name);
    from_val.addr = (XPointer)name;
    to_val.size = sizeof(XFontStruct*);
    to_val.addr = (XPointer)&font;
    XtConvertAndStore((Widget)w, XtRString, &from_val, XtRFontStruct, &to_val);
    return(font);
  }

static void Initialize (request, new)
Widget request, new;
{
    XpwCoreWidget w = (XpwCoreWidget)new;
    XtGCMask        valuemask;
    XGCValues       myXGCV;
    Pixmap pixmap;

    if (w->xpwcore.font_set)
        w->xpwcore.font = _XpwFont8OfFontSet(w, w->xpwcore.font_set, False);
    else
        w->xpwcore.font_set = _XpwFontSetFromFont(w, w->xpwcore.font);

    debug_msg("Initialize Start");
    /* get a Graphics Context */
    _xpwGCvaluemask = GCForeground | GCBackground ;

    _xpwGCvalues.font = w->xpwcore.font->fid;    /* get resource font ID */
    _xpwGCvaluemask |= GCFont;

    _xpwGCvalues.foreground = w->xpwcore.foreground_pixel;
    _xpwGCvalues.background = w->core.background_pixel;

    /* invert the auto flush */
    _XpwCondUpdateUsersGC(xpwCoreWidgetClass, w);
    debug_msg("Initialize End");
}



static Boolean SetValues (gcurrent, grequest, gnew)
Widget gcurrent, grequest, gnew;
{
    XGCValues gcv;
    XpwCoreWidget current = (XpwCoreWidget) gcurrent;
    XpwCoreWidget new = (XpwCoreWidget) gnew;
    Boolean redisplay = FALSE;
    Display *dpy = XtDisplay(gcurrent);
    GC currentGC = current->xpwcore.users_gc, newGC = new->xpwcore.users_gc;
    debug_msg("SetValues Start");
    setvalues_done = TRUE;

    /* look for any proposed changes to the widget - and perform necessary
       actions to achieve changes. */

    /* changes to the Graphics Context are recorded in the _xpwGCvalues
       structure. These are then made by a call to _XpwUpdateUsersGC.
    */
    _xpwGCvaluemask = 0;

    if (new->core.background_pixel != current->core.background_pixel) {
        _xpwGCvaluemask |= GCBackground;
        _xpwGCvalues.background = new->core.background_pixel;
    }
    if (new->xpwcore.foreground_pixel != current->xpwcore.foreground_pixel) {
        _xpwGCvaluemask |= GCForeground;
        _xpwGCvalues.foreground = new->xpwcore.foreground_pixel;
    }

    if (new->xpwcore.font_set != current->xpwcore.font_set)
      { new->xpwcore.font = _XpwFont8OfFontSet(new, new->xpwcore.font_set, False);
        _xpwGCvaluemask |= GCFont;
        _xpwGCvalues.font = new->xpwcore.font->fid;
      }
    else if (new->xpwcore.font != current->xpwcore.font)
      { if (new->xpwcore.font==NULL)
            new->xpwcore.font=current->xpwcore.font;
        else
          { _xpwGCvaluemask |= GCFont;
            _xpwGCvalues.font = new->xpwcore.font->fid;
          }
        new->xpwcore.font_set = _XpwFontSetFromFont(new, new->xpwcore.font);
      }

    if (new->xpwcore.pointer_shape != current->xpwcore.pointer_shape)
      { if (!new->xpwcore.pointer_shape)
            XUndefineCursor(dpy, XtWindow(new));
        else
          { _XpwRecolorPointer(new);
            XDefineCursor(dpy, XtWindow(new), new->xpwcore.pointer_shape);
          }
      }
    else if (new->xpwcore.pointer_shape &&
       (new->xpwcore.pointer_foreground != current->xpwcore.pointer_foreground
     || new->xpwcore.pointer_background != current->xpwcore.pointer_background)
    )
        _XpwRecolorPointer(new);

    if (newGC != currentGC) {
        /*user has modified GC resource,so extract new values from GC */
        XGetGCValues(dpy, newGC, GCFont | GCForeground | GCBackground, &gcv);
        new->xpwcore.foreground_pixel=gcv.foreground;
        new->core.background_pixel=gcv.background;
        if (new->xpwcore.font->fid != gcv.font)
            new->xpwcore.font = XQueryFont(dpy, gcv.font);
    }


    _XpwCondUpdateUsersGC(xpwCoreWidgetClass, new);

    if (new->xpwcore.auto_flush != current->xpwcore.auto_flush) {
        if (new->xpwcore.auto_flush) XFlush(XtDisplay(new));
    }

    debug_msg("SetValues End");
    return (redisplay);
}

static void Realize (gw, valueMask, attrs)
    Widget gw;
    XtValueMask *valueMask;
    XSetWindowAttributes *attrs;
{       XpwCoreWidget w=(XpwCoreWidget)gw;
    if (attrs->cursor = w->xpwcore.pointer_shape)
      { *valueMask |= CWCursor; _XpwRecolorPointer(w); }

    XtCreateWindow( gw, InputOutput, (Visual *)CopyFromParent,
             *valueMask, attrs);

}


static void Resize (gw)
    Widget gw;
{
    /* don't do this computation if window hasn't been realized yet. */
    if (XtIsRealized(gw)) {
        XtCallCallbacks(gw,XtNxpwCallback, (caddr_t)ConfigureNotify);
    }
}


static void Redisplay (gw, event, region)
    Widget gw;
    XEvent *event;      /* unused */
    Region region;      /* unused */
{
    /* again we need a hook to Poplog */
    XtCallCallbacks(gw, XtNxpwCallback, (caddr_t)(event));
}


static void Destroy (gw)
     Widget gw;
{
    XpwCoreWidget w = (XpwCoreWidget) gw;
    register Display *dpy = XtDisplay(w);
    if (w->xpwcore.users_gc)
      { if (w->xpwcore.shared_gc) XtReleaseGC ((Widget)w, w->xpwcore.users_gc);
        else XFreeGC(dpy, w->xpwcore.users_gc);
      }

    return;
}

/****************************************************************
 *
 * Action procedures
 *
 ****************************************************************/

static void NotifyEvent (gw, event, params, num_params)
Widget gw;
XEvent *event;
String *params;
Cardinal *num_params;
{
    /* notify the clients of the event: */
    XtCallCallbacks(gw, XtNxpwCallback, (caddr_t)event);
}



/****************************************************************
 *
 * XpwCore Methods
 *
 ****************************************************************/

/* Call resource converter to load font */
static XpwMethodRet LoadFont (w, string)
XpwCoreWidget w;
char *string;
{
    if (string == NULL) string = XtDefaultFontSet;
    setvalues_done = FALSE;
    XtVaSetValues((Widget)w, XtVaTypedArg, XtNfontSet, XtRString,
            (XtArgVal)string, strlen(string)+1, NULL);
    if (setvalues_done) return((XpwMethodRet)w->xpwcore.font->fid);
    else return((XpwMethodRet)0);
}

static void FreeFont(w,string)
XpwCoreWidget w;
char *string;
{   /* do nothing now - leave free to the resource converters */
}

static XpwMethodRet LoadColor (w, string)
XpwCoreWidget w;
char *string;
{
    if (string == NULL) string = XtDefaultForeground;
    setvalues_done = FALSE;
    XtVaSetValues((Widget)w, XtVaTypedArg, XtNforeground, XtRString,
            (XtArgVal)string, strlen(string)+1, NULL);
    if (setvalues_done) return((XpwMethodRet)w->xpwcore.foreground_pixel);
    else return((XpwMethodRet)-1);
}

static void FreeColor(w,string)
XpwCoreWidget w;
char *string;
{
}

static XpwMethodRet SetCursor(w, shape)
XpwCoreWidget w;
unsigned int shape;
{
    register Display *dpy = XtDisplay(w);
    Cursor cur;
    setvalues_done = FALSE;
    cur = XCreateFontCursor(dpy, shape);
    if (cur) XtVaSetValues((Widget)w, XtNpointerShape, (XtArgVal)cur, NULL);
    if (setvalues_done) return((XpwMethodRet)w->xpwcore.pointer_shape);
    else return(0);
}

static void FreeCursor(w,shape)
XpwCoreWidget w;
unsigned int shape;
{
}

static XpwMethodRet LoadPixmap(w, string, fg, bg, depth)
XpwCoreWidget w;
char *string;
int fg, bg, depth;
{
    int width_ret, height_ret, num;
    Pixmap new;
    Screen *screen = XtScreen(w);
    if (!depth) depth = w->core.depth;
    if (bg == -1) bg = w->core.background_pixel;
    if (fg == -1) fg = w->xpwcore.foreground_pixel;

    if (new = XpwLocateBitmapFile(screen, string, NULL,0,NULL,NULL,NULL,NULL))
       {
        /* unless the bitmap has the correct depth, foreground and
           background, recolor it.
        */

        if (depth != 1 || fg != 1 || bg != 0)
            new = XpwRecolorPixmap(screen, new, 1, 0,
                fg, bg, depth, TRUE);
        return((XpwMethodRet)new);
       }
    return((XpwMethodRet)None);
}

static void FreePixmap(w, pixmap)
Widget w;
Pixmap pixmap;
{
    XFreePixmap(XtDisplay(w), pixmap);
}

static XpwMethodRet ApplyMethod(w, method, args)
XpwCoreWidget w;
XpwMethod *method;
va_list args;
{   int i, num_args = method->num_args;
    XpwMethodProc proc=method->proc;
    Cardinal flags = method->flags;
    XpwMethodArg  arg_list[MAX_ARGS];
    XpwMethodRet ret_val;
    int count=0;
    if (flags & RequiresWidget) arg_list[count++]=(XpwMethodArg)w;
    else arg_list[count++]=(XpwMethodArg) XtDisplay(w);
    if (flags & RequiresDrawable)
        arg_list[count++]=(XpwMethodArg)XtWindow(w);
    if (flags & RequiresFontSet)
        arg_list[count++]=(XpwMethodArg)w->xpwcore.font_set;
    if (flags & RequiresGC)
        arg_list[count++]=(XpwMethodArg)w->xpwcore.users_gc;

    for (i=count; i<count+num_args; i++)
        arg_list[i] = (XpwMethodArg)va_arg(args, caddr_t);
    ret_val = (XpwMethodRet)_XpwMakeCall(proc, arg_list);
    return(ret_val);
}


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Aug 29 1997
        Changed _XpwFont8OfFontSet not to use strcasecmp()
--- John Gibson, Apr 29 1997
        Made _XpwFont8OfFontSet re-open the font by name to ensure
        per_char metrics loaded.
--- John Gibson, Apr  8 1997
        Added XtNfontSet resource; if not set, uses name of XtNfont to
        make font set (similarily if XtNfont set in SetValues). If
        XtNfontSet is set, makes XtNfont be the first font in the set.
--- John Gibson, Dec 19 1993
        Fixed ANSI warnings
--- Julian Clinton, Mar 29 1993
        Modified SetValues so that it doesn't grope into the internals of
        a GC.
--- Adrian Howard, Sep 11 1992
        Installed changes from JonM to fix bug with XtNautoFlush
--- John Gibson, Mar  9 1992
        VMS externaldef mods
--- John Gibson, Dec 18 1991
        Changed -Destroy- to test whether users_gc not null before
        trying to free it
--- John Gibson, Oct 31 1991
        VMS mods
--- John Gibson, Oct 24 1991
        Replaced defaultTranslations with NULL in tm_table (VMS C compiler
        won't allow NULL initialisation for it).
--- Jonathan Meyer, Sep 25 1991
    Fixed LoadPixmap so that it also recolors 1 bit deep bitmaps when the
    foreground or background are not 1 and 0 respectively. (Copes with
    X servers that have black as 1 and white as 0, etc).
--- John Gibson, Aug  2 1991
    Added _XpwRecolorPointer and changed SetValues to use it
--- Jonathan Meyer, Jul 30 1991
    Changed to call to XpwLocateBitmapFile to search bitmapFilePath for
    bitmaps. Made LoadColor return -1 if the colour is not found
--- John Gibson, Jun  7 1991
    Added missing size argument to 2 places where XtVaTypedArg is used
--- Jonathan Meyer, Jun  4 1991
    Fixed depth bug in LoadPixmap
--- Jonathan Meyer, May 30 1991
    removed tile fg resource
--- Jonathan Meyer, May 30 1991
    Changed core methods to use resource converters
--- Jonathan Meyer, Mar 29 1991
    Changed XtDestroyGC to XtReleaseGC - a bug in XtDestroyGC stops
    it working with multiple displays.
--- Jonathan Meyer, Sep  9 1990
    Removed printgc to UsersGC.c
--- Jonathan Meyer, Aug 24 1990
    Made use of _xpwMethodNames to record name of all methods registered
    Moved code for handling UsersGC resource in UsersGC.c - and
    modified XpwCore to handle changes: renamed occurances of
        xpwCoreGCValueMask to _xpwGCvaluemask,
        xpwCoreGCValues to _xpwGCvalues,
        _XpwCondUpdateUsersGC to _XpwCondUpdateUsersGC,
        UpdateGC to _XpwUpdateUsersGC.
--- Jonathan Meyer, Aug 21 1990
    Added X11 prefix again. Defined XpwNotInstalled. Removed some
    redundant #include's.
--- Jonathan Meyer, Aug 14 1990
    Made use of the XpwCvtStringToPixmap resource converter. Wrote the
    LoadBackgroundPixmap method. Used XpwRecolorPixmap.
    Modified Initialize to check for background_pixmap's.
--- Andreas Schoter, Jul 23 1990 Changed all pop* identifiers to xpw*
--- Andreas Schoter 16 July 1990
    Replaced all occurances of Pop* variable names  with Xpw*
--- Simon Nichols, Jun 19 1990
        Removed "X11/" prefix from #include file names.
 */
