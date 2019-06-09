/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XProperties.p
 > Purpose:
 > Author:          Gareth Palmer,  4 July 1989 (see revisions)
 > Documentation:
 > Related Files:
 */


uses XConstants;
uses XExtent;


global constant macro (

    AnyPropertyType = 0, /* special Atom, passed to GetProperty */

    /* Property modes */

    PropModeReplace         = 0,
    PropModePrepend         = 1,
    PropModeAppend          = 2,

);


external declare XProperties in c;
    (external_import_procedure XptImportProcedure)

    Atom *XListProperties(display, w, num_prop)
    Display *display;
    Window w;
    int *num_prop;                  ;;; RETURN
    {}

    void XDeleteProperty(display, w, property)
    Display *display;
    Window w;
    Atom property;
    {}

    void XChangeProperty(display, w, property, type, format, mode, data, nelements)
    Display *display;
    Window w;
    Atom property, type;
    int format;
    int mode;
    unsigned char *data;
    int nelements;
    {}

    void XSetStandardProperties(display, w, window_name, icon_name, icon_pixmap,
                                  argv, argc, hints)
    Display *display;
    Window w;
    char *window_name;
    char *icon_name;
    Pixmap icon_pixmap;
    char **argv;
    int argc;
    XSizeHints *hints;
    {}

    void XRotateWindowProperties(display, w, properties, num_prop, npositions)
    Display *display;
    Window w;
    Atom properties[];
    int num_prop;
    int npositions;
    {}

    char *XGetAtomName(display, atom)
    Display *display;
    Atom atom;
    {}

    Bool XGetFontProperty(font_struct, atom, value)
    XFontStruct *font_struct;
    Atom atom;
    unsigned long *value;               ;;; RETURN
    {}

    int XGetWindowProperty(display, w, property, long_offset, long_length, delete,
                   req_type, actual_type, actual_format, nitems, bytes_after, prop)
    Display *display;
    Window w;
    Atom property;
    long long_offset, long_length;
    Bool delete;
    Atom req_type;
    Atom *actual_type;                  ;;; RETURN
    int *actual_format;                 ;;; RETURN
    unsigned long *nitems;              ;;; RETURN
    unsigned long *bytes_after;         ;;; RETURN
    unsigned char **prop;               ;;; RETURN
    {}

    Atom XInternAtom(display, property_name, only_if_exists)
    Display *display;
    char *property_name;
    Bool only_if_exists;
    {}

endexternal;


xlib_external_require XProperties;


global vars XProperties = true;

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Jan 25 1991 Changed to use xlib_external_require
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
 */
