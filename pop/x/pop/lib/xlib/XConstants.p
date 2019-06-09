/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XConstants.p
 > Purpose:
 > Author:          Ian Rogers, Feb 17 1989 (see revisions)
 > Documentation:
 > Related Files:   XConstants.ph
 */



section;

loadinclude XConstants.ph;

external declare xpop in c;
    (external_import_procedure XptImportProcedure)

;;; I can't seem to find out from anywhere what a _XrmHashBucketRec should really be
    typedef struct _XrmHashBucketRec {int dummy;} *XrmHashBucket;
    typedef XrmHashBucket *XrmHashTable;
    typedef XrmHashTable XrmSearchList[];
    typedef struct _XrmHashBucketRec *XrmDatabase;

    typedef unsigned long Mask;

    typedef unsigned long Atom;

    typedef unsigned long VisualID;

    typedef unsigned long Time;

    typedef unsigned char KeyCode;

    typedef unsigned int caddr_t;


    typedef unsigned long XID;

    typedef XID Window;
    typedef XID Drawable;
    typedef XID Font;
    typedef XID Pixmap;
    typedef XID Cursor;
    typedef XID Colormap;
    typedef XID GContext;
    typedef XID KeySym;


    /*
     * #define Bool int
     * #define Status int
     * #define True 1
     * #define False 0
     */
    typedef enum {False, True} Bool, Status;



    /* Data structure for X{Set,Get}ModifierMapping */

    typedef struct {
        int max_keypermod;  ;;; The server's max # of keys per modifier */
        KeyCode *modifiermap;   ;;; An 8 by max_keypermod array of modifiers */
    } XModifierKeymap;

    /*
     * This file contains structures used by the extension mechanism.
     */
    typedef struct {        ;;; public to extension, cannot be changed */
        int extension;      ;;; extension number */
        int major_opcode;   ;;; major op-code assigned by server */
        int first_event;    ;;; first event number for the extension */
        int first_error;    ;;; first error number for the extension */
    } XExtCodes;

    /*
     * This structure is private to the library.
     */
    typedef struct _XExten {    ;;; private to extension mechanism */
        struct _XExten *next;   ;;; next in list */
        XExtCodes codes;    ;;; public information, all extension told */
        int (*create_GC)(); ;;; routine to call when GC created */
        int (*copy_GC)();   ;;; routine to call when GC copied */
        int (*flush_GC)();  ;;; routine to call when GC flushed */
        int (*free_GC)();   ;;; routine to call when GC freed */
        int (*create_Font)();   ;;; routine to call when Font created */
        int (*free_Font)(); ;;; routine to call when Font freed */
        int (*close_display)(); ;;; routine to call when connection closed */
        int (*error)();     ;;; who to call when an error occurs */
        int (*error_string)();  ;;; routine to supply error string */
    } _XExtension;


    /*
     * Extensions need a way to hang private data on some structures.
     */
    typedef struct _XExtData {
        int number;     ;;; number returned by XRegisterExtension */
        struct _XExtData *next; ;;; next item on list of data for structure */
        int (*free_private)();  ;;; called to free private storage */
        char *private_data; ;;; data private to this extension. */
    } XExtData;

    /*
     * Format structure; describes ZFormat data the screen will understand.
     */
    typedef struct {
        XExtData *ext_data; ;;; hook for extension to hang data */
        int depth;      ;;; depth of this image format */
        int bits_per_pixel; ;;; bits/pixel at this depth */
        int scanline_pad;   ;;; scanline must padded to this multiple */
    } ScreenFormat;


    /*
     * Visual structure; contains information about colormapping possible.
     */
    typedef struct {
        XExtData *ext_data; ;;; hook for extension to hang data */
        VisualID visualid;  ;;; visual id of this visual */
        int class;      ;;; class of screen (monochrome, etc.) */
        unsigned long red_mask, green_mask, blue_mask;  ;;; mask values */
        int bits_per_rgb;   ;;; log base 2 of distinct color values */
        int map_entries;    ;;; color map entries */
    } Visual;


    /*
     * Depth structure; contains information for each possible depth.
     */
    typedef struct {
        int depth;      ;;; this depth (Z) of the depth */
        int nvisuals;       ;;; number of Visual types at this depth */
        Visual *visuals;    ;;; list of visuals possible at this depth */
    } Depth;


    /*
     * Data structure for setting graphics context.
     */
    typedef struct {
        int function;       ;;; logical operation
        unsigned long plane_mask; ;;; plane mask
        unsigned long foreground; ;;; foreground pixel
        unsigned long background; ;;; background pixel
        int line_width;     ;;; line width
        int line_style;     ;;; LineSolid, LineOnOffDash, LineDoubleDash
        int cap_style;      ;;; CapNotLast, CapButt, CapRound, CapProjecting
        int join_style;     ;;; JoinMiter, JoinRound, JoinBevel
        int fill_style;     ;;; FillSolid, FillTiled, FillStippled,
                            ;;;   FillOpaeueStippled
        int fill_rule;      ;;; EvenOddRule, WindingRule */
        int arc_mode;       ;;; ArcChord, ArcPieSlice */
        Pixmap tile;        ;;; tile pixmap for tiling operations */
        Pixmap stipple;     ;;; stipple 1 plane pixmap for stipping */
        int ts_x_origin;    ;;; offset for tile or stipple operations */
        int ts_y_origin;
            Font font;          ;;; default text font for text operations */
        int subwindow_mode;     ;;; ClipByChildren, IncludeInferiors */
        Bool graphics_exposures; ;;; boolean, should exposures be generated */
        int clip_x_origin;  ;;; origin for clipping */
        int clip_y_origin;
        Pixmap clip_mask;   ;;; bitmap clipping; other calls for rects */
        int dash_offset;    ;;; patterned/dashed line information */
        char dashes;
    } XGCValues;


    /*
     * Graphics context.  All Xlib routines deal in this rather than
     * in raw protocol GContext ID's.  This is so that the library can keep
     * a "shadow" set of values, and thus avoid passing values over the
     * wire which are not in fact changing.
     */

    typedef struct _XGC {
        XExtData *ext_data; ;;; hook for extension to hang data */
        GContext gid;   ;;; protocol ID for graphics context */
        Bool rects;     ;;; boolean: TRUE if clipmask is list of rectangles */
        Bool dashes;    ;;; boolean: TRUE if dash-list is really a list */
        unsigned long dirty; ;;; cache dirty bits */
        XGCValues values;   ;;; shadow structure of values */
    } *GC;


    /*
     * Information about the screen.
     */
    typedef struct {
        XExtData *ext_data; ;;; hook for extension to hang data */
        struct _XDisplay *display; ;;; back pointer to display structure */
        Window root;        ;;; Root window id. */
        int width, height;  ;;; width and height of screen */
        int mwidth, mheight;    ;;; width and height of in millimeters */
        int ndepths;        ;;; number of depths possible */
        Depth *depths;      ;;; list of allowable depths on the screen */
        int root_depth;     ;;; bits per pixel */
        Visual *root_visual;    ;;; root visual */
        GC default_gc;      ;;; GC for the root root visual */
        Colormap cmap;      ;;; default color map */
        unsigned long white_pixel;
        unsigned long black_pixel;  ;;; White and Black pixel values */
        int max_maps, min_maps; ;;; max and min color maps */
        int backing_store;  ;;; Never, WhenMapped, Always */
        Bool save_unders;
        long root_input_mask;   ;;; initial root input mask */
    } Screen;

    /*
     * Display datatype maintaining display specific data.
     */
    typedef struct _XDisplay {
        XExtData *ext_data; ;;; hook for extension to hang data */
        struct _XDisplay *next; ;;; next open Display on list */
        int fd;         ;;; Network socket. */
        int lock;       ;;; is someone in critical section? */
        int proto_major_version; ;;; maj. version of server's X protocol */
        int proto_minor_version; ;;; minor version of servers X protocol */
        char *vendor;       ;;; vendor of the server hardware */
            long resource_base; ;;; resource ID base */
        long resource_mask; ;;; resource ID mask bits */
        long resource_id;   ;;; allocator current ID */
        int resource_shift; ;;; allocator shift to correct bits */
        XID (*resource_alloc)(); ;;; allocator function */
        int byte_order;     ;;; screen byte order, LSBFirst, MSBFirst */
        int bitmap_unit;    ;;; padding and data requirements */
        int bitmap_pad;     ;;; padding requirements on bitmaps */
        int bitmap_bit_order;   ;;; LeastSignificant or MostSignificant */
        int nformats;       ;;; number of pixmap formats in list */
        ScreenFormat *pixmap_format;    ;;; pixmap format list */
        int vnumber;        ;;; Xlib's X protocol version number. */
        int release;        ;;; release of the server */
        struct _XSQEvent *head, *tail;  ;;; Input event queue. */
        int qlen;       ;;; Length of input event queue */
        unsigned long last_request_read; ;;; seq number of last event read */
        unsigned long request;  ;;; sequence number of last request. */
        char *last_req;     ;;; beginning of last request, or dummy */
        char *buffer;       ;;; Output buffer starting address. */
        char *bufptr;       ;;; Output buffer index pointer. */
        char *bufmax;       ;;; Output buffer maximum+1 address. */
        unsigned max_request_size; ;;; maximum number 32 bit words in request*/
        struct _XrmHashBucketRec *db;
        int (*synchandler)();   ;;; Synchronization handler */
        char *display_name; ;;; "host:display" string used on this connect*/
        int default_screen; ;;; default screen for operations */
        int nscreens;       ;;; number of screens on this server*/
        Screen *screens;    ;;; pointer to list of screens */
        int motion_buffer;  ;;; size of motion buffer */
        Window current;     ;;; for use internally for Keymap notify */
        int min_keycode;    ;;; minimum defined keycode */
        int max_keycode;    ;;; maximum defined keycode */
        KeySym *keysyms;    ;;; This server's keysyms */
        XModifierKeymap *modifiermap;   ;;; This server's modifier keymap */
        int keysyms_per_keycode; ;;; number of rows */
        char *xdefaults;    ;;; contents of defaults from server */
        char *scratch_buffer;   ;;; place to hang scratch buffer */
        unsigned long scratch_length;   ;;; length of scratch buffer */
        int ext_number;     ;;; extension number on this display */
        _XExtension *ext_procs; ;;; extensions initialized on this display

         ;;; the following can be fixed size, as the protocol defines how
         ;;; much address space is available.
         ;;; While this could be done using the extension vector, there
         ;;; may be MANY events processed, so a search through the extension
         ;;; list to find the right procedure for each event might be
         ;;; expensive if many extensions are being used.

        Bool (*event_vec[128])();  ;;; vector for wire to event */
        Status (*wire_vec[128])(); ;;; vector for event to wire */
    } Display;

endexternal;


global vars XConstants  = true;

endsection;

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Jan 25 1991 Changed from include to loadinclude
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
--- Aaron Sloman, May 23 1990
    sectionised and put the global constant declarations into
    XConstants.ph
 */
