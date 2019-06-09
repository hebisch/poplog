/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XImages.p
 > Purpose:
 > Author:          Gareth Palmer,  4 July 1989 (see revisions)
 > Documentation:
 > Related Files:
 */


uses XConstants;

global constant macro (

/* ImageFormat -- PutImage, GetImage */

 XYBitmap        = 0,   /* depth = 1, XYFormat */
 XYPixmap        = 1,   /* depth == drawable depth */
 ZPixmap         = 2,   /* depth == drawable depth */

/* Byte order  used in imageByteOrder and bitmapBitOrder */

 LSBFirst        = 0,
 MSBFirst        = 1,

);


external declare XImages in c;
    (external_import_procedure XptImportProcedure)


    /*
     * Data structure for "image" data, used by image manipulation routines.
     */
    typedef struct _XImage {
        int width, height;      ;;; size of image */
        int xoffset;        ;;; number of pixels offset in X direction */
        int format;         ;;; XYBitmap, XYPixmap, ZPixmap */
        char *data;         ;;; pointer to image data */
        int byte_order;     ;;; data byte order, LSBFirst, MSBFirst */
        int bitmap_unit;        ;;; quant. of scanline 8, 16, 32 */
        int bitmap_bit_order;   ;;; LSBFirst, MSBFirst */
        int bitmap_pad;     ;;; 8, 16, 32 either XY or ZPixmap */
        int depth;          ;;; depth of image */
        int bytes_per_line;     ;;; accelarator to next line */
        int bits_per_pixel;     ;;; bits per pixel (ZPixmap) */
        unsigned long red_mask; ;;; bits in z arrangment */
        unsigned long green_mask;
        unsigned long blue_mask;
        char *obdata;       ;;; hook for the object routines to hang on */
        struct funcs {      ;;; image manipulation routines */
            struct _XImage *(*create_image)();
            int (*destroy_image)();
            unsigned long (*get_pixel)();
            int (*put_pixel)();
            struct _XImage *(*sub_image)();
            int (*add_pixel)();
        } f;
    } XImage;



XImage *XCreateImage(display, visual, depth, format, offset, data, width,
                       height, bitmap_pad, bytes_per_line)
Display *display;
Visual visual;
unsigned int depth;
int format;
int offset;
char *data;
unsigned int width;
unsigned int height;
int bitmap_pad;
int bytes_per_line;
{}

int XDestroyImage(ximage)
XImage *ximage;
{}

void XPutImage(display, drawable, gc, image, src_x, src_y, dst_x, dst_y,
                 width, height)
Display *display;
Drawable drawable;
GC gc;
XImage *image;
int src_x, src_y;
int dst_x, dst_y;
unsigned int width, height;
{}

XImage *XSubImage(ximage, x, y, subimage_width, subimage_height)
XImage *ximage;
int x;
int y;
unsigned int subimage_width;
unsigned int subimage_height;
{}

XImage *XGetImage(display, drawable, x, y, width, height, plane_mask, format)
Display *display;
Drawable drawable;
int x, y;
unsigned int width, height;
unsigned long plane_mask;
int format;
{}

XImage *XGetSubImage(display, drawable, x, y, width, height, plane_mask,
                        format, dest_image, dest_x, dest_y)
Display *display;
Drawable drawable;
int x, y;
unsigned int width, height;
unsigned long plane_mask;
int format;
XImage *dest_image;
int dest_x, dest_y;
{}

int XAddPixel(ximage, value)
XImage *ximage;
unsigned long value;
{}

int XPutPixel(ximage, x, y, pixel)
XImage *ximage;
int x;
int y;
unsigned long pixel;
{}

unsigned long XGetPixel(ximage, x, y)
XImage *ximage;
int x;
int y;
{}


endexternal;


xlib_external_require XImages;


;;; duplicated from XlibMacros
 ;;; #define ImageByteOrder(dpy)     ((dpy)->byte_order)
define global ImageByteOrder(dpy);
    lvars dpy;
    dpy #-> byte_order
enddefine;


global vars XImages = true;

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Jan 25 1991 Changed to use xlib_external_require
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
 */
