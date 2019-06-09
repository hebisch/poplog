/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XExtent.p
 > Purpose:
 > Author:          Ian Rogers, Aug 18 1989 (see revisions)
 > Documentation:
 > Related Files:   LIB * XText XFonts XProperties XWindowManager
 */

uses XConstants;

external declare xpop in c;
    (external_import_procedure XptImportProcedure)

    typedef struct {
        long flags; ;;; marks which fields in this structure are defined
        int x, y;
        int width, height;
        int min_width, min_height;
        int max_width, max_height;
        int width_inc, height_inc;
        struct {
            int x;  ;;; numerator
            int y;  ;;; denominator
        } min_aspect, max_aspect;
    } XSizeHints;

    /*
     * per character font metric information.
     */
    typedef struct {
        short   lbearing;   ;;; origin to left edge of raster */
        short   rbearing;   ;;; origin to right edge of raster */
        short   width;      ;;; advance to next char's origin */
        short   ascent;     ;;; baseline to top edge of raster */
        short   descent;    ;;; baseline to bottom edge of raster */
        unsigned short attributes;  ;;; per char flags (not predefined) */
    } XCharStruct;

    /*
     * To allow arbitrary information with fonts, there are additional properties
     * returned.
     */
    typedef struct {
        Atom name;
        unsigned long card32;
    } XFontProp;

    typedef struct {
        XExtData    *ext_data;  ;;; hook for extension to hang data */
        Font        fid;            ;;; Font id for this font */
        unsigned    direction;  ;;; hint about direction the font is painted */
        unsigned    min_char_or_byte2; ;;; first character */
        unsigned    max_char_or_byte2; ;;; last character */
        unsigned    min_byte1;  ;;; first row that exists */
        unsigned    max_byte1;  ;;; last row that exists */
        Bool    all_chars_exist; ;;; flag if all characters have non-zero size*/
        unsigned    default_char;   ;;; char to print for undefined character */
        int         n_properties;   ;;; how many properties there are */
        XFontProp   *properties;    ;;; pointer to array of additional properties*/
        XCharStruct min_bounds; ;;; minimum bounds over all existing char*/
        XCharStruct max_bounds; ;;; minimum bounds over all existing char*/
        XCharStruct *per_char;  ;;; first_char to last_char information */
        int     ascent;     ;;; log. extent above baseline for spacing */
        int     descent;    ;;; log. descent below baseline for spacing */
    } XFontStruct;

endexternal;

global vars XExtent = true;

/* --- Revision History ---------------------------------------------------
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
 */
