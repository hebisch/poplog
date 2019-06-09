/*--- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/include/XolConstants.ph
 > Purpose:         Open Look Widget Set Interface - global constants
 > Author:          Jonathan Meyer, Oct 25 1990 (see revisions)
 > Documentation:
 */

#_TERMIN_IF DEF XOL_CONSTANTS_INCLUDED

section;

/*
 ************************************************************************
 *  Define Constant Tokens
 ************************************************************************
 */

iconstant macro (
        OL_IGNORE                   = ~~0,

        OleditDone                  = 0,
        OleditError                 = 1,
        OleditPosError              = 2,
        OleditReject                = 3,

        OL_ABSENT_PAIR              = 0,
        OL_ALL                      = 1,
        OL_ALWAYS                   = 2,
        OL_ATOM_HELP                = 3,
        OL_BOTH                     = 4,
        OL_BOTTOM                   = 5,
        OL_BUTTONSTACK              = 6,
        OL_CENTER                   = 7,
        OL_CLASS_HELP               = 8,
        OL_COLUMNS                  = 9,
        OL_COPY_MASK_VALUE          = 10,
        OL_COPY_SIZE                = 11,
        OL_COPY_SOURCE_VALUE        = 12,
        OL_CURRENT                  = 13,
        OL_DEFAULT_PAIR             = 14,
        OL_DISK_SOURCE              = 15,
        OL_DISPLAY_FORM             = 16,
        OL_DOWN                     = 17,
        OL_EXISTING_SOURCE          = 18,
        OL_FIXEDCOLS                = 19,
        OL_FIXEDHEIGHT              = 20,
        OL_FIXEDROWS                = 21,
        OL_FIXEDWIDTH               = 22,
        OL_FLAT_BUTTON              = 23,
        OL_FLAT_CHECKBOX            = 24,
        OL_FLAT_CONTAINER           = 25,
        OL_FLAT_EXCLUSIVES          = 26,
        OL_FLAT_HELP                = 27,
        OL_FLAT_NONEXCLUSIVES       = 28,
        OL_HALFSTACK                = 29,
        OL_HORIZONTAL               = 30,
        OL_IMAGE                    = 31,
        OL_IN                       = 32,
        OL_INDIRECT_SOURCE          = 33,
        OL_LABEL                    = 34,
        OL_LEFT                     = 35,
        OL_MASK_PAIR                = 36,
        OL_MAXIMIZE                 = 37,
        OL_MILLIMETERS              = 38,
        OL_MINIMIZE                 = 39,
        OL_NEVER                    = 40,
        OL_NEXT                     = 41,
        OL_NONE                     = 42,
        OL_NONEBOTTOM               = 43,
        OL_NONELEFT                 = 44,
        OL_NONERIGHT                = 45,
        OL_NONETOP                  = 46,
        OL_NOTICES                  = 47,
        OL_NO_VIRTUAL_MAPPING       = 48,
        OL_OBLONG                   = 49,
        OL_OUT                      = 50,
        OL_OVERRIDE_PAIR            = 51,
        OL_PIXELS                   = 52,
        OL_POINTS                   = 53,
        OL_POPUP                    = 54,
        OL_PREVIOUS                 = 55,
        OL_PROG_DEFINED_SOURCE      = 56,
        OL_RECTBUTTON               = 57,
        OL_RIGHT                    = 58,
        OL_ROWS                     = 59,
        OL_SOURCE_FORM              = 60,
        OL_SOURCE_PAIR              = 61,
        OL_STAYUP                   = 62,
        OL_STRING                   = 63,
        OL_STRING_SOURCE            = 64,
        OL_TEXT_APPEND              = 65,
        OL_TEXT_EDIT                = 66,
        OL_TEXT_READ                = 67,
        OL_TOP                      = 68,
        OL_TRANSPARENT_SOURCE       = 69,
        OL_VERTICAL                 = 70,
        OL_VIRTUAL_BUTTON           = 71,
        OL_VIRTUAL_KEY              = 72,
        OL_WIDGET_HELP              = 73,
        OL_WINDOW_HELP              = 74,
        OL_WRAP_ANY                 = 75,
        OL_WRAP_WHITE_SPACE         = 76,

/*
 ************************************************************************
 *  Define Bitwise Tokens
 ************************************************************************
 */
         OL_B_OFF                   = 0,
         OL_B_HORIZONTAL            = (1<<0),
         OL_B_VERTICAL              = (1<<1),
         OL_B_BOTH                  = (OL_B_VERTICAL || OL_B_HORIZONTAL),
);

/*
 ************************************************************************
 *  Define Constant tokens that are being maintained for source
 *  compatibility with older versions of the toolkit.
 ************************************************************************
 */
/* not needed for us - we don't have any source to be compatible
         OL_BEEP_NEVER              = OL_NEVER,
         OL_BEEP_NOTICES            = OL_NOTICES,
         OL_BEEP_NOTICES_AND_FOOTERS= OL_ALWAYS,
         OL_BEEP_ALWAYS             = OL_ALWAYS,
         OL_AUTO_SCROLL_OFF         = OL_B_OFF,
         OL_AUTO_SCROLL_HORIZONTAL  = OL_B_HORIZONTAL,
         OL_AUTO_SCROLL_VERTICAL    = OL_B_VERTICAL,
         OL_AUTO_SCROLL_BOTH        = OL_B_BOTH,
         OL_GROW_OFF                = OL_B_OFF,
         OL_GROW_HORIZONTAL         = OL_B_HORIZONTAL,
         OL_GROW_VERTICAL           = OL_B_VERTICAL,
         OL_GROW_BOTH               = OL_B_BOTH,
);
*/

/*
 ************************************************************************
 *  Define public structures
 ************************************************************************
 */

i_typespec
    ntstring :byte[].exacc_ntstring,
    ntstring_ptr :exptr.:ntstring,
    ShortBool :byte#XptCoerceBoolean,
    LongBool :int#XptCoerceBoolean,
    OlDefine :short,

    OlFlatHelpId {
        widget :exptr.XptImportWidget,
        item_index :int,
    },

    OlTextBlock {
        firstPos :int,
        length :int,
        ptr :exptr,
    },
    OlTextBlockPtr :exptr.:OlTextBlock,

    OlTextVerifyCD {
        xevent :exptr,
        operation :long,
        doit :ShortBool,
        currInsert :long,
        newInsert :long,
        startPos :long,
        endPos :long,
        text :exptr.:OlTextBlockPtr,
    },
    OlTextVerifyPtr :exptr.:OlTextVerifyCD,

    OlPackedWidget {
        widget :exptr.XptImportWidget,
        name :ntstring_ptr,
        class_ptr :exptr.XptImportWidgetClass,
        descendant :ntstring_ptr,
        resources :exptr, ;;; ArgList
        num_resources :uint,
        managed :ShortBool,
    },
    OlPackedWidgetList :OlPackedWidget[],
;

/*
 *************************************************************************
 *  Macros
 *************************************************************************

The following C macros are not defined in Poplog Open Look. This is because
all of the functionality they provide is available using -XptConvertUnit-.

 */


            /* The following three macros would normally
             * appear in the private header file, but we
             * need them for the metric macros      */

/*
#define _OlFloor(value)     ((int) (value))
#define _OlCeiling(value)   ((int) (_OlFloor(value) == (value) ? \
                    (value) : ((value) + 1)))
#define _OlRound(dbl)       ((int) ((double) (dbl) + (double)\
                ((double)(dbl) < (double)0 ? -0.5 : 0.5)))
*/
                        /* Metric macros    */



                    /* Pixels to Millimeters */
/*
#define Ol_ScreenPixelToMM(direction, value, screen) \
    ((double) (value) * (double)(direction == OL_HORIZONTAL ? \
    ((double)WidthMMOfScreen(screen) / (double)WidthOfScreen(screen))  :\
    ((double)HeightMMOfScreen(screen) / (double)HeightOfScreen(screen))))

#define OlScreenPixelToMM(direction, value, screen) \
        _OlRound(Ol_ScreenPixelToMM(direction, value, screen))

#define Ol_PixelToMM(direction, value) \
        Ol_ScreenPixelToMM(direction, value, OlDefaultScreen)

#define OlPixelToMM(direction, value) \
        OlScreenPixelToMM(direction, value, OlDefaultScreen)
*/
                    /* Millimeters to Pixels */
/*
#define Ol_ScreenMMToPixel(direction, value, screen) \
    ((double) (value) * (double)(direction == OL_HORIZONTAL ? \
    ((double)WidthOfScreen(screen) / (double)WidthMMOfScreen(screen))  :\
    ((double)HeightOfScreen(screen) / (double)HeightMMOfScreen(screen))))

#define OlScreenMMToPixel(direction, value, screen) \
        _OlRound(Ol_ScreenMMToPixel(direction, value, screen))

#define Ol_MMToPixel(direction, value) \
        Ol_ScreenMMToPixel(direction, value, OlDefaultScreen)

#define OlMMToPixel(direction, value) \
        OlScreenMMToPixel(direction, value, OlDefaultScreen)
*/
                    /* Pixels to Points */
/*
#define Ol_ScreenPixelToPoint(direction, value, screen) \
    (2.834645669 * (double)(value) * (double)(direction == OL_HORIZONTAL ? \
    ((double)WidthMMOfScreen(screen) / (double)WidthOfScreen(screen))  :\
    ((double)HeightMMOfScreen(screen) / (double)HeightOfScreen(screen))))

#define OlScreenPixelToPoint(direction, value, screen) \
        _OlRound(Ol_ScreenPixelToPoint(direction, value, screen))

#define Ol_PixelToPoint(direction, value) \
        Ol_ScreenPixelToPoint(direction, value, OlDefaultScreen)

#define OlPixelToPoint(direction, value) \
        OlScreenPixelToPoint(direction, value, OlDefaultScreen)
*/
                    /* Points to Pixels */
/*
#define Ol_ScreenPointToPixel(direction, value, screen) \
    (0.352777777 * (double)(value) * (double)(direction == OL_HORIZONTAL ? \
    ((double)WidthOfScreen(screen) / (double)WidthMMOfScreen(screen))  :\
    ((double)HeightOfScreen(screen) / (double)HeightMMOfScreen(screen))))

#define OlScreenPointToPixel(direction, value, screen) \
        _OlRound(Ol_ScreenPointToPixel(direction, value, screen))

#define Ol_PointToPixel(direction, value) \
        Ol_ScreenPointToPixel(direction, value, OlDefaultScreen)

#define OlPointToPixel(direction, value) \
        OlScreenPointToPixel(direction, value, OlDefaultScreen)
*/

/*
 * Support for Text Verification Callbacks
 */

iconstant macro (
    motionVerify = 1, modVerify = 2, leaveVerify = 3,   ;;; OlVerifyOpType

    OlsdLeft = 1, OlsdRight = 2,                        ;;; OlScanDirection

    OlstPositions = 1, OlstWhiteSpace = 2, OlstEOL = 3,
    OlstParagraph = 4, OlstLast = 5                     ;;; OlScanType


);

/*
 *  These defines are for the Button scale resource.
 */

iconstant macro (
    SMALL_SCALE         = 10,
    MEDIUM_SCALE        = 12,
    LARGE_SCALE         = 14,
    EXTRA_LARGE_SCALE   = 19,
);


/*
 *  Static Text widget
 */
iconstant macro (
        StaticTextSelect    = 0,
        StaticTextAdjust    = 1,
        StaticTextEnd       = 2,
);


iconstant XOL_CONSTANTS_INCLUDED = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 30 1993
        Made this include file from Xol/XolConstants.p
--- Andreas Schoter, Jul 15 1991
    Added global constant XolConstants for compatibility with uses
 */
