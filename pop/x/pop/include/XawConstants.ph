/*--- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/include/XawConstants.ph
 > Purpose:         Athena Widget Set Interface - global constants
 > Author:          John Gibson, Apr 17 1993
 > Documentation:   REF * ATHENA
 */

#_TERMIN_IF DEF XAW_CONSTANTS_INCLUDED

section;

include xpt_xtypes.ph;

/* Converters */

iconstant macro(
    XtJustifyLeft = 0,
    XtJustifyCenter = 1,
    XtJustifyRight = 2,

    XtOrientHorizontal = 0,
    XtOrientVertical = 1,

    XmuShapeRectangle = 1,
    XmuShapeOval = 2,
    XmuShapeEllipse = 3,
    XmuShapeRoundedRectangle = 4,
);

/* TextSinkObject */

iconstant macro(
    XawisOn = 0,
    XawisOff = 1
);


/* TextSrcObject */

iconstant macro(

    XawstPositions = 0,
    XawstWhiteSpace = 1,
    XawstEOL = 2,
    XawstParagraph = 3,
    XawstAll = 4,

);

i_typespec
        XawTextPosition :ulong,
;


/* AsciiSrcObject */

iconstant macro(
    XawAsciiFile = 0,
    XawAsciiString = 1
);


/* TextWidget */

iconstant macro(
    XawTextSearchError = -12345,

    XawEditDone = 0,
    XawEditError = 1,
    XawPositionError = 2,

    XawtextScrollNever = 0,
    XawtextScrollWhenNeeded = 1,
    XawtextScrollAlways = 2,

    XawtextWrapNever = 0,
    XawtextWrapLine = 1,
    XawtextWrapWord = 2,

    XawtextResizeNever = 0,
    XawtextResizeWidth = 1,
    XawtextResizeHeight = 2,
    XawtextResizeBoth = 3,

    XawsdLeft = 0,
    XawsdRight = 1,

    XawtextRead = 0,
    XawtextAppend = 1,
    XawtextEdit = 2,

    XawselectNull = 0,
    XawselectPosition = 1,
    XawselectChar = 2,
    XawselectWord = 3,
    XawselectLine = 4,
    XawselectParagraph = 5,
    XawselectAll = 6
);

i_typespec
    XpawTextBlock {
        XpawTBFirstPos  :int,
        XpawTBLength    :int,
        XpawTBString    :XptString,
        XpawTBFormat    :XptAtom,
    },

    XpawTextBlockPtr: exptr.:XpawTextBlock,
;


/* StripChartWidget */

iconstant macro DEFAULT_JUMP = -1;


/* CommandWidget */

iconstant macro(
    XawShapeRectangle = 1,
    XawShapeOval = 2,
    XawShapeEllipse = 3,
    XawShapeRoundedRectangle = 4
);


/* GripWidget */

i_typespec
    GripCallDataRec {
        event :XptXEventPtr,
        params :exptr.:XptString[], ;;; String *
        num_params :uint,
    },
    GripCallData :exptr.:GripCallDataRec,
;


/* FormWidget */

iconstant macro(
    XtChainTop = 0,
    XtChainBottom = 1,
    XtChainLeft = 2,
    XtChainRight = 3,
    XtRubber = 4
);


/* ListWidget */

iconstant macro XAW_LIST_NONE = -1;

i_typespec
    XawListReturnStruct {
        string :XptString,
        list_index :int,
    },
;


/* PanedWidget */

iconstant macro(
    PANED_ASK_CHILD = 0,
    PANED_GRIP_SIZE = 0
);

iconstant XAW_CONSTANTS_INCLUDED = true;

endsection;
