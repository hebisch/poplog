/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.x/x/pop/include/XmConstants.ph
 > Purpose:         Motif Constants
 > Author:          Jonathan Meyer, Feb  9 1991 (see revisions)
 > Documentation:   HELP *MOTIF
 */

#_TERMIN_IF DEF XMCONSTANTS_INCLUDED

include xpt_xtypes.ph;

section;

iconstant macro (

    XmVersion   = XM_VERSION,
    XmVERSION   = XmVersion div 1000,
    XmREVISION  = XmVersion mod 1000,

    XmUNSPECIFIED_PIXMAP = 2,

/************************************************************************
 *  XmString/XmFontList defines
 ************************************************************************/

    XmFONT_IS_FONT = 0,
    XmFONT_IS_FONTSET = 1,

    XmSTRING_DIRECTION_L_TO_R = 0,
    XmSTRING_DIRECTION_R_TO_L = 1,
    XmSTRING_DIRECTION_DEFAULT = 255,

    XmSTRING_COMPONENT_UNKNOWN = 0,
    XmSTRING_COMPONENT_CHARSET = 1,
    XmSTRING_COMPONENT_TEXT = 2,
    XmSTRING_COMPONENT_DIRECTION = 3,
    XmSTRING_COMPONENT_SEPARATOR = 4,
    XmSTRING_COMPONENT_LOCALE_TEXT = 5,

    XmSTRING_COMPONENT_END = 126,
    XmSTRING_COMPONENT_USER_BEGIN = 128,
    XmSTRING_COMPONENT_USER_END = 255,

    XmSTRING_ISO8859_1 = 'ISO8859-1',
    XmSTRING_OS_CHARSET = XmSTRING_ISO8859_1,
    XmSTRING_DEFAULT_CHARSET = nullstring,
    XmFONTLIST_DEFAULT_TAG = 'FONTLIST_DEFAULT_TAG_STRING',

/************************************************************************
 *  Primitive defines
 ************************************************************************/

    XmCHANGE_ALL = 0,
    XmCHANGE_NONE = 1,
    XmCHANGE_WIDTH = 2,
    XmCHANGE_HEIGHT = 3,

    XmPIXELS = 0,
    Xm100TH_MILLIMETERS = 1,
    Xm1000TH_INCHES = 2,
    Xm100TH_POINTS = 3,
    Xm100TH_FONT_UNITS = 4,

    XmDESTROY = 0,
    XmUNMAP = 1,
    XmDO_NOTHING = 2,

    XmEXPLICIT = 0,
    XmPOINTER = 1,

/************************************************************************
 *  Navigation defines
 ************************************************************************/

    XmNONE = 0,
    XmTAB_GROUP = 1,
    XmSTICKY_TAB_GROUP = 2,
    XmEXCLUSIVE_TAB_GROUP = 3,

    XmDYNAMIC_DEFAULT_TAB_GROUP = 255,

    XmBELL = 1,

/************************************************************************
 *  Menu defines
 ************************************************************************/

    XmNO_ORIENTATION = 0,
    XmVERTICAL = 1,
    XmHORIZONTAL = 2,

    XmWORK_AREA = 0,
    XmMENU_BAR = 1,
    XmMENU_PULLDOWN = 2,
    XmMENU_POPUP = 3,
    XmMENU_OPTION = 4,

    XmNO_PACKING = 0,
    XmPACK_TIGHT = 1,
    XmPACK_COLUMN = 2,
    XmPACK_NONE = 3,

    XmTEAR_OFF_ENABLED = 0,
    XmTEAR_OFF_DISABLED = 1,

    XmUNPOST = 0,
    XmUNPOST_AND_REPLAY = 1,

    XmLAST_POSITION = -1,
    XmFIRST_POSITION = 0,

    XmALIGNMENT_CONTENTS_TOP = 3,
    XmALIGNMENT_CONTENTS_BOTTOM = 4,

/************************************************************************
 *  Label/Frame defines
 ************************************************************************/

    XmALIGNMENT_BEGINNING = 0,
    XmALIGNMENT_CENTER = 1,
    XmALIGNMENT_END = 2,

    XmALIGNMENT_BASELINE_TOP = 0,
    XmALIGNMENT_BASELINE_BOTTOM = 2,
    XmALIGNMENT_WIDGET_TOP = 3,
    XmALIGNMENT_WIDGET_BOTTOM = 4,

    XmFRAME_GENERIC_CHILD = 0,
    XmFRAME_WORKAREA_CHILD = 1,
    XmFRAME_TITLE_CHILD = 2,

/************************************************************************
 *  ToggleButton  defines
 ************************************************************************/

    XmN_OF_MANY = 1,
    XmONE_OF_MANY = 2,

/************************************************************************
 *  Form resources and defines
 ************************************************************************/

    XmATTACH_NONE = 0,
    XmATTACH_FORM = 1,
    XmATTACH_OPPOSITE_FORM = 2,
    XmATTACH_WIDGET = 3,
    XmATTACH_OPPOSITE_WIDGET = 4,
    XmATTACH_POSITION = 5,
    XmATTACH_SELF = 6,

    XmRESIZE_NONE = 0,
    XmRESIZE_GROW = 1,
    XmRESIZE_ANY = 2,

/****************************************************************************
 *  Callback reasons
 ****************************************************************************/

    XmCR_NONE = 0,
    XmCR_HELP = 1,
    XmCR_VALUE_CHANGED = 2,
    XmCR_INCREMENT = 3,
    XmCR_DECREMENT = 4,
    XmCR_PAGE_INCREMENT = 5,
    XmCR_PAGE_DECREMENT = 6,
    XmCR_TO_TOP = 7,
    XmCR_TO_BOTTOM = 8,
    XmCR_DRAG = 9,
    XmCR_ACTIVATE = 10,
    XmCR_ARM = 11,
    XmCR_DISARM = 12,
    XmCR_MAP = 16,
    XmCR_UNMAP = 17,
    XmCR_FOCUS = 18,
    XmCR_LOSING_FOCUS = 19,
    XmCR_MODIFYING_TEXT_VALUE = 20,
    XmCR_MOVING_INSERT_CURSOR = 21,
    XmCR_EXECUTE = 22,
    XmCR_SINGLE_SELECT = 23,
    XmCR_MULTIPLE_SELECT = 24,
    XmCR_EXTENDED_SELECT = 25,
    XmCR_BROWSE_SELECT = 26,
    XmCR_DEFAULT_ACTION = 27,
    XmCR_CLIPBOARD_DATA_REQUEST = 28,
    XmCR_CLIPBOARD_DATA_DELETE = 29,
    XmCR_CASCADING = 30,
    XmCR_OK = 31,
    XmCR_CANCEL = 32,
    XmCR_APPLY = 34,
    XmCR_NO_MATCH = 35,
    XmCR_COMMAND_ENTERED = 36,
    XmCR_COMMAND_CHANGED = 37,
    XmCR_EXPOSE = 38,
    XmCR_RESIZE = 39,
    XmCR_INPUT = 40,
    XmCR_GAIN_PRIMARY = 41,
    XmCR_LOSE_PRIMARY = 42,
    XmCR_CREATE = 43,
    XmCR_TEAR_OFF_ACTIVATE = 44,
    XmCR_TEAR_OFF_DEACTIVATE = 45,
    XmCR_OBSCURED_TRAVERSAL = 46,
    XmCR_PROTOCOLS = 47,

    XmCR_WM_PROTOCOLS = 666,

/************************************************************************
 *  PushButton defines
 ************************************************************************/

    XmMULTICLICK_DISCARD = 0,
    XmMULTICLICK_KEEP = 1,

/************************************************************************
 *  DrawnButton defines
 ************************************************************************/

    XmSHADOW_IN = 7,
    XmSHADOW_OUT = 8,

/************************************************************************
 *  Arrow defines
 ************************************************************************/

    XmARROW_UP = 0,
    XmARROW_DOWN = 1,
    XmARROW_LEFT = 2,
    XmARROW_RIGHT = 3,

/************************************************************************
 *  Separator defines
 ************************************************************************/

    XmNO_LINE = 0,
    XmSINGLE_LINE = 1,
    XmDOUBLE_LINE = 2,
    XmSINGLE_DASHED_LINE = 3,
    XmDOUBLE_DASHED_LINE = 4,
    XmSHADOW_ETCHED_IN = 5,
    XmSHADOW_ETCHED_OUT = 6,
    XmSHADOW_ETCHED_IN_DASH = 7,
    XmSHADOW_ETCHED_OUT_DASH = 8,
    XmINVALID_SEPARATOR_TYPE = 9,

    XmPIXMAP = 1,
    XmSTRING = 2,

/************************************************************************
 *  Drag and Drop defines
 ************************************************************************/

    XmWINDOW = 0,
    XmCURSOR = 2,

/************************************************************************
 *  ScrollBar defines
 ************************************************************************/

    XmMAX_ON_TOP = 0,
    XmMAX_ON_BOTTOM = 1,
    XmMAX_ON_LEFT = 2,
    XmMAX_ON_RIGHT = 3,

/************************************************************************
 *  List Widget defines
 ************************************************************************/

    XmSINGLE_SELECT = 0,
    XmMULTIPLE_SELECT = 1,
    XmEXTENDED_SELECT = 2,
    XmBROWSE_SELECT = 3,

    XmSTATIC = 0,
    XmDYNAMIC = 1,

    XmINITIAL = 0,
    XmADDITION = 1,
    XmMODIFICATION = 2,

/************************************************************************
 *  Scrolled Window defines
 ************************************************************************/

    XmVARIABLE = 0,
    XmCONSTANT = 1,
    XmRESIZE_IF_POSSIBLE = 2,

    XmAUTOMATIC = 0,
    XmAPPLICATION_DEFINED = 1,

    XmAS_NEEDED = 1,

    SW_TOP = 1,
    SW_BOTTOM = 0,
    SW_LEFT = 2,
    SW_RIGHT = 0,

    XmTOP_LEFT  = (SW_TOP || SW_LEFT),
    XmBOTTOM_LEFT   = (SW_BOTTOM  || SW_LEFT),
    XmTOP_RIGHT = (SW_TOP || SW_RIGHT),
    XmBOTTOM_RIGHT  = (SW_BOTTOM  || SW_RIGHT),

/************************************************************************
 *  MainWindow Resources
 ************************************************************************/

    XmCOMMAND_ABOVE_WORKSPACE = 0,
    XmCOMMAND_BELOW_WORKSPACE = 1,

/************************************************************************
 *  Text Widget defines
 ************************************************************************/

    XmMULTI_LINE_EDIT = 0,
    XmSINGLE_LINE_EDIT = 1,

    XmTEXT_FORWARD = 0,
    XmTEXT_BACKWARD = 1,

    XmSELECT_POSITION = 0,
    XmSELECT_WHITESPACE = 1,
    XmSELECT_WORD = 2,
    XmSELECT_LINE = 3,
    XmSELECT_ALL = 4,
    XmSELECT_PARAGRAPH = 5,

    XmHIGHLIGHT_NORMAL = 0,
    XmHIGHLIGHT_SELECTED = 1,
    XmHIGHLIGHT_SECONDARY_SELECTED = 2,

    XmCOPY_FAILED = 0,
    XmCOPY_SUCCEEDED = 1,
    XmCOPY_TRUNCATED = 2,

/************************************************************************
 *  Dialog defines (BulletinBoard and subclasses)
 ************************************************************************/

    XmDIALOG_NONE = 0,
    XmDIALOG_APPLY_BUTTON = 1,
    XmDIALOG_CANCEL_BUTTON = 2,
    XmDIALOG_DEFAULT_BUTTON = 3,
    XmDIALOG_OK_BUTTON = 4,
    XmDIALOG_FILTER_LABEL = 5,
    XmDIALOG_FILTER_TEXT = 6,
    XmDIALOG_HELP_BUTTON = 7,
    XmDIALOG_LIST = 8,
    XmDIALOG_LIST_LABEL = 9,
    XmDIALOG_MESSAGE_LABEL = 10,
    XmDIALOG_SELECTION_LABEL = 11,
    XmDIALOG_SYMBOL_LABEL = 12,
    XmDIALOG_TEXT = 13,
    XmDIALOG_SEPARATOR = 14,
    XmDIALOG_DIR_LIST = 15,
    XmDIALOG_DIR_LIST_LABEL = 16,

    XmDIALOG_HISTORY_LIST = XmDIALOG_LIST,
    XmDIALOG_PROMPT_LABEL = XmDIALOG_SELECTION_LABEL,
    XmDIALOG_VALUE_TEXT = XmDIALOG_TEXT,
    XmDIALOG_COMMAND_TEXT = XmDIALOG_TEXT,
    XmDIALOG_FILE_LIST = XmDIALOG_LIST,
    XmDIALOG_FILE_LIST_LABEL = XmDIALOG_LIST_LABEL,

    XmDIALOG_MODELESS = 0,
    XmDIALOG_PRIMARY_APPLICATION_MODAL = 1,
    XmDIALOG_FULL_APPLICATION_MODAL = 2,
    XmDIALOG_SYSTEM_MODAL = 3,

    XmDIALOG_APPLICATION_MODAL  = XmDIALOG_PRIMARY_APPLICATION_MODAL,

/************************************************************************
 *  XmSelectionBox, XmFileSelectionBox and XmCommand
 ***********************************************************************/

    XmPLACE_TOP = 0,
    XmPLACE_ABOVE_SELECTION = 1,
    XmPLACE_BELOW_SELECTION = 2,

    XmFILE_DIRECTORY = 2:1e0,
    XmFILE_REGULAR = 2:1e1,
    XmFILE_ANY_TYPE = (XmFILE_DIRECTORY || XmFILE_REGULAR),

    XmDIALOG_WORK_AREA = 0,
    XmDIALOG_PROMPT = 1,
    XmDIALOG_SELECTION = 2,
    XmDIALOG_COMMAND = 3,
    XmDIALOG_FILE_SELECTION = 4,

/************************************************************************
 *  XmMessageBox
 ***********************************************************************/

    XmDIALOG_TEMPLATE = 0,
    XmDIALOG_ERROR = 1,
    XmDIALOG_INFORMATION = 2,
    XmDIALOG_MESSAGE = 3,
    XmDIALOG_QUESTION = 4,
    XmDIALOG_WARNING = 5,
    XmDIALOG_WORKING = 6,

    XmVISIBILITY_UNOBSCURED = 0,
    XmVISIBILITY_PARTIALLY_OBSCURED = 1,
    XmVISIBILITY_FULLY_OBSCURED = 2,

    XmTRAVERSE_CURRENT = 0,
    XmTRAVERSE_NEXT = 1,
    XmTRAVERSE_PREV = 2,
    XmTRAVERSE_HOME = 3,
    XmTRAVERSE_NEXT_TAB_GROUP = 4,
    XmTRAVERSE_PREV_TAB_GROUP = 5,
    XmTRAVERSE_UP = 6,
    XmTRAVERSE_DOWN = 7,
    XmTRAVERSE_LEFT = 8,
    XmTRAVERSE_RIGHT = 9,

/************************************************************************
 *  SimpleMenu declarations and definitions
 ***********************************************************************/

    XmPUSHBUTTON = 1,
    XmTOGGLEBUTTON = 2,
    XmCHECKBUTTON = 2,
    XmRADIOBUTTON = 3,
    XmCASCADEBUTTON = 4,
    XmSEPARATOR = 5,
    XmDOUBLE_SEPARATOR = 6,
    XmTITLE = 7,

    XmVaPUSHBUTTON = 'pushButton',
    XmVaTOGGLEBUTTON = 'checkButton',
    XmVaCHECKBUTTON = 'checkButton',
    XmVaRADIOBUTTON = 'radioButton',
    XmVaCASCADEBUTTON = 'cascadeButton',
    XmVaSEPARATOR = 'separator',
    XmVaSINGLE_SEPARATOR = 'singleSeparator',
    XmVaDOUBLE_SEPARATOR = 'doubleSeparator',
    XmVaTITLE = 'title',

/************************************************************************
 *  ExtObject defines
 ***********************************************************************/

    XmCACHE_EXTENSION = 1,
    XmDESKTOP_EXTENSION = 2,
    XmSHELL_EXTENSION = 3,
    XmPROTOCOL_EXTENSION = 4,
    XmDEFAULT_EXTENSION = 5,

/************************************************************************
 *  functions renamed after 1.0 release due to resource name overlap
 ***********************************************************************/

    XmGetTopPosition = "XmGetTopCharacter",
    XmSetTopPosition = "XmSetTopCharacter",
    XmIsVendorShell = "XtIsVendorShell",

);

/************************************************************************
 *  Callback structures
 ************************************************************************/

i_typespec

    XmString: exptr#XpmImportString,
    XpmString: XmString#XpmCoerceString,
    XpmCopiedString :XmString#XpmCoerceCopiedString,
    XmStringTable: XmString[],
    XmStringComponentType: byte,
    XmFontList: exptr,
    XmFontListEntry: exptr,
    XmStringContext: exptr,
    XmFontContext: exptr,

    XmAnyCallbackStruct {
        reason: int,
        event: XptXEventPtr,
    },

    XmArrowButtonCallbackStruct {
        reason: int,
        event: XptXEventPtr,
        click_count: int
    },

    XmDrawingAreaCallbackStruct {
        reason: int,
        event: XptXEventPtr,
        window: XptWindow,
    },

    XmDrawnButtonCallbackStruct {
        reason: int,
        event: XptXEventPtr,
        window: XptWindow,
        click_count: int,
    },

    XmPushButtonCallbackStruct {
        reason: int,
        event: XptXEventPtr,
        click_count: int
    },

    XmRowColumnCallbackStruct {
        reason: int,
        event: XptXEventPtr,
        widget: XptWidget,
        data: exptr,
        callbackstruct: exptr,
    },

    XmScrollBarCallbackStruct {
        reason: int,
        event: XptXEventPtr,
        value: int,
        pixel: int,
    },

    XmToggleButtonCallbackStruct {
        reason: int,
        event: XptXEventPtr,
        set: int,
    },

    XmListCallbackStruct {
        reason: int,
        event: XptXEventPtr,
        item: XmString,
        item_length: int,
        item_position: int,
        selected_items: exptr.:XmString[],
        selected_item_count: int,
        selected_item_positions: exptr.:int[],
        selection_type: byte,
    },

    XmSelectionBoxCallbackStruct {
        reason: int,
        event: XptXEventPtr,
        value: XmString,
        length: int,
    },

    XmCommandCallbackStruct {
        reason: int,
        event: XptXEventPtr,
        value: XmString,
        length: int,
    },

    XmFileSelectionBoxCallbackStruct {
        reason: int,
        event: XptXEventPtr,
        value: XmString,
        length: int,
        mask: XmString,
        mask_length: int,
        dir: XmString,
        dir_length: int,
        pattern: XmString,
        pattern_length: int,
    },

    XmScaleCallbackStruct {
        reason: int,
        event: XptXEventPtr,
        value: int,
    },

    XmTextPosition :long,
    XmTextSource :exptr,
    XmTextFormat :ulong, ;;; Atom

    /* XmTextBlock's are used to pass text around. */

    XmTextBlockRec {
        ptr: exptr,
        length: int,
        format: XmTextFormat,
    },
    XmTextBlock: exptr.:XmTextBlockRec,

    XmTextVerifyCallbackStruct {
        reason: int,
        event: XptXEventPtr,
        doit: XptBoolean,
        currInsert: long,
        newInsert: long,
        startPos: long,
        endPos: long,
        text:XmTextBlock,
    },
    XmTextVerifyPtr: exptr.:XmTextVerifyCallbackStruct,

    XmButtonType: byte,
    XmButtonTypeTable: XmButtonType[],
    XmKeySymTable: ulong[], ;;; KeySym *

    XmStringDirection: byte,
    XmStringCharSet :XptString,
    XmStringCharSetTable: XmStringCharSet[],

    XmResourceBaseProc(2): exptr,

    XmSecondaryResourceDataRec {
        base_proc: exptr.:XmResourceBaseProc,
        client_data: XptPointer,
        name: XptString,
        res_class: XptString,
        resources: exptr,       ;;; .:XptResourceList,
        num_resources: uint,
    },
    XmSecondaryResourceData: exptr.:XmSecondaryResourceDataRec,

    XmNavigationType: byte,

    XmOffset: long,
    XmOffsetPtr: exptr.: XmOffset,

    XpmAnyCallbackStruct {
        XpmACSReason: int,
        XpmACSEvent: XptXEventPtr,
        :ulong[7] /* Padding fields */
    },
;

iconstant XMCONSTANTS_INCLUDED = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Jun 26 1997
        Added XmFontListEntry typespec
--- Robert John Duncan, Aug 31 1995
        More 1.2 definitions
--- Robert John Duncan, Jun 23 1995
        Added 1.2 definitions for Text and TextField
--- Robert John Duncan, Jan 20 1995
        Fixed definition for XmListCallbackStruct: selected_item_positions
        should be an int array
--- John Gibson, Apr 14 1993
        Made an include file, moved code part out to autoloadable
        XpmImportString.p
 */
