/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.x/x/pop/lib/X11/StringDefs.p
 > Purpose:
 > Author:          Ian Rogers, Mar 15 1990
 > Documentation:
 > Related Files:
 */

/***********************************************************
Copyright 1987, 1988 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/


/* Resource names */

INCLUDE_constant macro (

    XtNaccelerators     = 'accelerators\^@',
    XtNallowHoriz       = 'allowHoriz\^@',
    XtNallowVert        = 'allowVert\^@',
    XtNancestorSensitive    = 'ancestorSensitive\^@',
    XtNbackground       = 'background\^@',
    XtNbackgroundPixmap = 'backgroundPixmap\^@',
    XtNbitmap       = 'bitmap\^@',
    XtNborderColor      = 'borderColor\^@',
    XtNborder       = 'borderColor\^@',
    XtNborderPixmap     = 'borderPixmap\^@',
    XtNborderWidth      = 'borderWidth\^@',
    XtNcallback     = 'callback\^@',
    XtNcolormap     = 'colormap\^@',
    XtNdepth        = 'depth\^@',
    XtNdestroyCallback  = 'destroyCallback\^@',
    XtNeditType     = 'editType\^@',
    XtNfile         = 'file\^@',
    XtNfont         = 'font\^@',
    XtNforceBars        = 'forceBars\^@',
    XtNforeground       = 'foreground\^@',
    XtNfunction     = 'function\^@',
    XtNheight       = 'height\^@',
    XtNhighlight        = 'highlight\^@',
    XtNhSpace       = 'hSpace\^@',
    XtNindex        = 'index\^@',
    XtNinnerHeight      = 'innerHeight\^@',
    XtNinnerWidth       = 'innerWidth\^@',
    XtNinnerWindow      = 'innerWindow\^@',
    XtNinsertPosition   = 'insertPosition\^@',
    XtNinternalHeight   = 'internalHeight\^@',
    XtNinternalWidth    = 'internalWidth\^@',
    XtNjumpProc     = 'jumpProc\^@',
    XtNjustify      = 'justify\^@',
    XtNknobHeight       = 'knobHeight\^@',
    XtNknobIndent       = 'knobIndent\^@',
    XtNknobPixel        = 'knobPixel\^@',
    XtNknobWidth        = 'knobWidth\^@',
    XtNlabel        = 'label\^@',
    XtNlength       = 'length\^@',
    XtNlowerRight       = 'lowerRight\^@',
    XtNmappedWhenManaged    = 'mappedWhenManaged\^@',
    XtNmenuEntry        = 'menuEntry\^@',
    XtNname         = 'name\^@',
    XtNnotify       = 'notify\^@',
    XtNorientation      = 'orientation\^@',
    XtNparameter        = 'parameter\^@',
    XtNpixmap       = 'pixmap\^@',
    XtNpopupCallback    = 'popupCallback\^@',
    XtNpopdownCallback  = 'popdownCallback\^@',
    XtNresize       = 'resize\^@',
    XtNreverseVideo     = 'reverseVideo\^@',
    XtNscreen               = 'screen\^@',
    XtNscrollProc       = 'scrollProc\^@',
    XtNscrollDCursor    = 'scrollDCursor\^@',
    XtNscrollHCursor    = 'scrollHCursor\^@',
    XtNscrollLCursor    = 'scrollLCursor\^@',
    XtNscrollRCursor    = 'scrollRCursor\^@',
    XtNscrollUCursor    = 'scrollUCursor\^@',
    XtNscrollVCursor    = 'scrollVCursor\^@',
    XtNselection        = 'selection\^@',
    XtNselectionArray   = 'selectionArray\^@',
    XtNsensitive        = 'sensitive\^@',
    XtNshown        = 'shown\^@',
    XtNspace        = 'space\^@',
    XtNstring       = 'string\^@',
    XtNtextOptions      = 'textOptions\^@',
    XtNtextSink     = 'textSink\^@',
    XtNtextSource       = 'textSource\^@',
    XtNthickness        = 'thickness\^@',
    XtNthumb        = 'thumb\^@',
    XtNthumbProc        = 'thumbProc\^@',
    XtNtop          = 'top\^@',
    XtNtranslations     = 'translations\^@',
    XtNupdate       = 'update\^@',
    XtNuseBottom        = 'useBottom\^@',
    XtNuseRight     = 'useRight\^@',
    XtNvalue        = 'value\^@',
    XtNvSpace       = 'vSpace\^@',
    XtNwidth        = 'width\^@',
    XtNwindow       = 'window\^@',
    XtNx            = 'x\^@',
    XtNy            = 'y\^@',

    /* Class types */
    XtCAccelerators         = 'Accelerators\^@',
    XtCBackground       = 'Background\^@',
    XtCBitmap       = 'Bitmap\^@',
    XtCBoolean      = 'Boolean\^@',
    XtCBorderColor      = 'BorderColor\^@',
    XtCBorderWidth      = 'BorderWidth\^@',
    XtCCallback             = 'Callback\^@',
    XtCColormap     = 'Colormap\^@',
    XtCColor        = 'Color\^@',
    XtCCursor       = 'Cursor\^@',
    XtCDepth        = 'Depth\^@',
    XtCEditType     = 'EditType\^@',
    XtCEventBindings    = 'EventBindings\^@',
    XtCFile         = 'File\^@',
    XtCFont         = 'Font\^@',
    XtCForeground       = 'Foreground\^@',
    XtCFraction     = 'Fraction\^@',
    XtCFunction     = 'Function\^@',
    XtCHeight       = 'Height\^@',
    XtCHSpace       = 'HSpace\^@',
    XtCIndex        = 'Index\^@',
    XtCInsertPosition   = 'InsertPosition\^@',
    XtCInterval     = 'Interval\^@',
    XtCJustify      = 'Justify\^@',
    XtCKnobIndent       = 'KnobIndent\^@',
    XtCKnobPixel        = 'KnobPixel\^@',
    XtCLabel        = 'Label\^@',
    XtCLength       = 'Length\^@',
    XtCMappedWhenManaged    = 'MappedWhenManaged\^@',
    XtCMargin       = 'Margin\^@',
    XtCMenuEntry        = 'MenuEntry\^@',
    XtCNotify       = 'Notify\^@',
    XtCOrientation      = 'Orientation\^@',
    XtCParameter        = 'Parameter\^@',
    XtCPixmap       = 'Pixmap\^@',
    XtCPosition     = 'Position\^@',
    XtCResize       = 'Resize\^@',
    XtCReverseVideo     = 'ReverseVideo\^@',
    XtCScreen               = 'Screen\^@',
    XtCScrollProc       = 'ScrollProc\^@',
    XtCScrollDCursor    = 'ScrollDCursor\^@',
    XtCScrollHCursor    = 'ScrollHCursor\^@',
    XtCScrollLCursor    = 'ScrollLCursor\^@',
    XtCScrollRCursor    = 'ScrollRCursor\^@',
    XtCScrollUCursor    = 'ScrollUCursor\^@',
    XtCScrollVCursor    = 'ScrollVCursor\^@',
    XtCSelection        = 'Selection\^@',
    XtCSensitive        = 'Sensitive\^@',
    XtCSelectionArray   = 'SelectionArray\^@',
    XtCSpace        = 'Space\^@',
    XtCString       = 'String\^@',
    XtCTextOptions      = 'TextOptions\^@',
    XtCTextPosition     = 'TextPosition\^@',
    XtCTextSink     = 'TextSink\^@',
    XtCTextSource       = 'TextSource\^@',
    XtCThickness        = 'Thickness\^@',
    XtCThumb        = 'Thumb\^@',
    XtCTranslations     = 'Translations\^@',
    XtCValue        = 'Value\^@',
    XtCVSpace       = 'VSpace\^@',
    XtCWidth        = 'Width\^@',
    XtCWindow       = 'Window\^@',
    XtCX            = 'X\^@',
    XtCY            = 'Y\^@',

    /* Representation types */

    XtRAcceleratorTable     = 'AcceleratorTable\^@',
    XtRBool         = 'Bool\^@',
    XtRBoolean      = 'Boolean\^@',
    XtRCallback             = 'Callback\^@',
    XtRCallProc             = 'CallProc\^@',
    XtRColor        = 'Color\^@',
    XtRCursor       = 'Cursor\^@',
    XtRDimension        = 'Dimension\^@',
    XtRDisplay      = 'Display\^@',
    XtREditMode     = 'EditMode\^@',
    XtRFile         = 'File\^@',
    XtRFont         = 'Font\^@',
    XtRFontStruct       = 'FontStruct\^@',
    XtRFunction     = 'Function\^@',
    XtRGeometry     = 'Geometry\^@',
    XtRImmediate        = 'Immediate\^@',
    XtRInt          = 'Int\^@',
    XtRJustify      = 'Justify\^@',
    XtRLongBoolean      = XtRBool       /* Compatibility */,
    XtROrientation      = 'Orientation\^@',
    XtRPixel        = 'Pixel\^@',
    XtRPixmap       = 'Pixmap\^@',
    XtRPointer      = 'Pointer\^@',
    XtRPosition     = 'Position\^@',
    XtRShort                = 'Short\^@',
    XtRString       = 'String\^@',
    XtRStringTable      = 'StringTable\^@',
    XtRUnsignedChar         = 'UnsignedChar\^@',
    XtRTranslationTable     = 'TranslationTable\^@',
    XtRWindow       = 'Window\^@',


    /* Boolean enumeration constants */

    XtEoff          = 'off\^@',
    XtEfalse        = 'false\^@',
    XtEno           = 'no\^@',
    XtEon           = 'on\^@',
    XtEtrue         = 'true\^@',
    XtEyes          = 'yes\^@',

    /* Orientation enumeration constants */

    XtEvertical     = 'vertical\^@',
    XtEhorizontal       = 'horizontal\^@',

    /* text edit enumeration constants */

    XtEtextRead     = 'read\^@',
    XtEtextAppend       = 'append\^@',
    XtEtextEdit     = 'edit\^@',

    /* color enumeration constants */

    XtExtdefaultbackground  = 'xtdefaultbackground\^@',
    XtExtdefaultforeground  = 'xtdefaultforeground\^@',

    /* font constant */

    XtExtdefaultfont    = 'xtdefaultfont\^@',

);

INCLUDE_constant StringDefs = true;
