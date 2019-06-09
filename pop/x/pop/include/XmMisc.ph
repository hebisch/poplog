/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/include/XmMisc.ph
 > Purpose:         Miscellaneous Xm definitions
 > Author:          Jonathan Meyer, Feb  9 1991 (see revisions)
 > Documentation:   HELP *MOTIF
 */

#_TERMIN_IF DEF XMMISC_INCLUDED

include xpt_xtypes.ph;

section;

/* Clipboard */

iconstant macro (
        ClipboardFail       = 0,
        ClipboardSuccess    = 1,
        ClipboardTruncate   = 2,
        ClipboardLocked     = 4,
        ClipboardBadFormat  = 5,
        ClipboardNoData     = 6,
);

i_typespec
    XmClipboardPendingRec {
        DataId: int,
        PrivateId: int
    },
    XmClipboardPendingList :exptr.:XmClipboardPendingRec,
;


/* New fields for the vendor shell widget. */

i_typespec
    XmFocusMovedCallbackStruct {
        reason: int,
        event: XptXEventPtr,
        cont: XptBoolean,
        old: XptWidget,
        new: XptWidget,
        focus_policy: byte,
    },
    XmFocusMovedCallback: exptr.:XmFocusMovedCallbackStruct,
;

;;; typedef struct _XmFocusDataRec *XmFocusData;

iconstant XMMISC_INCLUDED = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 16 1993
        typespec/macros -> this file from XmCutPaste/XmTraversal.p
--- Andreas Schoter, Jul 15 1991
    Added global constant XmCutPaste for compatibility with uses
 */
