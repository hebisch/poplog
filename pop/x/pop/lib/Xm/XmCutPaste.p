/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xm/XmCutPaste.p
 > Purpose:         Clipboard utils
 > Author:          Jonathan Meyer, Feb  9 1991 (see revisions)
 > Documentation:   HELP *MOTIF
 > Related Files:   LIB *MotifWidgetSet
 */
compile_mode :pop11 +strict;

section;

XptPopLoadProcedures XmCutPaste [^^XM_EXLIBS]
    XmClipboardBeginCopy(u,v,w,x,y,z) :int,
    XmClipboardStartCopy(t,u,v,w,x,y,z) :int,
    XmClipboardCopy(s,t,u,v,w,x,y,z) :int,
    XmClipboardEndCopy(x,y,z) :int,
    XmClipboardCancelCopy(x,y,z) :int,
    XmClipboardCopyByName(u,v,w,x,y,z) :int,
    XmClipboardUndoCopy(x,y) :int,
    XmClipboardLock(x,y) :int,
    XmClipboardUnlock(x,y,z) :int,
    XmClipboardStartRetrieve(x,y,z) :int,
    XmClipboardRetrieve(t,u,v,w,x,y,z) :int,
    XmClipboardEndRetrieve(x,y) :int,
    XmClipboardInquireCount(w,x,y,z) :int,
    XmClipboardInquireFormat(u,v,w,x,y,z) :int,
    XmClipboardInquireLength(w,x,y,z) :int,
    XmClipboardInquirePendingItems(v,w,x,y,z) :int,
    XmClipboardRegisterFormat(x,y,z) :int,
    XmClipboardWithdrawFormat(x,y,z) :int,
;

constant XmCutPaste = true;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 16 1993
        typespecs/macros -> XmMisc.ph
--- Andreas Schoter, Jul 15 1991
    Added global constant XmCutPaste for compatibility with uses
 */
