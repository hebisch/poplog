/* --- Copyright University of Sussex 1999. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xm/xmTextWidget.p
 > Purpose:         Motif widgetclass
 > Author:          John Gibson, Apr 14 1993 (see revisions)
 > Documentation:   HELP * MOTIF
 > Related Files:   Xm/xm*Widget.p etc
 */
compile_mode :pop11 +strict;

section;

include sysdefs

exload_batch;

include xpt_coretypes.ph;
include XmConstants.ph;

uses Xmgeneral;

XptLoadWidgetClass xmTextWidget [^^XM_EXLIBS]
    xmTextWidget    <- xmTextWidgetClass,
;

define XmIsText =
    XtIsSubclass(% xmTextWidget %)
enddefine;

;;; strings returned from the Text widget are copies, so we must free
;;; them after copying the contents
define lconstant CopyString(exptr) -> string;
    if is_null_external_ptr(exptr) then
        ;;; may be for XmTextGetSelection
        false -> string;
    else
        exacc_ntstring(exptr) -> string;
        fast_XtFree(exptr);
    endif;
enddefine;
;;;
l_typespec CopyString :exptr.CopyString;

;;; these functions take strings as input and so need auto-encoding
XptLoadProcedures ''
lvars
    XmTextInsert(widget,position,value) :void,
    XmTextReplace(widget,from_pos,to_pos,value) :void,
    XmTextSetString(widget,value) :void,
#_IF XM_VERSION >= 1002
    XmTextFindString(widget,start,string,direction,position) :XptBoolean,
#_ENDIF
;

;;; the rest are OK as they come
XptPopLoadProcedures ''
    XmCreateScrolledText(parent,name,arglist,argcount) :XptWidget,
    XmCreateText(parent,name,arglist,argcount) :XptWidget,
    XmTextClearSelection(widget,time) :void,
    XmTextCopy(widget,time) :XptBoolean,
    XmTextCut(widget,time) :XptBoolean,
;;; Now one that doesn't work with lesstif
#_IF not( DEF LINUX )
    XmTextGetAddMode(widget) :XptBoolean,
#_ENDIF
    XmTextGetBaseLine(widget) :int,
    XmTextGetBaseline(widget) :int,
    XmTextGetCursorPosition(widget) :XmTextPosition,
    XmTextGetEditable(widget) :XptBoolean,
    XmTextGetInsertionPosition(widget) :XmTextPosition,
    XmTextGetLastPosition(widget) :XmTextPosition,
    XmTextGetMaxLength(widget) :int,
    XmTextGetSelection(widget) :CopyString,
    XmTextGetSelectionPosition(widget,left,right) :XptBoolean,
    XmTextGetSource(widget) :XmTextSource,
    XmTextGetString(widget) :CopyString,
    XmTextGetTopCharacter(widget) :XmTextPosition,
    XmTextPaste(widget) :XptBoolean,
    XmTextPosToXY(widget,position,x,y) :XptBoolean,
    XmTextRemove(widget) :XptBoolean,
    XmTextScroll(widget,lines) :void,
    XmTextSetAddMode(widget,state) :void,
    XmTextSetCursorPosition(widget,position) :void,
    XmTextSetEditable(widget,editable) :void,
    XmTextSetHighlight(widget,left,right,mode) :void,
    XmTextSetInsertionPosition(widget,position) :void,
    XmTextSetMaxLength(widget,length) :void,
    XmTextSetSelection(widget,first,last,time) :void,
    XmTextSetSource(widget,source,top_character,cursor_position) :void,
    XmTextSetTopCharacter(widget,top_character) :void,
    XmTextShowPosition(widget,position) :void,
    XmTextXYToPos(widget,x,y) :XmTextPosition,
#_IF XM_VERSION >= 1002
    XmTextDisableRedisplay(widget) :void,
    XmTextEnableRedisplay(widget) :void,
    XmTextGetSubstring(widget,start,num_chars,buffer_size,buffer) :int,
#_ENDIF
;


    /*  Encoding wrappers for functions which take a string argument.
        NB: previous versions allowed anything as the value -- e.g. an
        external pointer -- so we maintain that here by encoding only
        when the value is known to be a string
    */
define XmTextInsert(widget, position, value);
    if isstring(value) then sys_encode_string(value) -> value endif;
    exacc[fast] raw_XmTextInsert(widget, position, sys_encode_string(value));
enddefine;

define XmTextReplace(widget, from_pos, to_pos, value);
    if isstring(value) then sys_encode_string(value) -> value endif;
    exacc[fast] raw_XmTextReplace(widget, from_pos, to_pos, value);
enddefine;

define XmTextSetString(widget, value);
    if isstring(value) then sys_encode_string(value) -> value endif;
    exacc[fast] raw_XmTextSetString(widget, value);
enddefine;

#_IF DEF raw_XmTextFindString
define XmTextFindString(widget, start, string, direction, position);
    if isstring(string) then sys_encode_string(string) -> string endif;
    exacc[fast] raw_XmTextFindString(widget, start, string, direction,
        position);
enddefine;
#_ENDIF

endexload_batch;
endsection;


/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Nov  6 1999
    On Julian Clinton's advice, prevented compilation of this line
        XmTextGet*AddMode(widget) :XptBoolean,
    on linux, so that it works with lesstif.
--- Robert Duncan, Mar 12 1997
        Changed to do string encoding
--- Robert John Duncan, Jun 23 1995
        Changed XmTextGetString and XmTextGetSelection to free the returned
        string as soon as it's been copied. Added new 1.2 functions.
 */
