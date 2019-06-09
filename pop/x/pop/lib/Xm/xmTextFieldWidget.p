/* --- Copyright University of Sussex 1999. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xm/xmTextFieldWidget.p
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

XptLoadWidgetClass xmTextFieldWidget [^^XM_EXLIBS]
    xmTextFieldWidget   <- xmTextFieldWidgetClass,
;

define XmIsTextField =
    XtIsSubclass(% xmTextFieldWidget %)
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
    XmTextFieldInsert(widget,position,value) :void,
    XmTextFieldReplace(widget,from_pos,to_pos,value) :void,
    XmTextFieldSetString(widget,value) :void,
;

;;; the rest are OK as they come
XptPopLoadProcedures ''
    XmCreateTextField(parent,name,arglist,argcount) :XptWidget,
    XmTextFieldClearSelection(widget,time) :void,
    XmTextFieldCopy(widget,time) :XptBoolean,
    XmTextFieldCut(widget,time) :XptBoolean,
;;; Now one that doesn't work with lesstif
#_IF not(DEF LINUX)
    XmTextFieldGetAddMode(widget) :XptBoolean,
#_ENDIF
    XmTextFieldGetBaseLine(widget) :int,
    XmTextFieldGetBaseline(widget) :int,
    XmTextFieldGetCursorPosition(widget) :XmTextPosition,
    XmTextFieldGetEditable(widget) :XptBoolean,
    XmTextFieldGetInsertionPosition(widget) :XmTextPosition,
    XmTextFieldGetLastPosition(widget) :XmTextPosition,
    XmTextFieldGetMaxLength(widget) :int,
    XmTextFieldGetSelection(widget) :CopyString,
    XmTextFieldGetSelectionPosition(widget,left,right) :XptBoolean,
    XmTextFieldGetString(widget) :CopyString,
    XmTextFieldPaste(widget) :XptBoolean,
    XmTextFieldPosToXY(widget,position,x,y) :XptBoolean,
    XmTextFieldRemove(widget) :XptBoolean,
    XmTextFieldSetAddMode(widget,state) :void,
    XmTextFieldSetCursorPosition(widget,position) :void,
    XmTextFieldSetEditable(widget,editable) :void,
    XmTextFieldSetHighlight(widget,left,right,mode) :void,
    XmTextFieldSetInsertionPosition(widget,position) :void,
    XmTextFieldSetMaxLength(widget,max_length) :void,
    XmTextFieldSetSelection(widget,first,last,time) :void,
    XmTextFieldShowPosition(widget,position) :void,
    XmTextFieldXYToPos(widget,x,y) :XmTextPosition,
#_IF XM_VERSION >= 1002
    XmTextFieldGetSubstring(widget,start,num_chars,buffer_size,buffer) :int,
#_ENDIF

;


    /*  Encoding wrappers for functions which take a string argument.
        NB: previous versions allowed anything as the value -- e.g. an
        external pointer -- so we maintain that here by encoding only
        when the value is known to be a string
    */
define XmTextFieldInsert(widget, position, value);
    if isstring(value) then sys_encode_string(value) -> value endif;
    exacc[fast] raw_XmTextFieldInsert(widget, position, value);
enddefine;

define XmTextFieldReplace(widget, from_pos, to_pos, value);
    if isstring(value) then sys_encode_string(value) -> value endif;
    exacc[fast] raw_XmTextFieldReplace(widget, from_pos, to_pos, value);
enddefine;

define XmTextFieldSetString(widget, value);
    if isstring(value) then sys_encode_string(value) -> value endif;
    exacc[fast] raw_XmTextFieldSetString(widget, value);
enddefine;

endexload_batch;
endsection;


/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Nov  6 1999
    On Julian Clinton's advice, prevented compilation of this line
        XmTextFieldGet*AddMode(widget) :XptBoolean,
    on linux, so that it works with lesstif.
--- Robert Duncan, Mar 12 1997
        Changed to do string encoding
--- Robert John Duncan, Jun 23 1995
        Changed XmTextFieldGetString and XmTextFieldGetSelection to free
        the returned string as soon as it's been copied. Added new 1.2
        functions.
 */
