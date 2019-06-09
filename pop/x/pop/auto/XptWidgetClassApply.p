/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptWidgetClassApply.p
 > Purpose:         Class apply for WidgetClass
 > Author:          Jonathan Meyer, Jun 27 1991
 > Documentation:   REF *XPT_CLASSAPPLY
 > Related Files:   LIB *XptWidgetApply LIB *XptScreenPtrApply
 */
compile_mode :pop11 +strict;

section;
exload_batch;

include xpt_coretypes;

define XptWidgetClassApply(name, desc);
    lvars name, desc;
    XptCheckWidgetClass(desc)->;
    if name == "superclass" then
        ;;; 1st field of a widgetclass is its superclass
        exacc [fast] :XptWidgetClass desc;
    elseif name == "name" then
        XptDataProps(desc);
    else
        mishap(name, 1, 'UNKNOWN WidgetClass FIELD');
    endif;
enddefine;

endexload_batch;
endsection;
