/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/widgetclass_key.p
 > Purpose:         Key for widgetclasses
 > Author:          John Gibson, Apr  5 1993
 */
compile_mode :pop11 +strict;

section;

lblock;

defclass lconstant widgetclass [external_ptr writeable]
    {   wc_props,
    >-> wc_ptr:exptr,
        wc_descriptor,
        wc_widgetset,
    };


#_IF DEF XptDescriptor_key or DEF POPC_COMPILING

XptDescriptor_apply -> class_apply(widgetclass_key);

procedure(wc);
    lvars wc;
    cucharout(`<`);
    if wc.wc_widgetset then spr(wc.wc_widgetset); endif;
    unless is_valid_external_ptr(wc) then
        appdata('(NULL)WidgetClass ', cucharout)
    else
        appdata('WidgetClass ', cucharout)
    endunless;
    pr(XptDataProps(wc));
    cucharout(`>`);
endprocedure -> class_print(widgetclass_key);

define XptPopImportWidgetClassPtr(exptr, wc);
    lvars exptr, wc, wcdesc;
    ;;; import the widget class from exptr,
    ;;; then set the -wc- descriptor field and pointer field for widget class,
    ;;; then register -wc- as the prefered representation for the widget class

    XptImportWidgetClassPtr(exptr) ->> wcdesc
                                    ->> wc.wc_descriptor -> wc.wc_ptr;
    wc -> XptRegister(wcdesc);
enddefine;

#_ENDIF


widgetclass_key

endlblock;

constant widgetclass_key = ();

endsection;
