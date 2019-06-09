/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.x/x/src/xt_widgetclass.p
 > Purpose:         creating widgetclass descriptors
 > Author:          Roger Evans, Jun 22 1990 (see revisions)
 > Documentation:   REF XTOOLKIT
 > Related Files:   xt_*.p
 */

#_INCLUDE 'xt_declare.ph'


;;; --------------------------------------------------------------------

section $-Sys$-Xt  =>
                        fast_XtIsShell,
                        XptImportWidgetClass,
                        XptImportWidgetClassPtr,
;

/*  Utilities for creating widget class descriptors */

;;; Create a basic widget_class descriptor
define lconstant Cons_WidgetClass_rec(_ptr,super) -> wc;
    lvars wc _ptr super;

    _ptr!XRWC_class_name -> $-Sys$-Sys_external_ptr!XP_PTR;
    Cons_XptDescriptor( _ptr,
                        XDT_WIDGETCLASS,
                        exacc_ntstring($-Sys$-Sys_external_ptr),
                        false,
                       ) -> wc;

    ;;; instantiate superclass in DEPENDENTS slot
    super -> wc!XD_DEPENDENTS;
enddefine;

;;; import a widgetclass descriptor
define ImportWidgetClassDesc(_ptr);
    lvars _ptr;
    Descriptor(XDT_WIDGETCLASS,_ptr) or
        Cons_WidgetClass_rec(_ptr,
            if _ptr!XRWC_superclass == _NULL then
                false
            else
                ImportWidgetClassDesc(_ptr!XRWC_superclass);
            endif);
enddefine;

;;; public implicit access routine
define XptImportWidgetClass(_ptr);
    lvars _ptr;
    Checkr_exptrclass_ptr(_ptr) -> _ptr;
    if _ptr == _NULL then
        false
    else
        Register(ImportWidgetClassDesc(_ptr));
    endif;
enddefine;

define updaterof XptImportWidgetClass(_ptr) -> _ptr;
    lvars _ptr;
    if _ptr then
        unless XptDataType(_ptr) == XDT_WIDGETCLASS then
            mishap(_ptr,1,'WidgetClass NEEDED');
        endunless;
    else
        null_external_ptr -> _ptr;
    endif;
enddefine;

;;; importing widgetclasses using external_load - extra indirection needed
define XptImportWidgetClassPtr(_ptr);
    lvars _ptr;
    Checkr_exptrclass_ptr(_ptr)!(w) -> _ptr;
    Register(ImportWidgetClassDesc(_ptr));
enddefine;

define fast_XtIsShell(w);
    ;;; XtIsShell returns Boolean (byte) result
    X_apply(w, _1, _extern XtIsShell) _bitst _16:FF;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Nov 10 1997
        Added fast_XtIsShell with proper test for byte result
--- Roger Evans, Jun 28 1991 altered for freelists
--- Roger Evans, Oct 11 1990 Much revised
--- Roger Evans, Jul  4 1990 changed to XptImportWidgetClass
--- Roger Evans, Jun 25 1990
    changed to use constant external_ptr record
--- Roger Evans, Jun 23 1990
    changed external_ptr_to_widgetclass to XptImportWidgetclass
    exported Cons_WidgetClass_rec (for system use)
 */
