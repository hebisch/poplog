/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/src/property_active.p
 > Purpose:
 > Author:          John Gibson, Mar 24 1988
 > Documentation:   REF *PROPS
 */

;;; ---------- ACCESS/UPDATE THE ACTIVE DEFAULT OF A PROPERTY ------------

#_INCLUDE 'declare.ph'

constant
        procedure (Sys$-Prop$-Checkr_prop)
    ;

;;; ---------------------------------------------------------------------

section $-Sys$-Prop => property_active;

define property_active() with_nargs 1;
    lvars clos;
    if Checkr_prop()!PT_ACTIVE ->> clos then
        clos!PD_CLOS_PDPART
    else
        false
    endif
enddefine;
;;;
define updaterof property_active(active_default, prop_p);
    lvars prop, procedure (clos, active_default, prop_p);
    Checkr_prop(prop_p) -> prop;
    if active_default then
        Check_procedure(active_default);
        unless prop!PT_ACTIVE ->> clos then
            Consclos_protect(identfn, prop_p, 1) ->> clos -> prop!PT_ACTIVE
        endunless;
        active_default -> clos!PD_CLOS_PDPART
    else
        false -> prop!PT_ACTIVE
    endif
enddefine;


endsection;     /* $-Sys$-Prop */
