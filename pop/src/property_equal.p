/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/src/property_equal.p
 > Purpose:         Equivalence test for properties. Needed for Common Lisp.
 > Author:          John Williams, Dec 22 1993
 > Documentation:   REF * PROPS
 > Related Files:   C.all/src/property_misc.p
 */

#_INCLUDE 'declare.ph'

constant
    procedure (Sys$-Prop$-Checkr_prop, Sys$-Prop$-Count,
                Prop_entry_state_init, Prop_entry_state_next,
                appproperty, fast_get_prop_entry, isproperty)
    ;


/* Two properties are equal if:

    (a) Both use the same equality procedure (PT_EQ_PDR).

    (b) Both have the same number of entries.

    (c) For every entry in one property, there is a corresponding entry
        in the other, and the PTE_VALUEs of these entries are equal as
        judged by the property equality procedure PT_EQ_PDR.
*/


section $-Sys$-Prop => property_equal;


define property_equal(prop1, prop2);
    lvars prop1, p1, p2, s, e1, e2;
    dlvars val_eq_p = false, prop2;

    unless isproperty(prop2) do
        /* Assume user supplied val_eq_p */
        Check_procedure(prop2);
        prop2 -> val_eq_p;
        prop1 -> prop2;
        -> prop1
    endunless;

    Checkr_prop(prop1) -> p1;
    Checkr_prop(prop2) -> p2;

    unless p1!PT_EQ_PDR == p2!PT_EQ_PDR then
        return(false)
    endunless;

    unless Count(p1) == Count(p2) then
        return(false)
    endunless;

    unless val_eq_p do
        p1!PT_EQ_PDR or nonop == -> val_eq_p
    endunless;

    if p1!PT_REHASH or p2!PT_REHASH then
        /* if GC occurs, property will be rehashed,
            so need to use appproperty
        */
        appproperty(
            prop1,
            procedure(i, v);
                lvars i, v, e;
                unless (fast_get_prop_entry(i, prop2) ->> e)
                and fast_apply(v, e!PTE_VALUE, val_eq_p) then
                    exitfrom(false, property_equal)
                endunless
            endprocedure);
            true
    else
        Prop_entry_state_init(prop1) -> s;
        while (Prop_entry_state_next(s) ->> e1) do
            unless (fast_get_prop_entry(e1!PTE_ARG, prop2) ->> e2)
            and fast_apply(e1!PTE_VALUE, e2!PTE_VALUE, val_eq_p) then
                return(false)
            endunless
        endwhile;
        true
    endif
enddefine;


endsection;
