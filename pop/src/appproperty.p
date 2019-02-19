/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/src/appproperty.p
 > Purpose:
 > Author:          John Gibson & John Williams (see revisions)
 > Documentation:   REF *PROPS
 */

;;; -------- APPLY A PROCEDURE TO EACH ENTRY IN A PROPERTY ---------------

#_INCLUDE 'declare.ph'

global constant
        $-Sys$-Gc$-dead_prop_entry
    ;

section $-Sys$-Prop;

constant
        procedure (Checkr_prop, Count)
    ;

endsection;

;;; ---------------------------------------------------------------------

section $-Sys$-Prop => appproperty;

    /*  Apply a procedure to each entry in a property
        do this with a temporary copy of the property in a vector,
        so the procedure can safely change the original
    */
define appproperty(prop, pdr);
    lvars procedure pdr, tempvec, prop, _cell, _entry, _lim, _vec;
    Checkr_prop(prop) -> prop;
    Check_procedure(pdr);

    ;;; insert all args and values in a vector
    ;;; N.B. the Initv can cause more dead entries to appear in the property!
    Initv(Count(prop) fi_<< 1, 0) -> tempvec;
    tempvec@V_WORDS -> _vec;
    prop!PT_TABLE -> prop;
    prop@V_WORDS -> _cell;
    _cell@(w)[prop!V_LENGTH] -> _lim;
    while _cell <@(w) _lim do
        _cell!(w)++ -> _cell -> _entry;
        while iscompound(_entry) do
            unless _entry!PTE_ARG == $-Sys$-Gc$-dead_prop_entry then
                _entry!PTE_ARG -> _vec!(w)++ -> _vec;
                _entry!PTE_VALUE -> _vec!(w)++ -> _vec
            endunless;
            _entry!PTE_NEXT -> _entry
        endwhile;
    endwhile;

    ;;; apply procedure to each arg/value
    @@(w){_vec, tempvec} -> _lim;       ;;; finishing offset
    @@V_WORDS -> _vec;
    while _vec _lt _lim do
        _CHECKUSER;
        tempvec@(w){_vec} -> _cell;
        pdr(_cell!(w)++ -> _cell, _cell!(w));
        @@(w[2]){_vec}++ -> _vec
    endwhile
enddefine;


    /*  Called from -appdata- on a property
    */
define App_data(prop, p);
    lvars prop, procedure p;
    appproperty(prop,
        procedure(pdr);
            lvars procedure pdr;
            pdr(conspair((), conspair((), [])))
        endprocedure(% p %))
enddefine;

endsection;     /* $-Sys$-Prop */



/* --- Revision History ---------------------------------------------------
--- John Gibson, May 24 1989
        Made -appproperty- check for dead entries
--- John Gibson, Mar 24 1988
        Moved out of appprop.p
 */
