/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/src/newproperty.p
 > Purpose:
 > Author:          John Gibson & John Williams (see revisions)
 > Documentation:   REF *PROPS
 > Related Files:   C.all/src/property.p, C.all/src/newanyproperty.p
 */

;;; ------------------ CONSTRUCTING SIMPLE PROPERTIES ---------------------

#_INCLUDE 'declare.ph'

global constant
        procedure (integer_length, Sys$-Prop$-Get),
        _pshift_testovf
    ;

;;; ----------------------------------------------------------------------

section $-Sys$-Prop => newproperty;

    /*  Round the table size for a property to a power of 2
    */
define Round_tabsize(size);
    lvars size;
    if isinteger(size) and size fi_> 0 then
        returnif(_pshift_testovf(1,  _int(integer_length(0 fi_- size)))) ()
    endif;
    mishap(size, 1, 'INVALID TABLE SIZE FOR PROPERTY')
enddefine;

define New_table(/*size*/) with_nargs 1;
    writeable Initv((), 0)
enddefine;

define Get_prop_rec(type, default, _size) -> prop;
    lvars prop, type, default, _size;

    ;;; Get the entry key
    if type == "perm" or type == true then
        perm_prop_entry_key
    elseif type == "tmparg" or not(type) then
        tmparg_prop_entry_key
    elseif type == "tmpval" then
        tmpval_prop_entry_key
    elseif type == "tmpboth" then
        tmpboth_prop_entry_key
    elseif type == "tmpclr" then
        tmpclr_prop_entry_key
    else
        mishap(type, 1, 'INVALID PROPERTY ENTRY TYPE')
    endif -> type;

    Get_store(@@(struct PROP)++) -> prop;
    property_key-> prop!KEY;
    default     -> prop!PT_DEFAULT;
    type        -> prop!PT_ENTRY_KEY;
    false       ->> prop!PT_ACTIVE
                ->> prop!PT_COUNT
                ->> prop!PT_EXPAND
                ->> prop!PT_EQ_PDR
                ->> prop!PT_HASH_PDR
                ->  prop!PT_REHASH;
    ;;; make safe for gc, in case getting table vector causes one
    ;;; (gc assumes it's a vector)
    {}          -> prop!PT_TABLE;
    New_table(_size) -> prop!PT_TABLE
enddefine;

define newproperty(init, size, default, type) -> prop_pdr;
    lvars size, init, default, type, prop, procedure prop_pdr;

    ;;; round table size to power of 2
    Get_prop_rec(type, default, Round_tabsize(size)) -> prop;

    Consclos_protect(Get, prop, 1) -> prop_pdr;
    prop_pdr!PD_FLAGS _biset _:M_PD_CLOS_PROPERTY -> prop_pdr!PD_FLAGS;

    until null(init) do
        fast_destpair(init) -> init -> type;
        hd(tl(type)) -> prop_pdr(fast_front(type))
    enduntil
enddefine;

endsection;     /* $-Sys$-Prop */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Jul 31 1989
        Added "tmpclr" property type
--- John Gibson, Jun 18 1989
        Added -New_table- for getting new writeable property table vec
--- John Gibson, May 24 1989
        Added -Entry_key- to return appropriate key for different perm/temp
        property types specified by keyword arg to -newproperty- and
        -newanyproperty-.
--- John Gibson, Mar 24 1988
        Moved out of props.p
 */
