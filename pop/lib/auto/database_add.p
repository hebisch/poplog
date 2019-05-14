/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/auto/database_add.p
 > Purpose:         Add an item to the database
 > Author:          John Gibson, Dec 28 1995
 > Documentation:   REF * DATABASE
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

define database_add(item);
    dlvars item;

    define lconstant check_no_matchvars(x) with_props false;
        lvars key = datakey(x);
        if key == matchvar_key then
            mishap(item, 1, 'ADDING UNDER-SPECIFIED ITEM TO DATABASE')
        elseif class_field_spec(key) then
            if key == pair_key then
                check_no_matchvars(fast_front(x));
                chain(fast_back(x), check_no_matchvars)
            else
                chain(x, check_no_matchvars, appdata)
            endif
        endif
    enddefine;

    check_no_matchvars(item);
    item -> database_it;
    item :: database -> database
enddefine;

endsection;
