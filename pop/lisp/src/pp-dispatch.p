/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/pp-dispatch.p
 > Purpose:         Pretty print dispatch tables
 > Author:          John Williams, Oct 16 1995
 > Documentation:
 > Related Files:
 */

lisp_compile_mode;

section $-lisp;

fastprocs subscrv, front, back;


/* Pprint dispatch tables:

List of three element vectors, of the form:

    {^type_specifier ^function ^priority}

First element of list is a label for the table, which ensures there
is always a pair for set_pprint_dispatch to modify.

Priorities of functions defined by the system are represented by complex
numbers.

*/


vars
    sys_pprint_dispatch_table   =   [% genstring('PPRINT-DISPATCH-TABLE-') %],
    print_pprint_dispatch       =   sys_pprint_dispatch_table,
    ;


define copy_pprint_dispatch(table) -> table;
    defaults table print_pprint_dispatch;
    if table == nil then
        sys_pprint_dispatch_table -> table
    else
        ;;; CHECK TABLE
    endif;
    copy_list(table) -> table;
    genstring('PPRINT-DISPATCH-TABLE-') -> front(table)
enddefine;


define lconstant Pprint_dispatch_before(v, w);
    subscrv(3, v) -> v;
    subscrv(3, w) -> w;
    if iscomplex(v) then
        if iscomplex(w) then
            lconstant IMAGPART = fast_back;
            IMAGPART(v) >= IMAGPART(w)
        else
            ;;; system priority v always loses to user priority w
            false
        endif
    elseif iscomplex(w) then
        ;;; user priority v always beats system priority w
        true
    else
        v >= w
    endif
enddefine;


define pprint_dispatch(object, table);
    lvars v, list = [];
    if table == nil then
        sys_pprint_dispatch_table -> table
    endif;
    for v in_cl_list back(table) do
        if typep(object, subscrv(1, v)) then
            insert(v, list, Pprint_dispatch_before) -> list
        endif
    endfor;
    ispair(list) and checkr_function(subscrv(2, front(list)))
enddefine;


define set_pprint_dispatch(type, fn, priority, table);
    lvars l, v;
    defaults priority 0, table print_pprint_dispatch;

    if table == nil then
        sys_pprint_dispatch_table -> table
    else
        ;;; CHECK TABLE
    endif;
    if lisp_system_building then
        0 +: priority -> priority
    elseunless isreal(priority) do
        lisp_error('Priority must be a real number', [^priority])
    endif;
    if fn == nil then
        ncdelete_if(back(table),
                    procedure(v); subscrv(1, v) == type endprocedure)
            -> back(table)
    else
        {^type ^fn ^priority} -> v;
        back(table) -> l;
        until endp(l) do
            if equal(subscrv(1, front(l)), type) then
                v -> front(l);
                return(nil)
            endif;
            back(l) -> l
        enduntil;
        conspair(v, back(table)) -> back(table)
    endif;
    nil
enddefine;


endsection;
