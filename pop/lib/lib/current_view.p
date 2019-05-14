/* --- Copyright University of Sussex 1989.  All rights reserved. ---------
 > File:           C.all/lib/lib/current_view.p
 > Purpose:        viewpoint/context mechanisms with CURRENT view.
 > Author:         Aaron Sloman, Nov 30 1986 (see revisions)
 > Documentation:  TEACH * VIEWS, HELP * VIEWS, *NEWANYPROPERTY, *SYSHASH
 > Related Files:  LIB * VIEWS, * NEWMAPPING
 */

#_TERMIN_IF DEF POPC_COMPILING
compile_mode:pop11 +defpdr +constr;

/*
LIB VIEWS provides a general viewpoint mechanism where a view may have
any number of ancestors and arbitrarily objects may have values in views.

This file extends the mechanism to provide a notion of a current_view
relative to which various actions can be done.

*/

section;
uses views;

;;; PROCEDURES FOR MANIPULATING THE "CURRENT" VIEW
;;; requires gensym

global vars
    use_global_view  = true,        ;;; If true, optimise large databases
                                    ;;; where most items don't change value
    global_view      = false,       ;;; Assigned a value by start_views
    procedure global_altered,       ;;; Ditto

    current_view     = false,       ;;; Updated by push_view and pop_view
    view_stack       = [],
    current_viewname = false,

    chatty;                         ;;; If true messages are printed out

unless isboolean(chatty) then false -> chatty endunless;

define lconstant chatty_printf();
;;; version of printf that does nothing if chatty is false
    dlocal cucharout;
    unless chatty then erase -> cucharout endunless;
    printf();
enddefine;


;;;; PROCEDURES FOR MANIPULATING CURRENT VIEW

define global set_current_view(view,name);
    lvars view,name;
    unless  view==current_view and name ==current_viewname then
        view -> current_view;
        name -> current_viewname;
        ;;; store new current view and its name on view_stack
        conspair(conspair(view,name),view_stack) -> view_stack;
        chatty_printf(name,'Starting new context %p\n');
    endunless;
enddefine;

define global push_view(size);
    ;;; make a new current view, whose parent is the previous one
    ;;; with a new name
    lvars viewname=gensym("view"),size;
    set_current_view(newsubview(current_view,size),viewname);
    ;;; declare the name
    sysVARS(viewname,0);
    current_view -> valof(viewname);
enddefine;

define global pop_view;
    ;;; restore the previous current_view
    lvars viewname;
    if back(view_stack)==[] then
        chatty_printf('No more views on stack\n');
    else
        back(destpair(view_stack) -> view_stack) -> viewname;
        chatty_printf(viewname,'Leaving view %p\n');
        destpair(front(view_stack))->viewname -> current_view;
        chatty_printf(viewname, ' - Resuming view %p\n');
        viewname -> current_viewname;
    endif;
enddefine;


define global start_views(size);
    lvars size;
    ;;; start a new current_view and view_stack
    [] -> view_stack;
    1 -> gensym("view");
    [] -> current_view; ;;; needed for push_view
    push_view(size);
    if use_global_view then
        current_view -> global_view;
        newmapping([],100,0,true) -> global_altered;
    endif;
enddefine;


define global current_view_value(item);
    ;;; get or update value of item in current view, or if not changed
    ;;; then from global view
    lvars item;
    if use_global_view then
        if global_altered(item) == 0 then
            ;;; printf('getting global view\n');
            view_value(global_view,item);
            return()
        endif;
    endif;
    ;;; printf('getting non-global view\n');
    view_value(current_view,item)
enddefine;

define updaterof current_view_value(item);
    lvars item;
    -> view_value(current_view,item);
    if use_global_view then
        global_altered(item) fi_+ 1 -> global_altered(item)
    endif;
enddefine;


/*
to be defined
define merge_current_view(view);
;;; merge the view with the current_view
    lvars view;
enddefine;

*/


;;;; A DATABASE OF FACTS WITH TRUE OR FALSE VALUES IN CURRENT VIEW

;;; The database is called "view_facts". All items given a value in
;;; any view are stored in the database

global vars
    view_facts = [],    ;;; a database

    it=false,           ;;; last database item accessed

    8 view_matches;     ;;; operator - default value is matches

if isundef(nonop view_matches) then nonop matches -> nonop view_matches endif;

define record_in_view(pattern,value);
;;; value is usually either true or false.
    lvars pattern;
    unless fast_member(pattern,view_facts) then
        [^pattern ^^ view_facts] -> view_facts
    endunless;
    value -> current_view_value(pattern);
    pattern -> it;
enddefine;

define true_in_view(pattern)-> result;
    lvars fact,pattern,result;
    fast_for fact in view_facts do
        if fact view_matches pattern
        and current_view_value(fact) == true
        then fact ->> result -> it;
            return
        endif
    endfor;
    false -> result;
enddefine;

define false_in_view(pattern);
    lvars pattern;
    not(true_in_view(pattern))
enddefine;

define view_lookup(pattern);
;;; check if true and bind variables if so
    lvars pattern;
    unless true_in_view(pattern) then
        mishap('PATTERN NOT TRUE IN CURRENT CONTEXT',[^pattern])
    endunless
enddefine;

;;; Now an analogy to FOREACH

/*
    FOR_VIEW <pattern>
    WITH_VALUE <exp>        ;;; optional
    IN_VIEW <view>          ;;; optional
    IN <list>               ;;; optional
    DO actions
    ENDFOR_VIEW

translates roughly to:

    for fact in <list> do
        if fact view_matches <pattern>
        and view_value(<view>,fact) == <exp>
        do
            fact -> it;
            actions
        endif
    endfor

Except that check_fact_val does the conjunctive test.

Default <exp> is TRUE and default <list> is view_facts, and
default <view> is current_view

The optional items can occur in any order.
*/


define lconstant check_fact_val(fact,pattern,val,view);
    lvars fact,pattern,val,view;
    if fact view_matches pattern then
        if view_value(view,fact) == val then
            fact -> it;
            return(true)
        endif
    endif;
    false
enddefine;

global vars syntax (
    endfor_view = pop_undef,
    with_value  = pop_undef,
    in_view     = pop_undef,
);


define global syntax for_view;
    lvars pattvar, factvar,
    ;;; possible variables for optional fields
         factsvar, valvar=false, viewvar=false,
         Startlab, Endlab, item, key,
         no_facts=true,keys= [do with_value in_view in];
    dlocal pop_new_lvar_list;

    sysNEW_LVAR() -> pattvar;
    sysNEW_LVAR() -> factvar;
    sysNEW_LVAR() -> factsvar;
    sysNEW_LABEL() -> Startlab;  pop11_loop_start(Startlab);
    sysNEW_LABEL() -> Endlab;   pop11_loop_end(Endlab);
    pop11_comp_expr_to(keys)->item;
    sysPOP(pattvar);
    until item == "do" do
        delete(item,keys) -> keys;
        pop11_comp_expr_to(keys)->key;
        if item == "with_value" then
            sysNEW_LVAR() -> valvar;
            sysPOP(valvar);
        elseif item == "in_view" then
            sysNEW_LVAR() -> viewvar;
            sysPOP(viewvar)
        elseif item == "in" then
            sysPOP(factsvar);
            false -> no_facts
        else mishap(item,'REPORT SYSTEM ERROR IN FOR_VIEW')
        endif;
        key -> item;
    enduntil;
    if no_facts then
        ;;; haven't read in list
        sysPUSH("view_facts"); sysPOP(factsvar)
    endif;
    sysLABEL(Startlab);
    sysPUSH(factsvar);
    sysCALLQ(null);
    sysIFSO(Endlab);
    sysPUSH(factsvar);
    sysCALLQ(fast_destpair);
    sysPOP(factsvar);
    sysPUSH(pattvar);
    if valvar then sysPUSH(valvar) else sysPUSHQ(true) endif;
    if viewvar then sysPUSH(viewvar) else sysPUSH("current_view") endif;
    sysCALLQ(check_fact_val);
    sysIFNOT(Startlab);
    pop11_comp_stmnt_seq_to([endfor_view endfor {close}])->;
    sysGOTO(Startlab);
    sysLABEL(Endlab);
enddefine;

define get_all_true(pattern);
    lvars pattern;
    [%for_view pattern do it endfor_view%]
enddefine;

endsection;

/*
global views
view_matches

*/

/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 13 1989
        Replaced old sys- compiler procedures with pop11_ ones, etc.
--- Aaron Sloman, Dec  2 1986
    Much revised for speed. Also push_view and start_view now take a size.
    HELP VIEWS REVISED
*/
