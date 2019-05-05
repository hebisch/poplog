/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/printcore.p
 > Purpose:         Basic printing utilities
 > Author:          John Williams, Oct 17 1995
 > Documentation:
 > Related Files:   C.all/lisp/src/{print,pp,write}.p
 */

lisp_compile_mode;

section $-lisp;

constant procedure (isboundtoken, lisp_class_print);

global vars                         /* global 'cos used in format_print */
    depth_printed       =   0,
    length_printed      =   0,
    print_circle        =   nil,
    print_length        =   nil,
    print_level         =   nil,
    print_lines         =   nil,
    print_miser_width   =   nil,
    print_pretty        =   true,
    print_readably      =   nil,
    print_right_margin  =   76,
    ;


define print_not_readable(item);
    dlocal print_readably = nil, read_eval = true;
    lisp_error(@PRINT-NOT-READABLE,
               {^@:MESSAGE ^('Print not readable') ^@:INVOLVING ^item})
enddefine;


define macro CHECK_PRINT_READABLY(item);
    [if print_readably /== nil then
        print_not_readable(^item)
     endif;].dl
enddefine;


define macro CHECK_PR_LEVEL;
    [if isinteger(print_level)
     and depth_printed fi_>= print_level
     and print_readably == nil
     then
        cucharout(`#`);
        return
     else
        depth_printed fi_+ 1 -> depth_printed
     endif;].dl
enddefine;


define macro CHECK_PR_LEN;
    [if isinteger(print_length)
     and length_printed fi_>= print_length
     and print_readably == nil
     then
        cucharout(`.`);
        cucharout(`.`);
        cucharout(`.`);
        return
     else
        length_printed fi_+ 1 -> length_printed
     endif;].dl
enddefine;


define pr_pop11_simple(item);
    CHECK_PRINT_READABLY item;
    cucharout(`#`);
    sys_syspr(item)
enddefine;


vars procedure
    print_by_key_hook = erase1_false;
    /* Re-defined when print_circle is non-nil */


define print_by_key(item);
    lvars p;
    returnif(print_by_key_hook(item));
    lisp_class_print(datakey(item)) -> p;
    if p == syspr then
        CHECK_PRINT_READABLY item
    endif;
    fast_apply(item, p)
enddefine;


define lconstant Try_print_object(item);
    lvars p;
    sf_token(@PRINT-OBJECT) -> p;
    if isboundtoken(p)
    and generic_function_p(ft_valof(p) ->> p) then
        lisp_apply(item, standard_output, p, 2, 0);
        true
    else
        false
    endif
enddefine;


define lconstant Try_pprint_dispatch(item);
    lvars p;
    if print_pretty /== nil
    and (pprint_dispatch(item, print_pprint_dispatch) ->> p) then
        lisp_apply(standard_output, item, p, 2, 0);
        true
    else
        false
    endif
enddefine;


define _lisppr(item);           /* All print procedures call this */
    if is_poplog_item(item) then
        unless Try_pprint_dispatch(item) or Try_print_object(item) do
            print_by_key(item)
        endunless
    else
        pr_pop11_simple(item)
    endif
enddefine;


define default_print_object(item, standard_output) -> item;
    dlocal standard_output;
    SET_CUCHAROUT;
    print_by_key(item)
enddefine;


define pprint_any(standard_output, item);
    dlocal standard_output;
    SET_CUCHAROUT;
    unless Try_print_object(item) do
        print_by_key(item)
    endunless
enddefine;


/* Print-circle */

fastprocs for, front, back, destpair, lmember, subscrv;

vars
    pc_phase        =   nil,
    pc_labcounts    =   [],
    pc_lablist      =   [],
    ;


define lconstant Pc_labcount(cuch);
    list_assoc(cuch, pc_labcounts) or 0
enddefine;


define updaterof lconstant Pc_labcount(n, cuch);
    lvars l;
    if (lmember(cuch, pc_labcounts) ->> l) then
        n -> front(back(l))
    else
        acons(cuch, n, pc_labcounts) -> pc_labcounts
    endif
enddefine;


define lconstant Pc_label(item, cuch);
    lvars vec;
    for vec in pc_lablist do
        if subscrv(1, vec) == item and subscrv(2, vec) == cuch then
            return(subscrv(3, vec))
        endif
    endfor;
    false
enddefine;


define updaterof lconstant Pc_label(lab, item, cuch);
    lvars vec;
    for vec in pc_lablist do
        if subscrv(1, vec) == item and subscrv(2, vec) == cuch then
            lab -> subscrv(3, vec);
            return
        endif
    endfor;
    conspair({^item ^cuch ^lab}, pc_lablist) -> pc_lablist
enddefine;


define vars Pc_marked(item, cuch);
    /* Defined as a 2d property in RESET_PRINT_VARS, below */
    false
enddefine;


define lconstant Pc_mark_hook(item);
    /* Return T if item should not be printed */

    lvars cuch;
    if issimple(item)
    or ischaracter(item)
    or (issymbol(item) and symbol_package(item) /== nil) then
        true
    else
        recursive_stream_write_p(standard_output) -> cuch;
        if Pc_marked(item, cuch) then
            unless Pc_label(item, cuch) do
                true -> Pc_label(item, cuch)
            endunless;
            true
        else
            true -> Pc_marked(item, cuch);
            false
        endif
    endif
enddefine;


define lconstant Pc_mark_apply(p);
    dlocal
        pc_phase            =   @:MARK,
        print_by_key_hook   =   Pc_mark_hook,
        print_pretty        =   nil,
        ;

    define dlocal str_output() with_nargs 1;
        ->; erase
    enddefine;

    SET_CUCHAROUT;
    fast_apply(p)
enddefine;


define pc_printed(item, dot);
    lvars cuch, lab;
    recursive_stream_write_p(standard_output) -> cuch;
    if (Pc_label(item, cuch) ->> lab) then
        if lab == true then
            Pc_labcount(cuch) fi_+ 1
                ->> lab ->> Pc_label(item, cuch) -> Pc_labcount(cuch);
            cucharout(`#`);
            sys_syspr(lab);
            cucharout(`=`);
            false
        else
            if pop_true(dot) then
                cucharout(`.`);
                cucharout(`\s`)
            endif;
            cucharout(`#`);
            sys_syspr(lab);
            cucharout(`#`);
            true
        endif
    else
        false
    endif
enddefine;


define lconstant Pc_print_hook() with_nargs 1;
    pc_printed(false)
enddefine;


define lconstant Pc_print_apply(p);
    dlocal
        pc_phase            =   @:PRINT,
        print_by_key_hook   =   Pc_print_hook,
        ;
    fast_apply(p)
enddefine;


define pc_apply(p);
    if pc_phase == nil then
        procedure();
          dlocal
            0 % clear_2d_property(Pc_marked), clear_2d_property(Pc_marked) %,
            pc_labcounts = [],
            pc_lablist = [],
            ;
          Pc_mark_apply(p);
          Pc_print_apply(p);
        endprocedure()
    else
        fast_chain(p)
    endif
enddefine;


define pc_list_tail_hook(list);
    returnunless(ispair(list)) (false);
    if pc_phase == @:MARK then
        Pc_mark_hook(list);
    elseif pc_phase == @:PRINT then
        pc_printed(list, true)
    else
        false
    endif
enddefine;


/* Main print procedure lisp_pr (can be assigned to pr, and used by WRITE) */


define vars lisp_pr(item);
    dlocal depth_printed;
    SET_CUCHAROUT;
    if print_circle /== nil then
        pc_apply(_lisppr(% item  %))
    else
        _lisppr(item)
    endif
enddefine;


constant macro RESET_PRINT_VARS =
    [dlocal
        depth_printed       =   0,
        length_printed      =   0,
        pr                  =   lisp_pr,
        print_by_key_hook   =   erase1_false,
        pc_phase            =   nil,
        Pc_marked           =   new_2d_property(64, false),
        pp_states           =   [],
        pp_streams          =   [],
        pp_top_level        =   true,
        str_output          =   stream_write_p,
        ;
    ];


endsection;
