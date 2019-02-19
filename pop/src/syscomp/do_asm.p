/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/syscomp/do_asm.p
 > Purpose:
 > Author:          John Gibson, May 27 1989 (see revisions)
 */

/* -------------------------------------------------------------------------

                    'COMPILING' ASSEMBLER FILES

--------------------------------------------------------------------------*/

#_INCLUDE 'common.ph'

section $-Popas;

vars procedure $-popc_after_compile_hook = identfn;

constant
        procedure (gen_perm_inits, autodef_int)
    ;


define popc_compile_files(fname_list, pr_fname, include_list, other);
    lvars f, i, fname_list, include_list, other, procedure pr_fname;
    dlocal pop_default_type = '.p', proglist_state;
    for f in fname_list do
        pr_fname(f);
        proglist_new_state(f) -> proglist_state;
        other <> proglist -> proglist;
        for i in rev(include_list) do
            "#_INCLUDE" :: (i :: proglist) -> proglist
        endfor;
        pop11_comp_stream();
        if stacklength() /== 0 then
            mishap(stacklength(), 'ITEMS LEFT ON STACK AFTER COMPILING FILE')
        endif
    endfor;

    ;;; call hook procedure after compiling file(s)
    chain(popc_after_compile_hook)
enddefine;

define read_paren_path(allow_upd);
    lvars item, allow_upd, upd = false;
    dlocal pop_autoload = false;
    if (itemread() ->> item) /== "(" then
        mishap(item, 1, '( EXPECTED BEFORE IDENTIFIER PATHNAME')
    endif;
    if (itemread() ->> item) == "->" then
        ;;; updater
        if allow_upd then
            true -> upd;
            itemread() -> item
        else
            mishap(0, 'INVALID CONTEXT FOR -> BEFORE IDENTIFIER PATHNAME')
        endif
    endif;
    if allow_upd then upd endif;
    if item == "weakref" then
        procedure();
            dlocal pop_expr_inst, pop_expr_item;
            valof("weakref")();
            pop_expr_item   ;;; a weakref pair
        endprocedure()
    else
        sys_read_path(item, false, false)
    endif;
    if (itemread() ->> item) /== ")" then
        mishap(item, 1, ') EXPECTED AFTER IDENTIFIER PATHNAME')
    endif
enddefine;      /* read_paren_path */

lvars procedure (
    read_input_paren_path   = read_paren_path,
    read_input_item         = itemread,
    );

define read_input_path_token();
    note_perm_ident(get_perm_ident(read_input_paren_path(false)),
                                                        IDT_GEN_IDENT)
enddefine;


;;; --- MACRO-PROCESSING ASSEMBLER FILES ----------------------------------

define lconstant C_LAB -> lab;
    lvars lab;
    if perm_const_lab(read_input_paren_path(true)) -> lab then
        updlabof(lab, true) -> lab
    endif
enddefine;

define lconstant I_LAB;
    identlabel(read_input_path_token())
enddefine;

define lconstant XC_LAB;
    execlabof(C_LAB(), true)
enddefine;

define lconstant def_lab(assign);
    lvars assign, item = read_input_item(), idprops = 0, word;

    if item == "procedure" or isinteger(item) then
        item -> idprops
    else
        item :: proglist -> proglist
    endif;
    if (read_input_paren_path(true) -> word) then
        ;;; updater
        updlabof(perm_const_lab(word), true)
    else
        sysSYNTAX(word, idprops, true);
        sysGLOBAL(word);
        ;;; initialise it to its own undef
        sys_current_val(word) -> sys_current_val(word);
        perm_const_lab(word)
    endif;
    if assign then
        unless (read_input_item() ->> item) == "=" then
            mishap(item, 1, '"=" EXPECTED BEFORE LABEL VALUE')
        endunless;
        read_input_item() -> item;
        if isinteger(item) then label_of(item, false) -> item endif;
        asm_outglabset((), item)
    else
        asm_outglab()
    endif
enddefine;

lconstant procedure
    pseudo_macs = newassoc([
        [C_LAB          ^ C_LAB]
        [I_LAB          ^ I_LAB]
        [XC_LAB         ^ XC_LAB]
        [DEF_C_LAB      % def_lab(% false %) %]
        [SET_C_LAB      % def_lab(% true %)  %]
    ]),

    Sys_Incharitem = pdpart(incharitem(identfn)),
;


define do_asm(fname_list, a_name, w_name, pr_fname, include_list);
    lvars   f, fname_list, a_name, w_name, include_list, procedure pr_fname;
    dlvars  asm_popautolist = [^autodef_int],
            pop_popautolist = popautolist;  ;;; already contains autodef_int

    define lconstant asm_trans();
        lvars   item, escape_stack = [], macp;
        dlvars  asm_table, pop_table, curr_table;
        dlocal  popnewline;

        lconstant
            ALPHA   = item_chartype(`a`),
            SEP     = item_chartype(`,`),
            WHITE   = item_newtype(),
            hash_include = '#_INCLUDE',
            ;

        define dlocal read_input_paren_path();
            dlocal popnewline = true, curr_table = pop_table;
            read_paren_path()
        enddefine;

        define dlocal read_input_item();
            dlocal curr_table = pop_table;
            itemread()
        enddefine;

        define lconstant asm_itemiser(list_ref) -> item;
            lvars item, list, t, list_ref;
            lconstant COND = 1, EVAL = 2;

            define lconstant compile_mode(escape);
                lvars escape;
                escape :: [] -> escape_stack;
                pop_table -> curr_table;
                pop_popautolist -> popautolist
            enddefine;

            fast_cont(list_ref) -> list;
            returnif(null(list)) (termin -> item);

            dest(list) -> list -> item;
            if escape_stack /== [] then
                if item == "#_IF" or item == "#_ELSEIF" then
                    COND :: escape_stack -> escape_stack
                elseif item == "#_<" then
                    EVAL :: escape_stack -> escape_stack
                elseif (item == "\n" and hd(escape_stack) == COND)
                or (item == ">_#" and hd(escape_stack) == EVAL)
                then
                    if (tl(escape_stack) ->> escape_stack) == [] then
                        asm_table -> curr_table;
                        asm_popautolist -> popautolist;
                        false ->> popnewline
                                -> caller_valof("popnewline", asm_trans)
                    endif
                endif
            elseif item == "#" and not(null(list)) then
                dest(list) -> t -> item;
                if item == "_ELSE" or item == "_ENDIF" then
                    "#" <> item -> item, t -> list
                elseif item == "_IF" or item == "_ELSEIF" then
                    compile_mode(COND);
                    true -> popnewline;
                    "#" <> item -> item, t -> list
                elseif item == "_INCLUDE" then
                    hash_include -> item, t -> list
                elseif item == "_" and not(null(t)) and hd(t) == "<" then
                    compile_mode(EVAL);
                    "#_<" -> item, tl(t) -> list
                else
                    "#" -> item
                endif
            elseif item == "_" and not(null(list))
            and hd(list) == ":" then
                "_:" -> item;
                tl(list) -> list
            endif;

            list -> fast_cont(list_ref)
        enddefine;      /* asm_itemiser */

        define lconstant set_asm_proglist(init);
            lvars c, itrep, init;

            define lconstant Inchar();
                chain((/*stream*/), curr_table, false,
#_IF pdnargs(Sys_Incharitem) == 4
                        false,
#_ENDIF
                        Sys_Incharitem)
            enddefine;

            ;;; get incharitem closure
            back(proglist) -> itrep;
            Inchar(% frozval(1, itrep) %) -> back(proglist);
            pdtolist(asm_itemiser(% consref(proglist) %)) -> proglist;
            returnunless(init);
            ;;; set up chartypes for reading assembler file
            12 -> item_chartype(`\\`, itrep);   ;;; \ alphabeticiser
            copy(frozval(2,itrep) ->> pop_table) -> frozval(2,itrep);
            for c in [`.` `_` `0` `1` `2` `3` `4` `5` `6` `7` `8` `9`] do
                ALPHA -> item_chartype(c, itrep)
            endfor;
            for c in [`'` `"` `\n`] do
                SEP -> item_chartype(c, itrep)
            endfor;
            WHITE ->> item_chartype(`\s`, itrep) -> item_chartype(`\t`, itrep);
            frozval(2, itrep) ->> asm_table -> curr_table
        enddefine;

        set_asm_proglist(true);

        until (itemread() ->> item) == termin do
            if item == hash_include then
                nonmac #_INCLUDE(read_input_item());
                set_asm_proglist(false);
                nextloop
            elseif isword(item) and (pseudo_macs(item) ->> macp) then
                [% macp() %] nc_<> proglist -> proglist;
                nextloop
            endif;
            if isinteger(item) then label_of(item, false) -> item endif;
            if isword(item) or isstring(item) then
                appdata(item, asmf_charout)
            else
                mishap(item, 1, 'INVALID ITEM FOR ASSEMBLER OUTPUT')
            endif
        enduntil
    enddefine;      /* asm_trans */

    dlocal  popautolist = asm_popautolist, asmf_charout,
            %syspop_mode, set_syspop_mode()% = true;

    discout(a_name) -> asmf_charout;
    add_created_file(a_name);

    popc_compile_files(fname_list, pr_fname, include_list, [^asm_trans();]);

    asmf_charout(termin);
    ;;; output assignments
    gen_perm_inits(w_name, true)
enddefine;      /* do_asm */


endsection;     /* $-Popas */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 29 1997
        Fixed Inchar to work with new Sys$-Incharitem having 4 frozvals
        instead of 3.
--- John Gibson, Oct  2 1995
        Added popc_after_compile_hook
--- John Gibson, Apr 18 1995
        Made `.` have item_chartype alpha in assembler itemiser
--- John Gibson, Oct 21 1994
        Added ASSIGN_C_LAB
--- Robert John Duncan, Apr 12 1994
        Fixed use of `\\` as an alphabeticiser in assembly code files
--- John Gibson, May 19 1993
        Made EXTERN_NAME a real macro in syspop.p
--- John Gibson, Oct 14 1992
        14.22 changes
--- Robert John Duncan, Jul 27 1992
        Added -EXTERN_NAME-
--- Andreas Schoter, Sep  9 1991
        Changed occurrances of -popliblist- to -popautolist-
--- John Gibson, Aug 18 1989
        Changed way assembler files are #_INCLUDED. Now deals with
        a group of files to produce one object module.
--- John Gibson, Aug  4 1989
        Version 13.66+
--- John Gibson, Jul 17 1989
        Version 13.66
 */
