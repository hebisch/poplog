/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/lib/lib/debugger/core.p
 > Purpose:         Symbolic debugger: core functionality
 > Author:          Simon Nichols and Robert John Duncan, Jun 18 1991 (see revisions)
 > Documentation:   HELP * DEBUGGER
 > Related Files:   debugger/user.p
 */

compile_mode:pop11 +strict;

section $-debugger =>
    debugger_debugging
;

constant
    debugger_section = current_section,
;

vars
    stepping    = false,
    animating   = false,
;

global vars
    debugger_debugging = false,
        ;;; possible values: <true>, <false>, "compile" or "execute"
;


/* The following are defined in debugger/user.p */

constant procedure (
    sys_debugger,
    debugger_interrupt,
);

vars procedure (
    pop_debugger,
);


/* Get the word identifier of -word- in -debugger_section- */

define macro WID word;
    lvars word;
    [word_identifier("^word", debugger_section, false)].dl;
enddefine;


/* Association list procedures */

define lconstant cons_assoc() with_nargs 3;
    conspair(conspair());
enddefine;

lconstant procedure list_assoc = $-lisp$-list_assoc;

define lconstant del_assoc(val, assoc_list);
    lvars val, assoc_list, list = assoc_list, last = false;
    repeat
        returnif(list == [])(false, assoc_list);
        fast_back(list) -> list;
        quitif(fast_front(list) == val);
        fast_back(list ->> last) -> list;
    endrepeat;
    if last then
        fast_back(list) -> fast_back(last);
        (true, assoc_list);
    else
        (true, fast_back(list));
    endif;
enddefine;


/* PDBR (procedure debugging record) */

defclass pdbr
{
    pdbr_name,
    pdbr_ident,
    pdbr_file,
    pdbr_lconstants,
    pdbr_ignore,
    pdbr_checkpoints,
    pdbr_breakpoints,
};


/* FDBR (file debugging record) */

defclass fdbr
{
    fdbr_name,
    fdbr_lexicals,
    fdbr_pdbrs,
};


/* File table: maps file names to FDBRs */

define file_table =
    newmapping([], 16, false, true);
enddefine;


/* Given a file name and a procedure name, return the corresponding PDBR */

define get_pdbr(name, file) -> pdbr;
    lvars file, fdbr, name, names, status, id = false, pdbr = false;

    define lconstant Get_pdbr(name, names, id, pdbrs);
        lvars pdbr, name, names, id, pdbrs;
        until pdbrs == [] do
            fast_destpair(pdbrs) -> (pdbr, pdbrs);
            if ispdbr(pdbr) and pdbr_name(pdbr) == name     ;;; same name
            and (not(id) or id == pdbr_ident(pdbr))         ;;; same section
            then
                if names == [] then
                    ;;; found the procedure
                    return(pdbr);
                else
                    ;;; interested in a nested procedure of this one
                    fast_destpair(names) -> (name, names);
                    false -> id;
                    pdbr_checkpoints(pdbr) -> pdbrs;
                endif;
            endif;
        enduntil;
        false;  ;;; not found
    enddefine;

    if isword(name) then
        ;;; name is a word representing a procedure name
        (name, []);
    else
         ;;; name is a list of words specifying a nested procedure
        fast_destpair(name);
    endif -> (name, names);
    if ispair(word_dict_status(name) ->> status) then
        ;;; name was produced by -word_identifier-
        isdefined(name) -> id;
        fast_front(status) -> name;
    endif;
    if file then
        if file_table(file) ->> fdbr then
            Get_pdbr(name, names, id, fdbr_pdbrs(fdbr)) -> pdbr;
        endif
    else
        for file, fdbr in_property file_table do
            returnif(Get_pdbr(name, names, id, fdbr_pdbrs(fdbr)) ->> pdbr);
        endfor;
    endif;
enddefine;


/* Apply a procedure to a PDBR and all its nested PDBRs */

define app_pdbr(pdbr, p);
    lvars pdbr, p;
    p(pdbr);
    lvars checkpoint;
    fast_for checkpoint in pdbr_checkpoints(pdbr) do
        if ispdbr(checkpoint) then
            app_pdbr(checkpoint, p);
        endif;
    endfor;
enddefine;


/* Apply a procedure to all PDBRs currently defined */

define app_all_pdbrs(p);
    lvars p;
    lvars filename, fdbr;
    fast_for filename, fdbr in_property file_table do
        lvars pdbr;
        fast_for pdbr in fdbr_pdbrs(fdbr) do
            app_pdbr(pdbr, p);
        endfor;
    endfor;
enddefine;


/* Checkpoint management */

lconstant
    invalid_line = -1,
;

lvars
    curr_fdbr   = false,
    curr_pdbr   = false,
    curr_line   = invalid_line,
    pdbr_stack  = [],
;

vars
    curr_exec_pdbr  = false,
    curr_exec_line  = invalid_line,
;

define pdbr_first_line(pdbr) -> pdbr;
    lvars pdbr;
    while ispdbr(pdbr) do
        fast_front(pdbr_checkpoints(pdbr)) -> pdbr;
    endwhile;
enddefine;

define pdbr_last_line(pdbr) -> pdbr;
    lvars pdbr;
    while ispdbr(pdbr) do
        last(pdbr_checkpoints(pdbr)) -> pdbr;
    endwhile;
enddefine;

define lconstant merge_fdbrs(old_fdbr, new_fdbr);
    lvars   old_pdbr, new_pdbr, old_pdbrs, new_pdbrs, old_fdbr, new_fdbr,
            new_first_line, new_last_line, old_last_line = false;
    fdbr_pdbrs(old_fdbr) -> old_pdbrs;
    fdbr_pdbrs(new_fdbr) -> new_pdbrs;
    returnif(new_pdbrs == []);
    ;;; delete from -old_fdbr- procedures which occur in -new_fdbr-
    [%  fast_for old_pdbr in old_pdbrs do
            fast_for new_pdbr in new_pdbrs do
                nextif(pdbr_name(old_pdbr) == pdbr_name(new_pdbr))(2);
            endfor;
            pdbr_last_line(old_pdbr) -> old_last_line;
            old_pdbr;
        endfor;
    %] -> old_pdbrs;
    ;;; splice in procedures from -new_fdbr- on the basis of line number
    pdbr_first_line(fast_front(new_pdbrs)) -> new_first_line;
    pdbr_last_line(last(new_pdbrs)) -> new_last_line;
    if old_last_line and old_last_line fi_> new_last_line then
        warning('LINE NUMBER INFORMATION MAY BE INACCURATE AFTER LINE '
            sys_>< new_last_line, []);
    endif;
    [%  until old_pdbrs == []
        or pdbr_last_line(fast_front(old_pdbrs)) fi_>= new_first_line
        do
            fast_destpair(old_pdbrs) -> old_pdbrs;
        enduntil;
        until old_pdbrs == []
        or pdbr_first_line(fast_front(old_pdbrs)) fi_> new_last_line
        do
            fast_back(old_pdbrs) -> old_pdbrs;
        enduntil;
        dl(new_pdbrs);
        dl(old_pdbrs);
    %] -> fdbr_pdbrs(old_fdbr);
enddefine;

define lconstant begin_file();
    consfdbr(popfilename, [], []) -> curr_fdbr;
    false -> curr_pdbr;
    [] -> pdbr_stack;
enddefine;

define lconstant end_file();
    lvars fdbr;
    fast_ncrev(fdbr_pdbrs(curr_fdbr)) -> fdbr_pdbrs(curr_fdbr);
    if file_table(fdbr_name(curr_fdbr)) ->> fdbr then
        merge_fdbrs(fdbr, curr_fdbr);
    else
        curr_fdbr -> file_table(fdbr_name(curr_fdbr));
    endif;
enddefine;

define lconstant begin_procedure(props);
    lvars props, id;
    conspair(curr_pdbr, pdbr_stack) -> pdbr_stack;
    unless isword(props)
    and (sys_current_ident(props) ->> id) and isident(id) /== "lextoken"
    then
        false -> id;
    endunless;
    conspdbr(props, id, popfilename, [], false, [], []) -> curr_pdbr;
    poplinenum -> curr_line;
enddefine;

define lconstant end_procedure();
    lvars prev_pdbr;
    fast_ncrev(pdbr_checkpoints(curr_pdbr)) -> pdbr_checkpoints(curr_pdbr);
    if pdbr_stack == [] then
        mishap(0, 'end_procedure: no matching begin_procedure');
    endif;
    fast_destpair(pdbr_stack) -> (prev_pdbr, pdbr_stack);
    if prev_pdbr then
        ;;; -curr_pdbr- corresponds to a local (nested) procedure
        conspair(curr_pdbr, pdbr_checkpoints(prev_pdbr))
            -> pdbr_checkpoints(prev_pdbr);
    elseif curr_fdbr then
        conspair(curr_pdbr, fdbr_pdbrs(curr_fdbr)) -> fdbr_pdbrs(curr_fdbr);
    endif;
    prev_pdbr -> curr_pdbr;
    invalid_line -> curr_line;
enddefine;

define lconstant add_checkpoint();
    lvars checkpoints;
    returnunless(curr_pdbr);
    pdbr_checkpoints(curr_pdbr) -> checkpoints;
    if checkpoints == [] or curr_line /== fast_front(checkpoints) then
        conspair(curr_line, checkpoints) -> pdbr_checkpoints(curr_pdbr);
    endif;
enddefine;


/* Breakpoints */

lvars
    curr_breakpoint_id = 0,
;

define lconstant get_breakpoint_id();
    curr_breakpoint_id + 1 ->> curr_breakpoint_id;
enddefine;

define lconstant breakpoint(pdbr, line);
    lvars pdbr, line;
    list_assoc(line, pdbr_breakpoints(pdbr));
enddefine;

define lconstant set_breakpoint(pdbr, line);
    lvars pdbr, line;
    unless list_assoc(line, pdbr_breakpoints(pdbr)) then
        cons_assoc(line, get_breakpoint_id(), pdbr_breakpoints(pdbr))
            -> pdbr_breakpoints(pdbr);
    endunless;
enddefine;

define del_breakpoint(bp, pdbr);
    lvars bp, pdbr;
    if bp then
        del_assoc(bp, pdbr_breakpoints(pdbr)) -> pdbr_breakpoints(pdbr);
    else
        [] -> pdbr_breakpoints(pdbr);
        true;
    endif;
enddefine;

;;; stop_at:
;;;     set a breakpoint at the given position within -file-.

define stop_at(file, posn);
    lvars file, posn;
    lvars found_pdbr = false, line = false;
    if isinteger(posn) then
        ;;; -posn- is a line number
        unless file then
            mishap(0, 'No current file (please specify file name)');
        endunless;
        lvars fdbr = file_table(file);
        lvars pdbrs = if fdbr then fdbr_pdbrs(fdbr) else [] endif;
        until pdbrs == [] do
            lvars pdbr;
            fast_destpair(pdbrs) -> (pdbr, pdbrs);
            if isinteger(pdbr) and pdbr fi_>= posn then
                pdbr -> line;
                quitloop;
            elseif ispdbr(pdbr) and pdbr_last_line(pdbr) fi_>= posn then
                pdbr -> found_pdbr;
                pdbr_checkpoints(found_pdbr) -> pdbrs;
            endif;
        enduntil;
    elseif get_pdbr(posn, file) ->> found_pdbr then
        pdbr_first_line(found_pdbr) -> line;
    endif;
    unless found_pdbr then
        if islist(posn) then last(posn) -> posn endif;
        mishap(
            if file then (file, ":", posn, 3) else (posn, 1) endif,
            if isinteger(posn) then
                'Can\'t set breakpoint on line';
            else
                'Can\'t set breakpoint: procedure not known to debugger';
            endif);
    endunless;
    set_breakpoint(found_pdbr, line);
enddefine;


/* Temporary breakpoints: used to implement "next" and "skip" commands */

lconstant
    tbp_anchor_point = sysCOMPILE,  ;;; n.b. value before redefinition
;

vars
    temp_breakpoint = false,        ;;; identifies a stack frame
;

define set_temp_breakpoint(n);
    lvars n, a = callstacklength(tbp_anchor_point);
    unless a then
        mishap(0, 'Can\'t set temporary breakpoint for next, skip, etc.');
    endunless;
    callstacklength(n) - a -> temp_breakpoint;
enddefine;

define at_temp_breakpoint();
    lvars a = callstacklength(tbp_anchor_point);
    a and temp_breakpoint and temp_breakpoint >= callstacklength(1) - a;
enddefine;


/* Run time checkpoint procedures */

;;; checkpoint:
;;;     check for a breakpoint at the given line.

define lconstant checkpoint(line);
    lvars line;
    line -> curr_exec_line;
    returnunless(
        (debugger_debugging == true or debugger_debugging == "execute")
        and curr_exec_pdbr and not(pdbr_ignore(curr_exec_pdbr)));
    if stepping then
        chain(curr_exec_pdbr, line, "stepping", pop_debugger);
    elseif breakpoint(curr_exec_pdbr, line) then
        chain(curr_exec_pdbr, line, "breakpoint", pop_debugger);
    elseif at_temp_breakpoint() then
        false -> temp_breakpoint;
        chain(curr_exec_pdbr, line, "temp_breakpoint", pop_debugger);
    elseif animating then
        chain(curr_exec_pdbr, line, "animating", pop_debugger);
    endif;
enddefine;


;;; checkpoint1, checkpoint2, checkpoint3:
;;;     sets a checkpoint 1, 2 or 3 lines beyond the current line.

define lconstant checkpoint1();
    chain(curr_exec_line fi_+ 1, checkpoint);
enddefine;

define lconstant checkpoint2();
    chain(curr_exec_line fi_+ 2, checkpoint);
enddefine;

define lconstant checkpoint3();
    chain(curr_exec_line fi_+ 3, checkpoint);
enddefine;


/* Current (logical) frame number and (actual) caller number */

vars
    curr_frame_num = false,
;

define get_base_frame();
    lvars base_frame;
    unless (iscaller(pop_debugger) ->> base_frame)
    or (iscaller(sys_debugger) ->> base_frame)
    then
        mishap(0, 'Can\'t determine base frame');
    endunless;
    base_frame fi_- 1;
enddefine;

define macro CALLER_NUM;
    [get_base_frame() fi_+ curr_frame_num].dl;
enddefine;


/* Compile time and run time representation of the lexical environment */

lvars
    lex_env = [],
        ;;; dummy for initialising -lex_env_id-
    lex_env_name,
        ;;; name of variable holding the lexical env of the current procedure
;

vars
    lex_env_id = ident lex_env,
;

define lconstant enter_procedure_scope(fdbr, id);
    lvars fdbr, id;
    id -> lex_env_id;
    if fdbr then
        [[% fdbr_lexicals(fdbr) %]] -> idval(id);
    endif;
    conspair([% pdbr_lconstants(curr_exec_pdbr) %], idval(id)) -> idval(id);
enddefine;

define lconstant enter_lblock_scope();
    conspair([], fast_front(idval(lex_env_id)))
        -> fast_front(idval(lex_env_id));
enddefine;

define lconstant leave_lblock_scope();
    fast_back(fast_front(idval(lex_env_id)))
        -> fast_front(idval(lex_env_id));
enddefine;

define lconstant bind_lconstant(name, val);
    lvars name, val, l = pdbr_lconstants(curr_pdbr);
    returnif(list_assoc(name, l));
    cons_assoc(name, val, l) -> pdbr_lconstants(curr_pdbr);
enddefine;

define lconstant bind_lvar(name, id);
    lvars name, id, l = fast_front(fast_front(idval(lex_env_id)));
    returnif(list_assoc(name, l));
    cons_assoc(name, id, l) -> fast_front(fast_front(idval(lex_env_id)));
enddefine;

define lconstant bind_file_lexical(name, id);
    lvars name, id, l = fdbr_lexicals(curr_fdbr);
    returnif(not(curr_fdbr) or list_assoc(name, l));
    cons_assoc(name, id, l) -> fdbr_lexicals(curr_fdbr);
enddefine;

define lconstant lookup_lex(name);
    lvars block, frame, val, name;
    if curr_frame_num then
        fast_for frame in idval(caller_valof(WID lex_env_id, CALLER_NUM)) do
            fast_for block in frame do
                returnif(list_assoc(name, block) ->> val)(val);
            endfor;
        endfor;
    endif;
    false;  ;;; not found
enddefine;

define lexical_bindings();
    lvars curr_env, block;
    idval(caller_valof(WID lex_env_id, CALLER_NUM)) -> curr_env;
    if curr_env == [] then
        [];
    else
        ;;; collect all the lvar bindings (ignoring lconstants)
        [%  fast_for block in fast_front(curr_env) do
                until block == [] do
                    unless isconstant(fast_front(fast_back(block))) then
                        (fast_front(block), fast_front(fast_back(block)));
                    endunless;
                    fast_back(fast_back(block)) -> block;
                enduntil;
            endfor;
        %];
    endif;
enddefine;


/* Variables local to the current procedure */

vars
    dlocal_env = [],
;

define lconstant bind_local(/* name, ident */) with_nargs 2;
    cons_assoc(/* name, ident */, dlocal_env) -> dlocal_env;
enddefine;

define permanent_bindings(p);
    lvars p;
    caller_valof(WID dlocal_env, CALLER_NUM),
enddefine;


/*  Returns the lexical or permanent identifier attached to -word- */

define db_identof(word);
    lvars word;
    lookup_lex(word) or isdefined(word);
enddefine;


/* Test for whether a procedure is compiled in debugging mode */

define compiled_in_debugging_mode(p);
    lvars p;
    isdlocal(WID lex_env_id, p);
enddefine;


/* Save the current values of code planting procedures used below */

lconstant
    SysCALL     = sysCALL,
    SysCALLQ    = sysCALLQ,
    SysUCALL    = sysUCALL,
    SysUCALLQ   = sysUCALLQ,
    SysIDENT    = sysIDENT,
    SysLOCAL    = sysLOCAL,
    SysPUSH     = sysPUSH,
    SysPUSHQ    = sysPUSHQ,
    SysPOP      = sysPOP,
;


/* Redefine VM code planting procedures for debugging */

lvars char_buff = false;    ;;; char buffer from Incharitem

define lconstant linenum_change() -> delta;
    lvars delta, char;
    returnif(curr_line == invalid_line)(999 -> delta);
    unless (poplinenum fi_- curr_line ->> delta) == 0 then
        if char_buff
        and ispair(cont(char_buff) ->> char) and fast_front(char) == `\n`
        then
            delta fi_- 1 -> delta;
        endif;
    endunless;
enddefine;

;;; code_planter_wrapper:
;;;     A generic wrapper, used for code planting procedures for which no
;;;     specific wrapper is defined.

define lconstant code_planter_wrapper(/* p */) with_nargs 1;
    lvars delta;
    if curr_pdbr and popfilename = pdbr_file(curr_pdbr)
    and not(pop_syntax_only)
    and (linenum_change() ->> delta) /== 0
    then
        if delta == 1 then
            SysCALLQ(checkpoint1);
        elseif delta == 2 then
            SysCALLQ(checkpoint2);
        elseif delta == 3 then
            SysCALLQ(checkpoint3);
        else
            SysPUSHQ(poplinenum);
            SysCALLQ(checkpoint);
        endif;
        poplinenum -> curr_line;
        add_checkpoint();
    endif;
    fast_apply(/* p */);
enddefine;

define lconstant sysCOMPILE_wrapper(/* compile_p, p */) with_nargs 2;
    lvars p;
    dlocal
        curr_fdbr    = false,
        curr_pdbr    = false,
        pdbr_stack   = [],
        lex_env_name = false,
        curr_line    = 0,
        char_buff    = false,
    ;
    if (debugger_debugging == true or debugger_debugging == "compile")
    and pop_debugging
    and not(pop_syntax_only)
    and not(iscaller(sys_autoload))
    and current_section /== debugger_section
    and popfilename
    then
        begin_file();
        if isincharitem(readitem) ->> p then
            frozval(1, p) -> char_buff;
        endif;
    endif;
    fast_apply(/* compile_p, p */);
    if curr_fdbr then
        end_file();
    endif;
enddefine;

define lconstant sysEXECUTE_wrapper(/* p */) with_nargs 1;
    dlocal
        stepping        = false,
        animating       = false,
        curr_exec_pdbr  = false,
        curr_exec_line  = invalid_line,
    ;
    fast_apply(/* p */);
enddefine;

define lconstant sysEXEC_COMPILE_wrapper(/* compile_p, has_result, p */) with_nargs 3;
    dlocal
        ;;; turn off debugging
        debugger_debugging  = false,
        curr_fdbr           = false,
        curr_pdbr           = false,
    ;
    fast_apply(/* compile_p, has_result, p */);
    invalid_line -> curr_line;
enddefine;

define lconstant sysEXEC_OPTION_COMPILE_wrapper(/* compile_p, p */) with_nargs 2;
    /* p */ -> ;
    fast_apply(/* compile_p */) -> ;
    invalid_line -> curr_line;
    false;  ;;; ensure the planted code is executed in context at run time
enddefine;

define lconstant sysPROCEDURE_wrapper(props, nargs, p);
    lvars props, nargs, p, toplevel = pop_vm_compiling_list == [];
    fast_apply(props, nargs, p);
    if curr_fdbr and pop_debugging and not(pop_syntax_only) then
        begin_procedure(props);
        SysLOCAL(WID curr_exec_pdbr);
        SysPUSHQ(curr_pdbr); SysPOP(WID curr_exec_pdbr);
        SysLOCAL(WID curr_exec_line);
        SysPUSHQ(curr_line); SysPOP(WID curr_exec_line);
        ;;; initialise the lvar environment
        if toplevel then
            sysNEW_LVAR() -> lex_env_name;
            SysPUSHQ(curr_fdbr);
        else
            SysLOCAL(lex_env_name);
            SysPUSHQ(false);
        endif;
        SysLOCAL(WID lex_env_id);
        SysIDENT(lex_env_name);
        SysCALLQ(enter_procedure_scope);
        ;;; initialise the local environment
        SysLOCAL(WID dlocal_env);
        SysPUSHQ([]); sysPOP(WID dlocal_env);
    endif;
enddefine;

define lconstant sysENDPROCEDURE_wrapper(/* p */) with_nargs 1;
    if debugger_debugging and curr_pdbr and not(pop_syntax_only) then
        if pdbr_checkpoints(curr_pdbr) == [] then
            ;;; ensure there's at least one checkpoint
            invalid_line -> curr_line;
        endif;
        code_planter_wrapper(/* p */);
        end_procedure();
    else
        fast_apply(/* p */);
    endif;
enddefine;

define lconstant sysLBLOCK_wrapper(/* executing, p */) with_nargs 2;
    fast_apply(/* executing, p */);
    if curr_pdbr and not(pop_syntax_only) then
        SysCALLQ(enter_lblock_scope);
    endif;
enddefine;

define lconstant sysENDLBLOCK_wrapper(/* p */) with_nargs 1;
    fast_apply(/* p */);
    if curr_pdbr and not(pop_syntax_only) then
        SysCALLQ(leave_lblock_scope);
    endif;
enddefine;

define lconstant sysLCONSTANT_wrapper(word, idprops, p);
    lvars word, idprops, procedure p;
    p(word, idprops);
    unless pop_syntax_only then
        if curr_pdbr then
            bind_lconstant(word, sys_current_ident(word));
        elseif curr_fdbr then
            bind_file_lexical(word, sys_current_ident(word));
        endif;
    endunless;
enddefine;

define lconstant sysLVARS_wrapper(word, idprops, p);
    lvars word, idprops, procedure p;
    p(word, idprops);
    unless pop_syntax_only then
        if curr_pdbr then
            SysPUSHQ(word), SysIDENT(word);
            SysCALLQ(bind_lvar);
        elseif curr_fdbr then
            SysPUSHQ(word), SysIDENT(word);
            SysCALLQ(bind_file_lexical);
        endif;
    endunless;
enddefine;

define lconstant sysLOCAL_wrapper(arg, p);
    lvars arg, procedure p, n;

    define lconstant no_debug(/* p */) with_nargs 1;
        dlocal curr_pdbr = false;
        fast_apply(/* p */);
    enddefine;

    define updaterof lconstant no_debug(/* p */) with_nargs 2;
        dlocal curr_pdbr = false;
        -> fast_apply(/* p */);
    enddefine;

    if curr_pdbr and not(pop_syntax_only) then
        if isprocedure(arg) then
            pdprops(arg) -> n;
            no_debug(% arg %) -> arg;
            n -> pdprops(arg);
            p(arg);
        else
            p(arg);
            SysPUSHQ(arg);
            SysIDENT(arg);
            SysCALLQ(bind_local);
        endif;
    else
        p(arg);
    endif;
enddefine;

define lconstant sysLABEL_wrapper(/* lab, p */) with_nargs 2;
    fast_apply(/* lab, p */);
    if curr_pdbr and not(pop_syntax_only) then
        invalid_line -> curr_line;
    endif;
enddefine;

;;; Mapping from code planting procedures to wrappers (or <false> if the
;;; corresponding VM instruction is to be ignored by the debugger)
lconstant code_planter_table = [
    {sysAND                 ^code_planter_wrapper}
    {sysCALL                ^code_planter_wrapper}
    {sysCALLQ               ^code_planter_wrapper}
    {sysCALLS               ^code_planter_wrapper}
    {sysCOMPILE             ^sysCOMPILE_wrapper}
    {sysCONSTANT            ^code_planter_wrapper}
    {sysDLABEL              ^sysLABEL_wrapper}
    {sysDLVARS              ^sysLVARS_wrapper}
    {sysENDLBLOCK           ^sysENDLBLOCK_wrapper}
    {sysENDPROCEDURE        ^sysENDPROCEDURE_wrapper}
    {sysERASE               ^code_planter_wrapper}
    {sysEXECUTE             ^sysEXECUTE_wrapper}
    {sysEXEC_COMPILE        ^sysEXEC_COMPILE_wrapper}
    {sysEXEC_OPTION_COMPILE ^sysEXEC_OPTION_COMPILE_wrapper}
    {sysFIELD               ^code_planter_wrapper}
    {sysGLOBAL              ^false}
    {sysGOTO                ^code_planter_wrapper}
    {sysGO_ON               ^code_planter_wrapper}
    {sysIDENT               ^code_planter_wrapper}
    {sysIFNOT               ^code_planter_wrapper}
    {sysIFSO                ^code_planter_wrapper}
    {sysLABEL               ^sysLABEL_wrapper}
    {sysLBLOCK              ^sysLBLOCK_wrapper}
    {sysLCONSTANT           ^sysLCONSTANT_wrapper}
    {sysLOCAL               ^sysLOCAL_wrapper}
    {sysLVARS               ^sysLVARS_wrapper}
    {sysNEW_LABEL           ^false}
    {sysNEW_LVAR            ^false}
    {sysOR                  ^code_planter_wrapper}
    {sysPASSIGN             ^false}
    {sysPLOG_ARG_PUSH       ^code_planter_wrapper}
    {sysPLOG_IFNOT_ATOM     ^code_planter_wrapper}
    {sysPLOG_RESTART        ^code_planter_wrapper}
    {sysPLOG_RESTORE        ^code_planter_wrapper}
    {sysPLOG_SAVE           ^code_planter_wrapper}
    {sysPLOG_TERM_SWITCH    ^code_planter_wrapper}
    {sysPOP                 ^code_planter_wrapper}
    {sysPROCEDURE           ^sysPROCEDURE_wrapper}
    {sysPUSH                ^code_planter_wrapper}
    {sysPUSHQ               ^code_planter_wrapper}
    {sysPUSHS               ^code_planter_wrapper}
    {sysSWAP                ^code_planter_wrapper}
    {sysSYNTAX              ^false}
    {sysUCALL               ^code_planter_wrapper}
    {sysUCALLQ              ^code_planter_wrapper}
    {sysUCALLS              ^code_planter_wrapper}
    {sysUFIELD              ^code_planter_wrapper}
    {sysUPASSIGN            ^false}
    {sysVARS                ^code_planter_wrapper}
];

;;; Mapping from code planters to their updaters
lconstant code_planter_updater_table = [
    {sysPUSH    sysPOP}
    {sysCALL    sysUCALL}
    {sysCALLQ   sysUCALLQ}
    {sysCALLS   sysUCALLS}
    {sysFIELD   sysUFIELD}
];

;;; Flag set <true> when code planting procedures have been redefined
lvars code_planters_redefined = false;


;;; redefine_code_planters:
;;;     redefines the code planting procedures to plant calls to run-time
;;;     debugging procedures.

define redefine_code_planters();
    returnif(code_planters_redefined);
    lvars entry;
    for entry in code_planter_table do
        lvars (name, wrapper) = explode(entry);
        if wrapper then
            sysunprotect(name);
            lvars code_planter = valof(name);
            lvars closure = wrapper(%code_planter%);
            closure -> valof(name);
            pdprops(code_planter) -> pdprops(closure);
            pdnargs(code_planter) -> pdnargs(closure);
            sysprotect(name);
        endif;
    endfor;
    for entry in code_planter_updater_table do
        valof(entry(2)) -> updater(valof(entry(1)));
    endfor;
    true -> code_planters_redefined;
enddefine;


;;; restore_code_planters:
;;;     restores the previous values of the code planting procedures.

define restore_code_planters();
    returnunless(code_planters_redefined);
    lvars entry;
    for entry in code_planter_table do
        lvars (name, wrapper) = explode(entry);
        if wrapper then
            sysunprotect(name);
            frozval(1, valof(name)) -> valof(name);
            sysprotect(name);
        endif;
    endfor;
    false -> code_planters_redefined;
enddefine;


/* A version of -compile- which uses the lexical environment and
   -debugger_interrupt- */

define lconstant db_compile_PUSH(wordarg);
    lvars wordarg, id;
    if isword(wordarg) and lookup_lex(wordarg) ->> id then
        SysPUSHQ(idval(id));
    else
        SysPUSH(wordarg);
    endif;
enddefine;
;;;
define updaterof db_compile_PUSH(wordarg);
    lvars wordarg, id;
    if isword(wordarg) and lookup_lex(wordarg) ->> id then
        SysPUSHQ(id); SysUCALLQ(idval);
    else
        SysPOP(wordarg);
    endif;
enddefine;

define lconstant db_compile_CALL(wordarg);
    lvars wordarg, id;
    if isword(wordarg) and lookup_lex(wordarg) ->> id then
        SysCALLQ(idval(id));
    else
        SysCALL(wordarg);
    endif;
enddefine;
;;;
define updaterof db_compile_CALL(wordarg);
    lvars wordarg, id;
    if isword(wordarg) and lookup_lex(wordarg) ->> id then
        SysUCALLQ(idval(id));
    else
        SysUCALL(wordarg);
    endif;
enddefine;

applist([sysPUSH sysPOP sysCALL sysUCALL], sysunprotect);
;;;
define db_compile(stream);
    lvars stream;
    dlocal
        sysPUSH     = db_compile_PUSH,
        sysPOP      = updater(db_compile_PUSH),
        sysCALL     = db_compile_CALL,
        sysUCALL    = updater(db_compile_CALL),
        interrupt   = debugger_interrupt,
    ;
    subsystem_compile(stream, "pop11");
enddefine;
;;;
applist([sysPUSH sysPOP sysCALL sysUCALL], sysprotect);

endsection; ;;; $-debugger


/* --- Revision History ---------------------------------------------------
--- Simon Nichols, Jan 19 1994
        Changed the way in VM code planting procedures are redefined for
        debugging. There is now a table mapping code planting procedures to
        wrappers. The generic wrapper has been renamed code_planter_wrapper
        and the specific ones have names formed from the code planting
        procedure with the suffix "_wrapper" added, e.g. sysLVARS_wrapper.
        There is also now a table mapping code planting procedures to their
        updaters.
        redefine_code_planters now does nothing if code_planters_redefined
        is <true>.
--- Simon Nichols, Oct 11 1993
        Numerous changes. In particular:
        o the procedures app_pdbr and app_all_pdbrs have been introduced to
          reduce the amount of information about the internals of PDBRs
          needed outside this file
        o stopat had been changed to improve its error reporting
        o the temporary breakpoint code now checks that the anchor prcedure
          is in the calling chain
        o permanent_bindings now uses CALLER_NUM to access the correct
          invocation of the procedure
--- John Gibson, Jan 21 1993
        Changed to use subsystem_compile
--- Simon Nichols, Jul  1 1992
        Changed -bind_lconstant-, -bind_lvar- and -bind_file_lexical- to
        check whether a constant/variable is already present in the
        environment before adding an entry for it.
        Changed the treatment of -sysEXEC_OPTION_COMPILE- in debugging mode
        to enable code inside list and vector brackets to be debugged.
--- Simon Nichols, Feb 21 1992
        Fixed -db_compile- to work after a -sys_lock_system- (see bugreport
        isl-fr.4410).
--- Simon Nichols, Feb 20 1992
        Fixed -set_temp_breakpoint- and -at_temp_breakpoint- to calculate
        call stack length relative to the value of -sysCOMPILE- before it's
        redefined (which is the version which will be active at run time).
        Changed -checkpoint- to pass to -pop_debugger- a word describing
        the reason why it has been invoked.
--- Simon Nichols, Jan 27 1992
        Changed declaration of -sys_debugger- from vars to constant, to
        accord with its definition in debugger/user.p.
--- Simon Nichols, Jan 23 1992
        Fixed -db_sysLOCAL- to handle ``dlocal weakref ... ''.
--- Simon Nichols, Oct  3 1991
        Changed the debugging version of -sysDLVARS- to call -sysLVARS- as
        all lvars are made type 3 by the debugger.
--- Simon Nichols, Sep 27 1991
        Numerous changes to support the proper run-time maintenance of the
        lexical environment, including lconstants and execute level lvars.
        Changed the management of temporary breakpoints.
        Reorganised.
 */
