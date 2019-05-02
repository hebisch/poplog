/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/break.p
 > Purpose:         Common Lisp BREAK facility
 > Author:          John Williams, June 1 1987 (see revisions)
 > Documentation:   HELP * BREAK
 */

lisp_compile_mode;

section $-lisp => break;

/* Debugger utilities */

constant
    procedure (break, describe, display_error, ed, lispinspect,
               lisp_interrupt, top_level_listen, top_level_loop)
    ;

vars
    breaklevel              =   0,
    break_on_errors         =   true,
    break_on_interrupts     =   true,
    break_on_warnings       =   nil,
    last_error,
    ;


propsheet_idents
    break_on_errors, break_on_interrupts, break_on_warnings;


defclass stackframe
    {   sf_owner,
        sf_callnum,
        sf_lvars,
        sf_pvars,
    };


vars
    db_condition            =   [],
    db_continue_restart     =   false,
    db_help_list            =   [],
    db_stackframe           =   false,
    db_first_callpdr        =   false,
    macro db_first_callnum  =   [iscaller(db_first_callpdr)],
    db_max_callnum          =   0,
    db_hidden_packages      =   [% system_package, pop11_package %],
    db_hidden_functions     =   [],
    db_last_error           =   false,
    db_restarts             =   [],
    ;


define break_ok(flag);
    pop_true(flag)
        and
    ((debug_io /== Debug_io) or systrmdev(pop_charin_device))
        and
    (Lispcompiler and true)         ;;; i.e. iscaller(lispCOMPILE)
enddefine;


define lconstant Db_apply(procedure pdr);
    lvars sl;
    dlocal
        break_on_errors         =   nil,
        break_on_interrupts     =   nil,
        break_on_warnings       =   nil,
        ;
    stacklength() -> sl;

    define dlocal interrupt();
        setstacklength(sl);
        exitfrom(Db_apply)
    enddefine;

    pdr()
enddefine;


define lconstant Db_read(popprompt) -> form;
    dlocal popprompt;
    lispreadform() -> form;
    if form == termin then
        exitfrom(Db_apply)
    endif
enddefine;


define lconstant Db_ask(string);
    lvars char;
    appdata(string, cucharout);
    appdata(' (y/n)\n', cucharout);
    sys_clear_input(poprawdevin);
    rawcharin() -> char;
    (char /== `n`) and (char /== `N`)
enddefine;


define db_caller(n);
    if (n fi_< 0) or (n fi_>= db_max_callnum) then
        false
    else
        caller(db_first_callnum fi_+ n)
    endif
enddefine;


define db_caller_valof(token);
    caller_valof(token,
                 db_first_callnum fi_+ sf_callnum(db_stackframe))
enddefine;


define updaterof db_caller_valof(val, token);
    val -> caller_valof(token,
                        db_first_callnum fi_+ sf_callnum(db_stackframe))
enddefine;


define db_caller_name(pdr) -> name;
    if lisp_system_building then
        recursive_front(pdprops(pdr)) -> name;
        return
    endif;
    if fast_lmember(pdr, db_hidden_functions) then
        false -> name;
        return
    endif;
    if pdr == dmac and isinteger(lex_dmac_subchar) then
        consstring(lex_dmac_char, uppertolower(lex_dmac_subchar), 2)
    else
        f_name(pdr)
    endif -> name;
    if (issymbol(name)
    and fast_lmember(symbol_package(name), db_hidden_packages)
    and (symbol_status(name) == symbol_status_internal))
    or (isword(name)
        and fast_lmember(pop11_package, db_hidden_packages))
    then
        false -> name
    endif
enddefine;


define lconstant Db_display_call(name, n);
    cucharout(`\s`);
    cucharout(if n == sf_callnum(db_stackframe) then `*` else `\s` endif);
    sys_syspr(n);
    cucharout(`:`);
    cucharout(`\s`);
    cucharout(`\s`);
    npr(name)
enddefine;


define lconstant Db_check_stack();
    unless isdlocal("ident Init_stklen", sf_owner(db_stackframe)) do
        advise('Warning: may not reset user stack correctly')
    endunless
enddefine;


define lconstant Db_reset_stack();
    if isdlocal("ident Init_stklen", sf_owner(db_stackframe)) then
        setstacklength(db_caller_valof("ident Init_stklen"))
    endif
enddefine;


define Db_quit();
    ;;; Exported to Lisp as sys:try-quit-to-top-level
    if iscaller(lispCOMPILE) then
        exitto(false, lispCOMPILE)
    else
        setpop()
    endif
enddefine;


/* Stack frame selection */

define lconstant Db_set_current(pdr, num);
    unless db_stackframe do
        consstackframe(false, 0, nullvector, nullvector) -> db_stackframe
    endunless;
    pdr -> sf_owner(db_stackframe);
    num -> sf_callnum(db_stackframe);
    if isdlocal("ident Lexidents", pdr) then
        consvector(destlist(db_caller_valof("ident Lexidents")))
    else
        nullvector
    endif -> sf_lvars(db_stackframe);
    f_specials(pdr) or nullvector -> sf_pvars(db_stackframe)
enddefine;


define lconstant Db_select(item);
    lvars num, pdr;
    if item == @+ then
        sf_callnum(db_stackframe) fi_+ 1 -> item
    elseif item == @- then
        sf_callnum(db_stackframe) fi_- 1 -> item
    endif;
    if isinteger(item) then
        db_caller(item ->> num) -> pdr
    else
        0 -> num;
        while (db_caller(num) ->> pdr) do
            quitif(f_name(pdr) == item);
            num fi_+ 1 -> num
        endwhile
    endif;
    if pdr then
        pdr, num
    else
        advise('Cannot select stackframe');
        interrupt()
    endif
enddefine;


define lconstant Db_frame_apply(contn);
    if top_level_listen() then
        Db_set_current(Db_select(Db_read('Frame? ')))
    endif;
    chain(db_stackframe, contn)
enddefine;


/* Debugger commands */

define lconstant Dbc_restart();
    lvars args;
    Db_check_stack();
    Db_read('New arguments? ') -> args;
    Db_reset_stack();
    #| eval(args) |# -> db_caller_valof("ident LISP_N_ARGS");
    chainfrom(
        callstacklength(db_first_callnum fi_+ sf_callnum(db_stackframe)),
        sf_owner(db_stackframe))
enddefine;


define lconstant Dbc_return();
    lvars vals;
    Db_check_stack();
    Db_read('Result(s)? ') -> vals;
    Db_reset_stack();
    eval(vals);
    chainfrom(callstacklength(db_first_callnum fi_+ sf_callnum(db_stackframe)),
                identfn)
enddefine;


define lconstant Dbc_backtrace();
    lvars count = -1, n = 0, l = 0, pdr, name;
 #_IF VED_LOADED
    lconstant Screen_length = vedscreenlength;
 #_ELSE
    lconstant Screen_length = 24;
 #_ENDIF
    if top_level_listen() then
        Db_read('How many frames? ') -> count;
        unless isinteger(count) do
            -1 -> count
        endunless
    endif;
    if top_level_listen() then
        Db_read('From which frame? ') -> n;
        unless isinteger(n) do
            0 -> n
        endunless
    endif;
    while (db_caller(n) ->> pdr) do
        quitif(pdr == top_level_loop or pdr == setpop);
        if (db_caller_name(pdr) ->> name) then
            quitif(count == 0);
            if l fi_> (Screen_length - 3) then
                quitunless(Db_ask(';;; MORE?'));
                0 -> l
            endif;
            Db_display_call(name, n);
            l fi_+ 1 -> l;
            count fi_- 1 -> count
        endif;
        n fi_+ 1 -> n
    endwhile
enddefine;


define lconstant Dbc_inspect();
    Db_frame_apply(lispinspect)
enddefine;


define lconstant Dbc_describe();
    Db_frame_apply(describe)
enddefine;


define lconstant Dbc_select();
    Db_frame_apply(procedure(sf);
                        Db_display_call(sf_owner(sf), sf_callnum(sf))
                    endprocedure)
enddefine;


define lconstant Dbc_ed();
    Db_frame_apply(procedure(sf);
                        ed(sf_owner(sf))
                    endprocedure)
enddefine;


define lconstant Dbc_hide();
    lvars arg;
    Db_read('What?  ') -> arg;
    if arg == @:FUNCTION then
        Db_select(Db_read('Frame? ')) -> -> arg;
        unless fast_lmember(arg, db_hidden_functions) do
            conspair(arg, db_hidden_functions) -> db_hidden_functions
        endunless
    else
        find_package(arg) -> arg;
        unless fast_lmember(arg, db_hidden_packages) do
            conspair(arg, db_hidden_packages) -> db_hidden_packages
        endunless
    endif;
    chain(Dbc_backtrace)
enddefine;


define lconstant Dbc_unhide();
    lvars arg;
    Db_read('What? ') -> arg;
    if arg == @:ALL then
        [] ->> db_hidden_functions -> db_hidden_packages
    elseif arg == @:FUNCTION then
        Db_select(Db_read('Frame? ')) -> -> arg;
        delete(arg, db_hidden_functions, nonop ==)
            -> db_hidden_functions
    else
        delete(find_package(arg), db_hidden_packages, nonop ==)
            -> db_hidden_packages
    endif;
    chain(Dbc_backtrace)
enddefine;


define lconstant Dbc_unhide_all();
    [] ->> db_hidden_functions -> db_hidden_packages;
    chain(Dbc_backtrace)
enddefine;


define lconstant Dbc_recall_err();
    if db_last_error then
        display_error(explode(db_last_error)) ->;
        cucharout(`\n`)
    else
        advise('No previous error to redisplay')
    endif
enddefine;


#_IF VED_LOADED
define lconstant Dbc_edit_err();
    lvars file, line;
    if db_last_error then
        db_last_error(7) -> file;
        db_last_error(8) -> line;
        if file then
            if line then line -> vvedgotoplace endif;
            vededit(file)
        else
            advise('Erring code not on disk or in Ved')
        endif
    else
        advise('This break not caused by an error')
    endif
enddefine;
#_ENDIF


define lconstant Dbc_describe_cond();
    if isinstance(db_condition) then
        describe(db_condition)
    endif
enddefine;


define lconstant Dbc_inspect_cond();
    if isinstance(db_condition) then
        lispinspect(db_condition)
    endif
enddefine;


define lconstant Dbc_pp_peek();
    lvars n, states, streams, Max, stream;

    if (iscaller(pprint_logical_block) ->> n) then
        caller_valof("ident pp_states", n) -> states;
        caller_valof("ident pp_streams", n) -> streams
    else
        advise('Not pretty printing');
        return
    endif;

    if top_level_listen() then
        Db_read('Max number of characters to output? ')
    else
        pop_max_int
    endif -> Max;

    if top_level_listen() then
        eval([^@VALUES ^(Db_read('Stream? '))])
    else
        standard_input
    endif -> stream;

    procedure(pp_states, pp_streams);
        dlocal pp_states, pp_streams;
        peek_pp_state(stream, Max, recursive_stream_write_p(debug_io))
    endprocedure(states, streams)
enddefine;


define lconstant Dbc_help();
    lvars string;
    if db_continue_restart then
        format(standard_output, ' :C    ~A~%', [^db_continue_restart]) ->
    endif;
    for string in db_help_list do
        format(standard_output, string, []) ->
    endfor
enddefine;


define lconstant Dbc_os();
#_IF VMS
    sysobey(nullstring)
#_ELSE
    sysobey(nullstring, `!`);
    cucharout(`\n`)
#_ENDIF
enddefine;


define lconstant Dbc_pop11();
    apply_in_pop11_environment(
        recursive_str_input(standard_input),
        pop11_compile);
    cucharout(`\n`)
enddefine;


define lconstant Dbc_quit();
    exitto(break)
enddefine;


define lconstant Db_report_restarts(do_help);
    lvars r, n = 1;

    unless db_restarts do
        if isinstance(db_condition) then
            [% for r in @COMPUTE-RESTARTS(db_condition, 1) do
                if fast_record_access(1, r) == @CONTINUE
                and not(db_continue_restart) then
                    r -> db_continue_restart
                else
                    r
                endif
            endfor %]
        else
            []
        endif -> db_restarts
    endunless;

    if do_help then
        format(debug_io, ' :H     Help~%', []) ->
    endif;
    if db_continue_restart then
        format(debug_io, ' :C     ~A~%', [^db_continue_restart]) ->
    endif;
    for r in db_restarts do
        format(debug_io, ' :C ~3S ~A~%', [^n ^r]) ->;
        n + 1 -> n
    endfor
enddefine;


define lconstant Dbc_list();
    Db_report_restarts(false)
enddefine;


define lconstant Dbc_continue();
    lvars n;

    define lconstant Invoke_restart(r);
        format(debug_io, ';;; ~A~%', [^r]) ->;
        @INVOKE-RESTART-INTERACTIVELY(r, 1)
    enddefine;

    if top_level_listen() then
        Db_read('Restart number? ')
    else
        0
    endif -> n;

    if n == 0 then
        if db_continue_restart then
            Invoke_restart(db_continue_restart)
        elseif (iscaller(break) ->> n)
        and caller(n + 1) == lisp_interrupt then
            advise('Return from break');
            exitfrom(break)
        else
            advise('No restart named CONTINUE')
        endif
    elseif isinteger(n) and listlength_>=(db_restarts, n) then
        Invoke_restart(fast_subscrl(n, db_restarts))
    else
        advise('No restart numbered ~S', [^n])
    endif
enddefine;


/* Break & Interrupt */

define vars break(message);
    lvars n;
    dlocal
        db_continue_restart =   false,
        db_first_callpdr    =   false,
        db_stackframe       =   false,
        db_max_callnum      =   0,
        db_last_error       =   last_error,
        db_restarts         =   false,
        read_suppress       =   nil,
        standard_input      =   debug_io,
        standard_output     =   debug_io,
     #_IF VED_LOADED
        vvedpromptchar      =   `\Sp`,
     #_ENDIF
        ;
    RESET_PRINT_VARS;

    clear_input(debug_io) ->;
    format(debug_io,   '~%Break:  ~@[~A~]~%', [^message]) ->;
    Db_report_restarts(true);
    write_char(CHARACTER `\n`, debug_io) ->;

    caller(2) -> db_first_callpdr;
    Db_set_current(db_first_callpdr, 0);
    unless (iscaller(top_level_loop, 2) ->> n) do
        2 -> n;
        while caller(n) do
            n fi_+ 1 -> n
        endwhile
    endunless;
    n fi_- 2 -> db_max_callnum;

    lisp_compile(debug_io);
    Db_quit()
enddefine;


define vars lisp_interrupt();
    lvars p;
    dlocal last_error = false;
    if functionp(valof(@POPLOG:*INTERRUPT*) ->> p) then
        lisp_apply(p, 0, 0)
    elseif break_ok(break_on_interrupts) then
        break('(Interrupt)')
    else
        setpop()
    endif
enddefine;


/* Set up debugger magic words */

defprop db_magic_word_handler;

procedure();
    /* This works better than just setpop in an RLE context */
    chainfrom(break, setpop)
endprocedure     ->  db_magic_word_handler(@:A);

Dbc_quit         ->  db_magic_word_handler(@:Q);
Dbc_continue     ->  db_magic_word_handler(@:C);
Dbc_os           ->  db_magic_word_handler(@:!);
Dbc_pop11        ->  db_magic_word_handler(@POPLOG:POP11);

Db_apply(% Dbc_backtrace  %)    ->  db_magic_word_handler(@:B);
Db_apply(% Dbc_describe   %)    ->  db_magic_word_handler(@:D);
Db_apply(% Dbc_describe_cond %) ->  db_magic_word_handler(@:DC);
Db_apply(% Dbc_help       %)    ->  db_magic_word_handler(@:H);
Db_apply(% Dbc_hide       %)    ->  db_magic_word_handler(@:HIDE);
Db_apply(% Dbc_inspect    %)    ->  db_magic_word_handler(@:I);
Db_apply(% Dbc_inspect_cond %)  ->  db_magic_word_handler(@:IC);
Db_apply(% Dbc_list       %)    ->  db_magic_word_handler(@:L);
Db_apply(% Dbc_pp_peek %)       ->  db_magic_word_handler(@:PP);
Db_apply(% Dbc_recall_err %)    ->  db_magic_word_handler(@:M);
Db_apply(% Dbc_restart    %)    ->  db_magic_word_handler(@:R);
Db_apply(% Dbc_return     %)    ->  db_magic_word_handler(@:V);
Db_apply(% Dbc_select     %)    ->  db_magic_word_handler(@:S);
Db_apply(% Dbc_unhide     %)    ->  db_magic_word_handler(@:UNHIDE);
Db_apply(% Dbc_unhide_all %)    ->  db_magic_word_handler(@:U);

#_IF VED_LOADED
Db_apply(% Dbc_edit_err   %)    ->  db_magic_word_handler(@:E);
Db_apply(% Dbc_ed         %)    ->  db_magic_word_handler(@:ED);
#_ENDIF

[
    ' :H    Help, prints this\n'
    ' :A    Abort, reset Lisp system\n'
    ' :Q    Quit to and resume previous break or top-level\n'
    ' :B    Display call stack backtrace\n'
    ' :M    Re-display last error message\n'
    ' :DC   Describe the condition on which the debugger was entered\n'
    ' :IC   Inspect the condition on which the debugger was entered\n'
    ' :L    List available restarts\n'
 ^(if VED_LOADED then
    ' :E    Edit top level form currently being evaluated\n'
   endif)
 ^(if VMS then
    ' :!    Spawn DCL sub-process\n'
   else
    ' :!    Fork a UNIX shell\n'
   endif)
    ' :S     <frame>     Select <frame>\n'
    ' :D     <frame>     Select and describe <frame>\n'
    ' :I     <frame>     Select and inspect <frame>\n'
 ^(if VED_LOADED then
    ' :ED    <frame>     Edit source code for <frame>\n'
   endif)
    ' :R     <args>      Re-call selected frame with <args>\n'
    ' :V     <values>    Return <values> from selected frame\n'
    ' :HIDE  <item>      Omit certain classes of functions from backtrace\n'
    ' :UNHIDE            Reverse effect of :HIDE\n'
    ' :PP <max> <stream> Peek at buffered pretty printer output\n'
] -> db_help_list;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  9 1995
        Removed redundant lvar declarations.
--- John Williams, Jul 18 1995
        break_ok now returns false unless called inside lispCOMPILE.
--- John Williams, Jun  1 1995
        Ved variables now guarded with #_IF VED_LOADED
--- John Williams, Mar 17 1995
        Added :U command, and made :A safer in an RLE context,
--- John Williams, Mar 15 1995
        Added :DC command (Dbc_describe_cond).
--- John Williams, Feb 27 1995
        Changes for the Condition system (Steele 1990 ch 29).
--- John Williams, Aug 25 1994
        Changes for Steele 1990 (Lisp version 1.6)
--- John Williams, Jun  7 1994
        Symbol POP11 now in the POPLOG package.
--- John Williams, May 23 1994
        Added :e option.
--- John Williams, Nov 30 1993
        Can now supply "how many frames" parameter to :b option.
--- John Williams, Jul  9 1993
        Uses defclass instead of recordclass
--- John Williams, Jun 30 1993
        Passed explicit nonop == to calls of delete for robustness.
--- John Williams, Jan 21 1993
        Now uses `\Sp` instead of 160
--- John Williams, Jan 15 1993
        Revised for new subsystem implementation
--- John Williams, Apr 13 1992
        Compilation of PWM code conditional on #_IF DEF POP_HAS_PWM
--- Simon Nichols, Dec  5 1991
        Made all references to PWM conditional on a successful compilation
        of $poppwmlib/pwm_windowtype.p.
--- John Williams, Jul 18 1989
        Removed code to output newline after <EOF>
--- John Williams, Jun 22 1989
        Added 'stackframe' recordclass
 */
