/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lisp/src/compile.p
 > Purpose:         Top-level and file compilation
 > Author:          John Williams, Oct 7 1986 (see revisions)
 > Documentation:   CLtL p321 - 326
 > Related Files:   C.all/lisp/modules/history.lsp
 */

lisp_compile_mode;

section $-lisp => lisp_compile, lisp_load, trylisp_compile, pop_show_code;


define lconstant Frig_usepop(file) -> file;
    lvars usepop;
#_IF VMS
    lconstant USEPOP = 'usepop:';
#_ELSE
    lconstant USEPOP = '$usepop';
#_ENDIF
    systranslate(USEPOP) -> usepop;
    if usepop
    and isstartstring(usepop, file) then
        USEPOP dir_>< allbutfirst(datalength(usepop), file) -> file
    endif
enddefine;


/* Interactive top level */

vars
    top_level_forms = [],
    minus1  =  nil,
    plus1   =  nil,     plus2   =  nil,     plus3   =  nil,
    star1   =  nil,     star2   =  nil,     star3   =  nil,
    slash1  =  nil,     slash2  =  nil,     slash3  =  nil,
    ;


define top_level_listen();
    lexchar /== `\n`
enddefine;


define read_toplevel_form() -> form;
    lvars char;
    lispreadform() -> form;
    unless form == termin do
        if top_level_listen() then
            unless (read_standard_input() ->> char) == `\n` do
                char -> read_standard_input()
            endunless
        endif
    endunless
enddefine;


define top_level_eval(form) -> results;
    plus2 -> plus3;
    plus1 -> plus2;
    minus1 -> plus1;
    form -> minus1;
    conslist(sysCOMPILE(form, lispEVAL)) -> results;
    slash2 -> slash3;
    slash1 -> slash2;
    results -> slash1;
    star2 -> star3;
    star1 -> star2;
    car(slash1) -> star1;
enddefine;


define top_level_print(results);
    SET_CUCHAROUT;
    if results == nil then
        cucharout(`\n`)
    else
        applist(results, npr)
    endif
enddefine;


define vars top_level_before_hook(form);
    lvars pdr;
    if atom(form) and (is_magic_word(form) ->> pdr) then
        apply_magic_word_handler(pdr)
    else
        false
    endif
enddefine;


define vars top_level_after_hook(results);
    false
enddefine;


define top_level_loop();
    lvars form, results;
    dlocal
        poptraceindent  = 0,
        top_level_forms = pdtolist(read_toplevel_form),
        ;
    until null(top_level_forms) do
        dest(top_level_forms) -> (form, top_level_forms);
        nextif(top_level_before_hook(form));
        top_level_eval(form) -> results;
        nextif(top_level_after_hook(results));
        top_level_print(results)
    enduntil;
    true
enddefine;


/* Non-interactive top level */

vars
    load_lock       =   nil,
    load_print      =   nil,
    load_verbose    =   true,
    ;


propsheet_idents
    load_lock, load_print, load_verbose;


define lconstant Filecompile();
    lvars form, nresults;
    until (lispreadform() ->> form) == termin do
        sysCOMPILE(form, lispEVAL) -> nresults;
        if load_print == nil then
            erasenum(nresults)
        else
            top_level_print(conslist(nresults))
        endif
    enduntil;
    if load_lock /== nil then
        sysgarbage();
        sys_lock_heap()
    endif;
    true
enddefine;


/* lisp_compile itself */

define global vars lisp_compile(input);

    if input == read_standard_input then
        charin -> input
    endif;

    dlocal
        cucharin            =   read_standard_input,
        cucharout           =   write_standard_output,
        cucharerr           =   write_error_output,
        breaklevel,
        interrupt           =   lisp_interrupt,
        is_magic_word,
        LISP_N_ARGS,
        pop_default_type,
        pop_exception_final = lisp_exception_final,
        popdprecision       =   "ddecimal",
        popfilename         =   false,
        poplinenum          =   1,
        poplineprefix       =   false,
        popprompt,
        popradians          =   true,
        standard_input,
        ;

    RESET_PRINT_VARS;

    /* Ensure pop_default_type is set correctly */
    default_pathname -> default_pathname;

    if caller(1) == break then
        breaklevel + 1;
        procedure(form);
            db_magic_word_handler(form) or magic_word_handler(form)
        endprocedure,
        '<' sys_>< breaklevel sys_>< '> '
    else
        0;
        magic_word_handler;
        lispprompt
    endif -> popprompt -> is_magic_word -> breaklevel;

    if isstream(input) then
        input -> standard_input
    else
        unless isprocedure(input) do
            discin(input) -> input
        endunless;
        make_standard_input(input)
    endif;

    recursive_str_input(standard_input) -> input;
    recursive_front(pdprops(input)) -> popfilename;
    if isstring(popfilename) then
        Frig_usepop(popfilename)
    else
        false
    endif -> popfilename;

    if is_interactive_repeater(input) then
        lispCOMPILE(top_level_loop)
    else
        procedure();
            dlocal package, readtable;
            lispCOMPILE(Filecompile)
        endprocedure()
    endif
enddefine;


define global trylisp_compile(file);
    lvars dev;
    if (readable(file) ->> dev) then
        subsystem_compile(dev, "lisp");
        true
    else
        false
    endif
enddefine;


/* Nested Pop-11 compilation */

define global apply_in_pop11_environment(procedure p);
    dlocal
        pr = syspr,
        popradians = false,
        popdprecision = false,
        ;
    SET_CUCHARIN;
    SET_CUCHAROUT;
    SET_CUCHARERR;
    p()
enddefine;


define pop11_val() with_nargs 1;
    pop11_compile(stringin(get_simple_string()))
enddefine;


/* Wrapper for switching out of lisp_compile */

define switch_lisp_to(ss);
    if iscaller(lisp_compile) then
        recursive_str_input(standard_input) -> cucharin;
        if pdpart(cucharin) == buffercharin then
            fast_frozval(1, cucharin) -> cucharin
        endif;
        if ss == "prolog" and cucharin == charin then
            "top" -> ss
        endif
    endif;
    ss -> sys_compiler_subsystem(`c`);
enddefine;

switch_lisp_to(% "pop11" %) -> magic_word_handler(@POPLOG:POP11);


/* Lisp function LOAD */

define lisploadwarn(file, is_lib);
    if pop_true(load_verbose) then
        advise((if is_lib then 'LOADING LIB ' else 'LOADING ' endif)
                sys_>< file)
    endif
enddefine;


define lisp_load(input);
    dlocal subsystem_compile_warn = #_< lisploadwarn(% false %) >_#;
    unless isdevice(input) or isprocedure(input) do
        if isstream(input) then
            recursive_str_input(input)
        else
            checkr_filename(input)
        endif -> input
    endunless;
    apply_in_pop11_environment(input, false, subsystem_compile);
    true
enddefine;


/* Lisp function DRIBBLE */

define dribble(file);
    defaults file nil;
    if isprocedure(poplogfile) then
        advise('Logfile ~@[~S ~]closed', [% lisp_true(pdprops(poplogfile)) %]);
        poplogfile(termin);
        false -> poplogfile;
    endif;
    if file /== nil then
        checkr_filename(file) -> file;
        discappend(file) -> poplogfile;
        advise('Logfile ~S opened', [^file]);
    endif
enddefine;


/* ED & DISASSEMBLE (see also C.all/lisp/src/lispved.p) */

global vars pop_show_code = false;

define disassemble(f);
    lvars lam;
    dlocal pop_show_code = false;
    useslib("showcode");
    if (f starts_with @LAMBDA and (f ->> lam))
    or (isprocedure(f) and (f_lamex(f) ->> lam))
    or (f_lamex(symbol_function(f)) ->> lam)
    then
        true -> pop_show_code;
        compile_lambda(lam) ->
    else
     #_IF VED_LOADED
        symbol_name(f) -> vedfstring;
        advise('Use ED or <ENTER> F to find the source for ~S,\n;;; then use <ENTER> DMR', [^f])
     #_ELSE
        lisp_error('Can not locate source code for ~S', [^f])
     #_ENDIF
    endif
enddefine;


#_IF not(VED_LOADED)

define ed(file);
    defaults file nil;
    lisp_error('Editor not available', [^file])
enddefine;

#_ENDIF


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Mar  6 1996
        lisp_compile sets new variable poplineprefix false.
--- John Williams, Feb 15 1996
        Changes for new exception handler.
--- John Williams, Aug  9 1995
        Removed redundant lvar declarations.
--- John Williams, Jun  6 1995
        disassemble now uses f_lamex if available.
--- John Williams, Jun  1 1995
        Added Ved-independent definitions of ed and disassemble.
--- John Williams, Apr 12 1995
        Slight changes to lisp_load.
--- John Williams, Mar 30 1995
        Changes for CLtL 2 streams.
--- John Williams, Jun  7 1994
        Symbol POP11 now in POPLOG package.
--- John Williams, Jan 27 1994
        lisp_compile now checks whether its input stream argument is the
        procedure lisp_charin.
--- John Williams, Mar 18 1993
        Now uses sys_file_exists
--- John Gibson, Jan 15 1993
        Added extra false arg to subsystem_compile
--- John Williams, Jan 15 1993
        Revised for new subsystem implementation
--- John Williams, Mar  4 1992
        -lisp_load- now wraps -apply_in_pop11_environment- around the
        call to -subsystem_nest-
--- John Williams, Oct 29 1991
        Fixed BR isl-fr.4322
--- John Williams, Jul 17 1990
        Revised for new LIB SUBSYSTEM
 */
