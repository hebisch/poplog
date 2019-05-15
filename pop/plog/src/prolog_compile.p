/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/src/prolog_compile.p
 > Purpose:         Prolog: the compiler
 > Author:          Robert Duncan and Simon Nichols, Jan 1987. (see revisions)
 > Documentation:
 > Related Files:   C.all/plog/src/prolog_subsystem_procedures.p
                    C.all/plog/src/compile.p
 */


section prolog;

constant
    procedure ( see, tell, is_special_input, prolog_macro_apply,
        predicate_has_clauses, predicate_isdefined, predicate_define,
        prolog_sysinvoke, prolog_safeinvoke, ),
;

vars
    procedure ( readterm ),
    current_input, current_output, prolog_expand_macros, readenv,
;

weak vars
    prologfiletypes,
;

;;; ========================================================================

vars
    procedure prolog_interrupt = identfn,
        ;;; user hook
    procedure compilerin,
        ;;; the compiler's input stream
    prolog_compile_mode = "reconsult",
        ;;; current compile mode
    default_compile_mode = "reconsult",
        ;;; default compile mode
    prolog_tags = true,
        ;;; <true> if predicates should be tagged with file names
;

lvars
    include_stack = false,
        ;;; input stack for included files
;

;;; prolog_file_type:
;;;     default file extension for Prolog program files

define prolog_file_type() -> default;
    lvars default = '.pl';
    if testdef prologfiletypes then
        lvars filetypes = weakref prologfiletypes;
        if islist(filetypes) and not(null(filetypes)) then
            hd(filetypes) -> filetypes;
        endif;
        if isstring(filetypes) then filetypes -> default endif;
    endif;
enddefine;

;;; prolog_sysinterrupt:
;;;     default value for -interrupt- while compiling Prolog.

define prolog_sysinterrupt(interrupt);
    dlocal interrupt, cucharout = cucharerr;
    prolog_interrupt(); ;;; User definable -- default is -identfn-.
    sysflush(pop_charout_device);
    printf('\n;;; [execution aborted]\n');
    sysflush(pop_charerr_device);
    tell("user");
    interrupt();
enddefine;

;;; open_file:
;;;     open a file for compiling. We don't use see because we don't
;;;     want the file to end up in the open file table: it should remain
;;;     private to the compiler. The file argument is usually a word
;;;     (Prolog atom) or a string (when called from Pop-11).

define lconstant open_file(file) -> char_rep;
    lvars file, char_rep;
    unless is_special_input(file) ->> char_rep then
        unless isword(file) then
            mishap(file, 1, 'STREAM NAME NEEDED');
        endunless;
        word_string(file) -> file;
        ;;; try the file name as given
        lvars dev;
        if readable(file) ->> dev then
            discin(dev) -> char_rep;
        else
            ;;; try with a default extension
            if sys_fname_extn(file) = nullstring then
                file <> prolog_file_type() -> file;
            endif;
            discin(file) -> char_rep;
        endif;
    endunless;
enddefine;

;;; readclause:
;;;     read a clause for the compiler.

define lconstant readclause() -> (fn, arity, clause, readenv);
    lvars   fn, arity, clause;
    dlocal  readenv;

    ;;; horrible hack for macros as commands
    if prolog_expand_macros then
        lvars item = nextreaditem();
        if prolog_macro(item) ->> item then
            readitem() -> ;
            consprologterm(item, "quote", 1),
                consprologterm((),"ident prolog_macro_apply", 1),
                consprologterm((), "prolog_eval", 1),
                consprologterm((), ":-", 1) -> clause;
            (":-", 1) -> (fn, arity);
            return;
        endif;
    endif;

    lvars clause = readterm();
    lvars (fn, arity) = prolog_termspec(prolog_head(clause));
    returnif(isprocedure(clause));  ;;; command

    if predicate_has_clauses("term_expansion", 2)
    or fn == "-->" and arity == 2
    then
        ;;; pre-process the clause
        if predicate_isdefined("expand_term", 2) then
            lconstant expand_term_goal =
                writeable consprologterm(undef, undef, "expand_term", 2);
            lvars new_clause_var = prolog_newvar();
            clause -> fast_prolog_arg(1, expand_term_goal);
            new_clause_var -> fast_prolog_arg(2, expand_term_goal);
            if prolog_sysinvoke(expand_term_goal) then
                prolog_deref(new_clause_var) -> clause;
                prolog_termspec(prolog_head(clause)) -> (fn, arity);
            endif;
            ;;; clear goal args for GC
            undef ->> fast_prolog_arg(1, expand_term_goal)
                -> fast_prolog_arg(2, expand_term_goal);
        endif;
    endif;
enddefine;

;;; prolog_query:
;;;     evaluates a single Prolog goal interactively by giving it as an
;;;     argument to the predicate prolog_toplevel/2. The env is an
;;;     environment mapping variables in the goal to their names as
;;;     constructed by readterm.

define prolog_query(goal, env, from_prolog);
    lvars goal, env, from_prolog;
    if predicate_isdefined("prolog_toplevel", 2) then
        prolog_maketerm(goal, env, "prolog_toplevel", 2) -> goal;
    endif;
    if from_prolog then prolog_sysinvoke else prolog_safeinvoke endif(goal);
enddefine;

;;; prolog_directive:
;;;     evaluates a single Prolog goal without interaction. Uses
;;;     prolog_directive/1 if it's available.

define prolog_directive(goal, from_prolog);
    lvars goal, from_prolog;
    if predicate_isdefined("prolog_directive", 1) then
        prolog_maketerm(goal, "prolog_directive", 1) -> goal;
    endif;
    if from_prolog then prolog_sysinvoke else prolog_safeinvoke endif(goal);
enddefine;

;;; prolog_syscompile_warn:
;;;     applied to each filename processed by prolog_syscompile

vars procedure prolog_syscompile_warn = erase;

;;; prolog_load:
;;;     main compiler loop (redefined by POPC)

define vars prolog_load();
    lvars (clauses, last) = (false, false);
    lvars (fn, arity) = (undef, -1);
    dlocal include_stack = [];
    ;;; create a unique tag for this load
    lvars tag = consref(prolog_tags and popfilename);
    repeat
        lvars (clause_fn, clause_arity, clause, clause_env) = readclause();
        unless clause_fn == fn and clause_arity == arity then
            if clauses then
                ;;; generate code for the procedure
                predicate_define(fn, arity, clauses, tag,
                    prolog_compile_mode == "reconsult");
                sysEXECUTE();
                false ->> clauses -> last;
            endif;
            (clause_fn, clause_arity) -> (fn, arity);
        endunless;
        if isprocedure(fn) then
            ;;; command -- execute it
            fn();
        elseif fn == "end_of_file" and arity == 0 then
            quitif(include_stack == []);
            Destpair(include_stack) -> (compilerin, include_stack);
            see("inchan");
        elseif fn == ":-" and arity == 1 then
            ;;; directive
            prolog_directive(prolog_arg(1, prolog_head(clause)), true) -> ;
            see("inchan");
        elseif fn == "?-" and arity == 1 then
            ;;; explicit query
            prolog_query(prolog_arg(1, prolog_head(clause)), clause_env, true) -> ;
            see("inchan");
        elseif prolog_compile_mode == "query" then
            ;;; query
            prolog_query(clause, clause_env, true) -> ;
            see("inchan");
        else
            ;;; database clause -- add to the current list
            conspair(clause, []) ->>
                if clauses then Back(last) else clauses endif -> last;
        endif;
    endrepeat;
enddefine;

define lconstant prolog_syscompile(input, mode);
    lvars input, mode;

    ;;; install Prolog interrupt handler if it's not been done already
    unless pdpart(interrupt) == prolog_sysinterrupt then
        define dlocal interrupt =
            prolog_sysinterrupt(% interrupt %);
        enddefine;
    endunless;

    ;;; set compile mode for this file, and reset default for nested calls
    dlocal (prolog_compile_mode, default_compile_mode) = (mode, "reconsult");
    ;;; set file type and prompt appropriate to the mode
    dlocal pop_default_type = prolog_file_type();
    dlocal popprompt = if mode == "query" then '?- ' else '| ' endif;
    dlocal prolog_read_prompt = '|: ';
    ;;; open the input for compiling
    dlocal compilerin = open_file(input);
    ;;; set up standard proglist state
    dlocal proglist_state = proglist_new_state(compilerin);
    ;;; initialise I/O streams and start compiling
    dlocal current_input; see("inchan");
    unless current_output then tell("user") endunless;
    prolog_syscompile_warn(popfilename);
    sysCOMPILE(prolog_load);
enddefine;

;;; prolog_compile, prolog_toplevel_compile:
;;;     prolog and top subsystem compilers. These must chain
;;;     prolog_syscompile so that it becomes the first caller after
;;;     subsystem_compile allowing sys_compiler_subsystem to find the
;;;     right value of cucharin when switching subsystem.

define prolog_compile(/* input */) with_nargs 1;
    chain(/* input, */ default_compile_mode, prolog_syscompile);
enddefine;

define prolog_toplevel_compile(/* input */) with_nargs 1;
    chain(/* input, */ "query", prolog_syscompile);
enddefine;


;;; include/1:
;;;     splice a file (or list of files) into the compiler's input
;;;     stream

define include\/1(files);
    lvars files, n;
    unless include_stack then
        mishap(0, 'NOT INSIDE THE PROLOG COMPILER');
    endunless;
    prolog_deref(files) -> files;
    (#| while prolog_checkspec(files, ".", 2) do
            prolog_args(files) -> files;
        endwhile;
        unless files == [] then files endunless;
    |#) -> n;
    fast_repeat n times
        ;;; save current input
        compilerin :: include_stack -> include_stack;
        ;;; see the next
        open_file(/* from stack */) -> compilerin;
        see("inchan");
    endrepeat;
    chain(prolog_apply_continuation);
enddefine;

endsection;     /* prolog */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Aug  4 1993
        Enabled prolog_tags
--- Robert John Duncan, Aug  2 1993
        Added prologfiletypes
--- Robert John Duncan, Jul 15 1993
        Renamed from old "compile.p"; new "compile.p" contains the Prolog
        compiler/subsystem interface.
        Reorganised to have a single procedure for all three compilation
        modes: consult, reconsult and query.
        Added support for include/1.
        Subsystem-related stuff moved out to "prolog_subsystem_procedures.p".
--- John Gibson, May  1 1993
        Changed prolog_consult to use subsystem_compile
--- John Gibson, Jan 18 1993
        o Made prolog_compile and prolog_toplevel_compile constants instead of
          vars and removed dlocal setting of popcom*piler. Added dlocals
          for cucharin and added comment as to why.
        o Changed prolog_library to use subsystem_libcompile
        o Replaced use of c*urrent_subsystem_name with
          sys_compiler_subsystem(`c`).
--- John Gibson, Jan 12 1993
        Removed constant declarations for prolog_help*dirs etc.
--- Robert John Duncan, Dec  9 1992
        Removed protect/unprotect for pop{filename,linenum}
--- Robert John Duncan, May 15 1992
        Made compileable in a system without VED.
--- Simon Nichols, Apr  7 1992
        -prolog_predof- now takes predicate name and arity as arguments.
--- Robert Duncan, Jan 28 1992
        Change to values of spy state variables
--- Simon Nichols, Nov  6 1990
        Changed -popdevX- to -pop_charX_device-.
--- Robert John Duncan, Oct 22 1990
        Added exit code to -prolog_syscompile- to close the input stream
        on an abnormal exit.
--- Simon Nichols, Aug 30 1990
        Changed n*ote to weak.
--- Simon Nichols, Aug  6 1990
        Added declaration of -prolog_refdirs-
--- Simon Nichols, Jul 17 1990
        - Moved out subsystem stuff to "startup.p"
        - Added -prolog_trycompile-.
        - Changed -prolog_load- and -prolog_toplevel- to support the new
            implementation of commands.
--- Rob Duncan, Jun  6 1990
        Moved documentation search procedures out to "plogved.p"
--- Rob Duncan, Apr  9 1990
        Removed call to -guard_module- from -prolog_syscompile- (see
        "modules.p"); added reset of -prolog_modules- in -prolog_setprolog-
        instead.
--- Rob Duncan, Sep  6 1989
    Added check on assignment to -interrupt- in -prolog_syscompile-
--- Rob Duncan, Aug  8 1989
    - sectionised and added #_INCLUDEs for POPC;
    - added declarations for user hooks at top of file, previously in
        "prolog.p";
    - changed -prolog_sysinterrupt- to call the previous value of
        -interrupt-;
    - added -prolog_directive- to allow ":-" directives to be run even
        before 'prolog_directive/1' is defined;
    - rewrote -prolog_load- to use dynamic lists of clauses so that
        predicates are compiled as they're read; also made use of new
        -prolog_define_dynamic-;
    - made -prolog_load- react to -pop_pas_mode- and added a call to
        -sysEXECUTE- to ensure that all code planted is really executed;
    - added localising of -guard_module- in -prolog_syscompile-;
    - moved subsystem initialisation into a separate procedure and got
        rid of -prolog_setup_environment-.
--- John Gibson, Aug  3 1989
        Replaced -sysfiletype- with -sys_fname_extn-.
--- Simon Nichols, Mar 14 1989
    Added initial newline to "execution aborted" message.
--- Rob Duncan, Sep  1 1988
    Changed -prolog_setup_environment- to set -pop_vedhelplist- only if == []
    This fixes it to be the helplist existing when the prolog compiler is
    first called. Also added call to -sysfiletype- in -prolog_library-,
    added some more -lconstant- declarations and generally tidied up.
--- Simon Nichols, Aug 10 1988
    Changed -prolog_consult- to use -sysfileok- on the filename and also to
    append a '.pl' if the filetype is not 'pl'.
--- Rob Duncan, Mar 16 1988
    Renamed from ploginvo.p
--- Rob Duncan, Nov 16 1987
    Made -prolog_syscompile- close the input stream before and after
    compiling, to avoid leaving the file open in -input_file_table-.
    Removed calls to -closein- from -prolog_consult- and -prolog_library-.
--- Simon Nichols, Oct 28 1987
    Added a trailing '/' to each directory path in -prolog_setup_environment-
    to ensure that they are correctly interpreted on VMS systems.
--- Simon Nichols, Oct 20 1987
    Changed -prolog_sysinterrupt- to do a ``tell("user")'' to ensure that the
    current output is set correctly after an interrupt in all circumstances.
--- John Gibson, Sep 20 1987
    Added a call of -kill_prolog_defn- when reconsulting in -prolog_load-,
    so that any existing definition for the predicate is zapped before
    starting to read the new one (can save a lot of space for a large
    predicate).
--- Robert Duncan, Aug 27 1987
    changed the usage of -readable- in -prolog_consult- so that it takes a
    string rather than a word, as the VMS version of -readable- won't accept
    anything else.
 */
