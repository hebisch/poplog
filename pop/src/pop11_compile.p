/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/pop11_compile.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF * POPCOMPILE
 */

;;; --------------------- POP11 COMPILER ------------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'ident.ph'
#_INCLUDE 'dirs.ph'
#_INCLUDE '../lib/include/vm_flags.ph'

constant
        procedure (isreal, sys_process_poparg1, subsystem_compile,
        pop11_need_nextitem,
        Sys$-Prglst$-Next_valof, Sys$-Prglst$-Next_is_expr_opener,
        Sys$-Prglst$-Next_is_equal),
    ;

vars
        procedure (pop11_define_declare, pop11_define_props,
        sysEXEC_COMPILE, sysCOMPILE),
        poplastitem, pop_vm_flags, pop_default_type,
        popdprecision, pop_syntax_only, pop_autoload,
        Sys$-Pop11$-loop_starts, Sys$-Pop11$-loop_ends,
        Sys$-Pop11$-cons_constant, Sys$-Prglst$-was_nonmac,
        Sys$-Prglst$-next_ident, Sys$-Prglst$- _next_code
    ;

weak constant
        procedure ($-Sys$-Ved$-Pop11_vedsetup)
    ;

    ;;; Force inclusion of syntax words in
    ;;; pop11_syntax.p, pop11_loops.p, pop11_sections.p
uses (Sys$-Pop11$-Syntax, Sys$-Pop11$-Loops, Sys$-Pop11$-Sections);


;;; -------------------------------------------------------------------

vars
    pop_pop11_flags     = 0,
    procedure pop_expr_inst,
    pop_expr_item,
    pop_expr_prec,
    pop_expr_update,
    ;

protected vars
    popclosebracket         = termin,
    popclosebracket_exec    = termin,
    ;

vars
    ;;; stacklength on entry to pop11_compile
    Sys$-Pop11$-compile_stacklen    = 0,
    ;

lconstant macro (
    PL      = [$-Sys$-Prglst$-],    ;;; proglist section
    POP11   = [$-Sys$-Pop11$-],     ;;; POP11 section
    );


define is_sub_syntax_word(item, construct_name);
    lvars item, construct_name;
    dlocal proglist, PL was_nonmac, PL next_ident, PL _next_code;
    returnunless(isword(item)) (false);
    ;;; process word thru proglist for autoloading (but preventing
    ;;; macro expansion)
    item :: proglist ->> proglist -> PL was_nonmac;
    itemread() -> item;
    if PL _next_code _bitst _:M_ID_SYNTAX then
        PL Next_valof(item) -> item;
        ;;; sub-syntax word encoded as [<construct_name>|<procedure>]
        if ispair(item) and fast_front(item) == construct_name
        and isprocedure(fast_back(item) ->> item) then
            return(item)
        endif
    endif;
    false
enddefine;

define Sys$-Pop11$-Test_missing_sep(item);
    lvars item;
    if PL Next_is_expr_opener(item) then
        mishap(poplastitem, item, 2, 'msep: MISSING SEPARATOR (eg semicolon)',
                                            'pop11-msep:syntax')
    endif
enddefine;


    ;;; dummy procedures for indicating that there is no
    ;;; instruction in -pop_expr_inst- to flush
define pop11_FLUSHED(dummy); lvars dummy; enddefine;
;;;
define updaterof pop11_FLUSHED(dummy);
    lvars dummy;
    mishap(0, 'iue: IMPERMISSIBLE UPDATE EXPRESSION (e.g. after -> or ->>)',
                                            'pop11-iue:syntax')
enddefine;

define pop11_EMPTY(dummy); lvars dummy; enddefine;
;;;
define updaterof pop11_EMPTY(dummy);
    lvars dummy;
    if isprocedure(pop_expr_update) then
        ;;; empty update expression, run the given procedure
        fast_apply(pop_expr_update)
    endif
enddefine;


  ;;; compile an expression upto a given precedence limit
define pop11_comp_prec_expr(pop_expr_prec, pop_expr_update) -> item;
    lvars item, _code, _opprec;
    dlocal
        pop_expr_prec,                  ;;; precedence limit for this expr
        pop_expr_update,                ;;; whether update mode expr
        pop_expr_inst = pop11_EMPTY,    ;;; instruction buffer
        pop_expr_item,                  ;;; arg for pop_expr_inst
        ;

    define lconstant Do_expr_syntax_opener(word);
        lvars word, p;
        dlocal POP11 loop_starts, POP11 loop_ends;
        if isprocedure(PL Next_valof(word) ->> p) then
            PL Chop();
            fast_apply(p);
            if pop_expr_inst == pop11_EMPTY then
                pop11_FLUSHED -> pop_expr_inst
            endif
        ;;; else empty expression
        endif
    enddefine;

    _checkall();            ;;; check for interrupts, recursion overflow etc
    nextitem() -> item;
    if (PL _next_code ->> _code) _bitst _:M_ID_IS_WORD then
        ;;; is a word - _next_code contains syntax/op identprops
        unless _code _bitst (_:M_ID_SYNTAX _biset _:M_ID_OPERATOR) then
            ;;; normal push of identifier
            item -> pop_expr_item;
            sysPUSH -> pop_expr_inst;
            PL Chop()
        elseunless _code _bitst _:M_ID_OPERATOR then
            ;;; syntax word
            unless PL Next_is_equal(item, popclosebracket) then
                Do_expr_syntax_opener(item)
            endunless
        else
            ;;; prefix operator -- test for special unary op
            if pop_expr_prec fi_> INTERNAL_OP_PREC 1
            and (Sys$-list_assoc_val(item,
                                    [- negate    +: unary_+:  -: unary_-:])
                                    ->> item) then
                ;;; unary -, +:, -:
                PL Chop();
                pop11_comp_prec_expr(INTERNAL_OP_PREC 1, false) -> ;
                item -> pop_expr_item;
                sysCALL -> pop_expr_inst
            ;;; else let it fall through
            endif
        endunless
    elseif item /== termin then
        ;;; push of number or structure, etc
        item -> pop_expr_item;
        sysPUSHQ -> pop_expr_inst;
        PL Chop()
    ;;; else empty expression
    endif;

    nextitem() -> item;

    ;;; deal with operators -- these may be either normal or syntax
    repeat
        if (PL _next_code ->> _code) _bitst _:M_ID_IS_WORD then
            ;;; is a word
            quitunless( _code _bitst _:M_ID_OPERATOR
                        and (PL next_ident!ID_NUM_ATTRIBUTE ->> _opprec)
                                    _lt _int(pop_expr_prec) );
            ;;; operator of precedence less than current, so grab it
            PL Chop();
            if _code _bitst _:M_ID_SYNTAX then
                ;;; syntax operator
                fast_apply(PL Next_valof(item));
                if pop_expr_inst == pop11_EMPTY then
                    pop11_FLUSHED -> pop_expr_inst
                endif;
                nextitem() -> item
            else
                ;;; ordinary operator
                pop_expr_inst(pop_expr_item);
                item -> pop_expr_item;
                sysCALL -> pop_expr_inst;
                pop11_comp_prec_expr(_pint(_opprec _biset _1), false) -> item
            endif
        elseif isreal(item) and item < 0 then
            ;;; asssume that binary minus was intended
            procedure();
                dlocal weakref popdprecision = true;
                negate()
            endprocedure(item) -> item;
            "-" :: (item :: fast_back(proglist)) -> proglist;
            nextitem() -> item
        else
            quitloop
        endif
    endrepeat;

    ;;; finished expression -- flush the current instruction
    ;;; if update mode, run the updater of the instruction procedure
    if pop_expr_update then
        if pop_expr_inst!PD_UPDATER ->> _code then
            fast_apply(pop_expr_item, _code)
        else
            ;;; produce mishap
            pop_expr_item -> pop11_FLUSHED()
        endif
    else
        pop_expr_inst(pop_expr_item)
    endif
enddefine;


lconstant macro LIMIT_PREC = INTERNAL_OP_PREC 12.8;

define pop11_comp_expr();
    pop11_comp_prec_expr(LIMIT_PREC, false) ->
enddefine;
;;;
define updaterof pop11_comp_expr();
    pop11_comp_prec_expr(LIMIT_PREC, true) ->
enddefine;

define pop11_comp_expr_to(popclosebracket);
    dlocal popclosebracket;
    pop11_comp_prec_expr(LIMIT_PREC, false) -> ;
    pop11_need_nextitem(popclosebracket)
enddefine;

define lconstant Comp_expr_seq(stack_proglists);
    lvars item, stack_proglists;
    repeat
        if stack_proglists then proglist endif;     ;;; for updater
        pop11_comp_prec_expr(LIMIT_PREC, false) -> item;
        quitunless(item == ",");
        PL Chop()
    endrepeat;
    unless item == ";" or PL Next_is_equal(item, popclosebracket) then
        POP11 Test_missing_sep(item)
    endunless
enddefine;

define pop11_comp_expr_seq();
    Comp_expr_seq(false)
enddefine;

    /*  Compile expression sequence in update mode -- expressions
        are evaluated backwards.
    */
define Sys$-Pop11$-Comp_upd_expr_seq(upd_arg);
    lvars upd_arg, save_syntax_only = pop_syntax_only;
    dlocal pop_syntax_only = true;
    popstackmark;
    Comp_expr_seq(true);
    save_syntax_only -> pop_syntax_only;
    procedure;
        dlocal proglist;
        until (->> proglist) == popstackmark do
            pop11_comp_prec_expr(LIMIT_PREC, upd_arg) ->
        enduntil
    endprocedure()
enddefine;

define pop11_comp_expr_seq_to(popclosebracket);
    dlocal popclosebracket;
    pop11_comp_expr_seq();
    pop11_need_nextitem(popclosebracket)
enddefine;

define pop11_comp_stmnt_seq();
    lvars item;
    repeat
        pop11_comp_prec_expr(LIMIT_PREC, false) -> item;
        quitunless(item == "," or item == ";");
        PL Chop()
    endrepeat;
    unless PL Next_is_equal(item, popclosebracket) then
        POP11 Test_missing_sep(item)
    endunless
enddefine;

define pop11_comp_stmnt_seq_to(popclosebracket);
    dlocal popclosebracket;
    pop11_comp_stmnt_seq();
    pop11_need_nextitem(popclosebracket)
enddefine;

define pop11_exec_stmnt_seq_to(closebracket);
    lvars closebracket;
    dlocal popclosebracket, popclosebracket_exec;

    /*  The following ensures that
            popclosebracket == popclosebracket_exec
        can only be true when NOT inside any other syntax procedures that
        locally alter the value of popclosebracket, i.e. the above test
        can be used to recognise 'execute' context in constructs like
        section and lblock etc.
    */
    if ispair(closebracket) then
        copy(closebracket)
    elseif closebracket == termin then
        termin      ;;; don't bother to put this in a list
    else
        closebracket :: []
    endif ->> popclosebracket -> popclosebracket_exec;

    repeat
        pop11_comp_expr_seq();
        quitunless(nextitem() == ";");
        PL Chop();
        sysEXECUTE()            ;;; to execute the code
    endrepeat;

    sysEXECUTE();
    pop11_need_nextitem(closebracket)
enddefine;

lconstant macro EXEC_LOCALS = [
        poplastitem,
        POP11 loop_starts = [],
        POP11 loop_ends = [],
        POP11 compile_stacklen = stacklength(),
        POP11 cons_constant,
        PL was_nonmac
    ];

define pop11_comp_stream();
    dlocal  EXEC_LOCALS,
            pop_pop11_flags,
            pop_vm_flags = pop_vm_flags &&~~ VM_POP11_ALWAYS_CLEAR,
            pop11_define_declare,
            pop11_define_props,
            PL next_ident, PL _next_code = _0,
        ;

    sysCOMPILE(termin, pop11_exec_stmnt_seq_to) ->
enddefine;

    /*  Evaluate expression(s) at compile-time
    */
define pop11_exec_compile(compile_p, flags);
    lvars compile_p, flags;
    dlocal  EXEC_LOCALS,
            pop_autoload = true,    ;;; enable autoloading
        ;

    sysEXEC_COMPILE(compile_p, flags)
enddefine;

define pop11_compile(stream);
    lvars   stream;
    dlocal  pop_default_type = '.p',
            popprompt = ': ',
            proglist_state = proglist_new_state(stream);
    pop11_comp_stream()
enddefine;

define trycompile(file);
    lvars file;
    ;;; file is a file specification
    if readable(file) ->> file then
        subsystem_compile(file, "pop11"), true
    else
        false
    endif
enddefine;



;;; --- Pop-11 SUBSYSTEM --------------------------------------------------

vars procedure popsetpop = identfn;

define lconstant Pop11_poparg1 =
    sys_process_poparg1(% pop11_compile, trycompile, '.p' %)
enddefine;

define lconstant Pop11_reset();
    popsetpop();
    if systrmdev(pop_charin_device) then
        appdata('\nSetpop\n', charout)
    endif
enddefine;

define sysinitcomp();

    define lconstant Compile_init_files(fname);
        lvars fname;
        trycompile(POPSYS dir_>< fname) ->;
        unless trycompile(POPLIB dir_>< fname) then
            trycompile(fname) ->
        endunless;
    enddefine;

    Compile_init_files('init.p');

    if popunderx then
        Compile_init_files('initx.p')
    endif
enddefine;

lconstant Pop11_subsystem_procedures =
    {%  pop11_compile,      ;;; SS_COMPILER
        Pop11_reset,        ;;; SS_RESET
        identfn,            ;;; SS_SETUP
        identfn,            ;;; SS_BANNER
        sysinitcomp,        ;;; SS_INITCOMP
        Pop11_poparg1,      ;;; SS_POPARG1
        VED_WEAK $-Sys$-Ved$-Pop11_vedsetup,    ;;; SS_VEDSETUP
        identfn             ;;; SS_XSETUP
    %};

;;; Force Pop-11 to be first in sys_subsystem_table
declare_incremental list[prec= -100] sys_subsystem_table;

subsystem_add_new(
        "pop11",
        Pop11_subsystem_procedures,
        '.p',
        ': ',
        [],
        'Pop-11'
);



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 11 1996
        Changed to use Next_is_equal for testing popclosebracket
--- John Gibson, Apr  3 1996
        Added some mishap id-strings
--- John Gibson, Nov  9 1995
        Removed pw*m stuff
--- John Gibson, May 16 1994
        Made test for popclosebracket inside pop11_comp_prec_expr deal with
        it being a list
--- John Gibson, Aug 19 1993
        Made pop11_comp_stream clear the flags VM_POP11_ALWAYS_CLEAR in
        pop_vm_flags, instead of clearing them all.
--- John Gibson, May 20 1993
        Moved proglist utility procedures pop11_try_nextitem etc to
        new file pop11_proglist_util.p, so using them doesn't incorporate
        the compiler.
--- John Gibson, Apr 26 1993
        Uses subsystem_add_new
--- John Williams, Jan 18 1993
        Declared pop11 subsystem writeable
--- John Gibson, Jan 11 1993
        o Added pop11 subsystem
        o com*pile -> pop11_compile
        o Moved load syntax to autoloadable library
--- John Gibson, Oct 17 1992
        Exported is_sub_syntax_word
--- John Gibson, Jun 20 1992
        Undid last change -- sys*prmessage is intended for things that are
        errors/warnings, whereas sysloadwarning is not really a "warning", it`s
        just information. (The point is that sys*prmessage does things like
        raise the basewindow in XVed, which you don't want for this.)
--- John Williams, Jun  4 1992
        -sysloadwarning- now uses -sys*prmessage- (at JonM's suggestion)
--- John Gibson, Sep 17 1991
        Removed buggy optimisation of pop11_comp_stmnt_seq_to to
        pop11_exec_stmnt_seq_to from pop11_exec_compile
--- John Gibson, Apr 16 1991
        Changed -com*pile- to use -proglist_state- and -proglist_new_state-.
        This will now compile a list, so moved -popval- out to the library.
--- Simon Nichols, Nov  8 1990
        Changed mishap codes to lower case.
--- John Gibson, Nov  6 1990
        Changed mishap message in updater of -pop11_FLUSHED-.
--- John Gibson, Aug 27 1990
        Changed n*ote to weak
--- John Gibson, Jul  7 1990
        Renamed -C*ompile_time_eval- as exported -pop11_exec_compile-
        (and using -sysEXEC_COMPILE-)
--- John Williams, Jul  6 1990
        Added variable -loadcompiler- (so that LIB SUBSYSTEM can change it)
--- John Gibson, Jul  5 1990
        Made -pop_autoload- locally true in -Compile_time_eval-.
--- Aaron Sloman, Jul  2 1990
        fixed silly typo
--- Aaron Sloman, Jul  2 1990
        Added -pop11_need_nextreaditem- and did a bit of tidying up.
--- John Gibson, Apr  9 1990
        Added -Comp_upd_expr_seq-.
--- John Gibson, Aug 11 1989
        Made -load- a syntax word.
--- John Gibson, Jun  2 1989
        Compiler-controlling variables popdefineconstant/procedure etc
        replaced with bits in -pop_pop11_flags-.
--- John Gibson, May 15 1989
        Included ident.ph
--- John Gibson, Apr 30 1989
        Vm procedures now in section $-Sys$-Vm.
--- John Gibson, Jan 29 1989
        Changes for 'weak' declarations
--- John Gibson, Nov  6 1988
        Added -popclosebracket_exec- to -pop11_exec_stmnt_seq_to-
--- John Gibson, Mar 27 1988
        -list_assoc_val- into section Sys
--- John Gibson, Mar  7 1988
        Renamed pop11_compile.p (was compile.p)
--- John Gibson, Mar  2 1988
        Made -pop11_comp_prec_expr- set -popdprecision- true when negating
        a negative literal.
--- John Gibson, Feb 21 1988
        Made -Compile_time_eval- use -sysEXECUTE- when possible.
--- John Gibson, Feb 14 1988
        weakref'ed -vedvedname- etc.
 */
