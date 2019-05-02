/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lisp/src/errors.p
 > Purpose:         Common Lisp error signalling
 > Author:          John Williams, June 1 1987  (see revisions)
 > Documentation:   Common Lisp Manual, p428-437
 */

lisp_compile_mode;

section $-lisp => lisp_exception_final;

vars
    error_print_array   =   nil,
    error_print_length  =   5,
    error_print_level   =   3,
    last_error          =   false,
    lisp_calling_limit  =   nil,
    ;

propsheet_idents
    error_print_length, error_print_level, lisp_calling_limit;


define sys_message_sprintf(string, arglist) -> (string, arglist);
    /* Maybe better to convert printf string to format_print string?? */

    dlocal cucharout = identfn, pr = syspr;
    consstring(#| sys_message_printf(string, arglist) -> arglist |#)
         -> string
enddefine;


/* display_error - main error message printing procedure */


define lconstant syntax ~#;
    sysPUSHQ(format_compile(readitem(), "lisp"))
enddefine;


define vars display_error(arg1, args, iserr, breaking,
                     compiling, doing, file, line) -> breaking;
    dlocal
        print_array,
        print_length,
        print_level,
        ;
    RESET_PRINT_VARS;

    pop_true(iserr) -> iserr;           /* Sometimes called from Lisp */
    pop_true(breaking) -> breaking;     /* Ditto */

    define lconstant Fpr(formatter, args);
        dlvars formatter, args;
        pprint_logical_block(
            error_output, false, false, false, false,
            procedure();
                call_format_print(error_output, formatter, args)
            endprocedure)
    enddefine;

    unless error_print_array == @:IGNORE do
        error_print_array -> print_array
    endunless;
    unless error_print_length == @:IGNORE do
        error_print_length -> print_length
    endunless;
    unless error_print_level == @:IGNORE do
        error_print_level -> print_level
    endunless;

    if breaking then
        {^arg1 ^args ^iserr ^breaking
            ^compiling ^doing ^file ^line}
    else
        false
    endif -> last_error;

    if isstream(standard_output) then
        finish_output(standard_output) ->
    endif;

    write_error_output(`\n`);
    pprint_logical_block(
        error_output, false, false, false, '; ',
        procedure();

            pprint_logical_block(
                error_output, false, false, false, false,
                procedure();

                    if iserr then
                        appdata('MISHAP - ', write_error_output)
                    else
                        appdata('Warning - ', write_error_output)
                    endif;
                    if isinstance(arg1) then
                        /* Assume its a condition */
                        @PRINC(arg1, error_output, 2) ->
                    else
                        if issymbol(arg1) then
                            symbol_name(arg1) -> arg1;
                            conslist(destvector(args)) -> args
                        endif;
                        conspair(arg1, args) -> args;
                        call_format_print(              ;;; why not Fpr?
                            error_output,
                            if iserr then
                                ~# '~@?~^~I~:@_INVOLVING:  ~:I~@{~:W ~:_~}'
                            else
                                ~# '~@?~^~I~:@_Involving:  ~:I~@{~:W ~:_~}'
                            endif,
                            args)
                    endif
                endprocedure);

            if file then
                Fpr(if iserr then
                        ~# '~@I~:@_FILE     :  ~A'
                    else
                        ~# '~@I~:@_File     :  ~A'
                    endif,
                    {^file});
                if line then
                    Fpr(if iserr then
                            ~# '~@I~:@_LINE     :  ~D'
                        else
                            ~# '~@I~:@_Line     :  ~D'
                        endif,
                        {^line})
                endif
            endif;

            if compiling then
                Fpr(if iserr then
                        ~# '~@I~:@_COMPILING:  ~:W'
                    else
                        ~# '~@I~:@_Compiling:  ~:W'
                    endif,
                    {^compiling})
            endif;

            if doing and compiling /== doing then
                3 -> print_length;
                Fpr(if iserr then
                        ~# '~@I~:@_DOING    :  ~:W'
                    else
                        ~# '~@I~:@_Doing    :  ~:W'
                    endif,
                    {^doing})
            endif;

            unless breaking do
                lblock;
                    lvars c = 0, n = 2, p, name;
                    {%  while (caller(n) ->> p)
                        and (p /== top_level_loop) do
                            if (db_caller_name(p) ->> name) then
                                quitif(isinteger(lisp_calling_limit)
                                        and c fi_>= lisp_calling_limit);
                                name;
                                c fi_+ 1 -> c
                            endif;
                            n fi_+ 1 -> n
                        endwhile
                    %} -> args
                endlblock;
                Fpr(if iserr then
                        ~# '~^~@I~:@_CALLING  :  ~:I~@{~W ~:_~}'
                    else
                        ~# '~^~@I~:@_Calling  :  ~:I~@{~W ~:_~}'
                    endif,
                    args);
            endunless
        endprocedure);

    write_error_output(`\n`);
    unless breaking do
        write_error_output(`\n`)
    endunless;
    finish_output(error_output) ->
enddefine;


define report_error() with_nargs 4;
    /* Exported to Lisp as SYS:REPORT-ERROR */

    chain(Compiling, Doing, popfilename, poplinenum, display_error)
enddefine;


/* lisp_exception_final
    (assigned to pop_exception_final by lisp_compile)
*/

constant type_from_err_string =
    newmapping(
        [
         ['(BIG)INTEGER'            ^@INTEGER]
         ['BITVECTOR'               ^@SIMPLE-BIT-VECTOR]
         ['DECIMAL OR DDECIMAL'     ^@FLOAT]
         ['INTEGER'                 ^@FIXNUM]
         ['NUMBER(S)'               ^@NUMBER]
         ['PAIR'                    ^@CONS]
         ['PROPERTY'                ^@HASH-TABLE]
         ['RANDOM_STATE'            ^@RANDOM-STATE]
         ['REAL NUMBER(S)'          ^@REAL]
         ['STRING'                  ^@SIMPLE-STRING]
         ['VECTOR'                  ^@SIMPLE-VECTOR]
        ], 16, false, false);


define lconstant Is_type_error(mess);
    lvars i, sym;
    if isstring(mess) and (isendstring(' NEEDED', mess) ->> i) then
        substring(1, i - 1, mess) -> mess;
        find_symbol(mess, lisp_package) -> (sym, i);
        if (i == @:EXTERNAL and type_predicate(sym))
        or (type_from_err_string(mess) ->> sym) then
            return(sym)
        endif
    endif;
    false
enddefine;


define lconstant Message_trans(mess, idstring, inv) -> (mess, inv);
    lvars item = nil, type;
    if idstring = 'arith-2arg:arith-div0' then
        @DIVISION-BY-ZERO -> mess;
        {^@:OPERATION ^@/ ^@:OPERANDS ^inv} -> inv
    elseif isendstring(':arith-fltovf', idstring) then
        @FLOATING-POINT-OVERFLOW -> mess;
        ;;; need to get operator from callstack
        {^@:OPERANDS ^inv} -> inv
    elseif isstartstring('callstack:mem-', idstring)
    and boundp(@SYS:RLE-STORAGE-CONDITION) then
        valof(@SYS:RLE-STORAGE-CONDITION) -> item;
        mess -> slot_value(item, @:MESSAGE);
        item -> mess, [] -> inv
    elseif issubstring(':mem-', idstring)
    and boundp(@SYS:ROM-STORAGE-CONDITION) then
        valof(@SYS:ROM-STORAGE-CONDITION) -> item;
        mess -> slot_value(item, @:MESSAGE);
        item -> mess, [] -> inv
    elseif (Is_type_error(mess) ->> type) then
        for item in inv do
            quitunless(typep(item, type))
        endfor;
        @TYPE-ERROR -> mess;
        {^@:EXPECTED-TYPE ^type ^@:DATUM ^item} -> inv
    elseif idstring = 'vm-ident:name-ref-none' and isword(fast_front(inv)) then
        'Declaring Pop-11 variable `~A\'' -> mess;
        [% fast_word_string(fast_front(inv)) %] -> inv
    else
        returnunless(isstring(mess) and datalength(mess) fi_> 0);
        if mess(1) == `%` then
            sys_message_sprintf(mess, inv) -> (mess, inv)
        endif;
        /* Convert to lowercase (except for 1st character) */
        uppertolower(mess);
        if dup() == mess then copy() endif -> mess;
        lowertoupper(mess(1)) -> mess(1)
    endif
enddefine;


define lisp_exception_final(n, mess, idstring, severity);
    lvars inv;
    conslist(n) -> inv;
    unless (callstacklength(sys_raise_exception) ->> n) do
        control_error('lisp_exception_final called out of context',
                      [^mess ^inv ^idstring ^severity]);
        return
    endunless;
    if isvector(mess) then mess(1) -> mess endif;
    Message_trans(mess, idstring, inv) -> (mess, inv);
    if severity == `I` then
        chainfrom(mess, inv, n, advise)
    elseif severity == `W` then
        chainfrom(mess, inv, n, procedure(); warn() -> endprocedure)
    else
        if severity == `R` then
            chainfrom('Continue computation', mess, inv, n, lisp_cerror)
        else
            chainfrom(mess, inv, n, lisp_error)
        endif
    endif
enddefine;


/*
Procedures for calling Lisp error and cerror from Pop.
These have to allow for the fact that the Lisp functions may not be
defined yet.
Arguments arg1 and rest may be either a string and list,
or a condition type (symbol) and components (vector).
*/


define lisp_error(arg1, rest);
    if fboundp(@ERROR) then
        if issymbol(arg1) then
            @ERROR(arg1, destvector(rest) + 1)
        elseif isinstance(arg1) then
            @ERROR(arg1, 1)
        else
            @ERROR(@POPLOG:MISHAP, @:MESSAGE, arg1, @:INVOLVING, rest, 5)
        endif
    else
        if report_error(arg1, rest, true, break_ok(break_on_errors)) then
            break('(Error)')
        endif;
        Db_quit()
    endif
enddefine;


define lisp_cerror(cstring, arg1, rest);
    if fboundp(@CERROR) then
        if issymbol(arg1) then
            @CERROR(cstring, arg1, destvector(rest) + 2)
        elseif isinstance(arg1) then
            @CERROR(cstring, arg1, 2)
        else
            @CERROR(cstring, @POPLOG:MISHAP, @:MESSAGE, arg1,
                    @:INVOLVING, rest, 6)
        endif ->
    else
        if report_error(arg1, rest, true, break_ok(break_on_errors)) then
            break('(Cerror)')
        else
            Db_quit()
        endif
    endif
enddefine;


define warn(arg1, args);
    if fboundp(@WARN) then
        @WARN(@POPLOG:WARNING-WITH-INVOLVING,
                @:MESSAGE, arg1, @:INVOLVING, args, 5)
    else
        if report_error(arg1, args, false, break_ok(break_on_warnings)) then
            break('(Warning)')
        endif;
        nil
    endif
enddefine;


defprop warn_once_prop;

define warn_once(arg1, list);
    unless warn_once_prop(arg1) then
        true -> warn_once_prop(arg1);
        warn(arg1, list) ->
    endunless
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Sep  5 1996
        lisp_exception_final allows that mess may be a vector.
        Message_trans checks for DECLARING VARIABLE messages.
        Added sys_message_sprintf.
--- John Williams, May  9 1996
        Message_trans checks for message strings starting with `%`.
--- John Williams, Apr 17 1996
        Message_trans now looks at id-string in certain cases.
        lispprmishap removed.
--- John Williams, Feb 15 1996
        Changes for new exception handler.
--- John Williams, Aug 11 1995
        Removed redundant lvar declarations.
--- John Williams, Mar 17 1995
        lispprmishap now creates typed errors when possible.
--- John Williams, Mar 15 1995
        Now handles typed errors.
--- John Williams, Feb 27 1995
        Changes for the Condition system (Steele 1990 ch 29).
--- John Williams, Feb  9 1995
        Added primitive version of IGNORE-ERRORS.
--- John Williams, Jun  7 1994
        Added lisp_calling_limit.
--- John Williams, Apr 27 1994
        Added warn_once.
--- John Williams, Feb 24 1994
        Added error_print_array too.
--- John Williams, Jun 17 1993
        Added new variables error_print_length and error_print_level.
        Now uses lblock rather than permanent identifier when loading
        all the format_print routines.
 */
