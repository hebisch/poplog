/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/write.p
 > Purpose:         Common Lisp printing functions
 > Author:          John Williams, May 29 1987 (see revisions)
 > Documentation:   CLtL, p382-408
 > Related Files:   C.all/lisp/src/streams.p
 */

lisp_compile_mode;

section $-lisp;


define write(item, standard_output, before, after) -> item;
    dlocal standard_output;
    if before /== nil then
        write_standard_output(char_code(before))
    endif;
    lisp_pr(item);
    if after /== nil then
        write_standard_output(char_code(after))
    endif
enddefine;


define write_byte(byte, stream) -> byte;
    fast_apply(byte, str_output(stream))
enddefine;


define write_char(char, standard_output) -> char;
    dlocal standard_output;
    write_standard_output(char_code(char))
enddefine;


define fast_write_string(string, standard_output) -> string;
    dlocal standard_output;
    write_standard_output(string)
enddefine;


define write_string(string, standard_output, lo, hi, flag) -> string;
    lvars str, i;
    dlocal standard_output;
    SET_CUCHAROUT;

    if isstring(string) and lo == nil and hi == nil then
        cucharout(string)
    else
        checkr_string_ends(string, lo, hi, false) -> str -> lo -> hi;
        fast_for i from lo to hi do
            cucharout(fast_subscrs(i, str))
        endfast_for
    endif;
    if flag /== nil then
        cucharout(`\n`)
    endif
enddefine;


define fresh_line(standard_output);
    dlocal cucharout, standard_output;
    if is_pp_stream(standard_output) then
        make_pp_event(PPE_FRESH_LINE);
        nil
    else
        recursive_stream_write_p(standard_output) -> cucharout;
        if (cucharout == charout and pop_charout_col == 0)
        or (cucharout == charerr and pop_charerr_col == 0)
        then
            nil
        else
            cucharout(`\n`);
            true
        endif
    endif
enddefine;


/* Interface to LIB FORMAT_PRINT */

vars error_print_array;     /* See C.all/lisp/src/errors.p */


define call_format_print(standard_output, string, args);
    dlocal standard_output, error_print_array = true;
    SET_CUCHAROUT;

    define dlocal pr() with_nargs 1;
        dlocal standard_output, pr = lisp_pr;
        if is_pp_stream(standard_output) then
            procedure() with_nargs 1;
                dlocal % PP_CHAROUT % = cucharout;
                pr()
            endprocedure()
        else
            make_standard_output(cucharout);
            pr()
        endif
    enddefine;

    unless isprocedure(string) do
        get_simple_string(string) -> string
    endunless;

    format_print(string, args, "lisp")
    /* Returns number of args used */
enddefine;


define format(stream, string, args);
    lvars getstring = false, n;
    if stream == true then
        standard_output -> stream
    elseunless isstream(stream) do
        stream == nil -> getstring;
        make_stringout_stream(stream) -> stream
    endif;
    if isprocedure(string) then
        stream, destlist(args) fi_+ 1 -> n;
        lisp_apply(string, n, 1)
    else
        call_format_print(stream, string, args) ->;
        if getstring then
            get_stringout_string(stream)
        else
            nil
        endif
    endif
enddefine;


define global format_apply(arg, params, colon, at, pname);
    ;;; Called by apply_directive in LIB * FORMAT_PRINT to handle ~/.../

    checkr_function(@READ-FROM-STRING(pname, 1) ->) -> pname;
    standard_output, arg, lisp_true(colon), lisp_true(at), appdata(params, lisp_true);
    lisp_apply(pname, fast_vector_length(params) + 4, 0)
enddefine;


define advise(string);
    lvars args;
    dlocal standard_output = debug_io;
    SET_CUCHAROUT;
    cucharout(`;`);
    cucharout(`;`);
    cucharout(`;`);
    cucharout(`\s`);
    if isstring(string) then
        appdata(string, cucharout)
    else
        string -> args;
        -> string;
        call_format_print(debug_io, string, args) ->
    endif;
    cucharout(`\n`)
enddefine;


/* Lisp only format directives */

procedure(n);
    defaults n 0;
    pprint_indent(
        if fpr$-f_colon then
            @:CURRENT
        elseif fpr$-f_at then
            @:ABSOLUTE
        else
            @:BLOCK
        endif, n, false)
endprocedure -> fpr$-f_proc(`I`);


procedure();
    pprint_newline(
        if fpr$-f_colon then
            if fpr$-f_at then
                @:MANDATORY
            else
                @:FILL
            endif
        elseif fpr$-f_at then
            @:MISER
        else
            @:LINEAR
        endif, false)
endprocedure -> fpr$-f_proc(`_`);


procedure();
    dlocal print_pretty, print_length, print_level, standard_output;
    if fpr$-f_colon then
        true -> print_pretty
    endif;
    if fpr$-f_at then
        nil ->> print_level -> print_length
    endif;
    fpr$-next_f_arg();
    if is_pp_stream(standard_output) then
        procedure() with_nargs 1;
            dlocal % PP_CHAROUT % = cucharout;
            _lisppr()
        endprocedure()
    else
        make_standard_output(cucharout);
        _lisppr()
    endif
endprocedure -> fpr$-f_proc(`W`);


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Sep  1 1995
        Added format_apply, for ~/.../ format directive.
--- John Williams, Aug  8 1995
        Removed redundant lvar declarations.
--- John Williams, Mar 30 1995
        Changes for CLtL 2 streams.
--- John Williams, Dec 16 1994
        format now redefines pr to ensure that standard_output is set up
        from the current value of cucharout. This fixes BR glennk.1,
        BR isl-fr.4426, and BR isl-fr.4526.
--- John Williams, Aug 31 1993
        Extra argument to checkr_string_ends.
--- John Williams, Feb 21 1992
        Changed -check_string- to -checkr_string_ends- (cf BR isl-fr.4412)
 */
