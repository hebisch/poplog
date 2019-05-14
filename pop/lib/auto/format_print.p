/* --- Copyright University of Sussex 1998. All rights reserved. ----------
 > File:           C.all/lib/auto/format_print.p
 > Purpose:        Common Lisp style formatted printing
 > Author:         John Williams, July 1984 (see revisions)
 > Documentation:  HELP * FORMAT_PRINT,  CLtL 2 chapter 22.3.3
 > Related Files:  LIB * FORMAT_COMPILE, C.all/lisp/flib  (directory)
 */

section $-lisp$-fpr => format_print format_string;

vars
    f_string,       ;;; control string
    f_char,         ;;; current character in f_string
    f_args,         ;;; argument vector (NOT list)
    f_str_index,    ;;; index of f_char
    f_arg_index,    ;;; index of current item in f_args
    f_str_len,      ;;; length of f_string
    f_arg_len,      ;;; number (length) of f_args
    f_colon,        ;;; boolean; records presence of : modifier
    f_at,           ;;; boolean: records presence of @ modifier
    f_column,       ;;; current column of cursor (if outputting to disc)
    f_charout_col,  ;;; procedure to return column count
    f_tabsize,      ;;; size of tabs
    f_liblist,      ;;; directory where directive procedures defined
    f_subsystem,    ;;; name of current subsystem (e.g. "pop11", "lisp)
    ;

0 -> f_column;

[% '$usepop/pop/lisp/flib/' dir_>< nullstring %] -> f_liblist;

"pop11" -> f_subsystem;

global vars procedure $-lisp$-format_apply;


/* Return/update procedure associated with format directive character
    (autoload if necessary)
*/

lconstant procedure F_proc_table = newarray([0 127], false);

define f_proc(char) -> pdr;
    F_proc_table(char) -> pdr;
    if isword(pdr) then
        unless syslibcompile(pdr, f_liblist) do
            mishap(pdr, consstring(`~`, f_char, 2), 2,
                    'Format_print library file not found')
        endunless;
        F_proc_table(char) -> pdr;
    endif
enddefine;


define updaterof f_proc(pdr, char);
    if isnumbercode(char) then
        mishap(consstring(char, 1), 1, 'Non-numeric character needed')
    endif;
    if isprocedure(pdr)
    and isinheap(pdr)
    and not(pdprops(pdr)) then
        consstring(`~`, char, 2) -> pdprops(pdr)
    endif;
    pdr -> F_proc_table(char)
enddefine;


/* Error signalling procedure */

define f_error(string);
    mishap(consstring(f_char, 1), f_string, 2, string)
enddefine;


/* Read next character from f_string, placing it in f_char */

define next_f_char();
    f_str_index fi_+ 1 -> f_str_index;
    if f_str_index fi_> f_str_len then
        f_error('Unexpected end of format control string')
    endif;
    fast_subscrs(f_str_index, f_string) -> f_char
enddefine;


/* As above, but convert to uppercase (used for parsing directives) */

define lconstant Next_f_char();
    next_f_char();
    lowertoupper(f_char) -> f_char;
    if f_char == `V` then `v` -> f_char endif
enddefine;


/* Get next argument from f_args */

define next_f_arg();
    if f_arg_index > 0 then
        f_arg_index fi_+ 1
    else
        1
    endif -> f_arg_index;
    if f_arg_index fi_> f_arg_len then
        mishap(f_args, f_arg_index, 2, 'Too few arguments')
    endif;
    fast_subscrv(f_arg_index, f_args)
enddefine;


/* Parse an integer parameter */

define lconstant Parse_integer() -> num;
    lvars neg, got_digit;
    if f_char == `-` then
        true -> neg;
        next_f_char()
    else
        false -> neg;
        if f_char == `+` then
            next_f_char()
        endif
    endif;
    0 -> num;
    false -> got_digit;
    while isnumbercode(f_char) do
        true -> got_digit;
        (num * 10) + (f_char fi_- `0`) -> num;
        next_f_char()
    endwhile;
    unless got_digit do
        f_error('Ill-formed directive parameter')
    endunless;
    if neg then
        negate(num) -> num
    endif;
    f_str_index fi_- 1 -> f_str_index
enddefine;


/* Parse a directive */

define parse_directive(check_params) -> (params, colon, at, pdr);

    lvars colon = false, at = false, got_param = false, j, n_params;

    #| repeat
        Next_f_char();
        quitif(f_proc(f_char) ->> pdr);
        if f_char == `/` and f_subsystem == "lisp" then
            unless (locchar(`/`, f_str_index fi_+ 1, f_string) ->> j) do
                mishap(f_string, 1, 'No terminating / character')
            endunless;
            substring(f_str_index fi_+ 1, j fi_- f_str_index fi_- 1, f_string)
                -> pdr;
            j -> f_str_index;
            quitloop
        endif;
        if colon and at then
            f_error(':@ must be followed by directive character')
        elseif f_char == `:` then
            if colon then
                f_error('Only one ":" allowed in directive')
            endif;
            true -> colon
        elseif f_char == `@` then
            if at then
                f_error('Only one "@" allowed in directive')
            endif;
            true -> at
        elseif colon or at then
            f_error('Unexpected character after : or @')
        elseif f_char == `,` then
            if got_param then
                /* Comma follows parameter on stack - do nothing */
                false -> got_param
            else
                /* Comma follows previous comma - leave false on stack
                    in place of missing parameter */
                false
            endif
        else
            if (f_char == `'` and f_subsystem == "lisp")
            or (f_char == ``` and f_subsystem == "pop11") then
                next_f_char();
                f_char
            elseif f_char == `v` then
                "V"
            elseif f_char == `#` then
                "#"
            else
                Parse_integer()
            endif;
            true -> got_param
        endif
    endrepeat |# -> n_params;

    if check_params then
        consvector(n_params) -> params;
        if isprocedure(pdr)
        and (n_params fi_> pdnargs(pdr)) then
            mishap(pdr, params, 2, 'Too many parameters for format directive')
        endif
    else
        erasenum(n_params);
        nullvector -> params
    endif
enddefine;


define transform_param(item) -> item;
    if item == "#" then
        (f_arg_len fi_- f_arg_index) -> item
    elseif item == "V" then
        next_f_arg(), if f_subsystem == "lisp" then pop_true() endif -> item
    endif
enddefine;


define apply_directive(params, f_colon, f_at, pdr);
    lvars n_params, i;
    dlocal f_colon, f_at;

    datalength(params) -> n_params;
    fast_for i from 1 to n_params do
        transform_param(fast_subscrv(i, params)) -> fast_subscrv(i, params)
    endfor;

    if isstring(pdr) then
        format_apply(next_f_arg(), params, f_colon, f_at, pdr)
    else
        explode(params);
        fast_repeat (pdnargs(pdr) fi_- n_params) times
            /* Stack false for each missing parameter */
            false
        endrepeat;
        pdr()
    endif
enddefine;


define process_directive();
    lvars params, pdr;
    parse_directive(true) -> (params, f_colon, f_at, pdr);
    apply_directive(params, f_colon, f_at, pdr)
enddefine;


/* Process each character in f_string. */

define do_fpr_string(f_string, f_str_index, f_str_len);
    dlocal f_char, f_string, f_str_index, f_str_len, f_colon, f_at;
    if isprocedure(f_string) then
        fast_apply(f_string)
    else
        unless isstring(f_string) do
            mishap(f_string, 1, 'String needed')
        endunless;
        unless f_str_len do
            datalength(f_string) -> f_str_len
        endunless;
        fast_for f_str_index from f_str_index to f_str_len do
            fast_subscrs(f_str_index, f_string) -> f_char;
            if f_char == `~` then
                process_directive()
            else
                cucharout(f_char)
            endif
        endfor
    endif
enddefine;


/* Process f_string with f_args */

define do_fpr_args(string, f_args, start, finish) -> f_arg_index;
    dlocal f_args, f_arg_index, f_arg_len;
    if islist(f_args) then
        consvector(destlist(f_args)) -> f_args
    elseunless isvector(f_args) do
        mishap(f_args, 1, 'List or vector needed')
    endif;
    0 -> f_arg_index;
    datalength(f_args) -> f_arg_len;
    do_fpr_string(string, start, finish)
enddefine;


/* Column counting wrapper for cucharout
    (not used if cucharout is charout or charerr)
*/

define f_charout(char, procedure pdr);
    pdr(char);
    if char == `\n` then
        0 -> f_column
    elseif char == `\t` then
        f_column fi_+ (f_tabsize fi_- (f_column mod f_tabsize)) -> f_column
    elseunless (char == termin) or (char fi_< 32) do
        f_column fi_+ 1 -> f_column
    endif
enddefine;


/* The procedure format_print itself */

define global format_print(string, args);
    dlocal
        cucharout,
        f_column            =   0,      ;;; Is this right?
        f_subsystem         =   "pop11",
        f_tabsize           =   8,
        f_charout_col,
        pop_pr_quotes       =   false,
        pop_pr_radix        =   10,
        pop_pr_places       =   6,
        pop_pr_exponent     =   false,
        poplinewidth        =   poplinemax,
        ;

    if cucharout == charout then
        procedure; pop_charout_col endprocedure
    elseif cucharout == charerr then
        procedure; pop_charerr_col endprocedure
    else
        f_charout(% cucharout %) -> cucharout;
        procedure; f_column endprocedure
    endif -> f_charout_col;

    if isword(args) then
        args -> f_subsystem;
        string -> args;
        -> string
    endif;

    do_fpr_args(string, args, 1, false);
    unless f_subsystem == "lisp" then ->; endunless
enddefine;


define global format_string(string, args);
    lvars ss;
    dlocal cucharout = identfn;     /* Not safe from lisp */
    if isword(args) then
        args -> ss;
        string -> args;
        -> string
    else
        "pop11" -> ss
    endif;
    consstring(#| format_print(string, args, ss) |#)
enddefine;


/* Utilities for the procedures that implement individual directives
    (Clisp already defines some of these, hence the #_IF's)
*/


;;; SYNTAX PROCEDURE TO AID READABILITY OF CODE:
;;;     defaults foo baz;
;;; is equivalent to
;;;     unless foo do baz -> foo endunless;

#_IF identprops("defaults") /== "syntax"

define syntax defaults;
    lvars lab, var, val;
    until nextitem() == ";" do
        sysNEW_LABEL() -> lab;
        readitem() -> var;
        if var == "," then
            readitem() -> var
        endif;
        sysPUSH(var);
        sysIFSO(lab);
        pop11_comp_expr();
        sysPOP(var);
        sysLABEL(lab)
    enduntil
enddefine;

#_ENDIF


define check_positive(x);
    unless isinteger(x) and x fi_>= 0 do
        mishap(x, 1, 'Positive integer needed')
    endunless
enddefine;


define check_integer(x);
    unless isinteger(x) do
        mishap(x, 1, 'Integer needed')
    endunless
enddefine;


define check_char(x);
    unless isinteger(x) and (x fi_>= 0) and (x fi_< 128) do
        mishap(x, 1, 'Character code between 0 and 127 (inclusive) required')
    endunless
enddefine;


define cuch_chars(n, char);
    defaults n 1;
    fast_repeat n times cucharout(char) endrepeat
enddefine;


define cuch_string() with_nargs 1;
    appdata(cucharout)
enddefine;


/* Utilities for bracketed directives */

define fmatch_bracket(bra, ket) -> (i, j, colon, at);
    lvars nested = 1;

    /* Assumes f_str_index points to opening bra.
       Leaves f_str_index pointing to closing ket.
    */
    next_f_char();
    f_str_index -> i;

    until nested == 0 do
        if (locchar(`~`, f_str_index, f_string) ->> f_str_index ->> j) then
            parse_directive(false) -> (, colon, at, );
            if f_char == bra then
                nested fi_+ 1 -> nested
            elseif f_char == ket then
                nested fi_- 1 -> nested
            endif
        else
            mishap(f_string, 1, 'Missing Closing ' <> consstring(ket, 1))
        endif
    enduntil;
    j fi_- 1 -> j              /* j is index of char before final ~ */
enddefine;


define read_clauses(bra, ket) -> (clauses, first_clause_args,
                                           last_clause_colon);
    lvars level = 1, i, j, colon;
    false -> first_clause_args;
    pop_undef -> last_clause_colon;

    /* Assumes f_str_index points to opening bra.
       Leaves f_str_index pointing to closing ket.
    */

    next_f_char();
    f_str_index -> i;

    {% until level == 0 do
        if (locchar(`~`, f_str_index, f_string) ->> f_str_index ->> j) then
            parse_directive(false) -> (, f_colon, f_at, );
            if f_char == bra then
                level fi_+ 1 -> level
            elseif f_char == `;` and level == 1 then
                substring(i, j fi_- i, f_string);
                unless first_clause_args do
                    substring(j, f_str_index fi_- j fi_+ 1, f_string)
                        -> first_clause_args
                endunless;
                f_colon -> last_clause_colon;
                f_str_index fi_+ 1 -> i
            elseif f_char == ket then
                level fi_- 1 -> level
            endif
        else
            mishap('Missing closing ~~~A', [% consstring(ket, 1), f_string %])
        endif
       enduntil;
       substring(i, j fi_- i, f_string);
    %} -> clauses
enddefine;


define misplaced_directive(char);
    lvars mess, lab;
    lconstant MESS = writeable 'Misplaced ~X directive';
    copy(MESS) -> mess;
    char -> mess(12);
    mishap(f_string, 1, mess)
enddefine;


/* Format directive library file names (must be VMS compatible!) */

"f_AKS"  ->> f_proc(`A`) ->> f_proc(`K`) -> f_proc(`S`);

"f_DBOX" ->> f_proc(`D`) ->> f_proc(`B`) ->> f_proc(`O`) -> f_proc(`X`);

"f_R" -> f_proc(`R`);
"f_P" -> f_proc(`P`);
"f_T" -> f_proc(`T`);
"f_C" -> f_proc(`C`);
"f_E" -> f_proc(`E`);
"f_F" -> f_proc(`F`);
"f_G" -> f_proc(`G`);
"f_dollar" -> f_proc(`$`);

cuch_chars(% `\n` %)  -> f_proc(`%`);
cuch_chars(% `\^L` %) -> f_proc(`|`);
cuch_chars(% `~` %)   -> f_proc(`~`);

"f_odds" ->> f_proc(`&`) ->> f_proc(`\n`) ->> f_proc(`*`) -> f_proc(`?`);

"f_case"    -> f_proc(`(`);
"f_cond"    -> f_proc(`[`);
"f_justify" -> f_proc(`<`);
"f_clause"  -> f_proc(`;`);
"f_loop"    -> f_proc(`{`);
"f_escape"  -> f_proc(`^`);


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug 11 1998
        Removed my test directory (!) from f_liblist.
--- John Williams, Sep  1 1995
        parse_directive now recognises ~/.../ directive.
--- John Williams, Aug 23 1995
        Removed redundant lvar declarations.
--- John Williams, Dec 16 1994
        Various improvements.
--- John Williams, Jun 14 1993
        Fixed BR isl-fr.4519
--- John Gibson, Jan 18 1993
        Replaced uses of current_subsystem_name with
        sys_compiler_subsystem(`c`)
--- John Williams, Jun 30 1992
        Various procedures moved out to flib/{float,number}number_utils.p
--- John Williams, Mar 11 1992
        -do_sign- now returns sign if arg = 0 (cf BR isl-fr.4423)
--- John Williams, May  4 1989
        Added some declarations to -do_fpr_string- and -do_fpr_args-
--- John Gibson, Feb 14 1988
        Replaced -vednullstring- with -nullstring-
--- John Williams, Nov 23 1987
        -f_column- now local to -format_print-
--- John Williams, Mar 22 1986
        FORMAT_PRINT and FORMAT_STRING now take an optional third argument
        specifying the current subsystem
--- John Williams, Jul 15 1985
        NUMSIZE fixed to use POP_PR_RADIX
--- John Gibson, Nov 12 1985
        Removed use of lvar "sign"
 */
