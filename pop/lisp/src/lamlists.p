/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/lamlists.p
 > Purpose:         Compiling code for lambda lists
 > Author:          John Williams, Nov  7 1985 (see revisions)
 > Documentation:   CLtL, ch 5.
 > Related Files:   C.all/lisp/src/defun.p
 */

lisp_compile_mode;

section $-lisp;

fastprocs front, back, destpair, lmember, ncrev, for, repeat, sysrepeat;


constant
    keyword_count_var       =   @#:KEY-COUNT,
    lambda_list_keywords    =   [^@&BODY ^@&WHOLE ^@&ENVIRONMENT
                                 ^@&OPTIONAL ^@&KEY ^@&REST ^@&AUX
                                 ^@&ALLOW-OTHER-KEYS],
    lambda_parameters_limit =   pop_max_int,
    ;


/* Checking procedures */


define islamkeysym() with_nargs 1;
    lmember(lambda_list_keywords)
enddefine;


define checkr_var(var) -> var;
    if islamkeysym(var) then
        program_error('Misplaced lambda-list keyword: ~A', [^var])
    endif
enddefine;


define destlamlist(lamlist) -> (lamkeysym, var, lamlist);
    ;;; Use if lambda-list starts with &REST, &BODY, &WHOLE, &ENVIRONMENT
    destpair(lamlist) -> lamlist -> lamkeysym;
    unless ispair(lamlist) do
        program_error('No parameter name after ~A', [^lamkeysym])
    endunless;
    checkr_var(destpair(lamlist) -> lamlist) -> var
enddefine;


define check_lamlist_end(lamlist, lamkeysym, allow_dot);
    if ispair(lamlist) then
        if lamkeysym then
            program_error('Unexpected form(s) after ~A', [^lamkeysym ^lamlist])
        else
            checkr_var(front(lamlist))
        endif
    elseunless lamlist == [] or allow_dot do
        program_error('Misplaced implicit &REST parameter: ~S', [^lamlist])
    endif
enddefine;


/* Parse lambda-list */

define parse_lamlist(lamlist, fni) -> (rargs, oargs, rest, kargs,
                                        allow_other_keys, aux);
    lvars fmin, fmax, lamkeysym;

    0 ->> fmin -> fmax;
    [] ->> rargs ->> oargs ->> kargs -> aux;
    false ->> rest ->> allow_other_keys -> lamkeysym;

RARGS:
    [% until atom(lamlist) or islamkeysym(front(lamlist)) do
        destpair(lamlist) -> lamlist;
        fmin fi_+ 1 -> fmin
    enduntil %] -> rargs;
OARGS:
    fmin -> fmax;
    if lamlist starts_with @&OPTIONAL then
        destpair(lamlist) -> lamlist -> lamkeysym;
        [% until atom(lamlist) or islamkeysym(front(lamlist)) do
            destpair(lamlist) -> lamlist;
            fmax fi_+ 1 -> fmax
        enduntil %] -> oargs
    endif;
REST:
    if lamlist starts_with @&REST then
        false -> fmax;
        destlamlist(lamlist) -> lamlist -> rest ->
    endif;
KEYS:
    if lamlist starts_with @&KEY then
        false -> fmax;
        destpair(lamlist) -> lamlist -> lamkeysym;
        [% until atom(lamlist) or islamkeysym(front(lamlist)) do
            destpair(lamlist) -> lamlist
        enduntil %] -> kargs;
        if lamlist starts_with @&ALLOW-OTHER-KEYS then
            destpair(lamlist) -> lamlist -> lamkeysym;
            true -> allow_other_keys
        endif
    endif;
AUX:
    if lamlist starts_with @&AUX then
        destpair(lamlist) -> lamlist -> lamkeysym;
        [% until atom(lamlist) or islamkeysym(front(lamlist)) do
            destpair(lamlist) -> lamlist
        enduntil %] -> aux
    endif;
END:
    check_lamlist_end(lamlist, lamkeysym, false);
    fill_function_info(fni, fmin, fmax, false);
enddefine;


/* Init-forms */

define lconstant Nc_dest_oarg(form, default) -> (var, init, svar);
    if ispair(form) then
        destlist(form)
    else
        form, 1
    endif;
    go_on (/* len */) to VAR VAR_INIT VAR_INIT_SVAR else ERR;
ERR:
    false;
VAR:
    default;
VAR_INIT:
    false;
VAR_INIT_SVAR:
    -> svar -> init -> var
enddefine;


define dest_oarg(form, default) -> (var, init, svar);
    Nc_dest_oarg(form, default) -> (var, init, svar);
    if var then
        checkr_var(var) ->
    else
        program_error('Malformed parameter', [^form])
    endif;
    if svar then
        checkr_var(svar) ->
    endif
enddefine;


/* &AUX parameters */

define compile_aux(aux);
    lvars var;
    for var in aux do
        if ispair(var) then
            unless destlist(var) == 2 then
                program_error('Strange variable/value pair after &AUX', [^var])
            endunless;
            compile_form(1);
            checkr_var() -> var
        else
            lispPUSHQ(nil)
        endif;
        lispLOCAL_POP(var)
    endfor
enddefine;


/* &KEY parameters */

define bad_key_args(fkeys, args);
    lvars badkeys, key;
    [] -> badkeys;
    until args == [] do
        destpair(args) -> (key, args);
        if key == @:ALLOW-OTHER-KEYS then
            returnif(pop_true(destpair(args) -> args))
        elseunless lmember(key, fkeys) then
            conspair(key, badkeys) -> badkeys
        endif;
        back(args) -> args
    enduntil;
    unless badkeys == [] do
        program_cerror(false, 'Unknown keywords passed to ~S',
                        [^(f_name(caller(1))) ^^(ncrev(badkeys))])
    endunless
enddefine;


define conskeylist(n) -> n;
    if testbit(n, 0) then
        conslist(n) -> n;
        program_error('Odd number of keyword arguments', [^n])
    else
        conslist(n)
    endif
enddefine;


define dest_karg(karg, default) -> key -> var -> init -> svar;
    dest_oarg(karg, default) -> (var, init, svar);
    if ispair(var) then
        if islistlength(var, 2)
        and issymbol(front(var)) then
            destlist(var) -> (key, var, )
        else
            program_error('Malformed &KEY parameter', [^karg])
        endif
    else
        conskeyword(var) -> key
    endif
enddefine;


define compile_karg(karg, keys, allow_other_keys) -> key;
    lvars init, var, svar, lab1, lab2;

    dest_karg(karg, nil) -> key -> var -> init -> svar;

    sysNEW_LABEL() -> lab1;
    sysNEW_LABEL() -> lab2;

    lispPUSHQ(key);
    lispPUSH(keys);
    sysCALLQ(list_assoc);
    sysOR(lab1);

    compile_form(init, 1);
    if svar then
        lispPUSHQ(nil)
    endif;
    sysGOTO(lab2);

    sysLABEL(lab1);
    unless allow_other_keys do
        lispPUSH(keyword_count_var);
        lispPUSHQ(2);
        sysCALLQ(nonop fi_-);
        lispPOP(keyword_count_var)
    endunless;
    if svar then
        lispPUSHQ(true)
    endif;

    sysLABEL(lab2);
    if svar then
        lispLOCAL_POP(svar)
    endif;
    lispLOCAL_POP(var)
enddefine;


define compile_kargs(kargs, keys, allow_other_keys);
    lvars karg, fkeys, lab;

    [% for karg in kargs do
        compile_karg(karg, keys, allow_other_keys)
    endfor %] -> fkeys;

    unless allow_other_keys do
        sysNEW_LABEL() -> lab;
        lispPUSH(keyword_count_var);
        lispPUSHQ(0);
        sysCALLQ(nonop ==);
        sysIFSO(lab);
        lispPUSHQ(fkeys);
        lispPUSH(keys);
        sysCALLQ(bad_key_args);
        sysLABEL(lab)
    endunless
enddefine;


/* Required, &OPTIONAL, &REST parameters */

define lconstant Check_oargs(oargs) -> optim;
    lvars oarg, init, svar;
    true -> optim;
    for oarg in oargs do
        dest_oarg(oarg, nil) -> (, init, svar);
        unless is_constant_value(init) and not(svar) do
            false -> optim
        endunless
    endfor
enddefine;


define compile_rest(offset, keys);
    lispNARGS();
    unless offset == 0 do
        sysPUSHQ(offset);
        sysCALLQ(nonop fi_-);
    endunless;
    if keys then
        sysCALLQ(conskeylist);
        lispLOCAL_POP(keyword_count_var)
    else
        sysCALLQ(conslist)
    endif
enddefine;


define compile_no_oargs(fmin, rest, keys);
    ;;; expects required parameter names to be on the stack */

    lispCHECK_NARGS(fmin, rest);
    if rest then
        compile_rest(fmin, keys);
        lispLOCAL_POP(rest)
    endif;
    repeat fmin times
        lispLOCAL_POP()
    endrepeat;
    lispSAVE_STKLEN(true)
enddefine;


define compile_optim_roargs(fmin, oargs, rest, keys);
    lvars fmax, morelab, labs, oarg, init;
    ;;; expects required parameter names to be on the stack */

    fmin fi_+ listlength(oargs) -> fmax;
    [% sysrepeat(fmax fi_+ 1, sysNEW_LABEL) %] -> labs;
    sysNEW_LABEL() -> morelab;
    lispNARGS();
    lispGO_ON(labs, morelab);

    ;;; do &rest
    if rest then
        sysLABEL(morelab);
        sysNEW_LABEL() -> morelab;
        compile_rest(fmax, keys);
        sysGOTO(morelab)
    endif;

    ;;; too few/many args
    repeat fmin times
        sysLABEL(destpair(labs) -> labs)
    endrepeat;
    unless rest do
        sysLABEL(morelab)
    endunless;
    sysCALLQ(runtime_nargs_error);

    ;;; push &optional defaults, stack parameter names
    for oarg in oargs do
        sysLABEL(destpair(labs) -> labs);
        Nc_dest_oarg(oarg, nil) -> (oarg, init, );
        compile_form(init, 1);
        oarg
    endfor;

    ;;; do pops
    sysLABEL(front(labs));
    if rest then
        lispPUSHQ(nil);
        sysLABEL(morelab);
        lispLOCAL_POP(rest)
    endif;

    repeat fmax times
        lispLOCAL_POP()
    endrepeat;
    lispSAVE_STKLEN(true)
enddefine;


define compile_roargs(fmin, oargs, rest, keys);
    lvars fmax, labs, morelab, oarg, svar, init;
    ;;; expects required parameter names to be on the stack */

    ;;; initialise &rest var
    if rest then
        lispPUSHQ(nil);
        lispLOCAL_POP(rest)
    endif;

    ;;; do first GO_ON
    fmin fi_+ listlength(oargs) -> fmax;
    [% sysrepeat(fmax fi_+ 1, sysNEW_LABEL) %] -> labs;
    sysNEW_LABEL() -> morelab;
    lispNARGS();
    lispGO_ON(labs, morelab);

    ;;; too few/many number of args
    repeat fmin times
        sysLABEL(destpair(labs) -> labs)
    endrepeat;
    unless rest do
        sysLABEL(morelab)
    endunless;
    sysCALLQ(runtime_nargs_error);
    if rest then
        sysLABEL(morelab);
        compile_rest(fmax, keys);
        lispPOP(rest)
    endif;

    ;;; optional arg pops
    ncrev(labs) -> labs;
    repeat destlist(oargs) times
        Nc_dest_oarg(nil) -> (oarg, , svar);
        sysLABEL(destpair(labs) -> labs);
        lispLOCAL_POP(oarg);
        if svar then
            lispPUSHQ(true);
            lispLOCAL_POP(svar)
        endif
    endrepeat;

    ;;; required arg pops
    sysLABEL(front(labs));
    repeat fmin times
        lispLOCAL_POP()
    endrepeat;

    lispSAVE_STKLEN(true);

    ;;; compile defaults for unsupplied &optional parameters
    [% sysrepeat(fmax fi_- fmin, sysNEW_LABEL) %] -> labs;
    sysNEW_LABEL() -> morelab;
    lispNARGS();
    unless fmin == 0 do
        sysPUSHQ(fmin);
        sysCALLQ(nonop fi_-);
    endunless;
    lispGO_ON(labs, morelab);

    for oarg in oargs do
        sysLABEL(destpair(labs) -> labs);
        Nc_dest_oarg(oarg, nil) -> (oarg, init, svar);
        compile_form(init, 1);
        lispPOP(oarg);
        if svar then
            lispPUSHQ(nil);
            lispPOP(svar)
        endif
    endfor;
    sysLABEL(morelab)
enddefine;


/* Compile code for whole lambda-list.
    N.B. this procedure side-effects Current_function_info and &_rest_var.
*/

define compile_lamlist(lamlist);
    lvars rargs, oargs, rest, kargs, allow_other_keys, aux, keys, tmp;

    parse_lamlist(lamlist, Current_function_info)
        -> (rargs, oargs, rest, kargs, allow_other_keys, aux);

    false -> tmp;
    if kargs == [] then
        false
    elseif rest then
        rest
    else
        lispNEW_VTOK() ->> tmp ->> rest
    endif -> keys;

    if oargs == [] then
        compile_no_oargs(destlist(rargs), rest, keys)
    else
        if Check_oargs(oargs) then
            compile_optim_roargs(destlist(rargs), oargs, rest, keys)
        else
            compile_roargs(destlist(rargs), oargs, rest, keys)
        endif
    endif;

    unless kargs == [] do
        compile_kargs(kargs, keys, allow_other_keys)
    endunless;

    unless aux == [] do
        compile_aux(aux)
    endunless;

    sys_grbg_list(rargs);
    sys_grbg_list(oargs);
    sys_grbg_list(kargs);
    sys_grbg_list(aux);
    rest -> &_rest_var;
    if tmp then
        lispPUSH(tmp);
        sysCALLQ(sys_grbg_list);
        tmp -> lispNEW_VTOK()
    endif
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  8 1995
        Removed redundant lvar declarations.
--- John Williams, Mar 15 1995
        Now signals typed errors.
--- John Williams, Aug 25 1994
        Changes for Steele 1990 (Lisp version 1.6)
--- John Williams, Feb 24 1994
        Keyword parameters no longer need to be keywords! (Steele 1990 p79).
 */
