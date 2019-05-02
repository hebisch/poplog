/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/destr.p
 > Purpose:         Generate code for macro lambda lists & DESTRUCTURING-BIND
 > Author:          John Williams, Jul  4 1994 (see revisions)
 > Documentation:   CLtL, ch8
 > Related Files:   C.all/lisp/src/defun.p, C.all/lisp/src/defs.lsp
 */

lisp_compile_mode;

section $-lisp;

fastprocs front, destpair, for;

lconstant
    Temp        =   @#:TEMP,
    Btemp       =   @#:BTEMP,
    Dummy       =   @#:DUMMY,
    ;


define do_destr(callform, default_default, name, lamlist, level);
    lvars checkend, dot, lamkeysym, var, init, svar, kargs, key, kc, keys;

    false ->> checkend ->> dot -> lamkeysym;

    define lconstant Skip_env();
        if lamlist starts_with @&ENVIRONMENT then
            if pop_true(name)
            and level == 0 then
                cddr(lamlist) -> lamlist
            else
                /* Signals an error */
                checkr_var(@&ENVIRONMENT) ->
            endif
        endif
    enddefine;

    define lconstant BIND(var, val);
        if ispair(var) then
            /* var is a nested destructuring lambda list */
            do_destr(val, default_default, name, var, level + 1)
        else
            [^var ^val]
        endif
    enddefine;

    unless issymbol(callform)
    and symbol_package(callform) == nil do
        gensymbol('L') -> var;
        BIND(var, callform);
        var -> callform
    endunless;

WHOLE:
    Skip_env();
    if lamlist starts_with @&WHOLE then
        destlamlist(lamlist) -> lamlist -> var -> lamkeysym;
        BIND(var, callform)
    endif;

    if pop_true(name)
    and level == 0 then
        /* Get the name given in the actual macro call */
        @#:NAME -> name;
        BIND(name, [^@POP ^callform])
    endif;

RARGS:
    Skip_env();
    until atom(lamlist) or islamkeysym(front(lamlist)) do
        destpair(lamlist) -> lamlist -> var;
        BIND(checkr_var(var),
             [^@IF ^callform
                   [^@POP ^callform]
                   [^@ERROR [^@QUOTE ^@PROGRAM-ERROR]
                    ^@:MESSAGE 'Missing ~A argument~@[ in ~A form~]'
                    ^@:INVOLVING [^@LIST [^@QUOTE ^var] ^name]]])
    enduntil;

OARGS:
    Skip_env();
    if lamlist starts_with @&OPTIONAL then
        destpair(lamlist) -> lamlist -> lamkeysym;
        until atom(lamlist) or islamkeysym(front(lamlist)) do
            destpair(lamlist) -> lamlist -> var;
            dest_oarg(var, default_default) -> (var, init, svar);
            if svar then
                BIND(var, [^@IF [^@SETQ ^Temp [^@CONSP ^callform]]
                                [^@POP ^callform]
                                ^init]);
                BIND(svar, Temp)
            else
                BIND(var, [^@IF ^callform [^@POP ^callform] ^init])
            endif
        enduntil
    endif;

REST:
    Skip_env();
    if atom(lamlist) and lamlist /== [] then
        /* Implicit &REST parameter */
        true -> dot;
        BIND(lamlist, callform)
    elseif lamlist starts_with @&REST
    or lamlist starts_with @&BODY then
        destlamlist(lamlist) -> lamlist -> var -> lamkeysym;
        if lamkeysym == @&BODY and isvector(var) then
            /* Poplog specific extension for parsing macro bodies */
            lblock;
                lvars len, v, i;
                if (datalength(var) ->> len) > 3 then
                    program_error('Malformed &BODY parameter', [^var])
                endif;
                BIND(Btemp,
                     [^@MULTIPLE-VALUE-CALL [^@FUNCTION ^@VECTOR]
                        [^@SYS:PARSE-BODY
                            ^callform ^(lisp_true(len == 3)) ^@NIL]]);
                for v with_index i in_vector var do
                    BIND(v, [^@SVREF ^Btemp ^(i - 1)])
                endfor
            endlblock
        else
            BIND(var, callform)
        endif
    else
        not(lamkeysym == @&WHOLE) -> checkend
    endif;

KEYS:
    Skip_env();
    if lamlist starts_with @&KEY then
        destpair(lamlist) -> lamlist -> lamkeysym;
        [% until atom(lamlist) or islamkeysym(front(lamlist)) do
            destpair(lamlist) -> lamlist
        enduntil %] -> kargs;
        if lamlist starts_with @&ALLOW-OTHER-KEYS then
            destpair(lamlist) -> lamlist -> lamkeysym;
            false -> kc
        else
            gensymbol('KC') -> kc;
            BIND(kc, [^@LENGTH ^callform])      /* Should check even length */
        endif;
        [] -> keys;
        for var in kargs do
            dest_karg(var, default_default) -> key -> var -> init -> svar;
            conspair(key, keys) -> keys;
            unless keywordp(key) do
                [^@QUOTE ^key] -> key
            endunless;
            if svar or kc then
                BIND(var,
                     [^@IF [^@EQ [^@SETQ ^Temp [^@GETF ^callform ^key ^pop_undef]]
                                 ^pop_undef]
                           ^init
                           ^(if kc then
                                [^@PROGN [^@SETQ ^kc [^@- ^kc 2]] ^Temp]
                             else
                                Temp
                             endif)]);
                if svar then
                    BIND(svar, [^@SYS:NEQ ^Temp ^pop_undef])
                endif
            else
                BIND(var, [^@GETF ^callform ^key ^init])
            endif
        endfor;
        if kc then
            BIND(Dummy,
                 [^@IF [^@SYS:NEQ ^kc 0]
                       [^bad_key_args [^@QUOTE ^keys] ^callform]])
        endif
    elseif checkend then
        BIND(Dummy,
             [^@IF ^callform
                   [^@ERROR [^@QUOTE ^@PROGRAM-ERROR]
                    ^@:MESSAGE 'Too many arguments supplied~@[ to macro ~A~]'
                    ^@:INVOLVING [^@LIST ^name ^callform]]])
    endif;

AUX:
    Skip_env();
    if (lamlist starts_with @&AUX)
    and level == 0 then
        destpair(lamlist) -> lamlist -> lamkeysym;
        until atom(lamlist) or islamkeysym(front(lamlist)) do
            destpair(lamlist) -> lamlist -> var;
            if islistlength(var, 2) then
                explode(var)
            elseif atom(var) then
                var, nil
            else
                program_error('Strange variable/value pair after &AUX', [^var])
            endif -> (var, init);
            BIND(checkr_var(var), init)
        enduntil
    endif;
END:
    Skip_env();
    check_lamlist_end(lamlist, lamkeysym, dot)
enddefine;


define make_destructuring_form(callform, env, default_default,
                                name, lamlist, decs, body);

    lvars l, var, bindings;

    [%  [^Temp ^nil];
        [^Btemp ^nil];

        if (lmember(@&ENVIRONMENT, lamlist) ->> l) then
            destlamlist(l) -> (, var, l);
            if lmember(@&ENVIRONMENT, l) then
                program_error('&ENVIRONMENT appears twice in lambda-list',
                              [^lamlist])
            endif;
            if pop_true(env) then
                [^var ^env]
            else
                /* Error signalled by do_destr above */
            endif
        endif;

        do_destr(callform, default_default, name, lamlist, 0)
    %] -> bindings;

    [^@LET* ^bindings
        [^@DECLARE [^@INLINE ^@POP]]
        ^^decs
        [^@SETQ ^Temp ^nil ^Btemp ^nil]     /* in case they hold garbage */
        ^^body]
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  8 1995
        Removed redundant lvar declarations.
--- John Williams, Mar 15 1995
        Now signals typed errors.
--- John Williams, Aug 25 1994
        Changes for Steele 1990 (Lisp version 1.6)
 */
