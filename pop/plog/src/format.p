/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/plog/src/format.p
 > Purpose:         Formatted printing
 > Author:          Robert John Duncan, Apr  8 1992
 > Documentation:   HELP * FORMAT
 > Related Files:   SRC * IOPREDS.P
 */


section prolog;

constant
    procedure ( prolog_display, prolog_writeq, prolog_print, prolog_atom, ),
;

;;; =======================================================================


define format(control, args);
    lvars control, args;

    if isword(control) then
        fast_word_string(control) -> control;
    endif;
    if isstring(control) then
        dlvars (s, i, lim) = (control, 1, datalength(control));
        define lvars next_char() -> c;
            lvars c;
            if i fi_<= lim then
                fast_subscrs(i, control) -> c;
                i fi_+ 1 -> i;
            else
                false -> c;
            endif;
        enddefine;
    else
        dlvars s = control;
        define lvars next_char() -> c;
            lvars c = false;
            unless s == [] then
                if ispair(s) then
                    prolog_deref(fast_front(s)) -> c;
                    prolog_deref(fast_back(s)) -> s;
                endif;
                unless isinteger(c) and c fi_>= 0 and c fi_<= 16:FF then
                    mishap(control, 1, 'ATOM OR STRING NEEDED');
                endunless;
            endunless;
        enddefine;
    endif;

    define lconstant next_arg() -> arg;
        lvars arg;
        if ispair(args) then
            prolog_deref(fast_front(args)) -> arg;
            prolog_deref(fast_back(args)) -> args;
        elseif isprologvar(args) or args == [] then
            mishap(0, 'TOO FEW ARGUMENTS FOR FORMAT');
        else
            (args, []) -> (arg, args)
        endif;
    enddefine;

    lconstant CONTROL_CHAR = `~`;

    define lconstant do_control();
        lvars   c, arg, count = false;
        dlocal  cucharout, pop_pr_places, pop_pr_exponent;

        ;;; initial count
        if isnumbercode(next_char() ->> c) then
            c fi_- `0` -> count;
            while isnumbercode(next_char() ->> c) do
                count * 10 + (c fi_- `0`) -> count;
            endwhile;
        elseif c == `*` then
            next_char() -> c;
            next_arg() -> count;
            unless isinteger(count) and count fi_>= 0 then
                mishap(count, 1, 'INTEGER >= 0 NEEDED');
            endunless;
        endif;

        ;;; term output
        if c == `p` then
            prolog_print(next_arg());
        elseif c == `w` then
            prolog_write(next_arg());
        elseif c == `q` then
            prolog_writeq(next_arg());
        elseif c == `k` then
            prolog_display(next_arg());
        elseif c == `i` then
            ;;; ignore
            erase(next_arg());

        ;;; numeric output
        elseif c == `d` then
            ;;; decimal integer
            round(next_arg()) -> arg;
            if count and count > 0 then
                ;;; shift right -count- decimal places
                if arg < 0 then
                    cucharout(`-`), abs(arg) -> arg;
                endif;
                arg sys_>< nullstring -> arg;
                datalength(arg) - count -> count;
                if count > 0 then
                    printf(allbutfirst(count, arg), substring(1, count, arg),
                        '%s.%s');
                else
                    cucharout(`0`), cucharout(`.`);
                    until count == 0 do
                        cucharout(`0`);
                        count + 1 -> count;
                    enduntil;
                    printf(arg);
                endif;
            else
                sys_syspr(arg);
            endif;
        elseif c == `e` or c == `E` or c == `f` then
            ;;; decimal float
            next_arg() -> arg;
            unless isdecimal(arg) then
                number_coerce(arg, 1.0d0) -> arg;
            endunless;
            if c == `E` then
                true -> pop_pr_exponent;
                lowertoupper <> cucharout -> cucharout;
            else
                c == `e` -> pop_pr_exponent;
            endif;
            min(16:FFFF, count or 6) || (`0` << 16) -> pop_pr_places;
            sys_syspr(arg);
        elseif c == `r` or c == `R` then
            ;;; based integer
            round(next_arg()) -> arg;
            if c == `r` and count and count > 10 then
                ;;; lower case alphabetic digits
                uppertolower <> cucharout -> cucharout;
            endif;
            radix_apply(arg, syspr, count or 8);

        ;;; typed output
        elseif c == `a` then
            ;;; atom
            unless prolog_atom(next_arg() ->> arg) then
                mishap(arg, 1, 'ATOM NEEDED');
            endunless;
            syspr(arg);
        elseif c == `c` then
            ;;; character code
            next_arg() -> arg;
            repeat count or 1 times
                cucharout(arg);
            endrepeat;
        elseif c == `s` then
            ;;; string
            next_arg() -> arg;
            while ispair(arg) and count /== 0 do
                cucharout(prolog_deref(fast_front(arg)));
                prolog_deref(fast_back(arg)) -> arg;
                if count then count - 1 -> count endif;
            endwhile;
            unless arg == [] or count == 0 then
                mishap(arg, 1, 'STRING NEEDED');
            endunless;

        ;;; formatting
        elseif c == `n` then
            ;;; newline
            repeat count or 1 times
                cucharout(`\n`);
            endrepeat;
        elseif c == CONTROL_CHAR then
            cucharout(CONTROL_CHAR);

        ;;; errors
        elseif not(c) then
            ;;; run out of characters
            mishap(control, 1, 'INCOMPLETE CONTROL STRING');
        else
            ;;; unrecognised
            mishap(consstring(CONTROL_CHAR, c, 2), 1,
                'ILLEGAL CONTROL SEQUENCE');
        endif;
    enddefine;

    lvars c;
    while next_char() ->> c do
        if c == CONTROL_CHAR then
            do_control();
        else
            cucharout(c);
        endif;
    endwhile;
enddefine;

define format0 =
    format(% [] %);
enddefine;

endsection;     /* prolog */
