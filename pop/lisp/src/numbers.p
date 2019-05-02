/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/numbers.p
 > Purpose:         Common Lisp numeric routines not defined in POPLOG core
 > Author:          John Williams, Jan 29 1985 (see revisions)
 > Documentation:   CLtL, p193 ff
 > Related Files:   C.all/lisp/src/bitwise.p
 */

lisp_compile_mode;

section $-lisp;

define lconstant Number_needed =
    type_error(% @NUMBER %)
enddefine;


lconstant macro CHECK_NUM_ON_STACK
    = [unless isnumber(dup()) do Number_needed() endunless];


/* Predicates on Numbers */

define num_=() with_nargs 2;
    lvars y;
    CHECK_NUM_ON_STACK -> y;
    CHECK_NUM_ON_STACK;
    sys_=(y)
enddefine;


/* Comparisons on Numbers */

define compare_nums(N, pred);
    lvars i, y;
    CHECK_NUM_ON_STACK;
    fast_for i from 1 to (N fi_- 1) do
        -> y;
        unless fast_apply(dup(), y, pred) do
            erasenum(N - i);
            return(nil)
        endunless;
    endfast_for;
    ->;
    true
enddefine;


define num_/=() with_nargs 2;
    all_different(sys_=, procedure(); CHECK_NUM_ON_STACK; endprocedure)
enddefine;


define min_n(N);
    CHECK_NUM_ON_STACK;
    fast_sysrepeat(N fi_- 1, min)
enddefine;


define max_n(N);
    CHECK_NUM_ON_STACK;
    fast_sysrepeat(N fi_- 1, max)
enddefine;


/* Arithmetic operations */

define plus_n(N);
    if N == 0 then
        0
    elseif N == 1 then
        CHECK_NUM_ON_STACK
    else
        fast_sysrepeat(N fi_- 1, nonop +)
    endif
enddefine;


define times_n(N);
    if N == 0 then
        1
    elseif N == 1 then
        CHECK_NUM_ON_STACK
    else
        fast_sysrepeat(N fi_- 1, nonop *)
    endif
enddefine;


define minus_n(N);
    if N == 1 then
        negate()
    else
        fast_sysrepeat(N fi_- 2, nonop +);
        nonop - ()
    endif
enddefine;


define divide_n(N);
    if N == 1 then
        reciprocal()
    else
        fast_sysrepeat(N fi_- 2, nonop *);
        nonop / ()
    endif
enddefine;


/* Exponential, log and trig functions */

define logN(num, base);
    log(num), if base then / log(base) endif
              ;;; defaults
enddefine;


define atan(y, x);
    if x then
        arctan2(x, y)
    else
        ;;; defaults
        arctan(y)
    endif
enddefine;


/* Type conversions */

define float_radix(num);
    unless isdecimal(num) do
        type_error(num, @FLOAT)
    endunless;
    pop_float_radix
enddefine;


define float(num, other) -> num;
    ;;; should really check for real num and float other
    if other then
        number_coerce(num, other) -> num
    else
        ;;; defaults
        unless isdecimal(num) do
            number_coerce(num, decimal_0) -> num
        endunless
    endif
enddefine;


define rational with_nargs 1;
    ;;; works for complex as well
    number_coerce(0)
enddefine;


define complex(re, im);
    defaults im 0;
    re +: im
enddefine;


define floor(n, d) -> r -> q;
    defaults d 1;
    n // d -> q -> r;
    if q > (n / d) then
        q - 1 -> q;
        r + d -> r;
    endif
enddefine;


define ceiling(n, d) -> r -> q;
    defaults d 1;
    n // d -> q -> r;
    if q < (n / d) then
        q + 1 -> q;
        r - d -> r;
    endif
enddefine;


define truncate(n, d) -> d -> n;
    defaults d 1;
    n // d -> n -> d
enddefine;


define round2(n, d) -> d -> q;
    lvars nd;
    defaults d 1;
    n / d -> nd;
    unless sys_=(abs(fracof(nd)), 0.5) and (intof(nd) ->> q) && 1 == 0 do
        round(nd) -> q
    endunless;
    n - (q * d) -> d
enddefine;


define lconstant Fcoerce(n, d);
    number_coerce(n, if isinteger(d) then decimal_0 else d endif)
enddefine;


define ffloor(n, d) -> d -> n;
    floor(n, d) -> d -> n;
    Fcoerce(n, d) -> n;
enddefine;


define fceiling(n, d) -> d -> n;
    ceiling(n, d) -> d -> n;
    Fcoerce(n, d) -> n;
enddefine;


define ftruncate(n, d) -> d -> n;
    truncate(n, d) -> d -> n;
    Fcoerce(n, d) -> n;
enddefine;


define fround(n, d) -> d -> n;
    round2(n, d) -> d -> n;
    Fcoerce(n, d) -> n;
enddefine;


define decode_float =
    float_decode(% false %)
enddefine;


define integer_decode_float =
    float_decode(% true %)
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  7 1995
        Removed redundant lvar declarations.
--- John Williams, Mar 15 1995
        Now signals typed errors.
--- John Williams, Aug 27 1993
        Arithmetic operations and comparisions now take their arguments
        of the stack.
--- John Williams, Apr 13 1992
        -float_decode- now fixed to return integer sign if second
        arg is true, so -integer_decode_float- is just a closure
        of -float_decode-
--- John Williams, Apr  9 1992
        Fixed BR isl-fr.4425 (third result of -integer_decode_float-)
 */
