/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/plogterms.p
 > Purpose:         PROLOGTERM data structures; pattern unification.
 > Author:          Jonathan Laventhol, July 1985 (see revisions)
 > Documentation:   REF *PROLOG
 */

#_INCLUDE 'declare.ph'
#_INCLUDE 'gctypes.ph'

global constant
        procedure (prolog_unify, prolog_deref, prolog_newvar,
        prolog_push_continuation, prolog_args, prolog_termspec,
        Sys$- Eq__Fullvec, Sys$-Conspair,
        Sys$-Wordvec_getsize, Sys$-Data_print,
        ),
        _plog_trail_push
    ;

global vars
        _plog_trail_sp
    ;

;;; ----------------------------------------------------------------------

section $-Sys$-Plog =>
                    isprologterm initprologterm consprologterm destprologterm
                    fast_prolog_functor fast_prolog_arity fast_prolog_arg
                    prologterm_dest_valof prologterm_apply
                    prolog_push_continuation_term prolog_complexterm
                    prolog_maketerm prolog_functor prolog_arity
                    prolog_checkspec prolog_termspec
                    Prolog_unpeel prolog_full_deref
                    prolog_arg_nd prolog_appargs_nd prolog_args_nd
                    prolog_arg prolog_appargs prolog_args
                    prolog_unify_patt prologterm_key
                    ;

;;; This controls whether functors in prolog terms must be fixed, or are
;;; allowed to be variables. The default for the standard system is "fixed"
;;; there is another one in plogcore.p
lconstant macro
    PROLOG_FIXED_FUNCTORS = true;


define isprologterm(item);
    lvars item;
    iscompound(item) and item!KEY == prologterm_key
enddefine;

;;; build a prologterm with known arity only.  not very useful!
define initprologterm(_arity) -> term;
    lvars term inititem _offs _arity;
    Check_integer(_arity, 0);
    _int(_arity) -> _arity;
    @@(w)[_arity] -> _offs;
    Get_store(@@PGT_ARGS{_offs} _sub @@POPBASE) -> term;
    prologterm_key -> term!KEY;
    _arity _add _1 -> term!PGT_LENGTH;
    "undef" -> term!PGT_FUNCTOR;
    _fill(0, _offs, term@PGT_ARGS)
enddefine;

;;; this MUST return a prologterm for any +ve arity and word functor
;;; even though we shouldn't have prologterms of arity zero, sometimes
;;; we need to make them.
;;;
define consprologterm(functor, _arity) -> _term;
    lvars functor, _term, _offs, _lim, _arity;
    Check_integer(_arity, 0);
#_IF PROLOG_FIXED_FUNCTORS
    unless isword(functor) then Check_word(functor) endunless;
#_ELSE
    ;;; allow functor to be plogvar too
    unless iscompound(functor) and functor!KEY == prologvar_key then
        Check_word(functor)
    endunless;
#_ENDIF
    _int(_arity) -> _arity;
    @@(w)[_arity] -> _offs;
    Get_store(@@PGT_ARGS{_offs} _sub @@POPBASE) -> _term;
    prologterm_key -> _term!KEY;
    _arity _add _1 -> _term!PGT_LENGTH;
    functor -> _term!PGT_FUNCTOR;
    _term@PGT_ARGS[_0] -> _lim;
    _lim@(w){_offs} -> _offs;
    ;;; _term must be nonpop here because if we get stack underflow
    ;;; it won't have been completely initialised
    while _offs >@(w) _lim do
        -> _offs--!(w) -> _offs
    endwhile
enddefine;

    ;;; optimises prolog_assign(consprologterm()) (see vm_optim.p)
define Assign_term(functor, _arity) with_props consprologterm;
    lvars functor, _term, _offs, _lim, _arity;
    _int(_arity) -> _arity;
    @@(w)[_arity] -> _offs;
    Get_store(@@PGT_ARGS{_offs} _sub @@POPBASE) -> _term;
    prologterm_key -> _term!KEY;
    _arity _add _1 -> _term!PGT_LENGTH;
    functor -> _term!PGT_FUNCTOR;
    _term@PGT_ARGS[_0] -> _lim;
    _lim@(w){_offs} -> _offs;
    ;;; _term must be nonpop here because if we get stack underflow
    ;;; it won't have been completely initialised
    while _offs >@(w) _lim do
        -> _offs--!(w) -> _offs
    endwhile;
    ;;; assign to plogvar on stack and push on trail
    -> _offs;                   ;;; the var
    _term -> fast_cont(_offs);
    _plog_trail_push(_offs)
enddefine;

    ;;; called only when _prolog_assign_pair finds _free_pairs empty
define Assign_pair() with_props conspair;
    lvars _pair, _var;
    Conspair() -> _pair;
    ;;; assign to plogvar on stack and push on trail
    -> _var;                    ;;; the var
    _pair -> fast_cont(_var);
    _plog_trail_push(_var)
enddefine;


;;; --- FAST BASIC ACCESS --------------------------------------------------
;;; POPC knows about these procedures.  The calls inside the procedures
;;; are expanded to better code than could be written here.

define fast_prolog_functor with_nargs 1;
    fast_prolog_functor()
enddefine;
;;;
define updaterof fast_prolog_functor() with_nargs 2;
    -> fast_prolog_functor()
enddefine;

define fast_prolog_arity with_nargs 1;
    fast_prolog_arity()
enddefine;

define fast_prolog_arg() with_nargs 2;
    fast_prolog_arg()
enddefine;
;;;
define updaterof fast_prolog_arg() with_nargs 3;
    -> fast_prolog_arg()
enddefine;

;;; --- APPLY AND UTILITIES ------------------------------------------------
;;; Occasionally we need to blow up a term in a way suitable for calling
;;; consprologterm.  The dest_valof procedure is useful in the current
;;; prolog system, but may be withdrawn later.  The behaviour of the apply
;;; procedure may also change.

define destprologterm(term);
    lvars term, _here, _offs, _lim;
    unless iscompound(term) and term!KEY == prologterm_key then
        mishap(term, 1, 'PROLOGTERM NEEDED');
    endunless;
    @@(w)[term!PGT_LENGTH] -> _offs;
    Alloc_user_space(@@(w){_offs}++);   ;;; +1 is for arity
    term@PGT_ARGS[_0] -> _here;
    _here--@(w){_offs} -> _lim;
    while _here <@(w) _lim do
        _here!(w)++ -> _here
    endwhile;
    fast_prolog_functor(term);
    _pint(term!PGT_LENGTH _sub _1)      ;;; arity
enddefine;

define prologterm_dest_valof() with_nargs 1;
    valof(destprologterm() ->) ;        ;;; remove arity
enddefine;

define prologterm_apply(term);
    lvars term;
    valof(destprologterm(term) -> )()           ;;; erase arity
enddefine;

define prolog_push_continuation_term(term);
    lvars term;
    prolog_args(term),
    prolog_termspec(term) fi_+ 1,   ;;; length!
    prolog_push_continuation()
enddefine;


;;; --- GENERAL PROLOG TERMS -----------------------------------------------
;;; Prolog terms are represented as shown in this table:
;;; Prolog      1  foo  []  [1|2]  X               f(1,2)
;;; POP11       1  foo  []  [1|2]  <prologvar _1>  <prologterm f 1 2>
;;; Prolog complex terms can therefore be either pairs or prologterms, and
;;; must have special checking manipulators.  A prolog variable is represented
;;; by a special datatype.  Note that the cont of an uninstantiated variable
;;; iteself. non of these procedures deref their argument.  the ones which
;;; extract arguments from terms come in two versions.  the one with the _nd
;;; suffix means no deref (on the result).

define prolog_complexterm(item);
    lvars item key;
    if iscompound(item)
     and ((item!KEY ->> key) == prologterm_key or key == pair_key) then
        true
    else
        false
    endif
enddefine;

define prolog_maketerm(functor, _arity);
    lvars functor _arity;
    if _arity == 0 then
        functor
    elseif _arity == 2 and functor == "." then
        conspair()
    else
        consprologterm(functor, _arity)
    endif
enddefine;

define prolog_functor(item);
    lvars item key;
    if iscompound(item) then
        if (item!KEY ->> key) == prologterm_key then
            fast_prolog_functor(item),
#_IF not(PROLOG_FIXED_FUNCTORS)
            prolog_deref();
#_ENDIF
            return
        elseif key == pair_key then
            return(".")
        endif;
    endif;
    item
enddefine;
;;;
define updaterof prolog_functor(item) with_nargs 2;
    lvars item key;
    if iscompound(item) and item!KEY == prologterm_key then
        -> fast_prolog_functor(item)
    else
        mishap(item, 1, 'PROLOGTERM NEEDED')
    endif;
enddefine;

define prolog_arity(item);
    lvars item key;
    if iscompound(item) then
        if (item!KEY ->> key) == prologterm_key then
            return(fast_prolog_arity(item));
        elseif key == pair_key then
            return(2)
        endif
    endif;
    0
enddefine;

;;; check functor and arity of given term
;;; this procedure will become increasingly important
define prolog_checkspec(term, functor, _arity);
    lvars term functor key _arity;
    if iscompound(term) then
        if ((term!KEY ->> key) == prologterm_key and
#_IF PROLOG_FIXED_FUNCTORS
            fast_prolog_functor(term) == functor and
#_ELSE
            prolog_deref(fast_prolog_functor(term)) == functor and
#_ENDIF
            fast_prolog_arity(term) == _arity)
                or
            (key == pair_key and functor == "." and _arity == 2)
                or
            (term == functor and _arity == 0) then
            true
        else
            false
        endif;
    elseif (term == functor and _arity == 0) then
        true
    else
        false
    endif
enddefine;

;;; return functor and arity
define prolog_termspec(term);
    lvars term key;
    if iscompound(term) then
        if (term!KEY ->> key) == prologterm_key then
            return(fast_prolog_functor(term),
#_IF not(PROLOG_FIXED_FUNCTORS)
                    prolog_deref(),
#_ENDIF
                    fast_prolog_arity(term));
        elseif key == pair_key then
            return(".", 2)
        endif
    endif;
    term, 0
enddefine;

define Prolog_unpeel(/*term,*/ key, p) with_nargs 3;
    dlvars key, procedure p, deref;

    define lconstant unpeel(term);
        lvars term, n, _copied;

        define lconstant unpeel_pair(pair) -> last_copied -> res;
            lvars pair, last_copied = false, res = pair, f, b, _copy;

            define lconstant copy_pairs(x, y);
                lvars x, l, y, r;
                if deref then prolog_deref(x) -> x endif;
                conspair(fast_destpair(x)) ->> l -> r;
                repeat
                    fast_back(x) -> x;
                    if deref then prolog_deref(x) -> x endif;
                    conspair(fast_destpair(x)) ->> fast_back(l) -> l;
                    returnif(x == y) (l, r)
                endrepeat
            enddefine;

            repeat
                unpeel(fast_front(pair)) -> _copy -> f;
                fast_back(pair) -> b;
                if deref then prolog_deref(b) -> b endif;
                if iscompound(b) and b!KEY == key then
                    unless p(b) then
                        true -> _copy
                    elseif dup() /== b then
                        unless ispair(dup()) then unpeel() -> endunless;
                        true -> _copy
                    endunless -> b
                elseunless ispair(b) then
                    if unpeel(b) then true -> _copy endif -> b
                endif;
                if _copy then
                    if last_copied then
                        if fast_back(last_copied) == pair then
                            dup(conspair(fast_destpair(pair)))
                        else
                            copy_pairs(fast_back(last_copied), pair)
                        endif -> fast_back(last_copied)
                    else
                        if res == pair then
                            dup(conspair(fast_destpair(pair)))
                        else
                            copy_pairs(res, pair)
                        endif -> res
                    endif -> last_copied;
                    f -> fast_front(last_copied);
                    b -> fast_back(last_copied)
                endif;
                quitunless(ispair(b));
                b -> pair
            endrepeat
        enddefine;      /* unpeel_pair */

        if deref then prolog_deref(term) -> term endif;
        returnunless(iscompound(term)) (term, false);
        if term!KEY == key then
            unless p(term) then
                (), true
            elseif dup() /== term then
                _checkall();
                unpeel() ->, true
            else
                (), false
            endunless
        elseif term!KEY == pair_key then
            _checkall();
            chain(term, unpeel_pair)
        elseif term!KEY == prologterm_key then
            _checkall();
            false -> _copied;
            fast_for n to fast_prolog_arity(term) do
                if unpeel(fast_prolog_arg(n, term)) then
                    true -> _copied
                endif
            endfast_for;
            if _copied then
                prolog_maketerm(prolog_termspec(term)), true
            else
                erasenum(fast_prolog_arity(term));
                term, false
            endif
        else
            term, false
        endif
    enddefine;

    key /== prologvar_key -> deref;
    unpeel() ->
enddefine;      /* prolog_unpeel */

define prolog_full_deref(/*term*/) with_nargs 1;
    Prolog_unpeel((), prologvar_key,
                    procedure(); prolog_deref(), true endprocedure)
enddefine;


;;; --- NON-DEREFFING ARGUMENT-FETCHERS ------------------------------------

define prolog_arg_nd(_n, item);
    lvars _n item key;
    Check_integer(_n, 1);
    if iscompound(item) then
        if (item!KEY ->> key) == prologterm_key then
            if _int(_n) _slt item!PGT_LENGTH then
                return(fast_prolog_arg(_n, item));
            endif;
            mishap(_n, 1, 'SUBSCRIPT GREATER THAN ARITY OF TERM');
        elseif key == pair_key then
            if _n == 1 then
                return(fast_front(item))
            elseif _n == 2 then
                return(fast_back(item))
            endif;
            mishap(_n, 1, 'SUBSCRIPT GREATER THAN ARITY OF PAIR');
        endif
    endif;
    mishap(_n, item, 2, 'PROLOGTERM OR PAIR NEEDED FOR SUBSCRIPTING')
enddefine;
;;;
define updaterof prolog_arg_nd(_n, item) with_nargs 3;
    lvars _n item key;
    Check_integer(_n, 1);
    if iscompound(item) then
        if (item!KEY ->> key) == prologterm_key then
            if _int(_n) _slt item!PGT_LENGTH then
                -> fast_prolog_arg(_n, item);
                return
            endif;
            mishap(_n, 1, 'SUBSCRIPT GREATER THAN ARITY OF TERM');
        elseif key == pair_key then
            if _n == 1 then
                -> fast_front(item); return
            elseif _n == 2 then
                -> fast_back(item); return
            endif;
            mishap(_n, 1, 'SUBSCRIPT GREATER THAN ARITY OF PAIR');
        endif
    endif;
    mishap(_n, item, 2, 'PROLOGTERM OR PAIR NEEDED FOR SUBSCRIPTING')
enddefine;

define prolog_appargs_nd(term, pdr);
    lvars term, pdr, key, _i;
    Check_procedure(pdr);
    if iscompound(term) then
        if (term!KEY ->> key) == prologterm_key then
            fast_for _i to fast_prolog_arity(term) do
                _CHECKUSER;
                fast_apply(fast_prolog_arg(_i, term), pdr);
            endfast_for
        elseif key == pair_key then
            _CHECKUSER;
            fast_apply(fast_front(term), pdr);
            _CHECKUSER;
            fast_apply(fast_back(term), pdr);
        endif
    endif
enddefine;

define prolog_args_nd(term);
    lvars term key _here _offs _lim;
    if iscompound(term) then
        if (term!KEY ->> key) == pair_key then
            fast_destpair(term)
        elseif key == prologterm_key then
            Alloc_user_space(--@@(w)[term!PGT_LENGTH] ->> _offs);
            term@PGT_ARGS[_0] -> _here;
            _here@(w){_offs} -> _lim;
            while _here <@(w) _lim do
                _here!(w)++ -> _here
            endwhile;
        endif
    endif
enddefine;

;;; --- DEREFFING ARGUMENT-FETCHERS ----------------------------------------

define prolog_arg with_nargs 2;
    prolog_deref(prolog_arg_nd())
enddefine;

define prolog_appargs(term, pdr);
    lvars term, pdr, key, _i;
    Check_procedure(pdr);
    if iscompound(term) then
        if (term!KEY ->> key) == prologterm_key then
            fast_for _i to fast_prolog_arity(term) do
                _CHECKUSER;
                fast_apply(prolog_deref(fast_prolog_arg(_i, term)), pdr)
            endfast_for
        elseif key == pair_key then
            _CHECKUSER;
            fast_apply(prolog_deref(fast_front(term)), pdr);
            _CHECKUSER;
            fast_apply(prolog_deref(fast_back(term)), pdr)
        endif
    endif
enddefine;

define prolog_args(term);
    lvars term key _here _offs _lim;
    if iscompound(term) then
        if (term!KEY ->> key) == pair_key then
            prolog_deref(fast_front(term));
            prolog_deref(fast_back(term));
        elseif key == prologterm_key then
            Alloc_user_space(--@@(w)[term!PGT_LENGTH] ->> _offs);
            term@PGT_ARGS[_0] -> _here;
            _here@(w){_offs} -> _lim;
            while _here <@(w) _lim do
                prolog_deref(_here!(w)++ -> _here)
            endwhile;
        endif
    endif
enddefine;

;;; --- TERM UNIFICATION ---------------------------------------------------

;;; Doing unification of complex terms by "structure sharing"

;;; UNIFICATION PATTERNS
;;; These are put into a vector to express a pattern to be matched against
;;; some term.  For example,
;;;   {^TERM 4
;;;         ^CONSTANT append
;;;         ^CONS ^FIRSTVAR A ^FIRSTVAR B
;;;         ^IGNORE
;;;         ^CONS ^NTHVAR A ^FIRSTVAR D
;;;   }
;;;
;;; would represent the pattern given by:
;;;     append([A|B],C,[A|D])      ( :- append(B,C,D). )
;;; which would be matched against a structure like
;;;     <prologterm append [a b c] [d e] <prologvar _1>>

lconstant macro (
    CONS        = 1,    ;;; A pair: followed by items for the front and then
                        ;;; the back.
    TERM        = 2,    ;;; A prologterm.  Followed by integer length
                        ;;; (=arity+1) and entries for the "principle functor"
                        ;;; and the arguments.
    IGNORE      = 3,    ;;; Indicates matching against a variable that is
                        ;;; never used
    FIRSTVAR    = 4,    ;;; First appearance of some local variable. Followed
                        ;;; by the name of the variable (a word).
    NTHVAR      = 5,    ;;; Indicates the nth occurrence of a local variable
                        ;;; (n>1). Followed by the name of the variable
                        ;;; (a word)
    CONSTANT    = 6,    ;;; Indicates a constant. Followed by the constant


    NEXT_PATT   = [ (patt_index fi_+ 1 -> patt_index,
                     fast_subscrv(patt_index, pattern)) ],
    );


;;; The following global variables follow the progress of the unifier
;;; through the "pattern" being matched

lvars
    pattern,        ;;; the pattern being used
    patt_index,     ;;; the pointer into the pattern
    ;


    ;;; constructing a term from the pattern
    ;;; patt_index assumed set to before start, and moved to point after
    ;;; the end of the pattern
define lconstant Construct_term();
    lvars pred, _n, _m;
    go_on NEXT_PATT to cons term ignore firstvar nthvar const else err;

    cons:   ;;; build a pair from the next two items in the pattern
        return(conspair(Construct_term(), Construct_term()));

    term:   ;;; build a prologterm from the given length and functor
        NEXT_PATT -> _n;
#_IF PROLOG_FIXED_FUNCTORS
        NEXT_PATT -> pred;
#_ELSE
        Construct_term() -> pred;
#_ENDIF;
        _n -> _m;
        until _m fi_<= 1 do Construct_term(); _m fi_- 1 -> _m enduntil;
        return(consprologterm(pred, _n fi_- 1));

    ignore:     ;;; build a variable
        return(prolog_newvar());

    firstvar:   ;;; build a variable and keep it for next use
        NEXT_PATT -> pred;
        return(prolog_newvar() ->> fast_cont(pred));

    nthvar:     ;;; use the same variable as last time
        return(fast_cont(NEXT_PATT));

    const:      ;;; use the given constant (number, nil, atom)
        return(NEXT_PATT);

    err:        ;;; complain
        mishap(0, 'Illegal prolog pattern')
enddefine;


    ;;; Unifying a real term against the pattern.
    ;;; patt_index assumed set to before start, and moved to point after
    ;;; the end of the pattern
define lconstant Unify_pattern(arg);
    lvars arg, pred, _n, _m, _plogvar_key = prologvar_key;
unistart:
    go_on NEXT_PATT to cons term ignore firstvar nthvar const else err;

    cons:
        if issimple(arg) then
            return(false)
        elseif arg!KEY == _plogvar_key then
            if (fast_cont(arg) ->> _n) == arg then
                conspair(Construct_term(), Construct_term())
                                            -> fast_cont(arg);
                _plog_trail_push(arg);
                return(true);
            else
                _n -> arg;
                goto cons
            endif
        elseif arg!KEY == pair_key and Unify_pattern(fast_front(arg)) then
            fast_back(arg) -> arg;
            goto unistart
        endif;
        return(false);

    term:
        if issimple(arg) then
            return(false)
        elseif arg!KEY == _plogvar_key then
            if (fast_cont(arg) ->> _n) == arg then
                NEXT_PATT -> _n;
#_IF PROLOG_FIXED_FUNCTORS
                NEXT_PATT -> pred;
#_ELSE
                Construct_term() -> pred;
#_ENDIF;
                _n -> _m;
                until _m fi_<= 1 do
                    Construct_term(); _m fi_- 1 -> _m
                enduntil;
                consprologterm(pred, _n fi_- 1) -> fast_cont(arg);
                _plog_trail_push(arg);
                return(true);
            else
                _n -> arg;
                goto term
            endif
        elseif arg!KEY == prologterm_key then
            NEXT_PATT -> _n;
            if _int(_n) == arg!PGT_LENGTH then
#_IF PROLOG_FIXED_FUNCTORS
                ;;; check functor with ==
                unless NEXT_PATT == fast_prolog_functor(arg) then
#_ELSE
                ;;; check functor by unifying
                unless Unify_pattern(fast_prolog_functor(arg)) then
#_ENDIF
                    return(false)
                endunless;
                _n fi_- 1 -> _n;
                1 -> _m;
                until _m == _n do
                    unless Unify_pattern(fast_prolog_arg(_m, arg)) then
                        return(false)
                    endunless;
                    _m fi_+ 1 -> _m;
                enduntil;
                fast_prolog_arg(_n, arg) -> arg;
                goto unistart
            endif;
        endif;
        return(false);

    ignore:
        return(true);

    firstvar:
        NEXT_PATT -> pred;
        arg -> fast_cont(pred);
        return(true);

    nthvar:
        return(prolog_unify(fast_cont(NEXT_PATT), arg));

    const:
        if iscompound(arg) and arg!KEY == _plogvar_key then
            if (fast_cont(arg) ->> _n) == arg then
                NEXT_PATT -> fast_cont(arg);
                _plog_trail_push(arg);
                return(true);
            else
                _n -> arg;
                goto const
            endif
        else
            return(NEXT_PATT = arg);    ;;; robd, 18/2/87. Changed == to =
        endif;
    err:
        mishap(0, 'Illegal Prolog pattern')
enddefine;

;;; This is the way in from outside. Values are assigned to global
;;; variables and then the main unification routine is called

define prolog_unify_patt() with_nargs 1;
    -> pattern;
    0 -> patt_index;
    Unify_pattern()
enddefine;


;;; --- KEYS ---------------------------------------------------------------

;;; Hash_prologterm:
;;;     special hash procedure for prologterms
define lconstant Hash_prologterm(term);
    lvars term;
    3610        ;;; = syshash("prologterm")
    fi_+ syshash(fast_prolog_functor(term));
    unless term!PGT_LENGTH == _1 then
        fi_+ fast_prolog_arity(term)
        fi_+ syshash(fast_prolog_arg(1, term))
    endunless;
enddefine;

constant
    prologterm_key = struct KEY_V =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_VECTOR
            _biset _:M_K_FULL_VECTOR
            _biset _:M_K_COPY,  ;;; K_FLAGS
        _:GCTYPE_VECTOR,        ;;; K_GC_TYPE
        Wordvec_getsize,        ;;; K_GET_SIZE

        "prologterm",           ;;; K_DATAWORD
        "full",                 ;;; K_SPEC
        isprologterm,           ;;; K_RECOGNISER
        WREF prologterm_apply,  ;;; K_APPLY
        Eq__Fullvec,            ;;; K_SYS_EQUALS
        WREF Eq__Fullvec,       ;;; K_EQUALS
        Data_print,             ;;; K_SYS_PRINT
        WREF Data_print,        ;;; K_PRINT
        WREF Hash_prologterm,   ;;; K_HASH

        _:NUMTYPE_NON_NUMBER,   ;;; K_NUMBER_TYPE
        _:PROLOG_TYPE_TERM,     ;;; K_PLOG_TYPE
        _:EXTERN_TYPE_NORMAL,   ;;; K_EXTERN_TYPE
        _0,                     ;;; K_SPARE_BYTE

        _:t_WORD,               ;;; K_FIELD_CODE_V
        initprologterm,         ;;; K_INIT_V
        consprologterm,         ;;; K_CONS_V
        destprologterm,         ;;; K_DEST_V
        prolog_arg_nd,          ;;; K_SUBSCR_V
        fast_prolog_arg         ;;; K_FAST_SUBSCR_V
        %};


endsection;     /* Sys$-Plog */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  7 1995
        Revised key layout
--- John Gibson, Mar 14 1990
        Change to key layout.
--- John Gibson, Dec  7 1989
        Changes for new pop pointers
--- John Gibson, Aug 30 1989
        Rewrote -prolog_full_deref- in terms of new (exported) procedure
        -Prolog_unpeel- which deals with lists interatively (also used
        by -prolog_generalise- and -prolog_instance-).
--- John Gibson, Apr  9 1989
        Whole file into section Sys$-Plog
--- Rob Duncan, Mar 30 1989
        Replaced CHECKUSER in -prolog_full_deref- by -checkall- as it can
        do unbounded recursion
--- John Gibson, Jan 22 1989
        Removed use of -apply- from -prologterm_apply-
--- John Gibson, Jan 22 1989
        Renamed plog_trail_push as _plog_trail_push
--- John Gibson, Aug  5 1988
        Corrected potential problem in -consprologterm- (qv)
--- John Gibson, Feb 28 1988
        Wordvec_getsize into section Sys
--- John Gibson, Feb 11 1988
        Check_integer in section Sys
--- John Gibson, Dec 18 1987
        Added declarations at top of file for -_plog_trail_push-,
        -prolog_args-, -prolog_termspec-
--- Robert Duncan, Sep  9 1987
        added -Hash_prologterm- and put this in the K_HASH field of
        -prologterm_key-.
--- John Gibson, Sep  5 1987
        New key format for prologterm_key
--- Robert Duncan, Feb. 1987
        Changed -Unify_pattern- to use '=' instead of '==' for unifying
        constants: '==' doesn't work for all constants (most notably floats).
 */
