/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/pml/src/data.p
 > Purpose:         PML: Run-time representation of ML data
 > Author:          Rob Duncan & Simon Nichols, Feb 13 1989 (see revisions)
 */


section $-ml =>
    ml_unit
    ml_constuple
    ml_desttuple
    ml_subscrtuple
    ml_consref
    ml_cont
    ml_consvector
    ml_destvector
    ml_subscrvector
    ml_consarray
    ml_destarray
    ml_subscrarray
;

/*
 *  Representations of Basic Data
 */

constant
    ml_unit = false,    ;;; any value will do
;

;;; constuple, desttuple, subscrtuple:
;;;     a 2-tuple is a pair; others are vectors

define ml_constuple(n);
    lvars n;
    if n == 2 then
        conspair();
    else
        consvector(n);
    endif;
enddefine;

define ml_desttuple(t);
    lvars t;
    if ispair(t) then
        fast_destpair(t), 2;
    else
        destvector(t);
    endif;
enddefine;

define ml_subscrtuple(i, t);
    lvars i, t;
    if ispair(t) then
        if i == 1 then fast_front(t) else fast_back(t) endif;
    else
        subscrv(i, t);
    endif;
enddefine;

define updaterof ml_subscrtuple(/*v,*/ i, t) with_nargs 3;
    lvars i, t;
    if ispair(t) then
        if i == 1 then /*v*/ -> fast_front(t) else /*v*/ -> fast_back(t) endif;
    else
        /*v*/ -> subscrv(i, t);
    endif;
enddefine;

;;; consref, cont:
;;;     an ML ref may be a POP reference or an identifier

constant procedure ml_consref = consref;

define ml_cont(r);
    lvars r;
    if isref(r) then
        fast_cont(r);
    else
        idval(r);
    endif;
enddefine;

define updaterof ml_cont(/*v,*/ r) with_nargs 2;
    lvars r;
    if isref(r) then
        /*v*/ -> fast_cont(r);
    else
        /*v*/ -> idval(r);
    endif;
enddefine;

;;; consvector, destvector, subscrvector:
;;;     ML vectors are the same as the Pop-11 equivalents, except that the
;;;     0-length vector is unique

constant ml_vector0 = consvector(0);

define ml_consvector(n);
    lvars n;
    returnif(n == 0)(ml_vector0);
    consvector(n);
enddefine;

constant procedure (
    ml_destvector = destvector,
    ml_subscrvector = subscrv,
);

;;; consarray, destarray, subscrarray:
;;;     ML arrays need a special type, because equality is defined as
;;;     pointer equality (like refs)

defclass mlarray [writeable] :full;
;;;
nonop== -> class_=(mlarray_key);

constant ml_array0 = consmlarray(0);

define ml_consarray(n);
    lvars n;
    returnif(n == 0)(ml_array0);
    consmlarray(n);
enddefine;

constant procedure (
    ml_destarray = destmlarray,
    ml_subscrarray = subscrmlarray,
);

;;; Subv0:
;;;     ML vectors and arrays are based at 0 rather than 1; this is a
;;;     horrible hack to avoid explicit additions

constant macro Subv0 = "fast_prolog_arg";

/*
 *  Representation of Constructed Data
 */

;;; Codes for possible data representations
;;; (constructors are sorted alphabetically before having their values
;;; allocated)

constant macro (

    T_NONE  = 0,
        ;;; not constructed
        ;;; (for abbreviated, primitive and external types)

    T_UNIT = 1,
        ;;; single nullary constructor
        ;;; e.g. datatype one = ONE;
        ;;; Constructor value: ()  (unit)

    T_BOOL = 2,
        ;;; two nullary constructors
        ;;; e.g. datatype bool = false | true;
        ;;; Constructor values: <false>, <true>

    T_ENUM = 3,
        ;;; n > 2 nullary constructors
        ;;; e.g. datatype day = Mon | ... | Sun;
        ;;; Constructor values: 1 ... n

    T_IDENT = 4,
        ;;; single unary constructor
        ;;; e.g. datatype age = Age of int;
        ;;; Constructor value: identfn (i.e. no extra tagging added)

    T_REF = 5,
        ;;; type "ref" (special because of updating and equality)
        ;;; Constructor value: consref

    T_LIST = 6,
        ;;; one nullary and one unary constructor
        ;;; e.g. datatype 'a list = nil | :: of 'a * 'a list;
        ;;; Constructor values: nil, identfn
        ;;; NB. Added qualification is that the argument to the unary
        ;;; constructor can't itself be a LIST type

    T_BINARY = 7,
        ;;; two constructor values
        ;;; e.g. datatype label = IntLab of int | SymLab of string;
        ;;; Constructor values: conspair(% false %), conspair(% true %)

    T_USER = 8,
        ;;; n > 2 constructors, mixed arities
        ;;; Constructor values: conspair(% i %), for i = 1 ... n

);

;;; bad_data_rep:
;;;     provides an error case for go_on's on a data rep

define bad_data_rep(rep);
    lvars rep;
    mishap(rep, 1, 'BAD DATA REPRESENTATION');
enddefine;

;;; data_rep:
;;;     determines the representation of a datatype from the number
;;;     and type of its constructors.

define data_rep(cbs);
    lvars cb, cbs, rep, tyexp, n = 0, n1 = 0;
    returnif(cbs == [])(T_NONE);
    For cb in cbs do
        n fi_+ 1 -> n;
        if second(cb) then n1 fi_+ 1 -> n1 endif;
    endfor;
    if n1 == 0 then
        ;;; enumerated type
        if n == 1 then
            T_UNIT;
        elseif n == 2 then
            T_BOOL;
        else
            T_ENUM;
        endif;
    elseif n == 1 then
        T_IDENT;
    elseif n == 2 then
        if n1 == 1 then
            ;;; one nullary, one unary constructor
            Front(cbs) -> cb;
            unless second(cb) then Front(Back(cbs)) -> cb endunless;
            ;;; -cb- is the unary constructor
            second(cb) -> tyexp;
            if isVarTyexp(tyexp)
            or isConsTyexp(tyexp)
            and ((tycon_datarep(first(tyexp)) ->> rep) == T_NONE
                 or rep == T_IDENT
                 or rep == T_LIST)
            then
                ;;; can't tell what the argument will look like at run-time
                ;;; so we must stick a constructor on it
                T_BINARY;
            else
                T_LIST;
            endif;
        else
            T_BINARY;
        endif;
    else
        T_USER;
    endif;
enddefine;

;;; Constructor values:
;;;     the value of the n'th constructor of any representation
;;;     will always be the same object

lconstant
    bin0 = {% conspair(false, false), conspair(false, true) %},
    bin1 = {% conspair(% false %), conspair(% true %) %},
;

lvars
    nuser = 16,
    user0 = initv(nuser),
    user1 = initv(nuser),
;

define lconstant user_value(n, arity) -> val;
    lvars n, arity, val;
    if n > nuser then
        move_subvector(1, user0, 1, initv(n) ->> user0, nuser);
        move_subvector(1, user1, 1, initv(n) ->> user1, nuser);
        n -> nuser;
    endif;
    if arity == 0 then
        if (Subscrv(n, user0) ->> val) == undef then
            conspair(false, n) ->> val -> Subscrv(n, user0);
        endif;
    else
        if (Subscrv(n, user1) ->> val) == undef then
            conspair(% n %) ->> val -> Subscrv(n, user1);
        endif;
    endif;
enddefine;

;;; data_value:
;;;     computes the value of constructor number -n- of datatype -rep-.
;;;     -arity- is the arity of the constructor (0 or 1)

define data_value(rep, n, arity);
    lvars rep, n, arity;
    go_on rep to
        t_unit t_bool t_enum t_ident t_ref t_list t_binary t_user
    else
        error;
    t_unit:     return(ml_unit);
    t_bool:     return(n == 2);
    t_enum:     return(n);
    t_ident:    return(identfn);
    t_ref:      return(consref);
    t_list:     return(if arity == 0 then nil else identfn endif);
    t_binary:   return(if arity == 0 then bin0(n) else bin1(n) endif);
    t_user:     return(user_value(n, arity));
    error:      bad_data_rep(rep);
enddefine;

endsection; /* $-ml */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jul  4 1995
        Made array class writeable by default
--- Robert John Duncan, Nov 24 1994
        Sectionised. Added arrays and vectors.
--- Robert John Duncan, Mar 22 1991
        Name changes.
--- Robert John Duncan, Feb 11 1991
        Deleted -con_*class-: now part of a val record in "env.p".
--- Rob Duncan, Jun 22 1990
        Replaced -unit- with -ml_unit- and changed its value to <false>
--- Rob Duncan, Apr 19 1990
        Added -ml_consref- for completeness with -ml_cont-; will allow
        representation to change in the future.
--- Rob Duncan, Jan 31 1990
        Moved all data construction & access procedures into here.
        Defined new -ml_cont- procedure for proper treatment of refs as
        identifiers.
 */
