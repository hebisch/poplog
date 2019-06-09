(* --- Copyright University of Sussex 1994. All rights reserved. ----------
 * File:            C.all/pml/src/StdValues.ml
 * Purpose:         PML: Declaration of builtin values and exceptions
 * Author:          Rob Duncan & Simon Nichols, Feb 13 1989 (see revisions)
 *)


;;; Signature of the standard environment

signature StdValues = sig

;;; Exceptions

exception Overflow
exception Quot
exception Div
exception Mod
exception Prod
exception Sum
exception Diff
exception Neg
exception Abs
exception Sqrt
exception Exp
exception Ln
exception Real
exception Floor
exception Ord
exception Chr
exception Bind
exception Match
exception Interrupt
exception Error of string
exception Impossible of string

;;; Functions

val not         : bool -> bool
val rev         : 'a list -> 'a list
val map         : ('a -> 'b) -> 'a list -> 'b list
val ~           : 'num -> 'num
val abs         : 'num -> 'num
val floor       : real -> int
val real        : int -> real
val sqrt        : real -> real
val sin         : real -> real
val cos         : real -> real
val arctan      : real -> real
val exp         : real -> real
val ln          : real -> real
val size        : string -> int
val chr         : int -> string
val ord         : string -> int
val explode     : string -> string list
val implode     : string list -> string
val !           : 'a ref -> 'a
val print       : 'a -> 'a
val makestring  : 'a -> string
val error       : string -> 'a
val impossible  : string -> 'a

;;; Operators

val /   : real * real -> real
val div : int * int -> int
val mod : int * int -> int
val *   : 'num * 'num -> 'num
val +   : 'num * 'num -> 'num
val -   : 'num * 'num -> 'num
val ^   : string * string -> string
val @   : 'a list * 'a list -> 'a list
val =   : ''a * ''a -> bool
val <>  : ''a * ''a -> bool
val <   : 'nums * 'nums -> bool
val >   : 'nums * 'nums -> bool
val <=  : 'nums * 'nums -> bool
val >=  : 'nums * 'nums -> bool
val o   : ('b -> 'c) * ('a -> 'b) -> ('a -> 'c)
val :=  : 'a ref * 'a -> unit

end;    (* signature StdValues *)


;;; Standard operator precedences

infix  7 / div mod *;
infix  6 + - ^;
infixr 5 ::;
infix  5 @;
infix  4 = <> < > <= >=;
infix  3 o :=;


(*
 *  Declare the StdValues structure in POP-11
 *)

pop11

section $-ml;

ml_structure StdValues : StdValues = struct

;;; Exceptions

ml_exception Overflow;
ml_exception Quot;
ml_exception Div;
ml_exception Mod;
ml_exception Prod;
ml_exception Sum;
ml_exception Diff;
ml_exception Neg;
ml_exception Abs;
ml_exception Sqrt;
ml_exception Exp;
ml_exception Ln;
ml_exception Real;
ml_exception Floor;
ml_exception Ord;
ml_exception Chr;
ml_exception Bind;
ml_exception Match;
ml_exception Interrupt;

ml_exception Error of string;
ml_exception Impossible of string;

;;; Make all standard maths exceptions the same as Overflow ...
applist([Quot Prod Sum Diff Neg Abs Exp Real Floor], procedure(w);
    lvars w;
    packet_id(exception("Overflow")) -> packet_id(exception(w));
endprocedure);
;;; ... except for -Mod-, which is raised on division by zero
packet_id(exception("Div")) -> packet_id(exception("Mod"));

;;; Functions

ml_val not : bool -> bool = not;

ml_val rev : 'a list -> 'a list =
procedure(list) -> newlist with_props rev;
    lvars list, newlist = [];
    until list == [] do
        Destpair(list) -> list;
        conspair(newlist) -> newlist;
    enduntil;
endprocedure;

ml_val map : ('a -> 'b) -> 'a list -> 'b list =
procedure(f, l) with_props map;
    lvars procedure f, l;
    [% until l == [] do f(Destpair(l) -> l) enduntil %];
endprocedure;

ml_val ~ : 'num -> 'num = negate;

ml_val abs : 'num -> 'num;

ml_val floor : real -> int =
procedure(x) with_props floor;
    lvars x, y;
    if x >= (intof(x) ->> y) then y else y - 1 endif
endprocedure;

ml_val real : int -> real =
procedure() with_nargs 1 with_props real;
    number_coerce(1.0d0);
endprocedure;

ml_val sqrt : real -> real =
procedure(x) with_props sqrt;
    lconstant SqrtExn = exception("Sqrt");
    lvars x;
    if x >= 0.0d0 then sqrt(x) else raise(SqrtExn) endif;
endprocedure;

ml_val sin : real -> real;

ml_val cos : real -> real;

ml_val arctan : real -> real;

ml_val exp : real -> real;

ml_val ln : real -> real =
procedure(x) with_props ln;
    lconstant LnExn = exception("Ln");
    lvars x;
    if x > 0.0d0 then log(x) else raise(LnExn) endif;
endprocedure;

ml_val size : string -> int = datalength;

ml_val chr : int -> string =
procedure(c) with_props chr;
    lconstant ChrExn = exception("Chr");
    lvars c;
    if 0 <= c and c <= 255 then mlstring(c, 1) else raise(ChrExn) endif;
endprocedure;

ml_val ord : string -> int =
procedure(s) with_props ord;
    lconstant OrdExn = exception("Ord");
    lvars s;
    if datalength(s) == 0 then raise(OrdExn) else Subscrs(1, s) endif;
endprocedure;

ml_val explode : string -> string list =
procedure(string) with_props explode;
    lconstant procedure chr = mlstring(% 1 %);
    lvars string;
    [% appdata(string, chr) %];
endprocedure;

ml_val implode : string list -> string =
procedure(slist) with_props implode;
    lvars n = stacklength(), slist;
    mlstring(app(slist, explode), stacklength() fi_- n);
endprocedure;

ml_val ! : 'a ref -> 'a = ml_cont;

ml_val print : 'a -> 'a =
procedure(x) with_props print;
    lvars x;
    syspr('-\n'); x;
endprocedure;

ml_val makestring : 'a -> string =
procedure(x) with_props makestring;
    lvars x;
    '-';
endprocedure;

ml_val error : string -> 'a =
procedure(s) with_props error;
    lconstant ErrorExn = exception("Error");
    lvars s;
    raise(ErrorExn(s));
endprocedure;

ml_val impossible : string -> 'a =
procedure(s) with_props impossible;
    lconstant ImpossibleExn = exception("Impossible");
    lvars s;
    raise(ImpossibleExn(s));
endprocedure;


;;; Operators

;;; Special (non-exported) string relations

ml_val string_lt : string * string -> bool =
procedure(/* x, y */) with_props < with_nargs 2;
    alphabefore(/* x, y */) == true;
endprocedure;

ml_val string_le : string * string -> bool =
procedure(/* x, y */) with_props <= with_nargs 2;
    alphabefore(/* x, y */) /== false;
endprocedure;

ml_val string_gt : string * string -> bool =
procedure(/* x, y */) with_props > with_nargs 2;
    alphabefore(/* x, y */) == false;
endprocedure;

ml_val string_ge : string * string -> bool =
procedure(/* x, y */) with_props >= with_nargs 2;
    alphabefore(/* x, y */) /== true;
endprocedure;

;;; Exported names

;;; divmod:
;;;     implements funny SML div/mod

define lconstant divmod(i, d) -> q -> r;
    lvars i, d, q, r;
    i // d -> q -> r;
    if d > 0 then
        unless r >= 0 then
            q - 1 -> q;
            r + d -> r;
        endunless;
    else
        unless r <= 0 then
            q - 1 -> q;
            r + d -> r;
        endunless;
    endif;
enddefine;

ml_val div : int * int -> int =
procedure() -> q with_props div with_nargs 2;
    lvars q;
    divmod() -> q -> ;
endprocedure;

ml_val mod : int * int -> int =
procedure() -> r with_props mod with_nargs 2;
    lvars r;
    divmod() -> -> r;
endprocedure;

ml_val / : real * real -> real = nonop /;

ml_val * : 'num * 'num -> 'num = nonop *;

ml_val + : 'num * 'num -> 'num = nonop +;

ml_val - : 'num * 'num -> 'num = nonop -;

ml_val ^ : string * string -> string = nonop <>;

ml_val @ : 'a list * 'a list -> 'a list =
procedure(l1, l2) with_props @;
    lvars l1, l2;
    returnif(l2 == [])(l1);
    [% until l1 == [] do Destpair(l1) -> l1 enduntil % ^^l2];
endprocedure;

ml_val = : ''a * ''a -> bool = nonop =;

ml_val <> : ''a * ''a -> bool = nonop /=;

ml_val < : 'nums * 'nums -> bool = nonop <;

ml_val > : 'nums * 'nums -> bool = nonop >;

ml_val <= : 'nums * 'nums -> bool = nonop <=;

ml_val >= : 'nums * 'nums -> bool = nonop >=;

ml_val o : ('b -> 'c) * ('a -> 'b) -> ('a -> 'c) =
procedure(x) with_props o;
    lvars x;
    Back(x) <> Front(x);
endprocedure;

ml_val := : 'a ref * 'a -> unit =
procedure(x) with_props :=;
    lvars x;
    Back(x) -> ml_cont(Front(x));
    ml_unit;
endprocedure;

;;; Optimisations

define optimise(id, pdr);
    lvars id, pdr;
    pdr -> val_optimiser(lookup_var(id));
enddefine;

define lconstant gen_swap(e1, e2);
    lvars e1, e2;
    if nonexpansive(e1) or nonexpansive(e2) then
        gen_exp(e2);
        gen_exp(e1);
    else
        gen_exp(e1);
        gen_exp(e2);
        mlSWAP();
    endif;
enddefine;

optimise("o", procedure(args) -> success;
    lvars arg, args, success = false;
    Destpair(args) -> args -> arg;
    if isTupleExp(arg) then
        gen_swap(dl(first(arg)));
        mlCALL("<>");
        args, true -> success;
    endif;
endprocedure);

optimise(":=", procedure(args) -> success;
    lvars arg1, arg2, args, success = false;
    Destpair(args) -> args -> arg1;
    if isTupleExp(arg1) then
        gen_swap(dl(first(arg1)));
        mlUCALL("ml_cont");
        mlPUSHQ(ml_unit);
        args, true -> success;
    elseif isConstExp(arg1) then
        Destpair(first(arg1)) -> arg2 -> arg1;
        mlPUSHQ(arg2), mlPUSHQ(arg1);
        mlUCALL(if isref(arg1) then "fast_cont" else "idval" endif);
        mlPUSHQ(ml_unit);
        args, true -> success;
    endif;
endprocedure);


;;; Specialising overloaded functions and operators

define lconstant overload(id, pd);
    lvars id, pd;
    pd -> val_overloading(lookup_var(id));
enddefine;

define lconstant specialise_num1(var, ty, linenum) -> var;
    lvars var, ty, linenum;
    find_instance(var, ty, linenum, [%
        funtype(inttype, inttype),
        funtype(realtype, realtype),
    %]) -> ;
enddefine;

define lconstant specialise_num2(var, ty, linenum) -> var;
    lvars var, ty, linenum;
    find_instance(var, ty, linenum, [%
        funtype(tupletype([^inttype ^inttype]), inttype),
        funtype(tupletype([^realtype ^realtype]), realtype),
    %]) -> ;
enddefine;

define lconstant specialise_nums(var, ty, linenum, svar);
    lconstant tys = [%
            funtype(tupletype([^stringtype ^stringtype]), booltype),
            funtype(tupletype([^inttype ^inttype]), booltype),
            funtype(tupletype([^realtype ^realtype]), booltype),
        %];
    lvars var, ty, linenum, svar;
    if find_instance(var, ty, linenum, tys) == Front(tys) then
        svar;
    else
        var;
    endif;
enddefine;

define lconstant specialise_print(var, ty, linenum) -> var;
    lvars var, id, ty, linenum;
    copy(var) -> var;
    copy(val_ident(var)) -> val_ident(var);
    type_deref(ty) ->> ty -> val_type(var);
    (ID_VAL, compile_print(type_domain(ty))) -> val_access(var);
enddefine;

define lconstant specialise_makestring(var, ty, linenum) -> var;
    lvars var, id, ty, linenum;
    copy(var) -> var;
    copy(val_ident(var)) -> val_ident(var);
    type_deref(ty) ->> ty -> val_type(var);
    (ID_VAL, compile_makestring(type_domain(ty))) -> val_access(var);
enddefine;

overload("~", specialise_num1);
overload("+", specialise_num2);
overload("-", specialise_num2);
overload("*", specialise_num2);
overload("<", specialise_nums(% lookup_var("string_lt") %));
overload("<=", specialise_nums(% lookup_var("string_le") %));
overload(">", specialise_nums(% lookup_var("string_gt") %));
overload(">=", specialise_nums(% lookup_var("string_ge") %));
overload("print", specialise_print);
overload("makestring", specialise_makestring);


;;; Primitive functions: these may be evaluated at compile-time

applist([
    ;;; basic maths functions
    ~ + - * / div mod abs real floor
    ;;; string functions
    size chr ord '^' implode explode
    ;;; others
    '@' o
], procedure(id);
    lvars id;
    unless isword(id) then consword(id) -> id endunless;
    true -> val_isprimitive(lookup_var(id));
endprocedure);

ml_endstructure;

endsection; /* $-ml */

ml

pervasive StdValues;


(* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Dec 20 1994
        New treatment of identifiers
--- Robert John Duncan, Nov 24 1994
        Sectionised
--- Robert John Duncan, Mar  1 1991
        Equality function "=" now declared properly.
--- Robert John Duncan, Feb 11 1991
        Renamed -var_X- to -val_X-
--- Robert John Duncan, Feb  4 1991
        Change to patch for "="
--- Robert John Duncan, Jan 21 1991
        Changed optimisation procedures to the new style.
        Added definitions of "primitive" functions which can be statically
        evaluated.
--- Robert John Duncan, Dec  7 1990
        Added missing "SWAP" to optimiser for :=
--- Robert John Duncan, Aug  9 1990
        Added -Overflow- exception and changed all the basic maths
        exceptions to be synonyms.
        Wrote special (non-dynamic) version of "@".
--- Robert John Duncan, Jul 24 1990
        Fixed div (and mod) to round down rather than towards zero.
--- Rob Duncan, Jun 22 1990
        Replaced -unit- with -ml_unit-
--- Simon Nichols, Jun 21 1990
        Removed special optimisation of binary operators. These are now
        handled as part of the general optimisation of tupled functions.
--- Rob Duncan, Jan 31 1990
        Changed -fast_cont- to -ml_cont-
--- Rob Duncan, Sep  4 1989
        Took -equal- out again!
--- Rob Duncan, Aug 30 1989
        Added -equal-. Moved in -impossible- and -error- with associated
        exceptions from "Useful.ml".
 *)
