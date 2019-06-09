(* --- Copyright University of Sussex 1994. All rights reserved. ----------
 * File:            C.all/pml/src/StdTypes.ml
 * Purpose:         PML: Declaration of builtin types
 * Author:          Rob Duncan & Simon Nichols, Feb 13 1989 (see revisions)
 *)


(*
 *  Declaration of standard type constructors
 *)

signature StdTypes = sig

eqtype int
eqtype real
eqtype string

type exn

datatype bool =
    false
|   true

datatype 'a ref =
    ref of 'a

datatype 'a list =
    nil
|   :: of 'a * 'a list

type unit

end;    (* signature StdTypes *)


infixr 5 :: ;

structure StdTypes : StdTypes = struct

(* Dummy definitions for primitive types *)

datatype int =
    INT

and real =
    REAL

and string =
    STRING

abstype exn =
    EXN
with end

(* Proper definitions *)

datatype bool =
    false
|   true

datatype 'a ref =
    ref of 'a;

datatype 'a list =
    nil
|   op :: of 'a * 'a list

type unit = {}

end;

pervasive StdTypes;

(*
 *  Update the constructors to reflect their primitive status
 *)

pop11

section $-ml;

;;; int

lconstant int_tycon = lookup_tycon("int");
constype(int_tycon, []) -> inttype;
constypename("int", 0, true) -> tycon_function(int_tycon);
print_num -> tycon_printer(int_tycon);

;;; real

lconstant real_tycon = lookup_tycon("real");
constype(real_tycon, []) -> realtype;
constypename("real", 0, true) -> tycon_function(real_tycon);
print_num -> tycon_printer(real_tycon);

;;; string

lconstant string_tycon = lookup_tycon("string");
constype(string_tycon, []) -> stringtype;
constypename("string", 0, true) -> tycon_function(string_tycon);
print_string -> tycon_printer(string_tycon);

;;; exn

lconstant exn_tycon = lookup_tycon("exn");
constype(exn_tycon, []) -> exntype;
constypename("exn", 0, false) -> tycon_function(exn_tycon);
print_exception -> tycon_printer(exn_tycon);

;;; bool

constype(lookup_tycon("bool"), []) -> booltype;
"bool" -> typename_contents(type_function(booltype));

;;; ref

lconstant ref_tycon = lookup_tycon("ref");

define reftype(ty);
    lvars ty;
    constype(ref_tycon, [^ty]);
enddefine;

"ref" -> typename_contents(tycon_function(ref_tycon));
"ref" -> typename_equality(tycon_function(ref_tycon));
print_ref -> tycon_printer(ref_tycon);
T_REF -> tycon_datarep(ref_tycon);

lconstant ref_con = lookup_con("ref");

consref -> val_value(ref_con);
true -> type_imptyvar(type_domain(val_type(ref_con)));
T_REF -> val_datarep(ref_con);

;;; list

lconstant list_tycon = lookup_tycon("list");

define listtype(ty);
    lvars ty;
    constype(list_tycon, [^ty]);
enddefine;

"list" -> typename_contents(tycon_function(list_tycon));
print_list -> tycon_printer(list_tycon);

lookup_con("nil") -> first(nil_node);
lookup_con("::") -> first(::_node);

;;; unit

constype(lookup_tycon("unit"), []) -> unittype;

endsection; /* $-ml */

ml


(* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 24 1994
        Sectionised
--- Robert John Duncan, Dec  6 1991
        Fixed equality attribute for primitive types
--- Robert John Duncan, Feb 11 1991
        Changes to recording of datarep for cons and tycons
--- Robert John Duncan, Feb  4 1991
        -type_imptyvar- now boolean
 *)
