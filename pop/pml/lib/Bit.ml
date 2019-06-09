(* --- Copyright University of Sussex 1990. All rights reserved. ----------
 * File:            C.all/pml/lib/Bit.ml
 * Purpose:         Bitwise operations on integers
 * Author:          Rob Duncan, Apr  9 1990
 * Documentation:   HELP * BIT
 * Related Files:   C.all/pml/lib/Bit.sig
 *)


external structure Bit : Bit = struct

ml_exception BitNumber;

lconstant BitNumber = ml_valof("BitNumber");

define lconstant CheckInt(n) -> n;
    lvars n;
    unless isinteger(n) then
        ml_raise(BitNumber);
    endunless;
enddefine;

define lconstant CheckPosInt(n) -> n;
    lvars n;
    unless isinteger(n) and n fi_>= 0 then
        ml_raise(BitNumber);
    endunless;
enddefine;

ml_val || : int * int -> int = nonop ||;
ml_val && : int * int -> int = nonop &&;
ml_val &~ : int * int -> int = nonop &&~~;
ml_val &? : int * int -> bool = nonop &&/=_0;
ml_val ~~ : int -> int;

ml_val set : int -> int -> int = nonop ||;
ml_val mask : int -> int -> int = nonop &&;
ml_val clear : int -> int -> int = nonop &&~~;
ml_val test : int -> int -> bool = nonop &&/=_0;
ml_val complement : int -> int = nonop ~~;

ml_val shiftr : int -> int -> int =
procedure(i, n) with_props shiftr;
    lvars i, n;
    i >> CheckInt(n);
endprocedure;

ml_val >> : int * int -> int = ml_valof("shiftr");

ml_val shiftl : int -> int -> int =
procedure(i, n) with_props shiftl;
    lvars i, n;
    i << CheckInt(n);
endprocedure;

ml_val << : int * int -> int = ml_valof("shiftl");

ml_val setbit : int -> int -> int =
procedure(n, i) with_props setbit;
    lvars n, i;
    true -> testbit(i, CheckPosInt(n));
endprocedure;

ml_val clearbit : int -> int -> int =
procedure(n, i) with_props clearbit;
    lvars n, i;
    false -> testbit(i, CheckPosInt(n));
endprocedure;

ml_val testbit : int -> int -> bool =
procedure(n, i) with_props testbit;
    lvars n, i;
    testbit(i, CheckPosInt(n));
endprocedure;

ml_val size : int -> int = integer_length;

ml_val bitcount : int -> int = integer_bitcount;

end;    (* structure Bit *)

infix 7 && &~ &?
infix 6 ||
infix 5 << >>
