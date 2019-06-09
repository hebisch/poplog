(* --- Copyright University of Sussex 1990. All rights reserved. ----------
 * File:            C.all/pml/lib/Protect.ml
 * Purpose:         Adding entry and exit code to functions
 * Author:          Robert John Duncan, Jul  6 1990
 * Related Files:   C.all/pml/lib/Protect.sig
 *)


external structure Protect : Protect = struct

ml_val protect : (unit -> 'a) * ('a -> unit) -> ('b -> 'c) -> 'b -> 'c =
procedure(g, f, x) with_props protect;
    lvars g, f, x;
    procedure();
        dlocal % ml_subscrtuple(1,g)(ml_unit), ml_subscrtuple(2,g)() -> %;
        fast_apply(x, f);
    endprocedure();
endprocedure;

ml_val guard : 'a ref -> ('b -> 'c) -> 'b -> 'c =
procedure(r, f, x) with_props guard;
    lvars r, f, x;
    procedure();
        dlocal % ml_cont(r) %;
        fast_apply(x, f);
    endprocedure();
endprocedure;

end;    (* structure Protect *)
