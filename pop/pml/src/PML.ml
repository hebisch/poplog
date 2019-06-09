(* --- Copyright University of Sussex 1994. All rights reserved. ----------
 * File:            C.all/pml/src/PML.ml
 * Purpose:         Module containing all PML built-in structures
 * Author:          Robert John Duncan, Oct 30 1990 (see revisions)
 * Documentation:
 * Related Files:
 *)

structure PML = struct
    structure Int = Int
    structure Real = Real
    structure String = String
    structure List = List
    structure Vector = Vector
    structure Array = Array
    structure Combinators = Combinators
    structure System = System
end;

(* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 24 1994
        Added Vector and Array
 *)
