(* --- Copyright University of Sussex 1990. All rights reserved. ----------
 * File:            C.all/pml/lib/Option.sig
 * Purpose:         Option signature
 * Author:          Rob Duncan, Jan 12 1990
 * Documentation:   HELP * OPTION
 * Related Files:   C.all/pml/lib/Option.ml
 *)


signature Option = sig

datatype 'a option =
    NONE
|   SOME of 'a

end;
