(* --- Copyright University of Sussex 1990. All rights reserved. ----------
 * File:            C.all/pml/lib/Option.ml
 * Purpose:         Datatype indicating the presence or absence of a value
 * Author:          Rob Duncan, Jan 12 1990
 * Documentation:   HELP * OPTION
 * Related Files:   C.all/pml/lib/Option.sig
 *)


structure Option : Option = struct

datatype 'a option =
    NONE
|   SOME of 'a

end;
