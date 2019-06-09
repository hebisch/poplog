(* --- Copyright University of Sussex 1994. All rights reserved. ----------
 * File:            C.all/pml/src/Memory.ml
 * Purpose:         PML: Controlling memory usage
 * Author:          Rob Duncan, Oct 27 1989 (see revisions)
 * Documentation:
 * Related Files:
 *)


signature Memory = sig

(* Memory Limits *)

val lolim           : int ref
val hilim           : int ref
val stacklim        : int ref

(* Garbage Collection *)

val gc              : unit -> unit
val gctrace         : bool ref
val usage           : int
val lock            : unit -> unit
val unlock          : unit -> unit

end;    (* Memory *)


pop11

section $-ml;

ml_structure Memory : Memory = struct

;;; Memory limits

ml_val hilim : int ref = identof("popmemlim");

ml_val lolim : int ref = identof("popminmemlim");

ml_val stacklim : int ref = identof("pop_callstack_lim");

;;; The garbage collector

ml_val gc : unit -> unit =
procedure() with_props gc with_nargs 1;
    ->, sysgarbage(), ml_unit;
endprocedure;

ml_val gctrace : bool ref = identof("popgctrace");

ml_val usage : int = popmemused;
(ID_PERM, "popmemused") -> val_access(lookup_var("usage"));

ml_val lock : unit -> unit = sys_lock_heap;

ml_val unlock : unit -> unit = sys_unlock_heap;

ml_endstructure;    /* Memory */

endsection; /* $-ml */

ml

(* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Dec 20 1994
        New treatment of identifiers
--- Robert John Duncan, Nov 24 1994
        Sectionised
--- Robert John Duncan, Feb 11 1991
        Substituted -val_ident- for -var_*ident-
--- Rob Duncan, Jan 31 1990
        Changed -stacklim- to identof "pop_callstack_lim"
 *)
