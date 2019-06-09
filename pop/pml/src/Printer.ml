(* --- Copyright University of Sussex 1994. All rights reserved. ----------
 * File:            C.all/pml/src/Printer.ml
 * Purpose:         PML: Controlling the top-level printer
 * Author:          Rob Duncan, Oct 31 1989 (see revisions)
 * Documentation:   HELP * PRINTER
 * Related Files:   print_data.p
 *)


signature Printer = sig

val depth           : int ref
val trace_depth     : int ref

val print_newline   : bool ref

val real_places     : int ref
val real_allplaces  : bool ref
val real_exponent   : bool ref

val string_quotes   : bool ref

end;    (* signature Printer *)


pop11


section $-ml;

ml_structure Printer : Printer = struct

ml_val depth : int ref = identof("ml_pr_level");

ml_val trace_depth : int ref = identof("ml_trace_pr_level");

ml_val print_newline : bool ref = identof("ml_pr_newline");

ml_val real_places : int ref = identof("ml_pr_places");

ml_val real_allplaces : bool ref = identof("ml_pr_allplaces");

ml_val real_exponent : bool ref = identof("ml_pr_exponent");

ml_val string_quotes : bool ref = identof("ml_pr_quotes");

ml_endstructure;    /* Printer */

endsection; /* $-ml */

ml

(* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Dec 20 1994
        Added trace_depth
--- Robert John Duncan, Nov 24 1994
        Sectionised
--- Rob Duncan, Jan  2 1990
        Added -print_newline-
 *)
