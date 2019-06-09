(* --- Copyright University of Sussex 1996. All rights reserved. ----------
 * File:            C.all/pml/src/Compile.ml
 * Purpose:         PML: Compiler functions and values
 * Author:          Rob Duncan, Oct 27 1989 (see revisions)
 * Documentation:
 * Related Files:
 *)


signature Compile = sig

(* Calling the Compiler *)

exception Use of string

val use             : string -> unit
val searchpath      : string list ref
val filetype        : string ref
val sigfiletype     : string ref

(* Compiler Flags *)

val autoload        : bool ref
val closure_rules   : bool ref
val warnings        : bool ref
val timer           : bool ref
val debug           : bool ref
val traceback       : bool ref
val quiet           : bool ref
val quiet_load      : bool ref

(* Localising Compiler Flags *)

val localise        : 'a ref -> 'a ref

end;


pop11

section $-ml;

ml_structure Compile : Compile = struct

;;; Calling the Compiler

ml_exception Use of string;

ml_val use : string -> unit =
procedure(name) with_props use;
    lconstant UseExn = exception("Use");
    lvars name, path;

    define dlocal pop_exception_handler(n, msg, idstring, severity);
        returnunless(severity == `E` or severity == `R`)(false);
        erasenum(n);
        raise(UseExn(name));
    enddefine;

    unless sourcefile(name) ->> path then raise(UseExn(name)) endunless;
    conspair(path, ml_uselist) -> ml_uselist;
    ml_unit;
endprocedure;

ml_val filetype : string ref = identof("ml_filetype");

ml_val sigfiletype : string ref = identof("ml_sigfiletype");

ml_val searchpath : string list ref = identof("ml_libdirs");

;;; Compiler flags

ml_val autoload : bool ref = identof("ml_autoloading");

ml_val closure_rules : bool ref = identof("ml_closure_rules");

ml_val timer : bool ref = identof("ml_timer");

ml_val debug : bool ref = identof("ml_compile_debug");

ml_val traceback : bool ref = identof("exn_traceback");

ml_val warnings : bool ref = identof("ml_warnings");

ml_val quiet : bool ref = identof("ml_quiet");

ml_val quiet_load : bool ref = identof("ml_quiet_load");

;;; Localising compiler flags

ml_val localise : 'a ref -> 'a ref =
procedure(r) -> r with_props localise;
    lvars r;
    ml_localise(r);
endprocedure;

ml_endstructure;    /* Compile */

endsection; /* $-ml */

ml

(* --- Revision History ---------------------------------------------------
--- Robert Duncan, Apr 26 1996
        Changes for new Poplog exception handling
--- Robert John Duncan, Dec  5 1994
        Added debug flag
--- Robert John Duncan, Nov 24 1994
        Sectionised
--- Robert John Duncan, Apr  6 1992
        Renamed -ml_*searchpath- to -ml_libdirs-
--- Robert John Duncan, Mar 18 1991
        Added -localise- function.
        Added dlocal -prm*ishap- to -use- in case of file name errors.
--- Robert John Duncan, Oct 30 1990
        Added filename argument to -Use- exception
--- Robert John Duncan, Oct  9 1990
        Exported -traceback- flag.
--- Rob Duncan, Jun 22 1990
        Replaced -unit- with -ml_unit-
 *)
