(* --- Copyright University of Sussex 1996.  All rights reserved. ---------
 * File:            C.all/pml/src/System.ml
 * Purpose:         PML: System functions and values
 * Author:          Robert Duncan & Simon Nichols, Feb 13 1989 (see revisions)
 * Documentation:   HELP * SYSTEM
 *)


signature System = sig

structure Compile   : Compile
structure Memory    : Memory
structure Printer   : Printer

val version         : string list ref
val revision        : int

val reset           : unit -> 'a
val restart         : unit -> 'a
val exit            : unit -> 'a

val make            : {image : string, lock : bool, share : bool,
                        banner : bool, init : bool} -> bool
val save            : string -> bool
val restore         : string -> 'a

val helppath        : string list ref
val teachpath       : string list ref
val readprompt      : string ref

end;


pop11

section $-ml;

ml_structure System : System = struct

/* Include other system modules as sub-structures */
ml_structure Compile : Compile = Compile;
ml_structure Memory  : Memory = Memory;
ml_structure Printer : Printer = Printer;

/* System Version */

ml_val version : string list ref = identof("ml_version_messages");

ml_val revision : int = ml_version_number * 100 + ml_revision_number;

/* Restarting and Leaving PML */

ml_val reset : unit -> 'a = setpop;

ml_val restart : unit -> 'a =
procedure() with_nargs 1;
    clearstack();
    chainto(setpop, procedure;
        "ml" -> subsystem;
        syssetup();
        setpop();
    endprocedure);
endprocedure;

ml_val exit : unit -> 'a =
procedure() with_nargs 1;
    ->, sysexit();
endprocedure;

/* Saving and Restoring the System State */

lvars image_id = 'PML';

define lconstant restart(banner, init);
    lvars   banner, init;
    dlocal  interrupt, pop_nobanner, pop_noinit, poparglist = [];
    unless banner then true -> pop_nobanner endunless;
    unless init then true -> pop_noinit endunless;
    syssetup();
enddefine;

ml_val make : {banner:bool,image:string,init:bool,lock:bool,share:bool} -> bool =
procedure(id) with_props make;
    lvars id, share, image, lock, init, banner;
    explode(id) -> share -> lock -> init -> image -> banner;
    if sys_fname_extn(image) = nullstring then image <> '.psv' -> image endif;
    if lock then
        image_id <> ' +' <> lowertoupper(sys_fname_nam(image)) -> image_id;
        sys_lock_system(image, share, image_id);
    else
        syssave(image);
    endif;
    if dup(/* result */) then
        restart(banner, init);
    endif;
endprocedure;

ml_val save : string -> bool =
procedure(image) with_props save;
    lvars image;
    if sys_fname_extn(image) = nullstring then image <> '.psv' -> image endif;
    if dup(syssave(image)) then
        restart(false, false);
    endif;
endprocedure;

ml_exception Restore of string * string list;

ml_val restore : string -> 'a =
procedure(image) with_props restore;
    define dlocal pop_exception_handler(n, msg, idstring, severity);
        lconstant Restore = ml_valof("Restore");
        returnunless(severity == `E` or severity == `R`)(false);
        lvars culprits;
        format_message(n, msg) -> (msg, culprits);
        raise(
            Restore(ml_constuple(msg,
            maplist(culprits, nonop sys_><(%nullstring%)),
            2)));
    enddefine;
    if sys_fname_extn(image) = nullstring then image <> '.psv' -> image endif;
    sysrestore(image);
endprocedure;

/* Miscellaneous Variables */

ml_val helppath : string list ref = identof("ml_helpdirs");
ml_val teachpath : string list ref = identof("ml_teachdirs");
ml_val readprompt : string ref = identof("ml_readprompt");

ml_endstructure;

endsection; /* $-ml */

ml

(* --- Revision History ---------------------------------------------------
--- Robert Duncan, Apr 26 1996
        Changes for new Poplog exception handling
--- Robert John Duncan, Nov 24 1994
        Sectionised
--- Robert John Duncan, Apr  6 1992
        Renamed search lists for help and teach
--- Robert John Duncan, Jun 26 1991
        Changed -restart- to dlocal -pop_noX-
--- Robert John Duncan, Jun  6 1991
        Replaced -%noX- with -pop_noX-
--- Robert John Duncan, Oct 17 1990
        Added -make- function; changed -version- & -revision- yet again.
        Changed -restore- to raise an exception rather than abort.
--- Robert John Duncan, Oct  9 1990
        Version back as it was; deleted banner.
        Fixed '.psv' extension in save/restore.
--- John Williams, Oct  4 1990
        Now uses -syssetup-
--- Robert John Duncan, Aug  8 1990
        Changed -restart- to use new subsystem startup routine.
        Added -banner- and -revision- and changed type of -version-.
--- Rob Duncan, Apr 19 1990
        Included -Compile-, -Memory- etc. as substructures
--- Rob Duncan, Mar 14 1990
        -ml_startup- no longer takes an argument
--- Rob Duncan, Jan 31 1990
        Added -restart- and -teachpath-
--- Rob Duncan, Oct 31 1989
        Moved out a lot of things to "Compile.ml", "Memory.ml" and
        "Printer.ml"
--- Rob Duncan, Oct 26 1989
        Added exception -Use- and values -print_places- and -quiet_load-
 *)
