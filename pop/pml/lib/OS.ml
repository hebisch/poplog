(* --- Copyright University of Sussex 1996. All rights reserved. ----------
 * File:            C.all/pml/lib/OS.ml
 * Purpose:         Operating System Interface
 * Author:          Robert John Duncan, Feb  1 1991 (see revisions)
 * Documentation:   HELP * OS
 * Related Files:   C.all/pml/lib/OS.sig
 *)


external structure OS : OS = struct

uses sysdefs;

/* Exceptions */

ml_exception Error of string * string * string;
ml_exception NotImplemented of string;

lconstant
    procedure Error             = ml_valof("Error"),
    procedure NotImplemented    = ml_valof("NotImplemented"),
    procedure SOME              = ml_valof('Option.SOME'),
    NONE                        = ml_valof('Option.NONE'),
;

define lconstant os_exception_handler(n, msg, idstring, severity, name, arg);
    if severity == `E` or severity == `R` then
        $-ml$-format_message(n, msg) -> (msg, _);
        unless arg then
            nullstring -> arg;
        endunless;
        ml_raise(Error(ml_constuple(name, msg, arg, 3)));
    endif;
    false;
enddefine;


/* O/S Version */

ml_val version : unit -> string =
procedure(_) with_props version;
    lvars version = [%
        lvars item;
        for item in sys_os_type do
            item;
            ;;; stop at version number
            quitif(isnumber(item));
        endfor;
    %];
    allbutfirst(1, allbutlast(1, version sys_>< nullstring));
endprocedure;

ml_val isunix : unit -> bool =
procedure(_) with_props isunix;
    DEF UNIX;
endprocedure;

ml_val isvms : unit -> bool =
procedure(_) with_props isvms;
    DEF VMS;
endprocedure;


/* The Current Process State */

ml_val arglist0: unit -> string list =
procedure(_) with_props arglist0;
    maplist(poparglist0, copy);
endprocedure;

ml_val arglist : unit -> string list =
procedure(_) with_props arglist;
    maplist(poparglist, copy);
endprocedure;

ml_val username : unit -> string =
procedure(_) with_props username;
    popusername;
endprocedure;

ml_val translate : string -> string Option.option =
procedure(name) with_props translate;
    dlocal pop_exception_handler = os_exception_handler(% 'translate', name %);
    lvars value = systranslate(name);
    value and SOME(value) or NONE;
endprocedure;

ml_val pid : unit -> int =
procedure(_) with_props pid;
    poppid;
endprocedure;

ml_val pwd : unit -> string =
procedure(_) with_props pwd;
    dlocal pop_exception_handler = os_exception_handler(% 'pwd', false %);
    current_directory;
endprocedure;

ml_val cd : string -> unit =
procedure(dir) with_props cd;
    dlocal pop_exception_handler = os_exception_handler(% 'cd', dir %);
    dir -> current_directory;
    ml_unit;
endprocedure;

ml_val date : unit -> string =
procedure(_) with_props date;
    dlocal pop_exception_handler = os_exception_handler(% 'date', false %);
    sysdaytime();
endprocedure;


/* Process Control */

ml_val obey : string -> int =
procedure(cmd) with_props obey;
    dlocal pop_exception_handler = os_exception_handler(% 'obey', cmd %);
#_IF DEF UNIX
    if systranslate('SHELL') then cmd, `!` -> cmd endif;
#_ENDIF
    sysobey(cmd);
    pop_status;
endprocedure;

ml_val wait : unit -> (int * int) Option.option =
procedure(_) with_props wait;
    dlocal pop_exception_handler = os_exception_handler(% 'wait', false %);
    lvars pid = syswait();
    pid and SOME(ml_constuple(pid, pop_status, 2)) or NONE;
endprocedure;

ml_val kill : int -> bool =
procedure(pid) with_props kill;
#_IF DEF UNIX or DEF VMS
    dlocal pop_exception_handler = os_exception_handler(% 'kill', false %);
    syskill(pid);
#_ELSE
    ml_raise(NotImplemented('kill'));
#_ENDIF
endprocedure;


/* UNIX-only */

ml_val fork : unit -> int Option.option =
procedure(_) with_props fork;
#_IF DEF UNIX
    dlocal pop_exception_handler = os_exception_handler(% 'fork', false %);
    lvars pid = sysfork();
    pid and SOME(pid) or NONE;
#_ELSE
    ml_raise(NotImplemented('fork'));
#_ENDIF
endprocedure;

ml_val execve : string -> string list -> string list -> 'a =
procedure(cmd, arglist, envlist) with_props execve;
#_IF DEF UNIX
    dlocal pop_exception_handler = os_exception_handler(% 'execve', cmd %);
    sysexecute(cmd, arglist, envlist);
#_ELSE
    ml_raise(NotImplemented('execve'));
#_ENDIF
endprocedure;

ml_val envlist : unit -> string list =
procedure(_) with_props envlist;
#_IF DEF UNIX
    popenvlist;
#_ELSE
    ml_raise(NotImplemented('envlist'));
#_ENDIF
endprocedure;


/* VMS-only */

ml_val spawn : string -> string -> string -> bool -> int Option.option =
procedure(cmd, input, output, wait) with_props spawn;
#_IF DEF VMS
    dlocal pop_exception_handler = os_exception_handler(% 'spawn', cmd %);
    if cmd = nullstring then false -> cmd endif;
    if input = nullstring then false -> input endif;
    if output = nullstring then false -> output endif;
    lvars pid = sysspawn(cmd, input, output, wait);
    pid and SOME(pid) or NONE;
#_ELSE
    ml_raise(NotImplemented('spawn'));
#_ENDIF
endprocedure;

ml_val attach : int -> bool =
procedure(pid) with_props attach;
#_IF DEF VMS
    dlocal pop_exception_handler = os_exception_handler(% 'attach', false %);
    sysattach(pid);
#_ELSE
    ml_raise(NotImplemented('attach'));
#_ENDIF
endprocedure;

ml_endstructure;    (* structure OS *)


(* --- Revision History ---------------------------------------------------
--- Robert Duncan, Apr 26 1996
        Changes for new Poplog exception handling
 *)
