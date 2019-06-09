(* --- Copyright University of Sussex 1991. All rights reserved. ----------
 * File:            C.all/pml/lib/OS.sig
 * Purpose:         Operating System Interface (signature)
 * Author:          Robert John Duncan, Feb  1 1991
 * Documentation:   HELP * OS
 * Related Files:   C.all/pml/lib/OS.ml
 *)


signature OS = sig

    (* Exceptions *)

    exception Error of string * string * string
    exception NotImplemented of string

    (* O/S Version *)

    val version     : unit -> string
    val isunix      : unit -> bool
    val isvms       : unit -> bool

    (* The Current Process State *)

    val arglist0    : unit -> string list
    val arglist     : unit -> string list
    val username    : unit -> string
    val pid         : unit -> int
    val pwd         : unit -> string
    val cd          : string -> unit
    val translate   : string -> string Option.option
    val date        : unit -> string

    (* Process Control *)

    val obey        : string -> int
    val wait        : unit -> (int * int) Option.option
    val kill        : int -> bool

    (* UNIX-only *)

    val envlist     : unit -> string list
    val fork        : unit -> int Option.option
    val execve      : string -> string list -> string list -> 'a

    (* VMS-only *)

    val spawn       : string -> string -> string -> bool -> int Option.option
    val attach      : int -> bool

end;    (* signature OS *)
