/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/src/compile.p
 > Purpose:         Prolog: calling the compiler from Prolog
 > Author:          Robert John Duncan, Jul 13 1993 (see revisions)
 > Documentation:
 > Related Files:   C.all/plog/src/prolog_compile.p
 */

uses prolog_subsystem;

section prolog;

constant
    procedure ( prolog_file_type, );

vars
    procedure prolog_syscompile_warn,
    default_compile_mode,
;

weak vars
    $-vedvedname,
;

;;; ========================================================================

;;; compile/2:
;;;     entry point to the compiler from Prolog

define compile\/2(files, mode);
    lvars files, mode;

    define lconstant Compile(file, report);
        lvars   file, report;
        dlocal  default_compile_mode;
        if prolog_checkspec(file, "+", 1) then
            "consult" -> default_compile_mode;
            prolog_arg(1, file) -> file;
        elseif prolog_checkspec(file, "-", 1) then
            "reconsult" -> default_compile_mode;
            prolog_arg(1, file) -> file;
        elseif prolog_checkspec(file, "?", 1) then
            "query" -> default_compile_mode;
            prolog_arg(1, file) -> file;
        endif;
        subsystem_compile(file, "prolog");
        if report then
            default_compile_mode -> report;
            if report == "query" then "queri" -> report endif;
            format('~w ~wed~n', [^file ^report]);
        endif;
    enddefine;

    define dlocal prolog_syscompile_warn(file);
        lvars file;
        if file and VEDLOADED then file -> VEDWEAK vedvedname endif;
    enddefine;

    prolog_deref(mode) -> mode;
    unless lmember(mode, #_<[consult reconsult query]>_#) then
        mishap(mode, 1, 'ILLEGAL COMPILE MODE');
    endunless;
    ;;; mode is the default compilation mode
    dlocal default_compile_mode = mode;
    prolog_deref(files) -> files;
    while prolog_checkspec(files, ".", 2) do
        Compile(prolog_arg(1, files), true);
        prolog_arg(2, files) -> files;
    endwhile;
    unless files == [] then
        Compile(files, false);
    endunless;
    chain(prolog_apply_continuation);
enddefine;

;;; prolog_library:
;;;     called by library/1

define prolog_library(name) -> found;
    lvars   name, found;
    dlocal  default_compile_mode = "reconsult",
            pop_default_type = prolog_file_type();
    unless isword(name) then
        mishap(name, 1, 'LIBRARY NAME NEEDED');
    endunless;
    unless subsystem_libcompile(name, []) ->> found then
        unless pop_default_type = '.pl' then
            '.pl' -> pop_default_type;
            subsystem_libcompile(name, []) -> found;
        endunless;
    endunless;
enddefine;

endsection;     /* prolog */

PROLOG

:- module prolog.

compile(Files) :-
    compile(Files, reconsult).

reconsult(Files) :-
    compile(Files, reconsult).

consult(Files) :-
    compile(Files, consult).

[File|Files] :-
    compile([File|Files], consult).
[].

library(Name) :-
    prolog_evaltrue(prolog_library(quote(Name))),
    !.
library(Name) :-
    format("Can't find library file: ~w~n", [Name]),
    fail.

prolog_language(Name) :-
    atom(Name),
    !,
    see(inchan),
    prolog_eval(apply(quote(Name),hd("c"),updater(valof(sys_compiler_subsystem)))).
prolog_language(String) :-
    name(Name, String),
    !,
    see(inchan),
    prolog_eval(apply(quote(Name),hd("c"),updater(valof(sys_compiler_subsystem)))).

prolog_current_language(Name) :-
    prolog_eval(apply(hd("c"),valof(sys_compiler_subsystem)), Name).

:- endmodule prolog.


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Aug  2 1993
        Changed to use prolog_file_type to get the default file extension.
 */
