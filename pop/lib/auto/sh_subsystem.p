/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.unix/lib/auto/sh_subsystem.p
 > Purpose:         Subsystem for sh
 > Author:          John Gibson, Jan 13 1993 (see revisions)
 > Documentation:   HELP * IMSH   HELP * CSH_COMPILE  HELP * VED_IMCSH
 > Related Files:
 */
compile_mode :pop11 +strict;

/*
Adapted from dcl_compile, roger evans, august 1983

sh_compile uses sh_send to communicate with a sh subprocess. It takes one
argument - a character repeater and passes strings delimited by linefeeds to
the sh process. The results are printed out. Suitable for use as POPCOMPILER
in VED etc.

sh_send takes one argument, a string, and sends it to a sh subprocess (which
it creates if necessary) as a command to be executed, and collects and prints
the resulting output.

The subprocess can be killed by giving FALSE to sh_send.
*/

section $-lib => sh_compile, sh_fixed_prompt, sh_subsystem;

uses shell_compile;

;;; User assignable prompt for use in ved_imsh window. Override
;;; user's definition in case it changes.
vars sh_fixed_prompt;
unless isstring(sh_fixed_prompt) then 'imsh$ ' -> sh_fixed_prompt endunless;

vars sh_send_process, sh_pid, sh_prompt;

define sh_send =
    shell_send(% ident sh_prompt, ident sh_fixed_prompt,
                 ident sh_send_process, ident sh_pid,
                 'PS1=', '>\s', '/bin/sh', ['imsh_from_ved' '-i'] %)
enddefine;

;;; N.B. This is a subsystem compiler, so must either be a closure or
;;; have cucharin dlocal to it
define sh_compile = shell_compile(% sh_send %) enddefine;

subsystem_add_new(
        "sh",
        sh_compile,
        '.sh',
        sh_fixed_prompt,
        [],
        'shell'
);

constant sh_subsystem = "sh";

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, May 22 1996
        Changed section to $-lib
--- John Gibson, Apr 26 1993
        Move in stuff from sh_compile; uses subsystem_add_new
 */
