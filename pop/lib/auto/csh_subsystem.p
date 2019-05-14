/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.unix/lib/auto/csh_subsystem.p
 > Purpose:         Subsystem for csh
 > Author:          John Gibson, Jan 13 1993 (see revisions)
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

/*

csh_compile uses csh_send to communicate with a csh subprocess. It takes one
argument - a character repeater and passes strings delimited by linefeeds to
the csh process. The results are printed out. Suitable for use as POPCOMPILER
in VED etc.

csh_send takes one argument, a string, and sends it to a csh subprocess (which
it creates if necessary) as a command to be executed, and collects and prints
the resulting output.

The subprocess can be killed by giving FALSE to csh_send.
*/

uses shell_compile;

section $-lib => csh_compile, csh_fixed_prompt, csh_subsystem;

;;; User assignable prompt for use in ved_imcsh window. Override
;;; user's definition in case it changes.
global vars csh_fixed_prompt;
unless isstring(csh_fixed_prompt) then 'imcsh% ' -> csh_fixed_prompt endunless;

vars csh_send_process, csh_pid, csh_prompt;

define csh_send =
    shell_send(% ident csh_prompt, ident csh_fixed_prompt,
                 ident csh_send_process, ident csh_pid,
                 'set prompt=', '?\s', '/bin/csh', ['imcsh_from_ved' '-i'] %)
enddefine;

;;; N.B. This is a subsystem compiler, so must either be a closure or
;;; have cucharin dlocal to it
define csh_compile = shell_compile(% csh_send %) enddefine;


subsystem_add_new(
        "csh",
        csh_compile,
        '.csh',
        csh_fixed_prompt,
        [],
        'cshell'
);

constant csh_subsystem = "csh";

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, May 22 1996
        Changed section to $-lib
--- John Gibson, Apr 26 1993
        Moved in stuff from csh_compile; uses subsystem_add_new
 */
