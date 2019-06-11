/* --- Copyright University of Sussex 1991.  All rights reserved. ---------
 > File:           C.unix/lib/ved/diredpipein.p
 > Purpose:        With LIB VED_DIRED, run command and read in output
 > Author:         Aaron Sloman, Oct 1 1988 (see revisions)
 > Documentation:   HELP *DIRED, *DIRED.SHORT
 > Related Files:   LIB *VED_DIRED
 */

#_TERMIN_IF DEF POPC_COMPILING

uses ved_dired;


section $-dired => diredpipein dired_current_file;

global vars dired_current_file;

unless isboolean(dired_current_file) then
    true -> dired_current_file;
endunless;

vars use_dired_current_file;    ;;; altered in ved_dired

define global diredpipein(command, file);
    ;;; Run the shell command, with file replacing `%` in command string
    ;;; and read any output into a temporary file.
    lvars i, c, command, file,
        display_file = true,    ;;; controls displaying of new file
        arglength = datalength(command);
    lconstant arglist=['/bin/csh' '-ce' 0];     ;;; for sysexecute
    dlocal vedargument,
        show_output_on_status=false;    ;;; force vedpipein to use VED buffer
    ;;; replace '%' with file in command
    cons_with consstring{%
             for i to arglength do
                 subscrs(i,command) ->c;
                 if c == `%` then explode(file) else c endif
             endfor
             %} -> arglist(3);
    ;;; call vedpipein, but via valof, to postpone autoloading
    vedputmessage('Please wait');
    valof("vedpipein")(
        '/bin/csh',
        arglist,
        unless dired_current_file and use_dired_current_file then
            ;;; use new file
            vedopen(dired_tempfile(file))
        else false ->> display_file
        endunless,
        procedure;
            false ->>vedcompileable ->>vedwriteable ->>vedbreak ->vednotabs;
            8 -> vedindentstep;         ;;; for output of "ls" etc.
        endprocedure,
        display_file,
        dired_header sys_>< arglist(3))     ;;; header for output file
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Jun  9 1991
    Fixed control over whether file is displayed or not
--- Aaron Sloman, Aug 10 1989
    Made dired_current_file default to -true-
--- Aaron Sloman, Aug  1 1989
    Introduced dired_current_file, to make it insert in current file,
    and use_dired_current_file to override it.
 */
